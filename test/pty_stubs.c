/*
 * pty_stubs.c - spawn a child on a real pseudo-terminal, for test/pty.re.
 *
 * WHY THIS EXISTS
 * ---------------
 * Every other layer of the test suite talks to Matcha through a pipe or an
 * in-process handle. Neither is a TTY, so neither exercises the code that
 * only runs on one: termios (raw mode, ISIG/IXON), the DSR round trip,
 * alternate-screen and mouse mode switching, SIGWINCH, and the restore
 * sequence on exit. A raw-mode bug that made Ctrl+C kill the process before
 * any handler or terminal restore could run was invisible to a pipe-based
 * suite - the pipe never had an ISIG to get wrong.
 *
 * PORTABILITY
 * -----------
 * Uses the POSIX pty API (posix_openpt/grantpt/unlockpt/ptsname) rather
 * than BSD's forkpty/openpty, so no -lutil is needed and the same code
 * builds on macOS and Linux. Follows the include/convention style of
 * lib/terminal_stubs.c.
 */

#if defined(__linux__)
/* glibc hides posix_openpt/grantpt/unlockpt/ptsname behind _XOPEN_SOURCE >=
 * 600; _GNU_SOURCE implies it and also exposes the ioctl numbers. Must come
 * before any include. */
#define _GNU_SOURCE
#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>

/* Copy an OCaml string array into a NULL-terminated C array. The copies are
 * made BEFORE fork() so the child never touches the OCaml heap. Returns NULL
 * on allocation failure. */
static char **copy_string_array(value v_arr) {
  mlsize_t n = Wosize_val(v_arr);
  char **out = (char **)malloc((n + 1) * sizeof(char *));
  mlsize_t i;
  if (out == NULL) return NULL;
  for (i = 0; i < n; i++) {
    out[i] = strdup(String_val(Field(v_arr, i)));
    if (out[i] == NULL) {
      mlsize_t k;
      for (k = 0; k < i; k++) free(out[k]);
      free(out);
      return NULL;
    }
  }
  out[n] = NULL;
  return out;
}

static void free_string_array(char **arr) {
  size_t i;
  if (arr == NULL) return;
  for (i = 0; arr[i] != NULL; i++) free(arr[i]);
  free(arr);
}

static void set_winsize(int fd, int width, int height) {
  struct winsize ws;
  memset(&ws, 0, sizeof(ws));
  ws.ws_col = (unsigned short)width;
  ws.ws_row = (unsigned short)height;
  ioctl(fd, TIOCSWINSZ, &ws);
}

/* caml_pty_spawn(prog, argv, env, width, height) -> (pid, master_fd)
 *
 * Opens a pty pair sized width x height, forks, and execs `prog` in the
 * child with the slave side as its controlling terminal and as fds 0/1/2.
 * The parent gets the master fd back and never sees the slave.
 *
 * The master fd is returned as an int; Unix.file_descr is an int on every
 * platform this test suite runs on (same representation OCaml's own unix
 * stubs rely on).
 */
CAMLprim value caml_pty_spawn(value v_prog, value v_argv, value v_env,
                              value v_width, value v_height) {
  CAMLparam5(v_prog, v_argv, v_env, v_width, v_height);
  CAMLlocal1(result);

  int master, slave;
  pid_t pid;
  char slave_path[1024];
  const char *name;
  char *prog = NULL;
  char **argv = NULL;
  char **envp = NULL;

  master = posix_openpt(O_RDWR | O_NOCTTY);
  if (master < 0) caml_failwith("pty: posix_openpt failed");
  if (grantpt(master) != 0) {
    close(master);
    caml_failwith("pty: grantpt failed");
  }
  if (unlockpt(master) != 0) {
    close(master);
    caml_failwith("pty: unlockpt failed");
  }
  name = ptsname(master);
  if (name == NULL) {
    close(master);
    caml_failwith("pty: ptsname failed");
  }
  if (strlen(name) >= sizeof(slave_path)) {
    close(master);
    caml_failwith("pty: slave path too long");
  }
  strcpy(slave_path, name);

  /* Size the pty BEFORE exec, so the child's very first
   * ioctl(TIOCGWINSZ) - Matcha calls it during Runtime.start - already
   * sees the size the test asked for. */
  set_winsize(master, Int_val(v_width), Int_val(v_height));

  prog = strdup(String_val(v_prog));
  argv = copy_string_array(v_argv);
  envp = copy_string_array(v_env);
  if (prog == NULL || argv == NULL || envp == NULL) {
    free(prog);
    free_string_array(argv);
    free_string_array(envp);
    close(master);
    caml_failwith("pty: out of memory");
  }

  pid = fork();
  if (pid < 0) {
    free(prog);
    free_string_array(argv);
    free_string_array(envp);
    close(master);
    caml_failwith("pty: fork failed");
  }

  if (pid == 0) {
    /* Child. Nothing here may touch the OCaml runtime; every string it
     * needs was copied out above. On any failure it _exit(127), which the
     * parent sees as a normal exit with a distinctive code. */
    close(master);
    if (setsid() < 0) _exit(127);
    slave = open(slave_path, O_RDWR);
    if (slave < 0) _exit(127);
#ifdef TIOCSCTTY
    /* On BSD/macOS opening the slave after setsid() is not enough to make
     * it the controlling terminal; this ioctl is. Harmless on Linux, where
     * the open already did it. */
    ioctl(slave, TIOCSCTTY, 0);
#endif
    set_winsize(slave, Int_val(v_width), Int_val(v_height));
    if (dup2(slave, 0) < 0) _exit(127);
    if (dup2(slave, 1) < 0) _exit(127);
    if (dup2(slave, 2) < 0) _exit(127);
    if (slave > 2) close(slave);
    /* A test harness must not inherit a weird signal disposition. */
    signal(SIGPIPE, SIG_DFL);
    execve(prog, argv, envp);
    _exit(127);
  }

  /* Parent. */
  free(prog);
  free_string_array(argv);
  free_string_array(envp);

  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(pid));
  Store_field(result, 1, Val_int(master)); /* Unix.file_descr */
  CAMLreturn(result);
}

/* caml_pty_resize(master_fd, width, height) -> unit
 *
 * TIOCSWINSZ on the MASTER side. The kernel both records the new size (so
 * the child's TIOCGWINSZ reports it) and delivers SIGWINCH to the pty's
 * foreground process group - which is what makes this a real test of
 * Matcha's resize handling rather than a simulated one.
 */
CAMLprim value caml_pty_resize(value v_fd, value v_width, value v_height) {
  CAMLparam3(v_fd, v_width, v_height);
  struct winsize ws;
  memset(&ws, 0, sizeof(ws));
  ws.ws_col = (unsigned short)Int_val(v_width);
  ws.ws_row = (unsigned short)Int_val(v_height);
  if (ioctl(Int_val(v_fd), TIOCSWINSZ, &ws) != 0) {
    caml_failwith("pty: TIOCSWINSZ failed");
  }
  CAMLreturn(Val_unit);
}
