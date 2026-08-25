#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <sys/ioctl.h>
#include <unistd.h>

CAMLprim value caml_get_terminal_size(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  
  struct winsize w;
  /* When stdout is not a TTY (piped/headless), the ioctl fails and would
   * leave w uninitialized; fall back to a standard 80x24 terminal. */
  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &w) != 0 || w.ws_col == 0 ||
      w.ws_row == 0) {
    w.ws_col = 80;
    w.ws_row = 24;
  }

  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(w.ws_col));  /* columns (width) */
  Store_field(result, 1, Val_int(w.ws_row));  /* rows (height) */
  
  CAMLreturn(result);
}

