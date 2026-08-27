/*
 * Pty - drive a real Matcha binary on a real pseudo-terminal.
 *
 * WHAT THIS LAYER SEES THAT THE OTHERS DO NOT
 * -------------------------------------------
 * - termios. Raw mode is only raw on a TTY. The bug where ISIG was left on
 *   (so Ctrl+C killed the process before any key handler or the terminal
 *   restore could run) cannot exist on a pipe, so a pipe-based test cannot
 *   find it. Here it shows up as an exit BY SIGNAL - see [exitStatus].
 * - The DSR round trip. Inline startup writes ESC[6n and waits for the
 *   terminal to say where the cursor is; [drain] answers it from the vterm,
 *   so the runtime's bottomRow tracking runs for real.
 * - Mode switching. Alternate screen, bracketed paste, mouse reporting and
 *   the kitty keyboard protocol are only emitted on the TTY path.
 * - SIGWINCH. [resize] does a TIOCSWINSZ on the master, which makes the
 *   kernel deliver a real SIGWINCH to the child's foreground process group.
 * - Batched input. [send] is ONE write, so several keystrokes land in a
 *   single read() on the child side - the case that broke event batching.
 *
 * DETERMINISM
 * -----------
 * Nothing here asserts after a fixed sleep. [drain] is a poll-until-quiet
 * loop: it reads until no bytes have arrived for `quietMs`, or until
 * `timeoutMs` overall. Every byte read is fed to BOTH a Vterm (so tests can
 * assert on the SCREEN) and a raw byte log (so tests can assert on the mode
 * sequences, which never reach the screen).
 *
 * HANG SAFETY
 * -----------
 * These sessions run WITHOUT MATCHA_HEADLESS - that is the point - so the
 * CLAUDE.md hang trap applies with full force: a session that is not reaped
 * leaves a raw-mode child running forever. Always use [withSession], which
 * kills and reaps in a Fun.protect finaliser even when the test body raises.
 */

type exitStatus =
  /* Normal termination with this exit code. */
  | Exited(int)
  /* Killed by this signal number. Distinguishing this from Exited is the
   * whole point of the ISIG bug class: a Ctrl+C that reaches the kernel
   * instead of the application shows up here as Signaled(2). */
  | Signaled(int)
  /* The child was still alive after the timeout and had to be SIGKILLed. */
  | TimedOut;

let exitStatusToString = (s: exitStatus): string =>
  switch (s) {
  | Exited(c) => "Exited(" ++ string_of_int(c) ++ ")"
  | Signaled(n) => "Signaled(" ++ string_of_int(n) ++ ")"
  | TimedOut => "TimedOut"
  };

type session = {
  pid: int,
  master: Unix.file_descr,
  vt: Vterm.t,
  log: Buffer.t,
  /* Set once the child has been reaped, so cleanup is idempotent. */
  mutable reaped: option(exitStatus),
  mutable masterClosed: bool,
};

external ptySpawn:
  (string, array(string), array(string), int, int) => (int, Unix.file_descr) =
  "caml_pty_spawn";

external ptyResize: (Unix.file_descr, int, int) => unit = "caml_pty_resize";

/* Environment for the child.
 *
 * MATCHA_HEADLESS and the MATCHA_WIDTH/MATCHA_HEIGHT overrides are stripped
 * deliberately: this harness exists to exercise the REAL terminal path, and
 * a stray MATCHA_HEADLESS=1 in the developer's shell would silently turn
 * every case below into a much weaker test. TERM is pinned so the child
 * cannot make terminal-dependent decisions we did not choose. */
let childEnv = (): array(string) => {
  let drop = ["MATCHA_HEADLESS", "MATCHA_WIDTH", "MATCHA_HEIGHT", "TERM"];
  let keep = (entry: string): bool =>
    switch (String.index_opt(entry, '=')) {
    | None => true
    | Some(i) => !List.mem(String.sub(entry, 0, i), drop)
    };
  Array.append(
    Array.of_list(List.filter(keep, Array.to_list(Unix.environment()))),
    [|"TERM=xterm-256color"|],
  );
};

/* Resolve a path that may be given relative to the repo root, so tests work
 * both under `dune runtest` (cwd = _build/default/test) and under
 * `dune exec test/run_tests.exe` (cwd = repo root). */
let resolve = (path: string): string =>
  if (Sys.file_exists(path)) {
    path;
  } else if (Sys.file_exists("../" ++ path)) {
    "../" ++ path;
  } else {
    let stripped =
      if (String.length(path) > 7 && String.sub(path, 0, 7) == "_build/") {
        path;
      } else {
        "_build/default/" ++ path;
      };
    stripped;
  };

let spawn =
    (~width: int=80, ~height: int=24, path: string, args: list(string))
    : session => {
  let prog = resolve(path);
  if (!Sys.file_exists(prog)) {
    failwith("Pty.spawn: no such binary: " ++ prog);
  };
  let argv = Array.of_list([prog, ...args]);
  let (pid, master) = ptySpawn(prog, argv, childEnv(), width, height);
  {
    pid,
    master,
    vt: Vterm.create(~width, ~height),
    log: Buffer.create(8192),
    reaped: None,
    masterClosed: false,
  };
};

let vterm = (s: session): Vterm.t => s.vt;
let byteLog = (s: session): string => Buffer.contents(s.log);

/* Write to the child's terminal. ONE write() call, on purpose: a multi-key
 * string then arrives in a single read() on the child side, which is what
 * exercises the runtime's event batching (deliverAll + flushDirty). */
let send = (s: session, data: string): unit => {
  let b = Bytes.of_string(data);
  ignore(Unix.write(s.master, b, 0, Bytes.length(b)));
};

let dsrReply = ((row, col): (int, int)): string =>
  Printf.sprintf("\027[%d;%dR", row + 1, col + 1);

/* What this harness answers an OSC 11 background-color query with: a DARK
 * background (#1e1e1e), in the 4-hex-digit X form real terminals use, ST
 * terminated. Fixed rather than configurable on purpose - the point is that
 * the query gets an answer at all, so the reply path runs for real. */
let oscBackgroundReply = "\027]11;rgb:1e1e/1e1e/1e1e\027\\";

/* Read whatever is available right now. Returns the number of bytes read,
 * or -1 for end of stream (the child closed its side / exited: a pty master
 * gives EOF on macOS and EIO on Linux once the last slave fd is gone). */
let readAvailable = (s: session, buf: Bytes.t): int =>
  if (s.masterClosed) {
    (-1);
  } else {
    switch (Unix.read(s.master, buf, 0, Bytes.length(buf))) {
    | 0 => (-1)
    | n => n
    | exception (Unix.Unix_error(Unix.EIO, _, _)) => (-1)
    | exception (Unix.Unix_error(Unix.EINTR, _, _)) => 0
    | exception (Unix.Unix_error(Unix.EAGAIN, _, _)) => 0
    | exception (Unix.Unix_error(Unix.EBADF, _, _)) => (-1)
    };
  };

/* Feed a chunk to the model and the log, then answer any DSR or OSC query
 * the chunk contained the way a real terminal would. */
let absorb = (s: session, chunk: string): unit => {
  Buffer.add_string(s.log, chunk);
  Vterm.feed(s.vt, chunk);
  List.iter(
    pos =>
      try(send(s, dsrReply(pos))) {
      | _ => ()
      },
    Vterm.takeDsrReplies(s.vt),
  );
  /* One reply per OSC 11 query, exactly like the DSR auto-reply above. Any
     other OSC code is consumed and left unanswered, which is also what a
     terminal does with a set (rather than query) request. */
  List.iter(
    ((code, _payload)) =>
      if (code == 11) {
        try(send(s, oscBackgroundReply)) {
        | _ => ()
        };
      },
    Vterm.takeOscQueries(s.vt),
  );
};

/* Read from the child until it has been quiet for `quietMs`, or `timeoutMs`
 * has elapsed overall, or the stream ends. Never asserts, never sleeps
 * blindly - this is the only way a test should wait for a frame. */
let drain = (~quietMs: int=150, ~timeoutMs: int=3000, s: session): unit => {
  let buf = Bytes.create(8192);
  let start = Unix.gettimeofday();
  let deadline = start +. float_of_int(timeoutMs) /. 1000.0;
  let quiet = float_of_int(quietMs) /. 1000.0;
  let lastData = ref(start);
  let rec loop = () => {
    let now = Unix.gettimeofday();
    if (now >= deadline) {
      ();
    } else if (now -. lastData^ >= quiet) {
      ();
    } else {
      let wait = min(deadline -. now, quiet -. (now -. lastData^));
      let (ready, _, _) =
        try(Unix.select([s.master], [], [], max(0.005, wait))) {
        | Unix.Unix_error(Unix.EINTR, _, _) => ([], [], [])
        | Unix.Unix_error(Unix.EBADF, _, _) => ([], [], [])
        };
      switch (ready) {
      | [] => loop()
      | _ =>
        let n = readAvailable(s, buf);
        if (n < 0) {
          (); /* stream ended - nothing more will ever arrive */
        } else {
          if (n > 0) {
            absorb(s, Bytes.sub_string(buf, 0, n));
            lastData := Unix.gettimeofday();
          };
          loop();
        };
      };
    };
  };
  loop();
};

/* Resize the pty. The kernel records the new size AND sends SIGWINCH to the
 * child's foreground process group, so this is a real resize, not a
 * simulated one. The model is resized to match, so grid assertions after
 * the next drain are made against the new geometry. */
let resize = (s: session, ~width: int, ~height: int): unit => {
  ptyResize(s.master, width, height);
  Vterm.resize(s.vt, ~width, ~height);
};

let closeMaster = (s: session): unit =>
  if (!s.masterClosed) {
    s.masterClosed = true;
    try(Unix.close(s.master)) {
    | _ => ()
    };
  };

let statusOf = (st: Unix.process_status): exitStatus =>
  switch (st) {
  | Unix.WEXITED(c) => Exited(c)
  | Unix.WSIGNALED(n) => Signaled(n)
  | Unix.WSTOPPED(n) => Signaled(n)
  };

/* Wait for the child to exit, keeping the master DRAINED while we wait (a
 * child blocked writing its restore sequence into a full pty buffer would
 * never exit).
 *
 * The master is closed only AFTER the child is reaped, never before: closing
 * it early sends the child a SIGHUP / makes its reads fail, which would
 * itself change the exit status we are trying to observe. On timeout the
 * child is SIGKILLed, reaped, and reported as TimedOut. */
let waitExit = (~timeoutMs: int=5000, s: session): exitStatus =>
  switch (s.reaped) {
  | Some(st) => st
  | None =>
    let buf = Bytes.create(8192);
    let deadline = Unix.gettimeofday() +. float_of_int(timeoutMs) /. 1000.0;
    let rec poll = (): exitStatus =>
      switch (Unix.waitpid([Unix.WNOHANG], s.pid)) {
      | (0, _) =>
        if (Unix.gettimeofday() >= deadline) {
          (try(Unix.kill(s.pid, Sys.sigkill)) {
           | _ => ()
           });
          switch (Unix.waitpid([], s.pid)) {
          | _ => TimedOut
          | exception _ => TimedOut
          };
        } else {
          let (ready, _, _) =
            try(Unix.select([s.master], [], [], 0.02)) {
            | Unix.Unix_error(_, _, _) => ([], [], [])
            };
          switch (ready) {
          | [] => ()
          | _ =>
            let n = readAvailable(s, buf);
            if (n > 0) {
              absorb(s, Bytes.sub_string(buf, 0, n));
            };
          };
          poll();
        }
      | (_, st) => statusOf(st)
      | exception (Unix.Unix_error(Unix.ECHILD, _, _)) => Exited(0)
      | exception (Unix.Unix_error(Unix.EINTR, _, _)) => poll()
      };
    let st = poll();
    s.reaped = Some(st);
    closeMaster(s);
    st;
  };

/* Kill and reap unconditionally. Safe to call twice. */
let cleanup = (s: session): unit => {
  switch (s.reaped) {
  | Some(_) => ()
  | None =>
    (try(Unix.kill(s.pid, Sys.sigkill)) {
     | _ => ()
     });
    switch (Unix.waitpid([], s.pid)) {
    | (_, st) => s.reaped = Some(statusOf(st))
    | exception _ => s.reaped = Some(TimedOut)
    };
  };
  closeMaster(s);
};

/* Run `f` against a fresh session, killing and reaping the child even if
 * `f` raises. Every test case must go through this: a leaked child is a
 * process left in raw mode on a pty forever. */
let withSession =
    (
      ~width: int=80,
      ~height: int=24,
      path: string,
      args: list(string),
      f: session => 'a,
    )
    : 'a => {
  let s = spawn(~width, ~height, path, args);
  Fun.protect(~finally=() => cleanup(s), () => f(s));
};

/* ============================================================================
 * Assertion helpers
 * ============================================================================ */

/* The whole screen, right-trimmed and newline-joined. */
let screen = (s: session): string => Vterm.text(s.vt);

/* Screen plus everything that scrolled off the top - "what the user could
 * see by scrolling back", which is where inline <Static> output ends up. */
let visible = (s: session): string => Vterm.fullText(s.vt);

/* Count non-overlapping occurrences of `needle` in `hay`. */
let countOccurrences = (hay: string, needle: string): int => {
  let hlen = String.length(hay);
  let nlen = String.length(needle);
  if (nlen == 0 || nlen > hlen) {
    0;
  } else {
    let rec go = (i, acc) =>
      if (i > hlen - nlen) {
        acc;
      } else if (String.sub(hay, i, nlen) == needle) {
        go(i + nlen, acc + 1);
      } else {
        go(i + 1, acc);
      };
    go(0, 0);
  };
};
