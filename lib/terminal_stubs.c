#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <sys/ioctl.h>
#include <unistd.h>

CAMLprim value caml_get_terminal_size(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  
  struct winsize w;
  ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);
  
  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(w.ws_row));
  Store_field(result, 1, Val_int(w.ws_col));
  
  CAMLreturn(result);
}

