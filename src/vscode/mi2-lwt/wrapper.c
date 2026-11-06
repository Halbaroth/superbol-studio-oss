#if !defined(_WIN32)
#include <limits.h>
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>

CAMLprim value
mi2_lwt_pipe_buf (value unit) {
#if !defined(_WIN32)
  CAMLparam1 (unit);
  CAMLreturn (Val_int (PIPE_BUF));
#else
  caml_invalid_argument ("mi2_lwt_pipe_buf: not supported");
#endif
}
