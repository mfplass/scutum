#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "scutum.h"

#define Q ""
static const char bootstrapload[] =
Q "(define (load filename)"
Q   "(define port (open-stdio-file filename \"r\"))"
Q   "(define (loop exp)"
Q     "(if (not (eof-object? exp))"
Q         "(begin (interp exp)"
Q                "(loop (read port)) )))"
Q   "(loop #f)"
Q   "(close-port port)"
Q   ")" ;

int main(int argc, char **argv)
{
  volatile Activation act = 0;
  volatile ThData th = 0;
  sc_set_simple_stack_base(&act);
  th = sc_new_ThData();
  sc_define_primitives(th);
  sc_bootstrap_defeval(th, "%reader", sc_bootstrap_reader_exp());
  sc_eval_c_string(th, bootstrapload);
  th->outport = (PortValue) sc_stdout(th);
  th->inport = (PortValue) sc_stdin(th);
  sc_eval_c_string(th, "(load \"lib.scheme\")");
  sc_eval_c_string(th, "(%read-eval-print)");
}
