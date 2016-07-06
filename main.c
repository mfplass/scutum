/*-
 * Copyright (c) 2016 Michael F. Plass
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "scutum.h"
#include "lib.h"

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
  sc_eval_c_string(th, lib_dot_scheme);
  sc_eval_c_string(th, "(%read-eval-print)");
  return 0;
}
