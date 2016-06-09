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
#include "scutum.h"

/* Some shorthand for list contruction */
#define __ sc_make_list_0
#define _ IV(0), SPECIAL(-1)

/* Shorthand for other constructors */
#define CVAL(ch) (VAL(CHARVAL_FROM_UINT(ch)))
static Value SYM(const char *s)
{
 return (VAL(sc_string_to_symbol(NOTH, sc_strlit(s))));
}

static Value QUOTE(Value v)
{
 return __(SYM("quote"), v, _);
}

static Value LIT(const char *s)
{
 return VAL(sc_strlit(s));
}

static Pair app1(Pair last, Value el)
{
 Pair l = sc_cons(el, EMPTY_LIST);
 last->cdr = VAL(l);
 return (l);
}

Value sc_bootstrap_reader_exp(void)
{
 ThData th = NOTH;
 Value chr_ATSI = CVAL('@');
 Value chr_BQUO = CVAL('`');
 Value chr_BSLA = CVAL('\\');
 Value chr_COMM = CVAL(',');
 Value chr_DOT = CVAL('.');
 Value chr_DQUO = CVAL('"');
 Value chr_HASH = CVAL('#');
 Value chr_LPAR = CVAL('(');
 Value chr_RPAR = CVAL(')');
 Value chr_SEMI = CVAL(';');
 Value chr_SQUO = CVAL('\'');
 Value chr_f = CVAL('f');
 Value chr_newline = CVAL('\n');
 Value chr_space = CVAL(' ');
 Value chr_t = CVAL('t');
 Value LESS_THAN = SYM("<");
 Value MINUS = SYM("-");
 Value QUOTE_DDD = QUOTE(SYM("..."));
 Value QUOTE_EMPTY_LIST = QUOTE(EMPTY_LIST);
 Value QUOTE_MINUS = QUOTE(SYM("-"));
 Value QUOTE_PLUS = QUOTE(SYM("+"));
 Value QUOTE_quasiquote = QUOTE(SYM("quasiquote"));
 Value QUOTE_quote = QUOTE(SYM("quote"));
 Value QUOTE_unquote = QUOTE(SYM("unquote"));
 Value QUOTE_unquote_splicing = QUOTE(SYM("unquote-splicing"));
 Value _closing = SYM("%closing");
 Value _dot = SYM("%dot");
 Value _eof_error = SYM("%eof-error");
 Value _if = SYM("if");
 Value _legal = SYM("%legal");
 Value _legal_or_end = SYM("%legal-or-end");
 Value _legal_qc = SYM("%legal-qc");
 Value _list = SYM("list");
 Value _named_chars_ = SYM("*named-chars*");
 Value _number_specials_ = SYM("*number-specials*");
 Value _number_start_ = SYM("*number-start*");
 Value _read = SYM("%read");
 Value _symbol_special_initials_ = SYM("*symbol-special-initials*");
 Value _symbol_specials_ = SYM("*symbol-specials*");
 Value _t = SYM("read-trace->");
 Value accum_list = SYM("accum-list");
 Value accum_number = SYM("accum-number");
 Value accum_string = SYM("accum-string");
 Value accum_string_escaped = SYM("accum-string-escaped");
 Value accum_symbol = SYM("accum-symbol");
 Value accum_unquote = SYM("accum-unquote");
 Value accum_vector = SYM("accum-vector");
 Value and = SYM("and");
 Value any = SYM("any");
 Value assq = SYM("assq");
 Value buf = SYM("buf");
 Value bufstr = SYM("bufstr");
 Value c = SYM("c");
 Value car = SYM("car");
 Value cdr = SYM("cdr");
 Value ch = SYM("ch");
 Value char_alphabetic = SYM("char-alphabetic?");
 Value char_ci_equal = SYM("char-ci=?");
 Value char_ci_in = SYM("char-ci-in");
 Value char_const = SYM("char-const");
 Value char_downcase = SYM("char-downcase");
 Value char_equal = SYM("char=?");
 Value char_numeric = SYM("char-numeric?");
 Value char_p = SYM("char?");
 Value char_upper_case = SYM("char-upper-case?");
 Value char_whitespace = SYM("char-whitespace?");
 Value cond = SYM("cond");
 Value cons = SYM("cons");
 Value d = SYM("d");
 Value define = SYM("define");
 Value demand_closing = SYM("demand-closing");
 Value eof_object = SYM("eof-object?");
 Value eq = SYM("eq?");
 Value eqv = SYM("eqv?");
 Value error = SYM("error");
 Value finish_dotted = SYM("finish-dotted");
 Value hash_dispatch = SYM("hash-dispatch");
 Value i = SYM("i");
 Value item = SYM("item");
 Value lambda = SYM("lambda");
 Value list2string = SYM("list->string");
 Value list2vector = SYM("list->vector");
 Value lookup_named_char = SYM("lookup-named-char");
 Value loop = SYM("loop");
 Value lower_first = SYM("lower-first");
 Value make_string = SYM("make-string");
 Value memq = SYM("memq");
 Value name = SYM("name");
 Value named_char_const = SYM("named-char-const");
 Value newline = SYM("newline");
 Value next = SYM("next");
 Value nullp = SYM("null?");
 Value number_or_peculiar = SYM("number-or-peculiar");
 Value or = SYM("or");
 Value pair = SYM("pair?");
 Value peek_char = SYM("peek-char");
 Value port = SYM("port");
 Value quote = SYM("quote");
 Value read_char = SYM("read-char");
 Value reverse = SYM("reverse");
 Value s = SYM("s");
 Value set = SYM("set!");
 Value set_cdr = SYM("set-cdr!");
 Value skip_comment = SYM("skip-comment");
 Value skip_to_token = SYM("skip-to-token");
 Value space = SYM("space");
 Value str = SYM("str");
 Value string2list = SYM("string->list");
 Value string2number = SYM("string->number");
 Value string2symbol = SYM("string->symbol");
 Value string_equal = SYM("string=?");
 Value string_length = SYM("string-length");
 Value string_ref = SYM("string-ref");
 Value symbol_char = SYM("symbol-char?");
 Value symbolize = SYM("symbolize");
 Value uq = SYM("uq");
 Value x = SYM("x");
 Value ans_thunk = __(lambda, EMPTY_LIST, _);
 Pair ans_last = (Pair)U_CDR(ans_thunk);
#define APA(el) { ans_last = app1(ans_last, el); }

    APA(__(define, __(_read, port, _),
          __(skip_to_token, __(_t, __(peek_char, port, _), _), port, _),  _))
    APA(__(define, __(next, port, _),
          __(read_char, port, _),
          __(_t, __(peek_char, port, _), _),  _))
    APA(__(define, __(bufstr, buf, _),
          __(list2string, __(reverse, buf, _), _),  _))
    APA(__(define, __(skip_to_token, ch, port, _),
           __(cond,
              __(__(eof_object, ch, _), ch, _),
              __(__(char_whitespace, ch, _),
                   __(skip_to_token, __(next, port, _), port, _),  _),
              __(__(char_equal, ch, chr_SEMI, _),
                   __(skip_comment, __(next, port, _), port, _),  _),
              __(__(or, __(char_numeric, ch, _),
                        __(memq, ch, _number_start_, _),  _),
                   __(accum_number, ch, QUOTE_EMPTY_LIST, port, _),  _),
              __(__(or, __(char_alphabetic, ch, _),
                        __(memq, ch, _symbol_special_initials_, _),  _),
                   __(symbolize, ch, QUOTE_EMPTY_LIST, port, _),  _),
              __(__(char_equal, ch, chr_LPAR, _),
                   __(next, port, _),
                   __(accum_list, __(_read, port, _),
                                  QUOTE_EMPTY_LIST,
                                  port,  _), _),
              __(__(char_equal, ch, chr_RPAR, _),
                   __(next, port, _),
                   _closing,  _),
              __(__(char_equal, ch, chr_HASH, _),
                   __(hash_dispatch, __(next, port, _), port, _),  _),
              __(__(char_equal, ch, chr_DQUO, _),
                   __(accum_string, __(next, port, _),
                                    QUOTE_EMPTY_LIST,
                                    port,  _), _),
              __(__(char_equal, ch, chr_SQUO, _),
                   __(next, port, _),
                   __(_list, QUOTE_quote, 
                             __(_legal, __(_read, port, _), _),   _),_),
              __(__(char_equal, ch, chr_BQUO, _),
                   __(next, port, _),
                   __(_list, QUOTE_quasiquote,
                             __(_legal, __(_read, port, _), _),  _),_),
              __(__(char_equal, ch, chr_COMM, _),
                   __(accum_unquote, __(next, port, _), port, _),  _),
              __(HASH_T, __(error, LIT("illegal,token"), ch, _), _),  _),_))
    APA(__(define, __(skip_comment, ch, port, _),
           __(cond,
              __(__(eof_object, ch, _), ch, _),
              __(__(char_equal, ch, chr_newline, _),
                   __(skip_to_token, __(next, port, _), port, _),  _),
              __(HASH_T,
                   __(skip_comment, __(next, port, _), port, _),  _),_),_))
    APA(__(define, __(accum_number, ch, buf, port, _),
           __(cond,
              __(__(or, __(char_numeric, ch, _),
                        __(char_ci_in, ch, _number_specials_, _),  _),
                __(accum_number, __(next, port, _),
                                 __(cons, ch, buf, _),
                                 port,  _), _),
              __(HASH_T,
                __(number_or_peculiar, __(bufstr, buf, _), _),  _),_),_))
    APA(__(define, __(symbolize, ch, buf, port, _),
           __(accum_symbol, __(next, port, _),
                            __(cons, __(char_downcase, ch, _), buf, _),
                            port,  _), _))
    APA(__(define, __(accum_symbol, ch, buf, port, _),
           __(cond,
              __(__(symbol_char, ch, _), __(symbolize, ch, buf, port, _), _),
              __(HASH_T, __(string2symbol, __(bufstr, buf, _), _), _),  _),_))
    APA(__(define, __(accum_vector, item, buf, port, _),
           __(cond,
              __(__(eof_object, item, _), __(_eof_error, _), _),
              __(__(eq, item, _closing, _),
                   __(list2vector, __(reverse, buf, _), _),  _),
              __(__(eq, item, _dot, _),
                       __(error, LIT("Illegal,vector"), _),  _),
              __(HASH_T,
                   __(accum_vector, __(_read, port, _),
                                    __(cons, item, buf, _),
                                    port,  _), _), _), _))
    APA(__(define, __(accum_list, item, buf, port, _),
           __(cond,
              __(__(eof_object, item, _), __(_eof_error, _), _),
              __(__(eq, item, _closing, _), __(reverse, buf, _), _),
              __(__(eq, item, _dot, _),
                   __(_if, __(nullp, buf, _),
                           __(error, LIT("Illegal,S_expression"), _),  _),
                   __(finish_dotted, __(_legal, __(_read, port, _), _),
                                     buf,
                                     port,  _), _),
              __(HASH_T,
                   __(accum_list, __(_read, port, _),
                                  __(cons, item, buf, _),
                                  port,  _), _), _), _))
    APA(__(define, __(finish_dotted, d, buf, port, _),
           __(cond,
              __(__(pair, buf, _),
                   __(__(lambda, __(c, _),
                         __(set, buf, __(cdr, c, _), _),
                         __(set_cdr, c, d, _),
                         __(finish_dotted, c, buf, port, _),  _),
                      buf,  _), _),
              __(HASH_T,
                   __(demand_closing, __(_read, port, _), _),
                   d,  _), _), _))
    APA(__(define, __(demand_closing, x, _),
           __(or, __(eq, x, _closing, _),
                  __(error, LIT("Malformed,dotted,list"), _),  _),_))
    APA(__(define, __(accum_string, ch, buf, port, _),
           __(cond,
              __(__(eof_object, ch, _), __(_eof_error, _), _),
              __(__(char_equal, ch, chr_DQUO, _),
                    __(next, port, _),
                    __(bufstr, buf, _),  _),
              __(__(char_equal, ch, chr_BSLA, _),
                    __(accum_string_escaped, __(next, port, _),
                                             buf,
                                             port,  _), _),
              __(HASH_T,
                    __(accum_string, __(next, port, _),
                                     __(cons, ch, buf, _),
                                     port,  _), _), _), _))
    APA(__(define, __(accum_string_escaped, ch, buf, port, _),
           __(accum_string, __(next, port, _),
                            __(cons, __(_legal_qc, ch, _), buf, _),
                            port,  _), _))
    APA(__(define, __(accum_unquote, ch, port, _),
           __(define, uq,
              __(cond,
                 __(__(eqv, ch, chr_ATSI, _),
                       __(next, port, _),
                       QUOTE_unquote_splicing,  _),
                 __(HASH_T, QUOTE_unquote, _), _),  _),
           __(_list, uq, __(_legal, __(_read, port, _), _), _),  _))
    APA(__(define, __(hash_dispatch, ch, port, _),
           __(cond,
              __(__(char_ci_equal, ch, chr_t, _),
                    __(next, port, _),
                    HASH_T,  _),
              __(__(char_ci_equal, ch, chr_f, _),
                    __(next, port, _),
                    HASH_F,  _),
              __(__(char_equal, ch, chr_BSLA, _),
                    __(char_const, __(next, port, _), port, _),  _),
              __(__(char_equal, ch, chr_LPAR, _),
                    __(next, port, _),
                    __(accum_vector, __(_read, port, _),
                                     QUOTE_EMPTY_LIST,
                                     port,  _), _),
              __(__(char_ci_in, ch, LIT("iebodx"), _),
                    __(accum_number, ch, __(_list, chr_HASH, _), port, _),  _),
              /* #! extension not in bootstrap */
              __(HASH_T, __(error, LIT("Bad,#"), ch, _), _),  _),_))
    APA(__(define, __(char_const, ch, port, _),
           __(cond,
              __(__(char_alphabetic, ch, _),
                    __(named_char_const, __(next, port, _),
                                         __(_list, ch, _),
                                         port,  _), _),
              __(HASH_T, __(next, port, _), ch, _),  _), _))
    APA(__(define, __(named_char_const, ch, buf, port, _),
           __(cond,
              __(__(char_alphabetic, ch, _),
                 __(named_char_const, __(next, port, _),
                    __(cons, __(char_downcase, ch, _),
                             __(lower_first, buf, _),  _),
                    port,  _), _),
              __(__(nullp, __(cdr, buf, _), _),
                    __(car, buf, _),  _),
              __(HASH_T,
                    __(lookup_named_char,
                       __(string2symbol, __(bufstr, buf, _), _),  _),_),_),_))
    APA(__(define, __(lower_first, x, _),
           __(cond,
              __(__(and, __(pair, x, _),
                         __(char_upper_case, __(car, x, _), _),  _),
                    __(cons, __(char_downcase, __(car, x, _), _),
                             __(cdr, x, _),  _), _),
              __(HASH_T, x, _),  _), _))
    APA(__(define, __(lookup_named_char, name, _),
           __(cdr, __(or, __(assq, name, _named_chars_, _),
                          __(error, LIT("Unknown,char,name"), name,_), _),_),_))
    APA(__(define, _symbol_special_initials_,
           __(string2list, LIT("!$%&*/:<=>?^_~"),_),  _))
    APA(__(define, _symbol_specials_,
           __(string2list, LIT("+-.@!$%&*/:<=>?^_~"),_),  _))
    APA(__(define, _number_start_, __(string2list, LIT("+-.@"), _), _))
    APA(__(define, _number_specials_, LIT("+-.@#/esfdliebodxabcdef"), _))
    APA(__(define, _named_chars_,
                   __(quote,
                      __(CONS(space, chr_space),
                         CONS(newline, chr_newline), _),  _),_))
    APA(__(define, __(symbol_char, x, _),
           __(and, __(char_p, x, _),
              __(or, __(char_alphabetic, x, _),
                     __(char_numeric, x, _),
                     __(memq, x, _symbol_specials_, _),  _),_),_))
    APA(__(define, __(number_or_peculiar, s, _),
           __(cond,
              __(__(string_equal, s, LIT("."), _), _dot, _),
              __(__(string_equal, s, LIT("+"), _), QUOTE_PLUS, _),
              __(__(string_equal, s, LIT("-"), _), QUOTE_MINUS, _),
              __(__(string_equal, s, LIT("..."), _), QUOTE_DDD, _),
              __(__(string2number, s, _), _),
              __(__(error, LIT("Malformed,number"), s, _), _),  _),_))
    APA(__(define, __(char_ci_in, ch, str, _),
           __(define, __(loop, i, _),
              __(cond,
                 __(__(LESS_THAN, i, FIXNUM_FROM_INT(0), _), HASH_F, _),
                 __(__(char_ci_equal, ch, __(string_ref, str, i, _), _),
                      HASH_T,  _),
                 __(HASH_T,
                      __(loop, __(MINUS, i, FIXNUM_FROM_INT(1),_),_),  _),_),_),
           __(loop, __(MINUS, __(string_length, str, _),
                    FIXNUM_FROM_INT(1),  _),_),_))
    APA(__(define, _closing,
           __(make_string, FIXNUM_FROM_INT(1), chr_RPAR, _),  _))
    APA(__(define, _dot,
           __(make_string, FIXNUM_FROM_INT(1), chr_DOT, _),  _))
    APA(__(define, __(_eof_error, _),
           __(error, LIT("Unexpected,EOF"), _),  _))
    APA(__(define, __(_legal, any, _),
           __(cond,
              __(__(eof_object, any, _), __(_eof_error, _), _),
              __(HASH_T, __(_legal_or_end, any, _), _),  _),_))
    APA(__(define, __(_legal_or_end, any, _),
           __(cond,
              __(__(eq, any, _closing, _),
                   __(error, LIT("Unbalanced,closing,parenthesis"), _),  _),
              __(__(eq, any, _dot, _),
                   __(error, LIT("Misplaced,dot"), _),  _),
              __(HASH_T, any, _),  _),_))
    APA(__(define, __(_legal_qc, ch, _),
           __(cond,
              __(__(eof_object, ch, _), __(_eof_error, _), _),
              __(__(memq, ch, QUOTE(__(chr_DQUO, chr_BSLA, _)), _), ch, _),
              __(HASH_T,
                 __(error, LIT("Invalid,escaped,character,in,string"), ch, _),
                           ch,  _),_),_))
    APA(__(lambda, __(port, _),
           __(_legal_or_end, __(_read, port, _), _),  _))
    return __(ans_thunk, _);
}
