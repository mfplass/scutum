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
#ifndef SCUTUM_H
#define SCUTUM_H
#include <stdint.h>
#include <stdbool.h>
#include "mm.h"

typedef intptr_t Int;           /* A native (untagged) range-limited integer */

/***
 A Value is large enough to hold a void* (for heap things)
 but is often not a pointer at all.
 ***/
typedef intptr_t Value;
#define MAX_INTPTR ((intptr_t)(((uintptr_t)-1)>>1))

/***
 We use the term 'word' to refer to storage unit just large
 enough to hold a Value. We assume that a word is at least 32 bits,
 and that normal heap-allocated things are doubleword-aligned,
 and that the machine is byte-addressable, so that there
 are at least 3 bits to use for tags in the low-order end of
 a Value.  For the 32-bit word case, these bits are as follows:
   000 - a heap-allocated object with a type field
   xx1 - a fixnum
   010 - special values, tags
   100 - a pair - heap-allocated, no type field*
   110 - a character
 * The pair tag will be #b1000 for a word size of 64 bits.
 A peculiarity of this representation is that 0 does
 not represent a legal Value!  This is because its tag
 bits would lead us to believe it to be a pointer, but it
 is not.
***/
#define TAG_MASK (2*(sizeof (intptr_t))-1)

#define IV(v) ((intptr_t)(v))
#define UV(v) ((uintptr_t)(v))
#define VAL(x) ((Value)(x))

/** fixnum values are immediate, tagged by the low order bit  **/
typedef Value FixnumValue;
#define FIXNUM_P(v)        (IV(v)&1)
#define FIXNUMS_P(v,w)     (IV(v)&IV(w)&1)
#define FIXNUMABLE(i)      ((IV(i)^(IV(i)<<1))>=0)
#define FIXNUM_FROM_INT(i) ((IV(i)<<1)|1)
#define INT_FROM_FIXNUM(v) (IV(v)>>1)

/***
 Characters are also immediate.  May be bigger than 8 bits
 We byte-align for aesthetic reasons.
 ***/
typedef Value CharValue;
#define CHAR_TAG 6
#define CHAR_P(v) ((IV(v)&0xFF)==CHAR_TAG)
#define CHARVAL_FROM_UINT(i) ((UV(i)<<8)|CHAR_TAG)
#define UINT_FROM_CHARVAL(v) (UV(v)>>8)

/***
 These are special values that we choose to make immediate.
 There is no really good reason to make #t one of these,
 but why not?
 ***/
#define SPECIAL(n) IV(((n)<<4)+10)
#define MAKE_BOOLEAN(x) SPECIAL(!!(x))
#define HASH_F     MAKE_BOOLEAN(false)
#define HASH_T     MAKE_BOOLEAN(true)
#define EMPTY_LIST SPECIAL(2)
#define UNDEF      SPECIAL(3)
#define WRONG_TYPE SPECIAL(4)
#define EOF_OBJECT SPECIAL(5)
#define MULTVALUES SPECIAL(6)
#define NYI        SPECIAL(7)
#define BEFORETHUNK SPECIAL(8)
#define AFTERTHUNK SPECIAL(9)
/** The UNSPEC special is designed so that memset with UNSPEC_BYTE works. **/
#define UNSPEC_BYTE 0x5a
#define UNSPEC     SPECIAL(UV(-1)/255*UNSPEC_BYTE/16)

/* R5RS and IEEE scheme semantics - #f is the only false Value */
typedef Value BooleanValue;
#define FALSE_P(v) (IV(v)==HASH_F)
#define TRUE_P(v)  (IV(v)!=HASH_F)
#define BOOLEAN_P(v) ((IV(v)&~(HASH_F^HASH_T))==HASH_F)

/* The empty list. */
#define NULL_P(v)  (IV(v)==EMPTY_LIST)

/***
 Each pair is allocated on an odd word address, and is 2 words long.
 All heap-allocated values other than pairs must be on an
 even word boundary.  A word is the memory unit big enough
 to hold a void* - note it may be as big as a double -
 or bigger!
 ***/
typedef Value PairValue;
typedef struct PairRep {
    Value car;
    Value cdr;
} *Pair;

#define PAIR_TAG      (sizeof (intptr_t))
#define PAIR_P(v)  ((IV(v)&TAG_MASK)==PAIR_TAG)

/***
 These type codes are used to distinguish allocated values
 and are stored in the first word in tagged form.  We assign
 codes for the types of the immediate values above, even though
 those codes are not manifest in the representations.
 ***/
enum type_code {
    code_undefined,
    code_boolean,               /* Not manifest - immediate */
    code_empty_list,            /* Not manifest - immediate */
    code_pair,                  /* Not manifest - see above */
    code_character,             /* Not manifest - immediate */
    code_fixnum,                /* Not manifest - immediate */
    code_float64,
    code_bignum,
    code_rational,
    code_complex,
    code_number,
    code_nan,                   /* Not used in objects */
    code_string,
    code_symbol,
    code_vector,
    code_port,
    code_proc,
    code_activation,
    code_thread_data,
    code_code,                  /* Not manifest - see below */
    code_box,
    code_filep
};
typedef Value TaggedTypeCode;
#define TAGTY(typecode) ((IV(typecode)<<4)+2)
#define TAGTY_P(v) ((IV(v)&15)==2)
#define UNTAGTY(v) ((enum type_code)(IV(v) >> 4))

#define HEAP_P(v) ((IV(v)&TAG_MASK) == 0)
#define TTCODE(v) (*((TaggedTypeCode *)(v)))
#define HEAP_CODE_P(v, typecode) (HEAP_P(v) && TTCODE(v) == TAGTY(typecode))

#define TCODE(x) \
              (TAGTY_P(TTCODE(x)) ? UNTAGTY(TTCODE(x)) : code_code)
#define SC_GET_CODE(x) (HEAP_P(x) ? TCODE(x) \
                    : FIXNUM_P(x) ? code_fixnum \
                    : CHAR_P(x) ? code_character \
                    : PAIR_P(x) ? code_pair \
                    : NULL_P(x) ? code_empty_list \
                    : BOOLEAN_P(x) ? code_boolean \
                    : code_undefined)

#define NUM_CODE(x) (HEAP_P(x) ? TCODE(x) : FIXNUM_P(x) ? code_fixnum : code_nan)
#define NUMBER_P(x) (FIXNUM_P(x) || (HEAP_P(x) && (TCODE(x) >= code_fixnum) && (TCODE(x) <= code_number)))
#define COMPLEX_P(x) (FIXNUM_P(x) || (HEAP_P(x) && (TCODE(x) >= code_fixnum) && (TCODE(x) <= code_complex)))
#define RATIONAL_P(x) (FIXNUM_P(x) || (HEAP_P(x) && (TCODE(x) >= code_fixnum) && (TCODE(x) < code_complex)))
#define NUM_FLAG  1             /* all number flag words look like fixnums */
#define NUM_INEXACT 2           /* for all number types */
#define NUM_NEG   4             /* for bignums */
#define EXACT_P(x) (FIXNUM_P(x) || (HEAP_P(x) && !((((Float64) (x))->flags) & NUM_INEXACT)))
#define INEXACT_P(x) (!(EXACT_P(x)))

typedef Value NumberValue;
typedef struct Float64_Rep {
    TaggedTypeCode tc;          /* code_float64 */
    Value flags;                /* exactness */
    double value;
} *Float64;

typedef Value FixnumList;
typedef struct Bignum_Rep {
    TaggedTypeCode tc;          /* code_bignum */
    Value flags;                /* exactness, sign */
    Value exponent;             /* scale mag by BASE**exponent */
    FixnumList mag;             /* list of fixnums, least significant first */
} *Bignum;

typedef struct Rational_Rep {
    TaggedTypeCode tc;          /* code_rational */
    Value flags;                /* exactness */
    Value numerator;            /* integer */
    Value denominator;          /* positive integer */
} *Rational;

typedef struct Complex_Rep {
    TaggedTypeCode tc;          /* code_complex */
    Value flags;
    Value real_part;
    Value imag_part;
} *Complex;

/***
 The List type is used for proper lists.  This should
 actually be a union type (with EMPTY_LIST), but
 let's not get carried away.
 ***/
typedef Value ListValue;
typedef Pair List;

/***
 Aggregates - basic strings and vectors
 These share a simple header, which is followed by
 the data.
 ***/
typedef struct Aggregate_Header {
    TaggedTypeCode tc;          /* code_string, ... */
    Value length;               /* a fixnum */
    /* followed by the elements themselves */
} *Aggregate;

/** String - this variant is packed 8 bits per character **/
#define SCHARS(v) ((uint8_t*)((Aggregate)(v)+1))
#define STRING_P(v) (HEAP_CODE_P(v, code_string))
#define STRING_LENGTH(s) ((size_t)(INT_FROM_FIXNUM(((String)(s))->length)))
typedef Value StringValue;
typedef Aggregate String;

/* Vector */
typedef Value VectorValue;
#define VECTOR_ARR(v) ((Value*)((Aggregate)(v)+1))
#define VECTOR_P(v) (HEAP_CODE_P(v, code_vector))
#define VECTOR_LENGTH(v) ((size_t)(INT_FROM_FIXNUM(((Vector)(v))->length)))
typedef Aggregate Vector;

/* Symbol */
typedef Value SymbolValue;
#define SYMBOL_P(v) (HEAP_CODE_P(v, code_symbol))
typedef struct Symbol_Rep {
    TaggedTypeCode tc;          /* code_symbol */
    String pval;                /* a string */
} *Symbol;

/* Environment */
typedef struct Env_Rep {
    TaggedTypeCode tc;          /* code_env */
    Value names;                /** for local env, the lambda list.
                                    #f for global env **/
    Value v;                    /** a vector of values (local)
                                    or alist
                                    or hash table of pairs (global) */
    Value parent;               /* lexically containing env */
} *NOTUSEDEnv;
typedef Value Env;

/* NativeCode (not a Scheme value) */
typedef struct Activation_Record *Activation;
typedef struct ThreadData_Record *ThData;
typedef Activation(*NativeCode) (Activation act, ThData th);

/* CodeHandle (not a Scheme value) */
/* This is special in that it does NOT have a tagged type code - */
/* the value of i can be anything but a tagged type code. */
#define CODE_P(v) (HEAP_P(v) && !TAGTY_P(TTCODE(v)))
typedef struct CodeHandle_Rep { /* immutable - represents compiled code */
    Value i;                    /* Interpretation of i,j,k is private */
    Value j;
    Value k;
    NativeCode pc;
} *CodeHandle;

/* Procedure */
typedef Value ProcedureValue;
#define PROC_P(v) (HEAP_CODE_P(v, code_proc))
typedef struct Proc_Rep {
    TaggedTypeCode tc;          /* code_proc */
    CodeHandle entry_pt;        /* pointer to code for proc */
    Value name;                 /* normally a symbol or string */
    Pair rep;                   /** similar to cdr of lambda form:
                                  car - the lambda's parameters -
                                       e.g. x, (x y), or (x y . z)
                                  cadr - if a string, the doc string
                                  cddr - anything proc's impl wants **/
    Env env;                    /* containing lexical environment */
} *Proc;

/* port */
typedef Value PortValue;
#define PORT_P(v) (HEAP_CODE_P(v, code_port))
#define INPUT_PORT_P(v) (PORT_P(v) && TRUE_P(((Port)(v))->read_char))
#define OUTPUT_PORT_P(v) (PORT_P(v) && TRUE_P(((Port)(v))->write_char))

typedef struct Port_Rep {
    TaggedTypeCode tc;          /* code_port */
    Value name;                 /* for identification */
    Value charbuf;              /* a putback char or eof object, or #f */
    Value data;                 /* instance data */
    Proc read_char;             /* (lambda (self-data wait?) ... char) or #f */
    Proc write_char;            /* (lambda (self-data char) ...) or #f */
    Proc close;                 /* (lambda (self-data) ...) */
} *Port;

/* To be able to keep track of things like FILE * we need a box. */
typedef struct Whatever_Rep {
    TaggedTypeCode tc;          /* code_box, code_filep, ... */
    void *sysdata;              /* the thing */
} *Whatever;

/* Activation (not a Scheme Value) */
#define ACTIVATION_P(v) (HEAP_CODE_P(v, code_activation))
/***
 This represents a partially-executed procedure activation.
 ***/
struct Activation_Record {
    TaggedTypeCode tc;          /* code_activation */
    CodeHandle codeh;
    Env env;                    /* active environment */
    Value a;                    /* multi-use registers */
    Value b;                    /* second arg */
    Value r;                    /* list of remaining args */
    Activation cont;            /* how to continue */
    Activation link;            /* to hotter one */
};

/* Thread data (not a Scheme Value) */
struct ThreadData_Record {
    TaggedTypeCode tc;          /* code_thread_data */
    Value narg;                 /* Argument count */
    Value val;                  /* Result */
    Value culprit;              /* For error reporting */
    List dynamic;               /* alist - the dynamic environment */
    List pairs;                 /* avail list of pairs */
    Value global;               /* global environment */
    PortValue inport;           /* current input port */
    PortValue outport;          /* current output port */
    Value symtrace;             /* trace symbols to stderr - debug aid */
    Pair dbgtracebuf;           /* trace symbols - circular buffer */
    /* lots of other stuff here, eventually */
};
#define NOTH ((ThData)0)

#define RETURN(value) { th->val = VAL(value); return act->cont; }
#define DEMAND_ARGS(N) { if (th->narg!=FIXNUM_FROM_INT(N)) {return WrongNumArgs(act, th);} }
#define ARGS_N(N) (th->narg==FIXNUM_FROM_INT(N))
Activation WrongNumArgs(Activation act, ThData th);
#define DEMAND(tst,culp) { if (!(tst)) { th->culprit = VAL(culp); return WrongType(act, th, SC_MODULE_ID + __LINE__); } }
Activation WrongType(Activation act, ThData th, int lineno);
/* The CONS macro assumes a local declaration of th - use NOTH if necessary */
#define CONS(a,d) VAL(sc_th_cons(th, VAL(a),VAL(d)))
#define U_CAR(x) (((Pair)(x))->car)
#define U_CDR(x) (((Pair)(x))->cdr)
typedef void (*PrimInitProc) (ThData th, CodeHandle c);

/***
These simple versions of the character predicates assume contiguous
and ordered alphabets and numbers, and treat anything outside of the
ASCII range as ordinary characters.  They do not check for character
values, but will not break if passed something else.
***/
#define SC_CHAR_SP VAL(CHARVAL_FROM_UINT(' '))
#define SC_CHAR_a VAL(CHARVAL_FROM_UINT('a'))
#define SC_CHAR_z VAL(CHARVAL_FROM_UINT('z'))
#define SC_CHAR_A VAL(CHARVAL_FROM_UINT('A'))
#define SC_CHAR_Z VAL(CHARVAL_FROM_UINT('Z'))
#define SC_CHAR_0 VAL(CHARVAL_FROM_UINT('0'))
#define SC_CHAR_9 VAL(CHARVAL_FROM_UINT('9'))
#define CHAR_LOWER_CASE_P(x) (SC_CHAR_a <= VAL(x) && VAL(x) <= SC_CHAR_z)
#define CHAR_UPPER_CASE_P(x) (SC_CHAR_A <= VAL(x) && VAL(x) <= SC_CHAR_Z)
#define CHAR_NUMERIC_P(x)    (SC_CHAR_0 <= VAL(x) && VAL(x) <= SC_CHAR_9)
#define CHAR_WHITESP_P(x)    (VAL(x) <= SC_CHAR_SP)

#include "support.h"
#endif
