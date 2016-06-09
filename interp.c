#define SC_MODULE_ID 1000
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "scutum.h"
#include "primitives.h"
#include "interp_items.h"

#define ATOM sc_c_string_to_symbol
#define LIST sc_make_list_0
#define _ IV(0), SPECIAL(-1)

typedef struct Syntax_Symbols {
    struct Aggregate_Header hdr;
    SymbolValue quote_sy;
    SymbolValue begin_sy;
    SymbolValue set_sy;
    SymbolValue define_sy;
    SymbolValue lambda_sy;
    SymbolValue if_sy;
    SymbolValue and_sy;
    SymbolValue or_sy;
    SymbolValue cond_sy;
    SymbolValue else_sy;
} *SyntaxSymbols;

static SyntaxSymbols getspecialsymbols_j(CodeHandle codeh)
{
    if (!VECTOR_P(codeh->j)) {
        SyntaxSymbols s = 0;
        int n = (sizeof(*s) - sizeof(s->hdr)) / sizeof(SymbolValue);
        s = (SyntaxSymbols) sc_make_vector(n, UNSPEC);
        s->quote_sy = (Value) sc_symlit("quote");
        s->begin_sy = (Value) sc_symlit("begin");
        s->set_sy = (Value) sc_symlit("set!");
        s->define_sy = (Value) sc_symlit("define");
        s->lambda_sy = (Value) sc_symlit("lambda");
        s->if_sy = (Value) sc_symlit("if");
        s->and_sy = (Value) sc_symlit("and");
        s->or_sy = (Value) sc_symlit("or");
        s->cond_sy = (Value) sc_symlit("cond");
        s->else_sy = (Value) sc_symlit("else");
        codeh->j = (Value) s;
    }
    return ((SyntaxSymbols) (codeh->j));
}

static Value gethandles(CodeHandle codeh)
{
    Vector v = (Vector) (codeh->i);
    int i = 0;
    if (VECTOR_P(v))
        return ((Value) v);
    v = sc_make_vector(k_num, UNSPEC);
    for (i = 0; i < k_num; i++) {
        CodeHandle ch = sc_new_CodeHandle(codex[i]);
        ch->i = (Value) v;
        VECTOR_ARR(v)[i] = (Value) ch;
    }
    codeh->i = (Value) v;
    return ((Value) v);
}

static CodeHandle get_code(Value v, enum kode_name k)
{
    CodeHandle ans = 0;
    assert(VECTOR_P(v) && UV(k) < VECTOR_LENGTH(v));
    ans = (CodeHandle) (VECTOR_ARR(v)[k]);
    assert(CODE_P(ans));
    return ans;
}

Activation scp_symtrace(Activation act, ThData th)
{
    Value old = th->symtrace;
    if (th->narg == FIXNUM_FROM_INT(1)) {
        th->symtrace = act->a;
    }
    else {
        DEMAND_ARGS(0);
    }
    RETURN(old);
}

Activation scp_interp(Activation act, ThData th)
{
    return sc_interp_enter(act, th);
}

Activation sc_interp_enter(Activation act, ThData th)
{
    Value kodes = gethandles(act->codeh);
    if (th->narg == FIXNUM_FROM_INT(1)) {
        act->b = EMPTY_LIST;
    }
    else {
        DEMAND_ARGS(2);
    }
    act->env = EMPTY_LIST;
    act->codeh = get_code(kodes, k_interp_expr);
    return act;
}

static Value sc_lambda(Value rep, Value env, Value kodes)
{
    CodeHandle ch = sc_new_CodeHandle(&sc_interpreted_proc_enter);
    Proc proc = sc_new_Proc(ch, HASH_F);
    proc->rep = (List) rep;
    proc->env = env;
    ch->i = U_CAR(rep);
    ch->j = U_CDR(rep);
    ch->k = kodes;
    return (VAL(proc));
}

Activation sc_interp_expr(Activation act, ThData th)
{
    Value expr = act->a;
    Value env = act->b;
    Value kodes = act->codeh->i;
    th->narg = FIXNUM_FROM_INT(1);      /* for error cases */
    if (NUMBER_P(expr) || CHAR_P(expr) || STRING_P(expr) || BOOLEAN_P(expr))
        RETURN(expr);
    if (SYMBOL_P(expr)) {
        Value binding = sc_assq(expr, env);
        th->dbgtracebuf->car = expr;
        th->dbgtracebuf = (Pair) (th->dbgtracebuf->cdr);
        if ((th->symtrace) == HASH_T)
            fprintf(stderr, "~%s~ ", SCHARS(((Symbol) expr)->pval));
        if (!PAIR_P(binding))
            binding = sc_assq(expr, th->global);
        DEMAND(PAIR_P(binding), expr);
        if (U_CDR(binding) == UNDEF) {
            fprintf(stderr, "[[[undefined: %s]]]", SCHARS(((Symbol) expr)->pval));
            U_CDR(binding) = UNSPEC;    /* Prevent cascading error */
            DEMAND(false, expr);
        }
        RETURN(U_CDR(binding));
    }
    if (!HEAP_P(act->link)) {
        return sc_prime_act_pump(act, th);
    }
    if (PAIR_P(expr)) {
        Value op = U_CAR(expr);
        if (SYMBOL_P(op)) {
            SyntaxSymbols sy = getspecialsymbols_j(act->codeh);
            if (op == sy->quote_sy) {
                DEMAND(PAIR_P(U_CDR(expr)), expr);
                RETURN(U_CAR(U_CDR(expr)));
            }
            if (op == sy->begin_sy) {
                act->a = U_CDR(expr);
                act->codeh = get_code(kodes, k_interp_eval_sequence);
                return act;
            }
            if (op == sy->set_sy) {
                Value name;
                CodeHandle interp1 = act->codeh;
                Value binding;
                DEMAND((sc_list_length(expr) == 3) && SYMBOL_P(name = U_CAR(U_CDR(expr))), expr);
                binding = sc_assq(name, env);
                if (!PAIR_P(binding))
                    binding = sc_assq(name, th->global);
                DEMAND(PAIR_P(binding) && U_CDR(binding) != UNDEF, name);
                act->a = binding;
                act->codeh = get_code(kodes, k_interp_finish_set);
                act = act->link;
                act->codeh = interp1;
                act->a = U_CAR(U_CDR(U_CDR(expr)));
                act->b = env;
                return act;
            }
            if (op == sy->define_sy) {
                Value name;
                CodeHandle interp1 = act->codeh;
                Value binding;
                intptr_t explen = sc_list_length(expr);
                Value proc = 0;
                DEMAND(explen >= 3, expr);
                name = U_CAR(U_CDR(expr));
                if (PAIR_P(name)) {     /** MIT-style **/
                    proc = sc_lambda(VAL(sc_cons(U_CDR(name), U_CDR(U_CDR(expr)))), env, kodes);
                    name = U_CAR(name);
                }
                else {
                    DEMAND(explen == 3, expr);
                }
                DEMAND(SYMBOL_P(name), expr);
                binding = sc_assq(name, env);   /* better be internal define */
                if (PAIR_P(binding)) {
                    DEMAND(U_CDR(binding) == UNDEF, binding);
                }
                else {
                    DEMAND(NULL_P(env), expr);
                    binding = VAL(sc_global_binding(th, name));
                }
                act->a = binding;
                act->codeh = get_code(kodes, k_interp_finish_set);
                if (proc) {
                    th->val = proc;
                }
                else {
                    act = act->link;
                    act->codeh = interp1;
                    act->a = U_CAR(U_CDR(U_CDR(expr)));
                    act->b = env;
                }
                return act;
            }
            if (op == sy->lambda_sy) {
                DEMAND(sc_list_length(expr) >= 3, expr);
                RETURN(sc_lambda(U_CDR(expr), env, kodes));
            }
            if (op == sy->if_sy) {
                CodeHandle interp1 = act->codeh;
                Value tst;
                DEMAND(PAIR_P(U_CDR(expr)), expr);
                tst = U_CAR(U_CDR(expr));
                act->codeh = get_code(kodes, k_interp_finish_if);
                act->a = U_CDR(U_CDR(expr));
                act = act->link;
                act->codeh = interp1;
                act->a = tst;
                act->b = env;
                act->env = EMPTY_LIST;
                return act;
            }
            if (op == sy->and_sy) {
                DEMAND(sc_list_length(expr) > 0, expr);
                act->codeh = get_code(kodes, k_interp_finish_and);
                act->a = U_CDR(expr);
                th->val = HASH_T;
                if (NULL_P(act->a))
                    RETURN(HASH_T);
                return act;
            }
            if (op == sy->or_sy) {
                DEMAND(sc_list_length(expr) > 0, expr);
                act->codeh = get_code(kodes, k_interp_finish_or);
                act->a = U_CDR(expr);
                th->val = HASH_F;
                if (NULL_P(act->a))
                    RETURN(HASH_F);
                return act;
            }
            if (op == sy->cond_sy) {
                DEMAND(sc_list_length(expr) > 0, expr);
                act->codeh = get_code(kodes, k_interp_cond_clauses);
                act->a = U_CDR(expr);
                th->val = UNSPEC;
                if (NULL_P(act->a))
                    RETURN(UNSPEC);
                return act;
            }
        }
        act->codeh = get_code(kodes, k_interp_eval_app);
        act = act->link;
        act->env = env;
        act->a = expr;
        act->b = HASH_F;
        act->r = EMPTY_LIST;
        act->codeh = get_code(kodes, k_interp_eval_arg_list);
        return act;
    }
    DEMAND(false, expr);
}

Activation sc_interp_eval_sequence(Activation act, ThData th)
{
    Value uneval = act->a;
    Value env = act->b;
    Value kodes = act->codeh->i;
    if (!PAIR_P(uneval))
        RETURN(UNSPEC);
    if (PAIR_P(U_CDR(uneval))) {
        if (!HEAP_P(act->link)) {
            return sc_prime_act_pump(act, th);
        }
        act->a = U_CDR(uneval);
        act = act->link;
    }
    act->a = U_CAR(uneval);
    act->b = env;
    act->codeh = get_code(kodes, k_interp_expr);
    return act;
}

Activation sc_interp_finish_set(Activation act, ThData th)
{
    DEMAND(PAIR_P(act->a), act->a);
    ((Pair) (act->a))->cdr = th->val;
    RETURN(UNSPEC);
}

Activation sc_interp_finish_if(Activation act, ThData th)
{
    Value expr = UNSPEC;
    Value tf = act->a;
    Value kodes = act->codeh->i;
    int tfl = sc_list_length(tf);
    DEMAND(1 <= tfl && tfl <= 2, tf);
    if (TRUE_P(th->val)) {
        expr = U_CAR(tf);
    }
    else {
        tf = U_CDR(tf);
        if (PAIR_P(tf)) {
            expr = U_CAR(tf);
        }
        else
            RETURN(UNSPEC);
    }
    act->a = expr;
    act->codeh = get_code(kodes, k_interp_expr);
    return act;
}

static Activation finish_andor(Activation act, ThData th, bool and)
{
    Value expr = UNSPEC;
    Value env = act->b;
    Value tf = act->a;
    Value kodes = act->codeh->i;
    if (!HEAP_P(act->link)) {
        return sc_prime_act_pump(act, th);
    }
    DEMAND(PAIR_P(tf), tf);
    if (and ? FALSE_P(th->val) : TRUE_P(th->val))
        RETURN(th->val);
    expr = U_CAR(tf);
    tf = U_CDR(tf);
    if (NULL_P(tf)) {
        act->a = expr;
        act->codeh = get_code(kodes, k_interp_expr);
        return act;
    }
    act->a = tf;
    act = act->link;
    act->codeh = get_code(kodes, k_interp_expr);
    act->a = expr;
    act->b = env;
    act->env = EMPTY_LIST;
    return act;
}

Activation sc_interp_finish_and(Activation act, ThData th)
{
    return finish_andor(act, th, true);
}

Activation sc_interp_finish_or(Activation act, ThData th)
{
    return finish_andor(act, th, false);
}

Activation sc_interp_cond_clauses(Activation act, ThData th)
{
    Value clause = UNSPEC;
    Value env = act->b;
    Value tf = act->a;
    Value kodes = act->codeh->i;
    if (!HEAP_P(act->link)) {
        return sc_prime_act_pump(act, th);
    }
    DEMAND(PAIR_P(tf), tf);
    clause = U_CAR(tf);
    DEMAND(PAIR_P(clause), clause);
    act->codeh = get_code(kodes, k_interp_cond_clause);
    act = act->link;
    act->codeh = get_code(kodes, k_interp_expr);
    act->a = U_CAR(clause);
    if (NULL_P(U_CDR(tf)) && act->a == getspecialsymbols_j(act->codeh)->else_sy) {
        act->a = HASH_T;
    }
    act->b = env;
    act->env = EMPTY_LIST;
    return act;
}

Activation sc_interp_cond_clause(Activation act, ThData th)
{
    Value tf = act->a;
    Value clause = U_CAR(tf);
    Value kodes = act->codeh->i;
    Value tst = th->val;
    if (TRUE_P(tst)) {
        Value expList = U_CDR(clause);
        if (NULL_P(expList))
            RETURN(tst);
        act->codeh = get_code(kodes, k_interp_eval_sequence);
        act->a = expList;
        return act;
    }
    tf = U_CDR(tf);
    if (NULL_P(tf))
        RETURN(UNSPEC);
    act->codeh = get_code(kodes, k_interp_cond_clauses);
    act->a = tf;
    return act;
}

Activation sc_interpreted_proc_enter(Activation act, ThData th)
{
    int narg = INT_FROM_FIXNUM(th->narg);
    Value actuals = EMPTY_LIST;
    Value formals = act->codeh->i;
    Value body = act->codeh->j;
    Value kodes = act->codeh->k;
    Value p;
    Value t;
    Value env = act->env;
    int rest = 0;
    int n = 0;
    Value define = (Value) sc_symlit("define");
    p = formals;
    while (PAIR_P(p)) {
        n++;
        DEMAND(SYMBOL_P(U_CAR(p)), formals);
        p = U_CDR(p);
    }
    rest = !NULL_P(p);
    DEMAND(NULL_P(p) || SYMBOL_P(p), formals);
    if (narg > 2)
        actuals = act->r;
    if (narg > 1)
        actuals = CONS(act->b, actuals);
    if (narg > 0)
        actuals = CONS(act->a, actuals);
    DEMAND((narg == n || (rest && narg >= n)), actuals);
    p = formals;
    t = actuals;
    while (PAIR_P(p)) {
        env = CONS(CONS(U_CAR(p), U_CAR(t)), env);
        t = U_CDR(t);
        p = U_CDR(p);
    }
    if (rest) {
        env = CONS(CONS(p, t), env);
    }
    /** introduce the internal defines into the lexical environment **/
    for (t = body; PAIR_P(t) && PAIR_P(U_CAR(t)) && U_CAR(U_CAR(t)) == define; t = U_CDR(t)) {
        if (PAIR_P(U_CDR(U_CAR(t)))) {
            Value name = U_CAR(U_CDR(U_CAR(t)));
            while (PAIR_P(name)) {
                name = U_CAR(name);
            }
            if (SYMBOL_P(name)) {
                env = CONS(CONS(name, UNDEF), env);
            }
        }
    }
    act->codeh = get_code(kodes, k_interp_eval_sequence);
    act->a = body;
    act->b = env;
    act->r = EMPTY_LIST;
    return act;
}

Activation sc_interp_eval_arg_list(Activation act, ThData th)
{
    Value uneval = act->a;
    Value env = act->env;
    Value kodes = act->codeh->i;
    Value expr;
    if (!PAIR_P(uneval)) {
        DEMAND(NULL_P(uneval), uneval);
        RETURN(act->r);
    }
    if (!HEAP_P(act->link)) {
        return sc_prime_act_pump(act, th);
    }
    expr = U_CAR(uneval);
    act->a = U_CDR(uneval);
    act->codeh = get_code(kodes, k_interp_stow_val);
    act = act->link;
    act->codeh = get_code(kodes, k_interp_enter);
    act->a = expr;
    act->b = env;
    th->narg = FIXNUM_FROM_INT(2);
    return act;
}

Activation sc_interp_stow_val(Activation act, ThData th)
{
    Value kodes = act->codeh->i;
    act->codeh = get_code(kodes, k_interp_eval_arg_list);
    act->r = CONS(th->val, act->r);
    return act;
}

Activation sc_interp_apply(Activation act, ThData th)
{
    return scp_apply(act, th);
}

Activation sc_interp_eval_app(Activation act, ThData th)
{
    Value kodes = act->codeh->i;
    DEMAND(PAIR_P(th->val), th->val);
    act->b = sc_dreverse(th->val);
    act->a = U_CAR(act->b);
    act->b = U_CDR(act->b);
    act->env = EMPTY_LIST;
    act->codeh = get_code(kodes, k_interp_apply);
    th->narg = FIXNUM_FROM_INT(2);
    return act;
}

void sc_bootstrap_defeval(ThData th, const char *name, Value expr)
{
    Value define = ATOM("define");
    Value sym = ATOM(name);
    Value defexp = LIST(define, sym, expr, _);
    sc_eval_now(th, defexp);
}

Value sc_eval_now(ThData th, Value expr)
{
    Value interp = ATOM("interp");
    Proc interpproc = (Proc) U_CDR(sc_global_binding(th, interp));
    Activation act = sc_prime_act_pump(0, th);
    th->val = NYI;
    if (PROC_P(interpproc)) {
        act->codeh = interpproc->entry_pt;
        act->env = EMPTY_LIST;
        act->a = expr;
        th->narg = FIXNUM_FROM_INT(1);
        sc_run_main_loop(act, th);
    }
    return th->val;
}

/*** sc_eval_c_string is for bootstrapping - do not overuse! */
Value sc_eval_c_string(ThData th, const char *s)
{
    Value read = ATOM("read");
    Value openst = ATOM("open-input-string");
    Value interp = ATOM("interp");
    Value expr = LIST(interp, LIST(read, LIST(openst, sc_strlit(s), _), _), _);
    return (sc_eval_now(th, expr));
}
