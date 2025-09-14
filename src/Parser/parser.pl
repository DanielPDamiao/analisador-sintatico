:- module(parser, [
    parse_program/2
]).

% ===============================================
% PREDICADO PRINCIPAL (Ponto de Entrada)
% ===============================================

parse_program(Tokens, AST) :- phrase(program(AST, 1, _), Tokens).

% ===============================================
% GRAMÁTICA DCG (Definite Clause Grammar)
% ===============================================

% Program = {Line}
program([], Line, Line) --> ['EOF'].
program([], Line, Line) --> [].
program(AST, LineIn, LineOut) --> 
    ['NEWLINE'],
    { LineNext is LineIn + 1 },
    program(AST, LineNext, LineOut).
program([H|T], LineIn, LineOut) --> 
    line(H, LineIn),
    program(T, LineIn, LineOut).

% Line = Stmt | Expr
line(AST, LineIn) --> stmt(AST, LineIn).
line(AST, LineIn) --> expr(AST, LineIn).

% Stmt = ident "=" Expr
stmt(assign(Ident, Expr), Line) --> 
    (['TkIdent', Ident] -> [] ; { throw(syntax_error("invalid syntax", Line)) }), 
    (['TkAssign'] -> [] ; { throw(syntax_error("invalid syntax", Line)) }),
    expr(Expr, Line).

% Expr = Term Expr'
expr(Expr, Line) --> 
    term(Term, Line),
    expr_rest(Term, Expr, Line).

% Expr' = Addop Term Expr' | Ɛ
expr_rest(Left, Expr, Line) -->
    addop(Op),
    term(Right, Line),
    { create_binary_op(Op, Left, Right, NewLeft) },
    expr_rest(NewLeft, Expr, Line).
expr_rest(Expr, Expr, _) --> [].

% Term = Factor Term'
term(Term, Line) -->
    factor(Factor, Line),
    term_rest(Factor, Term, Line).

% Term' = Mulop Factor Term' | Ɛ
term_rest(Left, Term, Line) -->
    mulop(Op),
    factor(Right, Line),
    { create_binary_op(Op, Left, Right, NewLeft) },
    term_rest(NewLeft, Term, Line).
term_rest(Term, Term, _) --> [].

% Factor = "(" Expr ")" | Unary | Value
factor(0, _) --> ['TkOpenPar'], ['TkClosePar'].
factor(Expr, Line) -->
    ['TkOpenPar'],
    (expr(Expr, Line) -> [] ; { throw(syntax_error("\'(\' was never closed", Line)) }),
    (['TkClosePar'] -> [] ; { throw(syntax_error("\'(\' was never closed", Line)) }).

factor(_, Line) -->
    ['TkClosePar'],
    { throw(syntax_error("unmatched \')\'", Line)) }.

factor(Unary, _) --> unary(Unary).
factor(Value, _) --> value(Value).

% Unary = "-" Factor | "+" Factor
unary(unary(-, Value)) --> ['TkMinus'], value(Value).
unary(unary(+, Value)) --> ['TkPlus'], value(Value).

% Value = number | ident
value(num(Num)) --> ['TkNumber', Num].
value(var(Ident)) --> ['TkIdent', Ident].

% Addop = "+" | "-"
addop(+) --> ['TkPlus'].
addop(-) --> ['TkMinus'].

% Mulop = "*" | "/"
mulop(*) --> ['TkMult'].
mulop(/) --> ['TkDiv'].

% ===============================================
% PREDICADOS AUXILIARES
% ===============================================

% Cria operação binária
create_binary_op(+, Left, Right, add(Left, Right)).
create_binary_op(-, Left, Right, sub(Left, Right)).
create_binary_op(*, Left, Right, mult(Left, Right)).
create_binary_op(/, Left, Right, div(Left, Right)).
