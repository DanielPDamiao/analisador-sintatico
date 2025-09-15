:- module(ast, [
    show_ast/1,
    save_ast/2
]).

% Mostra AST completo
show_ast(program(Lines)) :-
    show_lines(Lines).

show_lines([]).
show_lines([stmt(S)|T]) :-
    show_stmt(S),
    show_lines(T).
show_lines([expr(E)|T]) :-
    nl, write('AloneExpr'), nl,
    show_expr(E, 0),
    show_lines(T).

% Mostrar statement
show_stmt(assign(Name, Expr)) :-
    nl, format('Assign ~w~n', [Name]),
    show_expr(Expr, 2).

% Mostrar expressão
show_expr(number(N), Indent) :-
    indent(Indent), format('└─Num ~w~n', [N]).
show_expr(var(S), Indent) :-
    indent(Indent), format('└─Var ~w~n', [S]).
show_expr(plus(E1, E2), Indent) :-
    indent(Indent), write('└─Add'), nl,
    I2 is Indent + 2,
    show_expr(E1, I2),
    show_expr(E2, I2).
show_expr(minus(E1, E2), Indent) :-
    indent(Indent), write('└─Sub'), nl,
    I2 is Indent + 2,
    show_expr(E1, I2),
    show_expr(E2, I2).
show_expr(mult(E1, E2), Indent) :-
    indent(Indent), write('└─Mul'), nl,
    I2 is Indent + 2,
    show_expr(E1, I2),
    show_expr(E2, I2).
show_expr(div(E1, E2), Indent) :-
    indent(Indent), write('└─Div'), nl,
    I2 is Indent + 2,
    show_expr(E1, I2),
    show_expr(E2, I2).
show_expr(unary(Op, E), Indent) :-
    indent(Indent), format('└─Unary ~w~n', [Op]),
    I2 is Indent + 2,
    show_expr(E, I2).

% Auxiliar de indentação
indent(0).
indent(N) :-
    N > 0,
    write(' '),
    N1 is N - 1,
    indent(N1).


