:- module(ast, [
    show_ast/1,
    save_ast/2
]).

% Mostra AST completo
show_ast(syntax_error(Message, Line)) :-
    format('SyntaxError: ~w, line ~w~n', [Message, Line]), !.
show_ast(AST) :-
    show_lines(AST).

show_lines([]):- !.
show_lines([assign(S, E)|T]) :-
    show_stmt(assign(S, E)),
    show_lines(T), !.
show_lines([E|T]) :-
    nl, write('AloneExpr'), nl,
    show_expr(E, 0),
    show_lines(T).

% Mostrar statement
show_stmt(assign(Name, Expr)) :-
    nl, format('Assign ~w~n', [Name]),
    show_expr(Expr, 2).

% Mostrar expressão
show_expr(add(E1, E2), Indent) :-
    indent(Indent), write('└─Add'), nl,
    I2 is Indent + 2,
    show_expr(E1, I2),
    show_expr(E2, I2), !.
show_expr(sub(E1, E2), Indent) :-
    indent(Indent), write('└─Sub'), nl,
    I2 is Indent + 2,
    show_expr(E1, I2),
    show_expr(E2, I2), !.
show_expr(mult(E1, E2), Indent) :-
    indent(Indent), write('└─Mul'), nl,
    I2 is Indent + 2,
    show_expr(E1, I2),
    show_expr(E2, I2), !.
show_expr(div(E1, E2), Indent) :-
    indent(Indent), write('└─Div'), nl,
    I2 is Indent + 2,
    show_expr(E1, I2),
    show_expr(E2, I2), !.
show_expr(unary(Op, E), Indent) :-
    indent(Indent), format('└─Unary ~w~n', [Op]),
    I2 is Indent + 2,
    show_expr(E, I2), !.
show_expr(num(Num), Indent) :-
    indent(Indent), format('└─Num ~w~n', [Num]), !.
show_expr(var(Var), Indent) :-
    indent(Indent), format('└─Var ~w~n', [Var]), !.

% Auxiliar de indentação
indent(0).
indent(N) :-
    N > 0,
    write(' '),
    N1 is N - 1,
    indent(N1).

save_ast(Dir, AST) :-
    open(Dir, write, Stream),
    with_output_to(Stream, (write_canonical(AST), nl, show_ast(AST))),
    close(Stream).
