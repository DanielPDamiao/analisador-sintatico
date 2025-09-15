% Módulo principal do lexer
:- module(lexer, [
    lex_from_file/2, 
    lexer/2
]).

% Para mostrar a lista completa
:- set_prolog_flag(answer_write_options, [max_depth(0)]).

% ===============================================
% PREDICADO PRINCIPAL (Ponto de Entrada)
% ===============================================

% Lexer principal
lexer(Input, Tokens) :-
    string_chars(Input, Chars),
    lex_all(Chars, 1, Tokens).

% ===============================================
% LEXER
% ===============================================

lex_all([], _, ['EOF']).
lex_all([C|Cs], LineNum, Tokens) :-
    (   is_space(C), C == '\n'
    ->  Tokens = ['NEWLINE'|RestTokens],
        NextLine is LineNum + 1,
        lex_all(Cs, NextLine, RestTokens)
    ;   is_space(C)
    ->  lex_all(Cs, LineNum, Tokens)
    ;   C == '.'
    ->  lex_number([C|Cs], LineNum, Token, Value, Rest),
        Tokens = [Token, Value|RestTokens],
        lex_all(Rest, LineNum, RestTokens)
    ;   is_digit_char(C)
    ->  lex_number([C|Cs], LineNum, Token, Value, Rest),
        Tokens = [Token, Value|RestTokens],
        lex_all(Rest, LineNum, RestTokens)
    ;   is_alpha(C)
    ->  lex_identifier([C|Cs], LineNum, Token, Value, Rest),
        Tokens = [Token, Value|RestTokens],
        lex_all(Rest, LineNum, RestTokens)
    ;   C == '='
    ->  Tokens = ['TkAssign'|RestTokens],
        lex_all(Cs, LineNum, RestTokens)
    ;   C == '+'
    ->  Tokens = ['TkPlus'|RestTokens],
        lex_all(Cs, LineNum, RestTokens)
    ;   C == '-'
    ->  Tokens = ['TkMinus'|RestTokens],
        lex_all(Cs, LineNum, RestTokens)
    ;   C == '/'
    ->  Tokens = ['TkDiv'|RestTokens],
        lex_all(Cs, LineNum, RestTokens)
    ;   C == '*'
    ->  Tokens = ['TkMult'|RestTokens],
        lex_all(Cs, LineNum, RestTokens)
    ;   C == '('
    ->  Tokens = ['TkOpenPar'|RestTokens],
        lex_all(Cs, LineNum, RestTokens)
    ;   C == ')'
    ->  Tokens = ['TkClosePar'|RestTokens],
        lex_all(Cs, LineNum, RestTokens)
    ;
        throw(syntax_error("invalid syntax", LineNum))
    ).

% Lexer para identificadores
lex_identifier(Input, _LineNum, 'TkIdent', Ident, Rest) :-
    take_while(Input, is_alpha_num, IdentChars, Rest),
    atom_chars(IdentAtom, IdentChars),
    (   IdentAtom == '' -> Ident = '' ; Ident = IdentAtom ).

% Lexer para números
lex_number(Input, LineNum, 'TkNumber', Number, Rest) :-
    take_while(Input, is_number_char, NumberChars, Rest),
    validate_number(NumberChars, LineNum),
    atom_chars(NumberAtom, NumberChars),
    (   NumberAtom == '' -> Number = '' ; Number = NumberAtom ).

% Validação de números
validate_number(NumberChars, LineNum) :-
    (   \+ member('.', NumberChars),  % Se não é float
        NumberChars = ['0'|Rest],
        Rest \= [],
        \+ (Rest = ['.'|_])  % Não é um float que começa com 0.
    ->  throw(syntax_error("leading zeros in decimal integer literals are not permitted", LineNum))
    ;   true  % Caso contrário, é válido
    ).

% Função para ler arquivo
lex_from_file(FilePath, Tokens) :-
    catch(
        (   read_file_to_string(FilePath, String, []),
            lexer(String, Tokens)
        ),
        Error,
        (   Error = error(io_error, _)
        ->  throw(error('ReadFileError', Error))
        ;   throw(Error)
        )
    ).

% ===============================================
% PREDICADOS AUXILIARES
% ===============================================

take_while([], _, [], []).
take_while([C|Cs], Pred, [C|Taken], Rest) :-
    call(Pred, C),
    !,
    take_while(Cs, Pred, Taken, Rest).
take_while(Rest, _, [], Rest).

is_space(C) :- char_type(C, space).
is_digit_char(C) :- char_type(C, digit).
is_alpha(C) :- char_type(C, alpha) ; C == '_'.
is_alpha_num(C) :- is_alpha(C) ; is_digit_char(C).
is_number_char(C) :- is_digit_char(C) ; C == '.'.

% Para uso com SWI-Prolog
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).