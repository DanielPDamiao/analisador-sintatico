:- module(analisador, []).
:- set_prolog_flag(encoding, utf8).

% Importa e executa as configurações.
:- use_module(setup).
:- initialization(configs).
% Importa e executa o programa principal.
:- use_module('../src/CLI/cli').
:- initialization(main).
