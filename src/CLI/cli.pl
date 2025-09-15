:- module(cli, [main/0]).

:- set_prolog_flag(encoding, utf8).

:- use_module('../Lexer/lexer').
:- use_module('../Parser/parser').
:- use_module('../AST/ast').
% Loop principal do programa.
main :-
    write('\nAnalisador Lêxico-Sintático (v0.1.0.0)\n'),
    write("Digite '--help' para mais informações.\n"),
    loop.

loop :-
    write('>>> '),
    read_line_to_string(user_input, Input),
    atomic_list_concat(Command, ' ', Input),
    execute(Command),
    loop.

% Interações do programa.
execute(['exit()']):- halt.

execute(['--help']):- 
            write('Uso:\n'),
            write('arquivo.py          # faz a análise padrão de um arquivo python\n'),
            write('arquivo.py -s       # faz a análise de um arquivo python e salva o resultado em ./ast_results\n'),
            write('--help              # mostra descrições dos comandos\n'),
            write('-tests              # executa todos os testes de arquivos python internos e salva em ./test/Logs\n'),
            write('exit()              # encerra o analisador\n'),
            !.

execute(['-tests']):- 
            nb_getval(localArquivesDir, Dir),
            (exists_directory(Dir) ->
                write('Executando testes internos...\n'),
                directory_files(Dir, Arquives),
                pick_py(Dir, Arquives, PythonArquives),
                (PythonArquives == [] -> 
                    write('Não existem testes disponiveis no momento.\n');
                atom_concat(Dir, 'Logs/', LogPath),
                make_directory_path(LogPath),
                runTests(LogPath, PythonArquives));
            write('Diretório de testes não encontrado. Tente --help para mais informações.\n')), !.

execute([PathRaw]):-
            (is_valid_path(PathRaw) ->
                write('Analisando...\n'),
                (catch(
                    lex_from_file(PathRaw, Tokens), 
                    syntax_error(Message, Line), 
                    show_ast(syntax_error(Message, Line))) ->
                    (catch(
                        parse_program(Tokens, AST), 
                        syntax_error(Message, Line), 
                        show_ast(syntax_error(Message, Line))) ->
                            show_ast(AST);
                    !);
                !);
            !).

execute([PathRaw, '-s']):-
            (is_valid_path(PathRaw) ->
                nb_getval(astSaveResultsDir, Dir),
                find_save_path(Dir, PathRaw, SavePath),
                write('Analisando...\n'),
                make_directory_path(Dir),
                (catch(
                    lex_from_file(PathRaw, Tokens), 
                    syntax_error(Message, Line), 
                    (show_ast(syntax_error(Message, Line)), save_ast(SavePath, syntax_error(Message, Line)), false)) ->
                    (catch(
                        parse_program(Tokens, AST), 
                        syntax_error(Message, Line), 
                        (show_ast(syntax_error(Message, Line)), save_ast(SavePath, syntax_error(Message, Line)), false)) ->
                            show_ast(AST),
                            save_ast(SavePath, AST);
                    !);
                !);
            !).

execute(_):-
            write('Comando invalido. Tente --help para mais informações.\n').

% Comandos auxiliares.
pick_py(_, [],[]):- !.
pick_py(Dir, [ArquiveRaw|Others], PythonArquives):-
            atom_chars(ArquiveRaw, Arquive),
            (is_valid_py(Arquive) -> 
                atom_concat(Dir, ArquiveRaw, ArquivePath),
                PythonArquives = [ArquivePath|Tail], 
                pick_py(Dir, Others, Tail); 
            pick_py(Dir, Others, PythonArquives)).

is_valid_py([_, '.', 'p', 'y']) :-  !.
is_valid_py([_|Tail]) :-
            is_valid_py(Tail).

is_valid_path(Path) :-
            (exists_file(Path) -> 
                split_string(Path, "/\\", "", SubStrings),
                last(SubStrings, ArquiveRawStr),
                atom_string(ArquiveRawAtom, ArquiveRawStr),
                atom_chars(ArquiveRawAtom, Arquive),
                (is_valid_py(Arquive) -> true;
                write('Arquivo invalido. Tente --help para mais informações.\n'), false);
            write('Caminho não encontrado. Tente --help para mais informações.\n'), false).

runTests(_, []):- !.
runTests(DirPath, [ArquivePath|Rest]):- 
            find_save_path(DirPath, ArquivePath, SavePath),
            (catch(
                lex_from_file(ArquivePath, Tokens), 
                syntax_error(Message, Line), 
                (save_ast(SavePath, syntax_error(Message, Line)), false)) ->
                (catch(
                    parse_program(Tokens, AST), 
                    syntax_error(Message, Line), 
                    (save_ast(SavePath, syntax_error(Message, Line)), false)) ->
                        save_ast(SavePath, AST),
                        runTests(DirPath, Rest);
                runTests(DirPath, Rest));
            runTests(DirPath, Rest)).

find_save_path(DirPath, ArquivePathAtom, SavePath) :-
            atom_string(ArquivePathAtom, ArquivePathStr),
            split_string(ArquivePathStr, "/\\", "", SubStrings), last(SubStrings, ArquiveStr),
            atom_string(ArquiveAtom, ArquiveStr),
            atom_concat(ArquiveName, '.py', ArquiveAtom), 
            atom_concat(ArquiveName, '_ast.txt', ArquiveTestName),
            atom_concat(DirPath, ArquiveTestName, SavePath).

            
            
        