:- module(setup, [configs/0]).
:- set_prolog_flag(encoding, utf8).

% Setup inicial do programa, definindo diretórios, etc.
% localArquivesDir: diretório de leitura dos arquivos de teste local. (NECESSÁRIO EXISTENCIA PREVIA)
% astSaveResultsDir: diretório onde os resultados da análise serão salvos. (CRIADO CASO NÃO EXISTA)
% Para modificar os diretórios mude o segundo parâmetro da função para um caminho terminado em '/'.
configs :-
    nb_setval(localArquivesDir, '../tests/Examples/'),
    nb_setval(astSaveResultsDir, '../ast_results/'),
    verifica_dirs.

verifica_dirs :-
    nb_getval(localArquivesDir, Dir1),
    nb_getval(astSaveResultsDir, Dir2),
    atom_chars(Dir1, DirChars1),
    atom_chars(Dir2, DirChars2),
    last(DirChars1, Ultimo1),
    last(DirChars2, Ultimo2),
    (Ultimo1 == '/', Ultimo2 == '/', exists_directory(Dir1) -> 
        true; 
    write('Configuração de diretorios invalida. Revise as configurações em "configs" no programa "setup" e tente novamente.\n'), halt).
