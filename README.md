# Analisador Sintático em Haskell

Projeto desenvolvido para a disciplina de Paradigmas de Linguagens de Programação, com foco educacional na implementação de um analisador léxico e sintático voltado para uma “sublinguagem” de Python.

## :man_technologist: Integrantes

- Arthur Félix — `ArthurHappx`
- Daniel Damião — `DanielPDamiao`
- João Moitinho — `DevMoitinho`
- Arthur Rodrigues — `Arthyp`

## :page_facing_up: Especificação do trabalho

A descrição formal do projeto pode ser encontrada no arquivo:

:arrow_right: [especificacao-projeto-plp.pdf](especificacao-projeto-plp.pdf)

---

## :rocket: Execução

```bash
cabal run analisador
```
```bash
"Analisador Léxico-Sintático (v0.1.0.0)"
"Digite '--help' para main informações."
>>> [args]
```

Argumentos possíveis:
```bash
../path/arquivo.py          # análise normal, exibe ast visual no terminal
../path/arquivo.py -s       # analisa e salva o resultado em ./ast_results/arquivo_ast.txt
--help                      # mostra esta ajuda
-tests                      # executa testes internos e salva em ./test/Logs/arquivo_ast.txt
exit()                      # encerra o programa
```
