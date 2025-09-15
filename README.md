# Analisador Sintático em Prolog

![swi-prolog](https://img.shields.io/badge/swi--prolog-8.4.2-orange) ![language](https://img.shields.io/badge/language-Prolog-blue) ![version](https://img.shields.io/badge/version-v0.1.0-green) ![license](https://img.shields.io/badge/license-MIT-lightgrey)

Projeto desenvolvido para a disciplina de Paradigmas de Linguagens de Programação, com foco educacional na implementação de um analisador léxico e sintático voltado para uma “sublinguagem” de Python.

🔗 Versão em Haskell: [analisador-sintatico-haskell](https://github.com/ArthurHappx/analisador-sintatico)

---

## :man_technologist: Integrantes

- Arthur Félix — [`ArthurHappx`](https://github.com/ArthurHappx)
- Daniel Damião — [`DanielPDamiao`](https://github.com/DanielPDamiao)
- João Moitinho — [`DevMoitinho`](https://github.com/DevMoitinho)
- Arthur Rodrigues — [`Arthyp`](https://github.com/Arthyp)


---

## :page_facing_up: Especificação do trabalho

A descrição formal do projeto pode ser encontrada no arquivo:

➡️ [especificacao-projeto-plp.pdf](especificacao-projeto-plp.pdf)

---

## :rocket: Execução

Para executar o analisador:
```bash
swipl ./app/analisador.pl   #certifique-se de estar na pasta raiz
```
```bash
"Analisador Léxico-Sintático (v0.1.0.0)"
"Digite '--help' para main informações."
>>> [args]
```

Argumentos suportados:
```bash
../path/arquivo.py          # análise normal, exibe ast visual no terminal
../path/arquivo.py -s       # analisa e salva o resultado em ./ast_results/arquivo_ast.txt
--help                      # mostra esta ajuda
-tests                      # executa testes internos e salva em ./test/Logs/arquivo_ast.txt
exit()                      # encerra o programa
```
<blockquote style="background-color: transparent; padding: 8px 14px; border-left: 4px solid rgba(0,0,0,0.35);">
📝 <strong>Observação:</strong><br>
O caminho (<code>path</code>) considerado na análise de arquivos é relativo à pasta <code>./app</code>.
</blockquote>

