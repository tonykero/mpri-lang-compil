# Compilation M1 MPRI

Compilation d'un langage impératif vers l'assembleur MIPS de [MARS](http://courses.missouristate.edu/KenVollmar/mars/)

## Build

le projet utilise menhir et ocamlbuild  
Il y a 2 scripts pour compiler:
* [build_impc.sh](src/build_impc.sh) pour compiler la partie impc 
* [build_func.sh](src/build_func.sh) pour compiler la partie func 

## Execution

Le jar de Mars est directement dans le projet, il faut que java soit installé.

* Le script [run_impc.sh](src/run_impc.sh) va compiler et exécuter un test inclut dans [src/impc/tests](src/impc/tests)
* Le script [run_func.sh](src/run_func.sh) va compiler et exécuter un test inclut dans [src/func/tests](src/func/tests)

**Example:**
```
./run_impc.sh circle
./run_impc.sh fun2
```

Les scripts [run_impc_tests.sh](src/run_impc_tests.sh) et [run_func_tests.sh](src/run_func_tests.sh) compile et exécute tous les tests présents dans [src/impc/tests](src/impc/tests) et [src/func/tests](src/func/tests) respectivement


## Extensions

> Tous les sujets sont complets, seules quelques extensions du sujet 1.2 n'ont pas été réalisées.
>>Concernant malloc() et free(), l'implémentation est une liste chaînée unidirectionnelle first-fit
>>> free() fusionne tous les blocs à droite si ils sont à la suite et non alloués
>>>> malloc et free ne sont pas builtins, et il y a donc des implémentations minimales afin de rendre des exemples fonctionnels
>>>> L'implémentation complète réside dans [malloc.imp](src/impc/tests/malloc.imp)

**Sujet 1.1:**
- [x] Instructions Combinées
- [x] Expressions constantes pré-calculées
- [x] Utilisation non-systématique de la pile
- [x] Précalculation des adresses (global)
- [x] Opérateurs paresseux
- [x] Interruptions de boucles
- [x] Boucle for
- [x] en plus: Opérateurs binaires (shift, and, or)

**Sujet 1.2:**
- [x] A: Procédures
- [x] C: Fonction main
- [ ] C: Convention affinée
- [ ] C: Tail call optimization
- [ ] B: Convention d'appel avec registres
- [ ] C: Tail Call + B
- [ ] D: TAC IR
- [ ] D: Variables locales -> $t* autant que possible

**Sujet 1.3:**
- [x] Tableau: Séquences de valeurs
- [x] Tableau: Répétition d'une valeur
- [x] Tableau: Compréhension
