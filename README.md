# Compilation M1 MPRI

Compilation d'un sous-ensemble de caml vers l'assembleur MIPS de [MARS](http://courses.missouristate.edu/KenVollmar/mars/)

## Build

le projet utilise menhir et ocamlbuild  
Il y a 3 scripts pour compiler:
* [build_impc.sh](src/build_impc.sh) pour compiler la partie impc 
* [build_func.sh](src/build_func.sh) pour compiler la partie func 
* [build_mmlc.sh](src/build_mmlc.sh) pour compiler la partie mmlc 

## Execution

Le jar de Mars est directement dans le projet, il faut que java soit installé.

* Le script [run_impc.sh](src/run_impc.sh) va compiler et exécuter un test inclut dans [src/impc/tests](src/impc/tests)
* Le script [run_func.sh](src/run_func.sh) va compiler et exécuter un test inclut dans [src/func/tests](src/func/tests)
* Le script [run_mmlc.sh](src/run_mmlc.sh) va compiler et exécuter un test inclut dans [src/mmlc/tests](src/mmlc/tests)

**Example:**
```
./run_impc.sh circle
./run_impc.sh fun2
./run_mmlc.sh equality
```

Les scripts [run_impc_tests.sh](src/run_impc_tests.sh), [run_func_tests.sh](src/run_func_tests.sh) et [run_mmlc_tests.sh](src/run_mmlc_tests.sh) compile et exécute tous les tests présents dans [src/impc/tests](src/impc/tests), [src/func/tests](src/func/tests) et [src/mmlc/tests](src/mmlc/tests) respectivement


## Extensions

> Tous les sujets sont complets, seules les extensions du sujet 1.2 et des sujets 2 n'ont pas été réalisées.
>>Concernant malloc() et free(), l'implémentation est une liste chaînée unidirectionnelle first-fit
>>> free() fusionne tous les blocs à droite si ils sont à la suite et non alloués
>>>> malloc et free ne sont pas builtins, et il y a donc des implémentations minimales afin de rendre des exemples fonctionnels
>>>> L'implémentation complète réside dans [malloc.imp](src/impc/tests/malloc.imp)

### Module 1
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

### Module 2

**Sujet 2.1:**
- [ ] Affichage simplifié
- [ ] Garbage collector
- [ ] Optimisation des appels terminaux

**Sujet 2.2:**
- [ ] n-uplets généraux
- [ ] Structures de données & match/with
- [ ] Inférence de types
- [ ] Polymorphisme paramètrique
