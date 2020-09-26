# Compilation M1 MPRI

Compilation d'un langage impératif vers l'assembleur MIPS de [MARS](http://courses.missouristate.edu/KenVollmar/mars/)

## Build

le projet utilise menhir et ocamlbuild  
Il y a un [script](src/build.sh) pour compiler

## Execution

Le jar de Mars est directement dans le projet, il faut que java soit installé.

```
cd src
./run.sh circle
```
Va compiler le fichier [circle.imp](src/tests/circle.imp) puis éxécuter l'assembleur généré avec Mars.

Le script [run_tests.sh](src/run_tests.sh) va compiler et exécuter tous les scripts imp présents dans le dossier [tests](src/tests)

