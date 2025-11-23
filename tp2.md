# Projets ENSIIE - Programmation Raisonnée PROG2 - 2025 - 2026

## Mise en place

Cloner le [répertoire github](https://github.com/tlegall-ensiie/tp2.git) qui reprend l'analyseur [stan](https://gitlab.com/vprevosto/stan). Lire le README, installer et compilez l'exécutable *live_var.exe* donné en exemple. Tester l'exécutable (qui se trouve dans le répertoire **_build** sur un des exemples du répertoire **tests**.


## Propagation des constantes

Compléter le fichier *constant_propagation.ml* pour implémenter une analyse de propagation des constantes comme vue en cours. 

## Définitions "sures"

Compléter le fichier *constant_propagation.ml* pour implémenter une analyse de définitions sûres.

On dit qu'une expression arithmétique est sûre si toutes les variables qui y apparaissent y sont sûrement définies, et une variable *v* est sûrement définie si elle a bien été affectée *v:=e* avec *e* une expression sûre.

On veut réaliser une analyse de type "MAY-analysis", c'est-à-dire qu'on considère qu'on cherche à calculer, pour chaque noeud du CFG, l'ensemble des variables qui sont (potentiellement) sûrement définies.


## Rendu de TP

En fin de TP, envoyer vos fichiers (sous forme d'une archive) aux deux adresses suivantes: 
- [virgile.prevosto@cea.fr]
- [tristan.le-gall@cea.fr]
  
