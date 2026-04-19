# Projet Slitherlink
Auteur: Théo Morlevat (Master Bioinformatique)
Contexte: Réalisation d'un projet dans l'Unité d'enseignement HAX815X(R programming) | Encadrement : Jean-Michel Marin

L'objectif de ce projet est de produire un package permettant le lancement d'une application SLitherlink jouable.

## Lancement du jeu
Le jeu peux être lancé à partir d'un terminal R.

L'installation de la librairie shiny sera requise pour le bon fonctionnement du programme.
```R
install.packages("shiny")
```
Vous pourrez ensuite charger le package via la commande:
```R
pkgload::load_all()
```
Le lancement du jeu se fait avec la fonction:
```R
shiny::runApp("script_app.R")

Le script "script.R" contient le nécessaire pour le lancement de l'application.
