(* PRINCIPE DU PROJET *)

Le but de ce projet est de programmer les différentes variantes du rami qui utilisent des lettres (rami des lettres), des chiffres (Rummikub)) ou des cartes (rami ou canasta). 
Ces variantes peuvent être vues comme l'application d'un même foncteur (de jeu) à plusieurs modules (de règles).
Une partie importante du projet concerne la manipulation de structures de données : nous devronsgérer des ensembles (de tuiles ou de cartes), et vériffer qu'un mot joué est présent dans un dictionnaire.
D'autre part, nous aurons différentes régles de comptage des scores en fonction des différentes versions,ce qu'on réalise à l'aide du système de mo ules. Enfin il faudra sauvegarder et charger une partie entre plusieurs joueurs, ce qui met en oeuvre l'analyse syntaxique.Les plus courageux pourront réaliser une intelligence artificielle afinn de jouer contre la machine.

(* CE QUI MARCHE/NE MARCHE PAS *)

Le sujet de base ( sans les bonus ) à été réalisé 

-MultiEnsemble : Toutes les fonctions fonctionnent
-Dictionnaire  : Toutes les fonctions fonctionnent
-Save/Load     : Toutes les fonctions fonctionnent
-Rami          : Toutes les fonctions fonctionnent
-Rummikub      : Toutes les fonctions fonctionnent
-Jeu           : Toutes les fonctions fonctionnent



NB : La fonction Random renvoie toujours la même suite de nombre , ce qui ne permet pas d'avoir des distributions de jeu différentes à chaque partie. Elle sera aléatoire mais ce sera toujours la même.

(* UTILISTION DU JEU *)

-make pour compiler les fichiers


-Pour lancer le jeu il faut faire un :

"./main.native"

A partir de ce moment la les différents choix sont guidés dans le jeu

-Pour charger un jeu , il faut que la sauvegarde corresponde au type du jeu selectionné

-Pour rentrée la "Main" / le "Nouveau Jeu" / les "Prenoms"
Il faut : 
* Une combinaison par ligne / Prenom
* Séparer les différentes tuiles par un espace / Compacte pour les prenoms
* Appuyer sur "Enter" pour rentrer une nouvelle combinaison  / Nouveau prenom

EXEMPLE : 

==============================================================
Nom des joueurs?
==============================================================

Quentin
Thibaut

 ----------------------------------------------------------

==============================================================
Entrez le nouveau jeu
==============================================================

H E L L O
W O R L D 

 ----------------------------------------------------------

==============================================================
Entrez votre nouvelle main :
==============================================================

A Z E R T Y

