
POLYTECH RICM3 Année 2014-15
Algorithmique et Programmation Fonctionnelle
Pro jet : Variations sur le rami
Ob jectifs
Le but de ce pro jet est de programmer les diérentes variantes du rami qui utilisent des lettres
(rami des lettres), des chires (
Rummikub
) ou des cartes (rami ou canasta). Ces variantes p euvent
être vues comme l'application d'un même foncteur (de jeu) à plusieurs mo dules (de règles).
Une partie imp ortante du pro jet concerne la manipulation de structures de données : nous devrons
gérer des ensembles (de tuiles ou de cartes), et vérier qu'un mot joué est présent dans un dictionnaire.
D'autre part, nous aurons diérentes régles de comptage des scores en fonction des diérentes versions,
ce qu'on réalise à l'aide du système de mo dules. Enn il faudra sauvegarder et charger une partie entre
plusieurs joueurs, ce qui met en ÷uvre l'analyse syntaxique.Les plus courageux p ourront réaliser une
intelligence articielle an de jouer contre la machine.
Rapp elons enn le princip e général d'un pro jet. Il ne s'agit pas comme dans certains examens
de traiter des questions sans rapp ort un p eu partout p our récup érer un maximum de p oints, mais
au contraire d'avoir un pro duit ni qui fonctionne, quitte à ce que certaines fonctionnalités en soient
absentes. À vous donc de déterminer quelles parties de ce sujet vous semblent prioritaires, et en cas
de dicultés sur un p oint, s'il vaut mieux insister p our le résoudre ou l'abandonner complètement.
Quelques chiers de départ vous sont fournis, ainsi que des compléments en cas de b esoin
1
.
Travail demandé
Le barème est donné à titre indicatif, notamment p our vous p ermettre d'anticip er le volume de
travail que représente chaque mo dule. Le TP en entier est à rendre par courriel à tous les enseignants,
imp érativement avant le
19 décembre 2014 minuit
sous la forme d'un chier
noms.tar.gz
. Seuls les
TP resp ectant les contraintes suivantes seront corrigés :
1.
noms
corresp ond aux noms des membres du group e
2.
la commande
tar xzf nom.tar.gz
génère un rép ertoire
noms
qui contiendra
obligatoire-
ment
: les sources de votre application, un chier
Makefile
, un chier
README
(et rien d'autre).
3.
la commande
make
génère un chier exécutable (ou plusieurs suivant les jeux que vous avez
implémentés)
main.native
ou
main
.
Par ailleurs, le chier
README
doit contenir :

le princip e du TP en une dizaine de lignes ;

exercice par exercice, la liste de ce qui marche et ce qui ne marche pas ;

si vous vous êtes fait aider, par qui et à quel endroit ;

comment utiliser votre programme, quels chiers il faut charger, les diérents exemples de votre
pro jet que vous avez programmés, etc.
Dans les chiers d'interface, et directement en entête de chaque fonction, vous préciserez :

le typ e de la fonction ;

le rôle de la fonction et à quoi corresp ondent ses entrées et ses sorties ;

si b esoin est, un commentaire sur les choix d'implémentation que vous avez faits, et plus géné-
ralement toute information susceptible d'aider à la b onne lecture du co de.
Rapp el :
la note de contrôle continu est constituée de la note de pro jet (50%), de la note de DM
(30%) et des TPs (20%, automatiquement acquis si tous les TPs sont rendus).
1.
http://www- verimag.imag.fr/~wack/APF_2014_2015/fichiers_projet.tar.gz
1 Vue d'ensemble : les règles communes
Nous présentons d'ab ord les parties communes dans les règles des diérentes variantes de rami. Cela
nous p ermet de dénir une signature
REGLE
que nous sp écialiserons en diérents mo dules (
Lettres
,
Rummikub
, . . . ).
Les règles communes sont les suivantes :
1.
Le jeu se joue avec des tuiles comp ortant une valeur (typ e
t
dans la signature). Par exemple
dans le Rummikub une valeur est un couple (nombre, couleur). Les tuiles du jeu sont en nombre
ni et sont représentées par un multi-ensemble nommé
paquet
.
2.
Chaque joueur disp ose d'un certain nombre de tuiles devant lui qu'il est le seul à voir, et
qui forment sa
main
. Au centre de la table sont disp osées les tuiles déjà en jeu par group es
qui forment nécessairement des combinaisons valides. Les tuiles du paquet qui ne sont ni en
jeu dans des combinaisons, ni dans les mains des joueurs, constituent la
pioche
. Le typ e des
combinaisons est nommé
combi
et il est imp osé égal à
t list
. Le typ e d'une main, et de la
pio che, est
main
.
3.
Au début du jeu, il n'y a pas de tuile en jeu et la main de chaque joueur est formée d'un certain
nombre de tuiles (le même p our chaque joueur, nommé
main_initiale
dans la signature) tirées
au hasard parmi les tuiles de la pio che. Le score initial de chaque joueur est
0
.
4.
À son tour, un joueur p eut pio cher (tirer au hasard une tuile de la pio che et la mettre dans sa
main), et terminer son tour ; ou placer un nombre non nul des tuiles de sa main dans le jeu et
réorganiser toutes les tuiles en jeu en combinaisons valides. Il est donc interdit de réorganiser les
tuiles en jeu si on n'en p ose pas. Il n'y a pas de restriction dans la réorganisation des tuiles en
jeu à condition que celles-ci soient regroup ées en combinaisons valides, et de resp ecter la règle
particulière des jokers (cf b onus 1). La fonction
combi_valide
détermine si une combinaison
est valide. Dans le cas où la pio che est vide et le joueur souhaite pio cher, le tour passe au joueur
suivant sans mo dication du jeu ni de la main du joueur.
5.
La première fois qu'un joueur souhaite p oser des tuiles de sa main, il ne p eut pas mo dier les
combinaisons déjà en place : il ne p eut qu'a jouter de nouvelles combinaisons à partir de tuiles
provenant de sa main uniquement. Il existe de plus des contraintes supplémentaires propres
à chaque variante et la fonction
premier_coup_valide
p ermet de tester si un coup est valide
p our une première p ose.
6.
Après son tour, un joueur p eut être contraint de pio cher des tuiles p our ramener sa main à une
taille déterminée par la variante particulière de rami que l'on considère et nommé
main_min
.
7.
Un joueur qui p ose p eut marquer des p oints (cela dép end de la règle utilisée). Ces p oints
dép endent des tuiles p osées en jeu et des combinaisons formées par le joueur. Le calcul des
p oints gagnés lors d'un coup est eectué par la fonction
points
.
8.
La partie se termine dès qu'un joueur vide sa main lors de son tour, avec p our certaines
variantes la condition supplémentaire que la pio che restante soit vide (dans ce cas le b o oléen
fin_pioche_vide
vaut
true
).
9.
À la n de la partie, chaque joueur p eut éventuellement marquer des p oints en fonction de sa
main restante. On les calcule avec la fonction
points_finaux
.
10.
Un certain nombre des tuiles disp onibles sont des jokers et p euvent prendre la valeur de n'im-
p orte quelle autre tuile lors de la formation des combinaisons.
La signature
REGLE
vous est fournie. Comme vous p ouvez le voir, nous aurons b esoin de manipuler
des multi-ensembles (p our toutes les variantes). De plus p our la variante se jouant avec des lettres,
nous aurons b esoin de savoir gérer un dictionnaire. Ces mo dules complémentaires sont l'ob jet des deux
sections suivantes.
La fonction
lit_valeur
p ermet de réaliser l'analyse syntaxique d'une valeur (parser). Récipro que-
ment,
ecrit_valeur
p ermet de convertir une valeur en chaîne de caractères. Cela sera utile p our les
fonctionnalités de sauvegarde/chargement de partie.

