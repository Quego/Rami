type bool_ = string
;;

type prenom = string
;;

type tuile = string
;;

type tuiles = tuile list
;;

type coup = Coup of tuiles
;;

type coups = coup list
;;

type joueur = prenom * int * bool_ * coup
;;

type joueurs = joueur list
;;

type jeu = joueurs * coups * tuiles * int
;;

type ident = string;;

