(defun c:Pli()
;Fonction de dessin des marques de pliage

;Saisie du point de base
(setq pt0 (getpoint "\nPoint bas gauche : "))

;Saisie de la direction horizontale
(setq pt1 (getpoint "\nPoint bas droit : "))

;Saisie de la direction verticale
(setq pt2 (getpoint "\nPoint haut gauche : "))

;On enregistre les variables pour les passer à 0
(setq oldsnap (getvar "osmode")) ;snap
(setq oldblipmode (getvar "blipmode")) ;blipmode
(setvar "osmode" 0)

(setvar "blipmode" 0)

;On vérifie que les axes soient bien Hz et Vt donc Perp
(if 
	(or
		(/= (cadr pt0) (cadr pt1))
		(/= (car pt0) (car pt2))
	) ;or
(Princ "\nLes axes ne sont pas horizontaux ou verticaux")
) ;if

;On vérifie que les formats soient bien des multiples de A4
(setq distHz (rtos(distance pt0 pt1) 2 1))
(setq distVt (rtos(distance pt0 pt2) 2 1))

;On dessine les lignes
(Command "ligne" pt0 (polar pt0 (fromgrade 0.0) 2.5) ""
		 "ligne" pt0 (polar pt0 (fromgrade 100) 2.5) ""
) ;command

;On rétablit les variables
(setvar "osmode" oldsnap)
(setvar "blipmode" oldblipmode)

(princ)
;Clean fonction

)

;********************************************

;Fonction de conversion des grades en radians
(defun fromGrade (x)

(* pi (/ x 200.0))

) ;Function

(prompt
  "
	Marques de pliage par CADaGEO chargé - taper pli pour lancer la commande"
)

(princ)
;Clean chargement