(defun c:DTcloop()

;On récupère la distance choisie par l'utilisateur
(setq intDist (getreal "\nDistance : "))
(setq strBloc (getstring "\nNom du Bloc : "))
(setq strAlign (getstring "\nAligné (O/N) : "))

;On récupère la sélection de l'utilisateur
(setq sset (ssget))

;On défini la variable qui boucle
(setq i 0)

;On boucle sur chaque élément
(repeat (sslength sset)
(setq item (ssname sset i))
	(command "_MEASURE" item "B" strBloc strAlign intDist)
(setq i (1+ i))
);repeat
	
(princ)

) ;defun

(princ)