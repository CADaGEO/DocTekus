(defun c:DTcloop()

;On r�cup�re la distance choisie par l'utilisateur
(setq intDist (getreal "\nDistance : "))
(setq strBloc (getstring "\nNom du Bloc : "))
(setq strAlign (getstring "\nAlign� (O/N) : "))

;On r�cup�re la s�lection de l'utilisateur
(setq sset (ssget))

;On d�fini la variable qui boucle
(setq i 0)

;On boucle sur chaque �l�ment
(repeat (sslength sset)
(setq item (ssname sset i))
	(command "_MEASURE" item "B" strBloc strAlign intDist)
(setq i (1+ i))
);repeat
	
(princ)

) ;defun

(princ)