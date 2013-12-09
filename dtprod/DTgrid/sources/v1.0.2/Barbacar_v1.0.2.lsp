;;Copyright 2012, CADaGEO - Guillaume BERSON
;;Version 1.0.2 du 18/01/2013
;;
;;This file is part of BarbaCar.
;;
;;    BarbaCar is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    BarbaCar is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with Foobar.  If not, see <http://www.gnu.org/licenses/>
;;
;;Pour créer un carroyage dans un rectangle...
;;Taper "car" dans la fenêtre de commande et se laisser guider 


;Fonction de dessin de Carroyage
(defun c:Car()

;On passe en SCU Général si ce n'est pas le cas (passage obligatoire)
(setq bolSCU "True")
(if (= (getvar "worlducs") 0)
	(progn
		(command "scu" "g")
		(setq bolSCU "False")
	)
) ;if

;Saisie du point de base
(setq pt1 (getpoint "\nPoint bas gauche : "))

;Saisie de la direction horizontale
(setq pt2 (getpoint "\nPoint bas droit : "))

;Saisie de la direction verticale
(setq pt4 (getpoint "\nPoint haut gauche : "))

;On calcule le dernier point du rectangle
(setq pt3 (polar pt2 (angle pt1 pt4) (distance pt1 pt4)))

;Echelle du dessin
(setq echelle (getreal "\nEchelle du dessin (50, 100, 200, ...) : "))

;Espacement entre deux croix du dessin
(if (< echelle 1000)
	(setq espace (* 0.1 echelle))
	(setq espace (* 0.05 echelle))
) ;if
;(setq espace (getreal "\nEspace papier (cm) entre deux croix : "))
;(setq espace (* 0.01 (* espace echelle)))

;Taille de la croix sur le dessin
(setq taillecroix (* 0.0025 echelle))
;Taille du texte
(setq tailletexte (* 0.002 echelle))

;On enregistre les variables pour les passer à 0
(setq oldsnap (getvar "osmode")) ;snap
(setq oldblipmode (getvar "blipmode")) ;blipmode
(setq angleunit (getvar "aunits")) ;unité d'angles
(setq anglebase (getvar "angbase")) ;unité d'angles
(setq angledir (getvar "angdir")) ;unité d'angles
(setvar "osmode" 0)
(setvar "blipmode" 0)
(setvar "aunits" 3)
(setvar "angbase" (/ pi 2))
(setvar "angdir" 1)

;On dessine le cadre
(Command "polylign" pt1 pt2 pt3 pt4 "c")

;On cherche les points extrêmes, on les arrondis et on enlève 1 espace
(setq listy (list (cadr pt1) (cadr pt2) (cadr pt3) (cadr pt4)))
(setq listx (list (car pt1) (car pt2) (car pt3) (car pt4)))
(ListLow listy)
(setq startHz (- (round intlistLow espace) espace))
(ListHigh listy)
(setq endHz (+ (round intlistHigh espace) espace))
(ListLow listx)
(setq startVt (- (round intlistLow espace) espace))
(ListHigh listx)
(setq endVt (+ (round intlistHigh espace) espace))

;On boucle sur chaque ligne pour dessiner les croix de carroyage
(setq currentHz startHz)
(while (<= currentHz endHz)

	(SetCroix currentHz)
	(setq currentHz (+ currentHz espace))

) ;while

;On boucle sur chaque ligne Verticale pour dessiner les lignes de rappel verticales
(setq currentVt startVt)
(while (<= currentVt endVt)

	(SetRappelVt currentVt)
	(setq currentVt (+ currentVt espace))

) ;while

;On rétablit les variables
(setvar "osmode" oldsnap)
(setvar "blipmode" oldblipmode)
(setvar "aunits" angleunit)
(setvar "angbase" anglebase)
(setvar "angdir" angledir)
(if (= bolSCU "False")
	(progn
		(command "SCU" "p")
		(princ "Le carroyage a été dessiné dans le SCU Général")
	) ;progn
) ;If

(princ)
;Clean fonction

)

;******************|MODULES|********************

;;Fonction qui traite les traits de rappel Verticaux
(defun SetRappelVt (coord)

;On crée la ligne horizontale au X défini
(setq ptHz1 (list coord -999999999))
(setq ptHz2 (list coord 999999999999))

;On cherche toutes les intersections
(setq ptInter1 (inters ptHz1 ptHz2 pt1 pt2 1))
(setq ptInter2 (inters ptHz1 ptHz2 pt2 pt3 1))
(setq ptInter3 (inters ptHz1 ptHz2 pt3 pt4 1))
(setq ptInter4 (inters ptHz1 ptHz2 pt4 pt1 1))

;On crée une liste avec tous les points d'intersection
(setq listptInter (list ptInter1 ptInter2 ptInter3 ptInter4))
(setq listptInter (vl-remove nil listptInter))

(if (= (length listptInter) 2)
;On en sort ensuite le point d'intersection de départ et d'arrivée
(cond
	((< (cadr(nth 0 listptInter)) (cadr(nth 1 listptInter)))
	
			(setq ptVtMin (nth 0 listptInter))
			(setq ptVtMax (nth 1 listptInter))
			(DrawRappelVt ptVtMin ptVtMax)
	);Premier cas
	((> (cadr(nth 0 listptInter)) (cadr(nth 1 listptInter)))
			(setq ptVtMin (nth 1 listptInter))
			(setq ptVtMax (nth 0 listptInter))
			(DrawRappelVt ptVtMin ptVtMax)
	);Deuxième cas
) ;cond
) ;If

);Function


;;Fonction qui dessine les traits de rappel verticaux
(defun DrawRappelVt (ptVtMin ptVtMax)

;Traits du bas
(setq ang (/ pi 2))
(setq ptTemp (polar ptVtMin ang taillecroix))
(InRectangle ptTemp)
(if (= i 1) (command "ligne" ptVtMin ptTemp ""))
(command "texte" "j" "md" (list (car ptTemp) (+ (cadr ptTemp) tailleTexte)) tailleTexte pi (rtos (car ptTemp) 2 0))

;Traits du haut
(setq ang (/ (* 3 pi) 2))
(setq ptTemp (polar ptVtMax ang taillecroix))
(InRectangle ptTemp)
(if (= i 1) (command "ligne" ptVtMax ptTemp ""))
(command "texte" "j" "mg" (list (car ptTemp) (- (cadr ptTemp) tailleTexte)) tailleTexte pi (rtos (car ptTemp) 2 0))

);Function


;;Fonction qui traite les croix de carroyage
(defun SetCroix (coord)

;On crée la ligne horizontale au Y défini
(setq ptHz1 (list -999999999 coord))
(setq ptHz2 (list 999999999999 coord))

;On cherche toutes les intersections
(setq ptInter1 (inters ptHz1 ptHz2 pt1 pt2 1))
(setq ptInter2 (inters ptHz1 ptHz2 pt2 pt3 1))
(setq ptInter3 (inters ptHz1 ptHz2 pt3 pt4 1))
(setq ptInter4 (inters ptHz1 ptHz2 pt4 pt1 1))

;On crée une liste avec tous les points d'intersection
(setq listptInter (list ptInter1 ptInter2 ptInter3 ptInter4))
(setq listptInter (vl-remove nil listptInter))

(if (= (length listptInter) 2)
;On en sort ensuite le point d'intersection de départ et d'arrivée
(cond
	((< (car(nth 0 listptInter)) (car(nth 1 listptInter)))
	
			(setq ptHzMin (nth 0 listptInter))
			(setq ptHzMax (nth 1 listptInter))
			(DrawCroix ptHzMin ptHzMax)
	);Premier cas
	((> (car(nth 0 listptInter)) (car(nth 1 listptInter)))
			(setq ptHzMin (nth 1 listptInter))
			(setq ptHzMax (nth 0 listptInter))
			(DrawCroix ptHzMin ptHzMax)
	);Deuxième cas
) ;cond
) ;If

) ;Function


;;Fonction qui dessine toutes les croix d'un point à un autre
(defun DrawCroix (ptHzMin ptHzMax)

;Dessin du trait horizontal sur le bord gauche
(setq ang 0)
(setq ptTemp (polar ptHzMin ang taillecroix))
(InRectangle ptTemp)
(if (= i 1) (command "ligne" ptHzMin ptTemp ""))
(if (< (car pt1) (car pt2)) ;On gère le cas du cadre à l'envers
	(command "texte" "j" "mg" (list (+ (car ptTemp) tailleTexte) (cadr ptTemp)) tailleTexte (/ pi 2) (rtos (cadr ptTemp) 2 0))
	(command "texte" "j" "md" (list (+ (car ptTemp) tailleTexte) (cadr ptTemp)) tailleTexte (/ (* 3 pi) 2) (rtos (cadr ptTemp) 2 0))
) ;if

;On dessine ensuite les croix à l'intérieur
(setq ang 0)
(setq HzMinIn (- (round (car ptHzMin) espace) espace))
(setq ptBaseTemp (list HzMinIn (cadr ptHzMin)))

(While (< (car ptBaseTemp) (car ptHzMax))
(InRectangle ptBaseTemp)
	(if (= i 1)
		(Repeat 4
		(setq ptTemp (polar ptBaseTemp ang taillecroix))
		(InRectangle ptTemp)
		(if (= i 1) (command "ligne" ptBaseTemp ptTemp ""))
			(setq ang (+ (/ pi 2) ang))
		) ;Repeat
	);If
(setq ptBaseTemp (list (+ (car ptBaseTemp) espace) (cadr ptBaseTemp)))
);Repeat


;Dessin sur le bord droit
(setq ang pi)
(setq ptTemp (polar ptHzMax ang taillecroix))
(InRectangle ptTemp)
(if (= i 1) (command "ligne" ptHzMax ptTemp ""))
(if (< (car pt1) (car pt2)) ;On gère le cas du cadre à l'envers
	(command "texte" "j" "md" (list (- (car ptTemp) tailleTexte) (cadr ptTemp)) tailleTexte (/ pi 2) (rtos (cadr ptTemp) 2 0))
	(command "texte" "j" "mg" (list (- (car ptTemp) tailleTexte) (cadr ptTemp)) tailleTexte (/ (* 3 pi) 2) (rtos (cadr ptTemp) 2 0))
) ;if

) ;Function


;;Fonction qui détecte la plus petite valeur d'une liste
(defun ListLow (listTest)

(setq listIndex 0)
(setq intlistLow (nth 0 listTest))

(Repeat (length ListTest)

(if (< (nth listIndex ListTest) intListLow)
(setq intlistLow (nth listIndex ListTest))
);if

(setq listIndex (1+ ListIndex))

);repeat


);Function

;;Fonction qui détecte la plus grande valeur d'une liste
(defun ListHigh (listTest)

(setq listIndex 0)
(setq intlistHigh (nth 0 listTest))

(Repeat (length ListTest)

(if (> (nth listIndex ListTest) intListHigh)
(setq intlistHigh (nth listIndex ListTest))

);if

(setq listIndex (1+ ListIndex))

);repeat

);Function


;;Fonction qui détecte si un point est dans un polygone
(defun InRectangle (ptIn)

(setq ptOut (list '-1000 '-1000))
(setq i 0)

;On compte le nombre de fois où on croise les arrêtes
(if (/= (inters ptIn ptOut pt1 pt2) nil)
(setq i (1+ i))
) ;if

(if (/= (inters ptIn ptOut pt2 pt3) nil)
(setq i (1+ i))
) ;if

(if (/= (inters ptIn ptOut pt3 pt4) nil)
(setq i (1+ i))
) ;if

(if (/= (inters ptIn ptOut pt4 pt1) nil)
(setq i (1+ i))
) ;if

) ;Function


;;Fonction qui fait un arrondi
(defun round (value to)

(setq to (abs to))
(* to (fix (/ ((if (minusp value) - +) value (* to 0.5)) to)))

) ;Function


(prompt
  "
	Carroyage par CADaGEO chargé - taper car pour lancer la commande"
)

(princ)
;Clean chargement