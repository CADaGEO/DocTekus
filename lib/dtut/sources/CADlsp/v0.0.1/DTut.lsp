;;Copyright 2013, Guillaume Berson, Couskou, Cristel LEGRAND
;;Version 0.0.1 du 25/11/2013
;;
;;This file is part of Doctekus.
;;
;;    Doctekus is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    Doctekus is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with Foobar.  If not, see <http://www.gnu.org/licenses/>
;;
;;
;DESCRIPTION ********************************************************
;********************************************************************
;;Fonctions AutoCAD utilitaires
;;
;;INSTALLATION ET FONCTIONNEMENT *************************************
;*********************************************************************
;; Non autonome
;;
;;CHANGELOG **********************************************************
;;********************************************************************
;;
;;Version 0.0.1 du 25/11/2013
;;- version initiale
;;
;;
;;DEPENDANCES *******************************************************
;;*******************************************************************
;;Aucune
;;
;;
;;TODO, pochaine version *********************************************
;;********************************************************************
;;
;**********************************************************************
;**********************************************************************

;;; Retourne la valeur k

(defun DTReturn (k) (setq k k))


;; DTDrawLineO : créé les entités lignes dans le dessin (espace objet)
(defun DTDrawLineO (DTpt1 DTpt2 Layer)
	
	(entmake (list '(0 . "LINE")
		 '(100 . "AcDbEntity")
		 '(67 . 0) ; espace objet
		 '(410 . "Model")
		 (cons 8 Layer) ; calque
		 '(100 . "AcDbLine")
		 (cons 10 DTpt1) ; point de départ
		 (cons 11 DTpt2) ; point arrivée ligne
		 )
	   )
)

;; DTDrawTextO : créé les entités textes dans le dessin
(defun DTDrawTextO (pti pt2 AngleRotate ValText Layer Style AlignVert AlignHoriz tailletexte / taillen ahn avn x y)
	(if (not (numberp tailletexte)) (setq taillen (atoi tailletexte)) (setq taillen tailletexte))
	(if (not (numberp AlignHoriz)) (setq ahn (atoi AlignHoriz)) (setq ahn AlignHoriz))
	(if (not (numberp AlignVert)) (setq avn (atoi AlignVert)) (setq avn AlignVert))
	(setq x (car pti))
	(setq y (cadr pti))
	(entmake (list '(0 . "TEXT")
		 '(100 . "AcDbEntity")
		 '(67 . 0) ; espace objet
		 '(410 . "Model")
		 (cons 8 Layer) ; calque
		 '(100 . "AcDbText")
		 (cons 10 pti) ; x insertion
		 (cons 40 taillen) ; taille du texte
		 (cons 1 ValText) ; index
		 (cons 50 AngleRotate) ; angle de rotation
		 (cons 7 Style) ; style
		 '(71 . 0)
		 (cons 72 ahn) ; alignement horizontal / 0=gauche 1=centre 3=droite
		 (cons 11 pt2) ; second point d'alignement - position
		 '(100 . "AcDbText")
		 (cons 73 avn) ; alignement vertical / 0=ligne de base 1= haut 2=milieu 3=haut
		 )
	   )
)

;;Fonction qui détecte la plus petite valeur d'une liste
(defun DTListLow (listTest / listIndex intlistLow)

	(setq listIndex 0)
	(setq intlistLow (nth 0 listTest))

	(Repeat (length ListTest)
		(if (< (nth listIndex ListTest) intListLow)
			(setq intlistLow (nth listIndex ListTest))
			);if
		(setq listIndex (1+ ListIndex))
	);repeat
	
	(DTReturn intlistLow)

);Function


;;Fonction qui détecte la plus grande valeur d'une liste
(defun DTListHigh (listTest / listIndex intListHigh)

	(setq listIndex 0)
	(setq intlistHigh (nth 0 listTest))

	(Repeat (length ListTest)
		(if (> (nth listIndex ListTest) intListHigh)
			(setq intlistHigh (nth listIndex ListTest))
			);if
		(setq listIndex (1+ ListIndex))
	);repeat
	
	(DTReturn intListHigh)

);Function


;;Fonction qui détecte si un point est dans un polygone --> attention limite de taille, recherche à 1000u autour du rectangle
(defun DTInRectangle (ptIn pt1 pt2 pt3 pt4 / ptOut i)

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

  	(DTReturn i)

) ;Function


;;Fonction qui fait un arrondi
(defun DTround (val to /)

	(setq to (abs to))
	(DTReturn (* to (fix (/ ((if (minusp val) - +) val (* to 0.5)) to))))
	
) ;Function

;; DTTableLayer -> monte une liste des calques présents dans la base de donnée dessin
(defun DTTableLayer (/ ETDE x xox LayerName LayerIndexMax TABLELAY BARLAYER BARLAYERTL)
  (setq ETDE t)
  (setq TABLELAY ())
  (while (setq x (tblnext "LAYER" ETDE))
    (setq ETDE nil)
    (if	(zerop (logand (cdr (assoc 70 x)) 16)) ; filtre les calques des Xrefs)
      (setq TABLELAY (cons x TABLELAY))
      (setq TABLELAY (cons x TABLELAY))
    )
  )
  (setq TABLELAY (reverse TABLELAY))
  (setq LayerName (mapcar 'cdr BARLAYER))
  (setq LayerIndexMax (length BARLAYER))
  
  (foreach xox TABLELAY
    (if (not (member (cdr (assoc 2 xox)) LayerName))
      (progn
	(setq LayerIndexMax (1+ LayerIndexMax))
	(setq BARLAYER (AtomAppend BARLAYER (cons LayerIndexMax (cdr (assoc 2 xox)))))
	;; pour uniquement assurer la cohérence avec la liste BARLAYER...
	(setq BARLAYERCL (AtomAppend BARLAYERCL (cons LayerIndexMax (itoa (cdr (assoc 62 xox))))))
	(setq BARLAYERTL (AtomAppend BARLAYERTL (cons LayerIndexMax (cdr (assoc 6 xox)))))
	)
      )
    )
	(DTReturn TABLELAY)
)

;; DTTableStyleTxt -> monte une liste des styles de textes présents dans la base de donnée dessin
(defun DTTableStyleTxt (/ ETDE x xox ListStyle ListRelScale ListIncline TABLESTYLETXT)
  (setq ETDE t
	ListStyle '()
	ListRelScale '()
	ListIncline '())
  
  (while (setq x (tblnext "STYLE" ETDE))
    (setq ETDE nil)
    (setq TABLESTYLETXT (cons x TABLESTYLETXT))
  )
  (setq TABLESTYLETXT (reverse TABLESTYLETXT))
  
  (foreach xox TABLESTYLETXT
    (setq ListStyle (AtomAppend ListStyle (cdr (assoc 2 xox))))
    (setq ListRelScale (AtomAppend ListRelScale (cdr (assoc 41 xox))))
    (setq ListIncline (AtomAppend ListIncline (cdr (assoc 50 xox))))
    )
  )
  
   (prompt "\nDocTekus chargement DTut v0.0.1 - licence GNU GPL v3")
  (princ)