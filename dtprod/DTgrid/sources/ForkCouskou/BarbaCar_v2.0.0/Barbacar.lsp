;;;Copyright 2012, CADaGEO - Guillaume BERSON
;;;Version 1.0.0 du 30/10/2012
;;;Couskou
;;;Version 2.0.0 du 17/02/2013
;;;
;;; Fichiers associés :
;;;
;;; Bar_Data.lsp : données des listes de la boite de dialogue
;;; Barbacar.dcl : définition de la boite de dialogue
;;;
;;;Modifications par Couskou :
;;; * Gestion des infos par boite de dialogue
;;; * Gestion des calques & de leur propriété
;;; * Recherche & sélection des styles de texte
;;; * Insertion du bloc symbole "flèche NORD"
;;; * Acquisition du contour par choix de 3 points ou sélection de cadre rectangulaire
;;; * + quelques réarangements mineurs
;;;
;;; 18/02/2013 - ATTENTION : il y a un bug sur l'orientation des textes de coordonnées
;;; après une sélection sur cadre existant (1 cas sur 4 pour l'instant).
;;; Si cela se produit, utiliser la sélection par 3 points.
;;; 
;;;This file is part of BarbaCar.
;;;
;;;    BarbaCar is free software: you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.
;;;
;;;    BarbaCar is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with Foobar.  If not, see <http://www.gnu.org/licenses/>
;;;
;;; Installation Barbacar v2.0.0
;;;
;;;	- sous AutoCAD(c), menu "Options" indiquer via le "chemin d'accès au fichiers de support", le dossier "Barbacar_vn.n.n"
;;;	- utiler "APPLOAD" pour charger "Barbacar.lsp" (ou taper (load "Barbacar") à partir de la ligne de commande d'AutoCAD(c) )
;;;
;;;Pour créer un carroyage dans un rectangle...
;;;Taper "bar" dans la fenêtre de commande et se laisser guider 


;Fonction de dessin de Carroyage
(defun c:bar (/ $Var $Sav $Vers BarId $BarTABLELAY $BarTABLESTYLETXT $ROBJET)

  (load "Bar_Data.lsp") ; chargement des données de propriétés personnalisées (calques, blocs...)

  ;;Initialisation des variables globales
  (setq $Var '()
	$Sav '()
	$Vers "2.0.0"
	BarId nil)
  
  ;; on lance la boite de dialogue Barbacar
  (setq BarId (load_dialog "Barbacar"))
  ;; on lance la fonction principale
  (BARBACAR)
  ;; on libére la boite de dialogue Barbacar
  (unload_dialog BarId)
  ;; on rétablit les variables à leur valeur d'origine
  (UtRestVar)
  (princ)
  )

 
;;; Fontion principale
(defun BARBACAR (/ bolSCU xox ok ListLWpt ListStyle ListRelScale ListIncline pt1 pt2 pt3 pt4 listx listy espace taillecroix tailletexte startHz endHz startVt endVt currentHz currentVt)

  ;; initialisation des variables fonction principale
  (setq	bolSCU T
	ok nil
	pt1 '()
	pt2 '()
	pt3 '()
	pt4 '()
	listx '()
	listy '()
	espace 0
	taillecroix 0
	tailletexte 0
	)

  ;;On passe en SCU Général si ce n'est pas le cas (passage obligatoire)
  (if (= (getvar "worlducs") 0)
	(progn
		(command "_UCS" "_W")
		(setq bolSCU nil)
	)
    ) ;if
  
  (BarTableLayer) ; Acquisition des calques dèjà présents dans le dessin actif et mise à jour des listes boites de dialogue
  (BarTableStyleTxt) ; Acquisition des styles de texte présents
  (BarDial) ; Acquisition des données de base par boite de dialogue
  (if ok
    (progn

      ;; on créé la liste des variables modifiées et leur nouvelle valeur
      (setq $Var (list (cons "cmdecho" 0) (cons "osmode" 0) (cons "blipmode" 0) (cons "aunits" 3)  (cons "angbase" (/ pi 2)) (cons "angdir" 1)(cons "clayer" "0")))

      ;; on sauvegarde l'état des variables AutoCAD
      (UtSaveVar)
      
      ;; on initialise les variables avec nos valeur
      (UtInitVar)

      ;; Traitement du cadre rectangulaire sélectionné
      (if $ROBJET
	(progn
	  (redraw (cdr (assoc -1 $RObjet)) 4)
	  (BarPtExtract)
	  )
	)
      
       ;On calcule le dernier point du rectangle
       (if (not pt3) (setq pt3 (polar pt2 (angle pt1 pt4) (distance pt1 pt4))))
	;Espacement entre deux croix du dessin
	(if (< (atoi NewScale) 1000)
	  (setq espace (* 0.1 (atoi NewScale)))
	  (setq espace (* 0.05 (atoi NewScale)))
	  ) ;if

	;Taille de la croix sur le dessin
	(setq taillecroix (* 0.0025 (atoi NewScale)))
	;Taille du texte
	(setq tailletexte (* 0.002 (atoi NewScale)))
      
      (if (not (tblobjname "LAYER" NewLayer)); on créé le calque si inexistant
	(entmake (list '(0 . "LAYER")
		       '(100 . "AcDbSymbolTableRecord")
		       '(100 . "AcDbLayerTableRecord")
		       (cons 2 NewLayer)
		       '(70 . 0)
		       (cons 62 (atoi (cdr (nth NewIndex $BARLAYERCL))))
		       (cons 6 (cdr (nth NewIndex $BARLAYERTL)))
		     )
	       )
      )
      (setvar "CLAYER" NewLayer); active le calque sélectionné

      (command "_PLINE" pt1 pt2 pt3 pt4 "_C") ;On dessine le cadre
      ;; Important : le bloc standard NORD01.DWG doit être présent dans le même dossier que Barbacar
      (command "_-INSERT" NewBlock (inters pt1 pt3 pt2 pt4)(* (atoi NewScale) 0.001) (* (atoi NewScale) 0.001) 0)

	;On cherche les points extrêmes, on les arrondis et on enlève 1 espace
	(setq listy (list (cadr pt1) (cadr pt2) (cadr pt3) (cadr pt4)))
	(setq listx (list (car pt1) (car pt2) (car pt3) (car pt4)))
	(ListLow listy)
	(setq startHz (- (BarRound intlistLow espace) espace))
	(ListHigh listy)
	(setq endHz (+ (BarRound intlistHigh espace) espace))
	(ListLow listx)
	(setq startVt (- (BarRound intlistLow espace) espace))
	(ListHigh listx)
	(setq endVt (+ (BarRound intlistHigh espace) espace))

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

	(if (= bolSCU nil)
	  (progn
	    (command "_UCS" "_P")
	    (alert "Le carroyage a été dessiné dans le SCU Général")
	    ) ;progn
	  ) ;If
	) ;Progn
      ) ;If

(princ)
;Clean fonction

)

;******************|BOITE DE DIALOGUE|********************

(defun BarDial(/ BarDone)
  (setq BarDone 6)
  (while (< 2 BarDone)
  (if (not (new_dialog "barbacar" BarId))
    (alert "Dialogue non trouvé : barbacar")
  )
  (set_tile "popup_list_BARLAYER" "")
  (BarDialInit)
  (action_tile "button_BAROBJECT" "(done_dialog 4)")
  (action_tile "button_BARPOINT" "(done_dialog 5)")
  (action_tile "popup_list_BARLAYER" "(BarEditLayer)")
    (action_tile "popup_list_BARSTYLE" "(BarEditStyle)")
  (action_tile "popup_list_BARSCALE" "(BarEditScale)")
  (action_tile "popup_list_BARBLOCK" "(BarEditBlock)")
  (action_tile "button_BARCADAGEO" "(BarCadageoInfo)")
  (action_tile "cancel" "(setq ok nil)(done_dialog 0)")
  (action_tile "accept" "(setq ok T)(done_dialog 1)")
  (setq BarDone (start_dialog))
  (cond ((= BarDone 4) (BarGetObjet))
	((= BarDone 5) (BarGetPoint))
	((= Bardone 0) (setq ok nil))
	((= Bardone 1) (setq ok T))
	)
    )
  )

;; BarDialInit -> Initialisation des listes

(defun BarDialInit (/ i)
  (BarDialInitLayer)
  (BarDialInitStyle)
  (BarDialInitScale)
  (barDialInitBlock)
)

;; BarCadageoInfo : Accés internet du contenu CadaGeo

(defun BarCadageoInfo(/ BarURL)
  (setq BarURL "http://vimeo.com/54238638")
  (startapp "c:\\Program Files\\Internet Explorer\\iexplore.exe" BarURL)
  )

;; BarGetObjet : sélection du contour polyligne rectangulaire existant

(defun BarGetObjet (/ GetObj)
    (setq GetObj (entsel "\nSélectionnez un contour polyligne rectangulaire..."))
    (if GetObj
      (progn
	(if $RObjet (redraw (cdr (assoc -1 $RObjet)) 4))
	(setq $RObjet (entget(car GetObj)))
	(redraw (cdr (assoc -1 $RObjet)) 3)
	(redraw)
	(if (not (member (cdr (assoc 0 $RObjet)) '("LWPOLYLINE" "POLYLINE")))
	  (progn
	    (redraw (cdr (assoc -1 $RObjet)) 4)
	    (alert "Entité sélectionnée non valide")
	    )
	  (progn
	    (redraw (cdr (assoc -1 $RObjet)) 3)
	    (setq NewLayer (cdr (assoc 8 $ROBJET)))
	    (BarDialInitLayer)
	    )
	  )
	)
      )
  )

;; BarGetPoint : sélection du contour rectangulaire par 3 points

(defun BarGetPoint (/ go GetObj)
  (while (not go)
    ;Saisie du point de base
    (setq pt1 (getpoint "\nPoint bas gauche : "))
    ;Saisie de la direction horizontale
    (setq pt2 (getpoint "\nPoint bas droit : "))
    ;Saisie de la direction verticale
    (setq pt4 (getpoint "\nPoint haut gauche : "))
    (setq go t)
    )
)

;; BarEditLayer -> Gestion évenementiel de la liste déroulante des calques

(defun BarEditLayer ()
  (setq NewLayer (nth (atoi $value) ListLayer))
  (BarDialInitLayer)
  (mode_tile "popup_list_BARLAYER" 2)
)

;; BarDialInitLayer -> Mise à jour de la valeur active dans la liste déroulante des calques

(defun BarDialInitLayer (/ i)
  (start_list "popup_list_BARLAYER")
  (end_list)
  (mode_tile "popup_list_BARLAYER" 1)
  (setq ListIndex (mapcar 'car $BARLAYER))
  (setq ListLayer (mapcar 'cdr $BARLAYER))
  (mode_tile "popup_list_BARLAYER" 0)
  (start_list "popup_list_BARLAYER")
  (mapcar 'add_list ListLayer)
  (end_list)
  (if (and NewLayer (member NewLayer ListLayer))
    (setq i (UtPosiEltInList NewLayer ListLayer))
    (setq i 0)
  )
  (setq NewLayer (nth i ListLayer))
  (setq NewIndex (nth i ListIndex))
  (set_tile "popup_list_BARLAYER"
	    (itoa i)
  )
)

;; BarEditStyle -> Gestion évenementiel de la liste déroulante des styles de texte

(defun BarEditStyle ()
  (setq NewStyle (nth (atoi $value) ListStyle))
  (BarDialInitStyle)
  (mode_tile "popup_list_BARSTYLE" 2)
)

;; BarDialInitStyle -> Mise à jour de la valeur active dans la liste déroulante des styles de texte

(defun BarDialInitStyle (/ i)
  (start_list "popup_list_BARSTYLE")
  (end_list)
  (mode_tile "popup_list_BARSTYLE" 1)
  (mode_tile "popup_list_BARSTYLE" 0)
  (start_list "popup_list_BARSTYLE")
  (mapcar 'add_list ListStyle)
  (end_list)
  (if (and NewStyle (member NewStyle ListStyle))
    (setq i (UtPosiEltInList NewStyle ListStyle))
    (setq i 0)
  )
  (setq NewStyle (nth i ListStyle))
  (setq NewRelScale (nth i ListRelScale))
  (setq NewIncline (nth i ListIncline))
  (set_tile "popup_list_BARSTYLE"
	    (itoa i)
  )
)

;; BarEditScale -> Gestion évenementiel de la liste déroulante des échelles plan

(defun BarEditScale ()
  (setq NewScale (nth (atoi $value) ListScale))
  (BarDialInitScale)
  (mode_tile "popup_list_BARSCALE" 2)
)

;; BarDialInitScale -> Mise à jour de la valeur active dans la liste déroulante des échelles plan

(defun BarDialInitScale (/ i)
  (start_list "popup_list_BARSCALE")
  (end_list)
  (mode_tile "popup_list_BARSCALE" 1)
  (setq ListScale (mapcar 'car $BARSCALE))
  (setq ListSTxt (mapcar 'cdr $BARSCALE))
  (mode_tile "popup_list_BARSCALE" 0)
  (start_list "popup_list_BARSCALE")
  (mapcar 'add_list ListScale)
  (end_list)
  (if (and NewScale (member NewScale ListScale))
    (setq i (UtPosiEltInList NewScale ListScale))
    (setq i 0)
  )
  (setq NewScale (nth i ListScale))
  (setq NewSTxt (nth i ListSTxt))
  (set_tile "popup_list_BARSCALE"
	    (itoa i)
  )
)

;; BarEditBlock -> Gestion évenementiel de la liste déroulante blocs symboles

(defun BarEditBlock ()
  (setq NewBlock (nth (atoi $value) ListBlock))
  (BarDialInitBlock)
  (mode_tile "popup_list_BARBLOCK" 2)
)

;; BarDialInitBlock -> Mise à jour de la valeur active dans la liste déroulante blocs symboles

(defun BarDialInitBlock (/ i)
  (start_list "popup_list_BARBLOCK")
  (end_list)
  (mode_tile "popup_list_BARBLOCK" 1)
  (mode_tile "popup_list_BARBLOCK" 0)
  (start_list "popup_list_BARBLOCK")
  (mapcar 'add_list $BARBLOCK)
  (end_list)
  (if (and NewBlock (member NewBlock $BARBLOCK))
    (setq i (UtPosiEltInList NewBlock $BARBLOCK))
    (setq i 0)
  )
  (setq NewBlock (nth i $BARBLOCK))
  (set_tile "popup_list_BARBLOCK"
	    (itoa i)
  )
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
(defun DrawRappelVt (ptVtMin ptVtMax / ang ptTemp)

;Traits du bas
(setq ang (/ pi 2))
(setq ptTemp (polar ptVtMin ang taillecroix))
(InRectangle ptTemp)
(if (= i 1) (BarDrawLine ptVtMin ptTemp))
  (BarDrawText ptTemp (list (car ptTemp) (+ (cadr ptTemp) tailleTexte)) (rtos (car ptTemp) 2 2) (/ (* 3 pi) 2) 2 2)

;Traits du haut
(setq ang (/ (* 3 pi) 2))
(setq ptTemp (polar ptVtMax ang taillecroix))
(InRectangle ptTemp)
(if (= i 1) (BarDrawLine ptVtMax ptTemp))
  (BarDrawText ptTemp (list (car ptTemp) (- (cadr ptTemp) tailleTexte)) (rtos (car ptTemp) 2 2) (/ (* 3 pi) 2) 0 2)
  
);Function


;;Fonction qui traite les croix de carroyage
(defun SetCroix (coord / ptHz1 ptHz2 ptinter1 ptinter2 ptinter3 ptinter4 listptInter ptHzMin ptHzMax)

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
(defun DrawCroix (DrptMin DrptMax / ang ptTemp )

;Dessin du trait horizontal sur le bord gauche
(setq ang 0)
(setq ptTemp (polar DrptMin ang taillecroix))
(InRectangle ptTemp)
  (if (= i 1)(BarDrawLine DrptMin ptTemp))
  (if (< (car pt1) (car pt2)) ;On gère le cas du cadre à l'envers
	(BarDrawText ptTemp (list (+ (car ptTemp) tailleTexte) (cadr ptTemp)) (rtos (cadr ptTemp) 2 2) 0 0 2)
	(BarDrawText ptTemp (list (+ (car ptTemp) tailleTexte) (cadr ptTemp)) (rtos (cadr ptTemp) 2 2) (/ (* 3 pi) 2) 2 2)
) ;if

;On dessine ensuite les croix à l'intérieur
(setq ang 0)7
(setq HzMinIn (- (BarRound (car DrptMin) espace) espace))
(setq ptBaseTemp (list HzMinIn (cadr DrptMin)))

(While (< (car ptBaseTemp) (car DrptMax))
(InRectangle ptBaseTemp)
	(if (= i 1)
		(Repeat 4
		(setq ptTemp (polar ptBaseTemp ang taillecroix))
		(InRectangle ptTemp)
		(if (= i 1) (BarDrawLine ptBaseTemp ptTemp))
			(setq ang (+ (/ pi 2) ang))
		) ;Repeat
	);If
(setq ptBaseTemp (list (+ (car ptBaseTemp) espace) (cadr ptBaseTemp)))
);Repeat


;Dessin sur le bord droit
(setq ang pi)
(setq ptTemp (polar DrptMax ang taillecroix))
(InRectangle ptTemp)
  (if (= i 1)(BarDrawLine DrptMax ptTemp))
  (if (< (car pt1) (car pt2)) ;On gère le cas du cadre à l'envers
	(BarDrawText ptTemp (list (- (car ptTemp) tailleTexte) (cadr ptTemp)) (rtos (cadr ptTemp) 2 2) 0 2 2)
	(BarDrawText ptTemp (list (- (car ptTemp) tailleTexte) (cadr ptTemp)) (rtos (cadr ptTemp) 2 2) (/ (* 3 pi) 2) 0 2)
) ;if
) ;Function


;; BarDrawLine : créé les entités lignes dans le dessin

(defun BarDrawLine (BDpt1 BDpt2)
  (entmake (list '(0 . "LINE")
		 '(100 . "AcDbEntity")
		 '(67 . 0)
		 '(410 . "Model")
		 (cons 8 NewLayer)
		 '(100 . "AcDbLine")
		 (cons 10 BDpt1)
		 (cons 11 BDpt2)
		 )
	   )
)

;; BarDrawText : créé les entités textes dans le dessin

(defun BarDrawText (BDpt1 BDpt2 ValText AngleRotate AlignHoriz AlignVert)
  
  (entmake (list '(0 . "TEXT")
		 '(100 . "AcDbEntity")
		 '(67 . 0)
		 '(410 . "Model")
		 (cons 8 NewLayer)
		 '(100 . "AcDbText")
		 (cons 10 BDpt1)
		 (cons 40 tailleTexte)
		 (cons 1 ValText)
		 (cons 50 AngleRotate)
		 (cons 41 NewRelScale)
		 (cons 51 NewIncline)
		 (cons 7 NewStyle)
		 '(71 . 0)
		 (cons 72 AlignHoriz)
		 (cons 11 BDpt2)
		 '(100 . "AcDbText")
		 (cons 73 AlignVert)
		 )
	   )
)

;;Fonction qui détecte la plus petite valeur d'une liste
(defun ListLow (listTest / listIndex)

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
(defun ListHigh (listTest / listIndex)

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
(defun InRectangle (ptIn / ptOut)

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

;; BarPtExtract : extraction des points pt1, pt2 & pt4 de cadre sélectionné
(defun BarPtExtract(/ ListLWpt lx ly xmin ymin xmax ymax xox)
  
  (if $ROBJET (setq ListLWpt (UtLpXpoly (cdr(assoc -1 $ROBJET)))))
  
  (setq pt1 (nth 0 ListLWpt))
  (setq pt2 (nth 1 ListLWpt))
  (setq pt3 (nth 2 ListLWpt))
  (setq pt4 (nth 3 ListLWpt))
  )

;;Fonction qui fait un arrondi
(defun BarRound (value to)

(setq to (abs to))
(* to (fix (/ ((if (minusp value) - +) value (* to 0.5)) to)))

) ;Function


;;; Gestion des variables
;;;par  Pierre Rigert - CadUC

(defun UtSaveVar ()
  (setq	$Sav
	 (mapcar 'getvar (mapcar 'car $Var))
  )
)

(defun UtInitVar ()
  (mapcar
    'setvar
    (mapcar 'car $var)
    (mapcar 'cdr $var)
  )
)

(defun UtRestVar ()
  (mapcar
    'setvar
    (mapcar 'car $Var)
    $Sav
  )
  (setq	$Sav nil
	$Var nil
  )
)
;;; Fin de gestion des variables

;;; Fonctions numériques
(defun deg2rad (a)
  (/ (* pi a) 180.0)
)

;;; Retourne la valeur k

(defun UtReturn (k) (setq k k))

(defun UtPosiEltInList (Elt Liste / l1 l2)
  ;; Retourne la position d'un élément dans une liste à partir de zéro
  (setq l1 (length Liste))
  (setq l2 (length (member Elt Liste)))
  (- l1 l2)
)

;; Retourne la liste des points d'une POLYLIGNE dans le SCU courant
(defun UtLpPoly	(e / ne p lp)
  (while (= "VERTEX"
	    (cdr (assoc 0 (setq ne (entget (setq e (entnext e))))))
	 )
    (setq
      p	 (trans (cdr (assoc 10 ne)) e 1)
      lp (append lp (list p))
    )
  )
)

;; Retourne la liste des points d'une LWPOLYLIGNE dans le SCU courant
(defun UtLpLwPoly (e / ne nnn lp)
  (setq ne (entget e))
  (foreach nnn ne
    (if	(= 10 (car nnn))
      (setq lp (append lp (list (trans (cdr nnn) 0 1))))
    )
  )
  (UtReturn lp)
)

;; Retourne la liste des points d'une Polyligne ou d'une LWpolyligne
(defun UtLpXpoly (e / ty lp)
  (setq ty (cdr (assoc 0 (entget e))))
  (cond
    ((= "POLYLINE" ty) (setq lp (UtLpPoly e)))
    ((= "LWPOLYLINE" ty) (setq lp (UtLpLwPoly e)))
  )
  (UtReturn Lp)
)

;;; Fonctions supplémentaires - E. Paris

;; Insertion d'un atome dans une liste, à la fin de celle-ci.
(defun AtomAppend (lst element / l1 cp)
    (setq l1 '())
    (setq cp 0)

    (setq l1(cons element (reverse lst)))
    (setq l1(reverse l1))
  )

;; BarTableLayer -> monte une liste des calques présents dans la base de donnée dessin

(defun BarTableLayer (/ ETDE x xox LayerName LayerIndexMax)
  (setq ETDE t)
  (while (setq x (tblnext "LAYER" ETDE))
    (setq ETDE nil)
    (if	(zerop (logand (cdr (assoc 70 x)) 16)) ; filtre les calques des Xrefs)
      (setq $BarTABLELAY (cons x $BarTABLELAY))
      (setq $BarTABLELAY (cons x $BarTABLELAY))
    )
  )
  (setq $BarTABLELAY (reverse $BarTABLELAY))
  (setq LayerName (mapcar 'cdr $BARLAYER))
  (setq LayerIndexMax (length $BARLAYER))
  
  (foreach xox $BarTABLELAY
    (if (not (member (cdr (assoc 2 xox)) LayerName))
      (progn
	(setq LayerIndexMax (1+ LayerIndexMax))
	(setq $BARLAYER (AtomAppend $BARLAYER (cons LayerIndexMax (cdr (assoc 2 xox)))))
	;; pour uniquement assurer la cohérence avec la liste $BARLAYER...
	(setq $BARLAYERCL (AtomAppend $BARLAYERCL (cons LayerIndexMax (itoa (cdr (assoc 62 xox))))))
	(setq $BARLAYERTL (AtomAppend $BARLAYERTL (cons LayerIndexMax (cdr (assoc 6 xox)))))
	)
      )
    )
)

;; BarTableStyleTxt -> monte une liste des calques présents dans la base de donnée dessin

(defun BarTableStyleTxt (/ ETDE x xox StyleName StyleIndexMax)
  (setq ETDE t
	ListStyle '()
	ListRelScale '()
	ListIncline '())
  
  (while (setq x (tblnext "STYLE" ETDE))
    (setq ETDE nil)
    (setq $BarTABLESTYLETXT (cons x $BarTABLESTYLETXT))
  )
  (setq $BarTABLESTYLETXT (reverse $BarTABLESTYLETXT))
  
  (foreach xox $BarTABLESTYLETXT
    (setq ListStyle (AtomAppend ListStyle (cdr (assoc 2 xox))))
    (setq ListRelScale (AtomAppend ListRelScale (cdr (assoc 41 xox))))
    (setq ListIncline (AtomAppend ListIncline (cdr (assoc 50 xox))))
    )
  )

(prompt
  "
	Carroyage par CADaGEO chargé - taper \"bar\" pour lancer la commande"
)
(princ)
;Clean chargement