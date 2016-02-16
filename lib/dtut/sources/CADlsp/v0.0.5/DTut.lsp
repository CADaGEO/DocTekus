;;Copyright 2013-2014, Guillaume Berson, Couskou, Cristel LEGRAND
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
;;Version 0.0.5 du 03/12/2014
;;- ajout fonctions DTLsBlkUsed et DTgetpath
;;
;;Version 0.0.4 du 23/07/2014
;;- ajout fonction DTFieldPos et autres suite DTdraw
;;
;;Version 0.0.3 du 20/01/2014
;;- ajout fonction de recherche de validité d'un type d'entité DTTypEntitie
;;
;;Version 0.0.2 du 09/12/2013
;;- ajout fonctions de calques DTSavLay, DTInitLay et DTRestLay
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

;;; Gestion d'erreurs améliorée (repris de UtDispError)
(defun DTDispError (Msg)
  (alert Msg)
  (if (= (getvar "CMDACTIVE") 1) (command))
  (exit)
)

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
; si le texte est justifié totalement à gauche, pti = point d'insertion et angle = rotation
; dans tous les autres cas, pt2 est le point d'insertion / pti un point d'alignement
;    l'angle est l'angle depuis l'angle de base (variable angbase) vers à priori la poignée qui marque l'alignement 
;    (dans le cas d'un alignement milieu gauche ou droit, la poignée est en bas à gauche du texte ou en bas à droite du texte)
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

;;Fonction qui teste si un texte correspond à un type d'entite DXF
(defun DTTypEntitie (TypEntitie / listEntitie Res)

	; Initialisation de la liste de tous les types d'entités DXF existants
	(setq listEntitie (list 
		'"3DFACE" '"3DSOLID" '"ACAD_PROXY_ENTITY" '"ARC" '"ATTDEF" '"ATTRIB" '"BODY" '"CIRCLE" '"DIMENSION" 
		'"ELLIPSE" '"HATCH" '"HELIX" '"IMAGE" '"INSERT" '"LEADER" '"LIGHT" '"LINE" '"LWPOLYLINE" '"MESH" 
		'"MLINE" '"MLEADERSTYLE" '"MLEADER" '"MTEXT" '"OLEFRAME" '"OLE2FRAME" '"POINT" '"POLYLINE" '"RAY" 
		'"REGION" '"SECTION" '"SEQEND" '"SHAPE" '"SOLID" '"SPLINE" '"SUN" '"SURFACE" '"TABLE" '"TEXT" '"TOLERANCE" 
		'"TRACE" '"UNDERLAY" '"VERTEX" '"VIEWPORT" '"WIPEOUT" '"XLINE"))
				
	(if (member TypEntitie listEntitie) (setq Res T) (setq Res nil))
	
	(DTReturn Res)

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
;; Attention variables globales
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
;; Attention variables globales
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
  
;; DTLsBlkUsed -> monte une liste des noms de blocs présents et utilisés dans la base de donnée dessin (hors Xréfs)
(defun DTLsBlkUsed (/ e1 ne1 r1 l1)
  (setq l1 '())
  (setq e1 (entnext))
  (while e1 ; le dessin n'est pas vide traitement des objets
      (if (= (cdr (assoc 0 (entget e1))) "INSERT") (progn ; c'est un bloc ou Xréf
	  
		(setq ne1 (cdr (assoc 2 (entget e1)))) ; on récupère le nom du bloc (code 2)
		(setq r1 (tblsearch "BLOCK" ne1 T)); On cherche la définition de ce bloc - l'option T permet de se positionner dans la table pour que ensuite tblnext fonctionne
					
		(if (not (assoc 1 r1)) (progn ; pas une référence externe : car pas de lien vers fichier externe (code 1)
			;; CAS des blocs dynamiques à revoir : ne fonctionne pas pour l'instant (passer par l'API pour récupérer le vrai nom du bloc ???)
			; (setq r1 (tblnext "BLOCK")); on récupère la "vrai" définition du bloc pour trouver son "vrai" nom
			; (setq ne1 (cdr (assoc 2 r1)))
			; (if (not (member ne1 l1)) (setq l1 (cons ne1 l1))) ; on ajoute le vrai nom du bloc à la liste s'il n'existe pas déjà
			(if (not (member ne1 l1)) (setq l1 (cons ne1 l1))) ; on ajoute le nom du bloc à la liste s'il n'existe pas déjà
		))
	  ))
	  (setq e1 (entnext e1)) ; passage à l'objet suivant
  )
  (DTReturn l1)
)

;; DTLsBlk -> monte une liste des noms de blocs définis dans la base de donnée dessin mais pas forcément utilisés (hors Xréfs)
(defun DTLsBlk (/ e1 ne1 r1 l1)
  ; (setq l1 '())
  ; (setq e1 (next))
  ; (while e1 ; le dessin n'est pas vide traitement des objets
      ; (if (= (cdr (assoc 0 (entget e1))) "INSERT") (progn ; c'est un bloc ou Xréf
	  
			; (setq ne1 (cdr (assoc 2 (entget e1)))) ; on récupère le nom du bloc (code 2)
			; (if (not (assoc 102 (entget e1))) ; c'est un bloc "simple" (non dynamique) : le nom du bloc correspond à son "vrai" nom
				; (if (not (member ne1 l1)) (setq l1 (cons ne1 l1))) ; on ajoute le nom du bloc à la liste s'il n'existe pas déjà
				
				; (progn ; on a un code 102 dans la description de l'objet : c'est une Xréf ou un bloc dyn (attention objets type OLE non traités)
					; (setq r1 (tblsearch "BLOCK" ne1 T)); l'option T permet de se positionner dans la table pour que ensuite tblnext fonctionne
					; (if (not (assoc 1 r1)) (progn ; pas une référence externe : car pas de lien vers fichier externe (code 1) - par déduction il s'agit d'un bloc dynamique
						; CAS des blocs dynamiques à revoir : ne fonctionne pas pour l'instant (passer par l'API pour récupérer le vrai nom du bloc ???)
						(setq r1 (tblnext "BLOCK")); on récupère la "vrai" définition du bloc pour trouver son "vrai" nom
						(setq ne1 (cdr (assoc 2 r1)))
						(if (not (member ne1 l1)) (setq l1 (cons ne1 l1))) ; on ajoute le vrai nom du bloc à la liste s'il n'existe pas déjà
					; ))
				; )
			; )
	  ; ))
	  ; (setq e1 (entnext e1)) ; passage à l'objet suivant
  ; )
  (DTReturn l1)
)


 
  
 
 ;;; Gestion des calques (gelés ou non / activés ou non)
  
 ;--> DTSaveLay : sauvegarde l'état actuel d'un calque (gelé ou non, activation)
 ; fournir en entrée le nom du calque
 ; retourne une liste (NomCalque ValeurCodeDXF70 ValeurCodeDXF62)
(defun DtSaveLay ( nlay / lay llaysav)
		(setq lay (tblsearch "LAYER" nlay))
		(if lay (progn
			(setq llaysav (list nlay (cdr (assoc 70 lay)) (cdr (assoc 62 lay))))
			(DTReturn llaysav)
		))
)

 
 ;--> DTInitLay : dégèle et active un calque donné
  ; fournir en entrée le nom du calque
(defun DtInitLay ( nlay / )
	(command "_.-layer" "_Thaw" nlay "_ON" nlay "")
	
; premier essai par codes DXF --> pb non résolu entmod ne fonctionne pas
		; (setq lay (tblsearch "LAYER" nlay))
		; (if lay (progn
			; (setq act (cdr (assoc 62 lay)))
			; (if (minusp act) (setq nact (* act -1)) (setq nact act))
			; (setq lay (subst '(70 . 0) (assoc 70 lay) lay))
		; ;	(setq lay (subst (cons 62 nact) (assoc 62 lay) lay))
			; (DTReturn (entmod lay))
			; )
			; (DTReturn nil)
		; )
 )

  ;--> DTRestLay : Restaure l'état d'un calque donné (gelé ou non, activé ou non)
  ; fournir en entrée une liste à restaurer (NomCalque ValeurCodeDXF70 ValeurCodeDXF62)
(defun DtRestLay ( llay / lay ngel act nact)

	(if (minusp (car (cddr llay))) ; initialement le calque était non activé
			(command "_.-layer" "_OFF" (car llay) "" ) ; on le désactive à nouveau
	)
	(if (or (= (cadr llay) 1) (= (cadr llay) 3)) ; initialement le calque était gelé
			(command "_.-layer" "_Freeze" (car llay) "" ) ; on le gèle à nouveau
    )

; premier essai par codes DXF --> pb non résolu entmod ne fonctionne pas
		; (setq lay (tblsearch "LAYER" (car llay)))
		; (if lay (progn
			; (setq ngel (cdr llay))
			; (setq nact (cddr llay))
			; (subst ngel (cdr (assoc 70 lay)) (assoc 70 lay))
			; (subst nact (cdr (assoc 62 lay)) (assoc 62 lay))
			; (entmod lay)
			; (DTReturn T)
			; )
			; (DTReturn nil)
		; )
)

	;--> DTCreaLay : Créée un calque avec ses propriétés ==> A POURSUIVRE
; (defun DTCreaLay ( nom couleur typeligne epaisseur desc / objAcad objLay)

	; ; Vérification des paramètres d'entrée
	; (if (tblsearch "LAYER" nom) (UtDispError (strcat "DTCreaLay erreur calque déjà existant : " nom))); le nom du calque ne doit pas déjà exister
	
	; (if (or (> '-249 (atoi couleur)) (< 249 (atoi couleur))) (UtDispError (strcat "DTCreaLay erreur couleur calque non valide : calque " nom))) ; la couleur doit être soit un entier valide
	
	; (if (not (tblsearch "LTYPE" typeligne)) (UtDispError (strcat "DTCreaLay erreur type de ligne calque manquant : calque " nom))) ; le type de ligne doit déjà exister
	; (if (and (/= "" epaisseur) (/= (type epaisseur) 'INT)) (UtDispError (strcat "DTCreaLay erreur epaisseur de ligne de calque non valide : calque " nom))) ; l'épaisseur est soit un chiffre, soit une châine vide si on prend celle par défaut --> revoir : valeurs d'énumération ? combien pour "par défaut" ?
	; à revoir car la variable n'est pas forcément typée et fonction type nok, s'inspirer de la couleur
	; ==> réponses trouvées :
	;		épaisseur de ligne, variable CELWEIGHT pour celle courante / valeur entre -3 et 211, énumération car que certaines valeurs acceptées / -3 = par défaut / -2 = dubloc / -1 = ducalque / 
	; 		 0, 5, 9, 13, 15, 18, 20, 25, 30, 35, 40, 50, 53, 60, 70, 80, 90, 100, 106, 120, 140, 158, 200 et 211
	;		 voir contrôle de l'épaisseur dans DTdraw\DTdobj
	; ; Création du calque
	; (setq objAcad (vla-get-activedocument (vlax-get-acad-object)))
	; (setq objLay (vla-add (vla-get-layers objAcad) nom))
	
	; ; Ajout de ses paramètres
	; (vlax-put objLay 'TrueColor (vla-get-
	; (vlax-put objLay 'Linetype typeligne)
	; --> pb de cette méthode = comprendre l'articulation de chaque propriété
	
	
	; (entmake
		; (list
			; (cons 0  "LAYER")
			; (cons 100 "AcDbSymbolTableRecord")
			; (cons 100 "AcDbLayerTableRecord")
			; (cons 2 nom)
			; ;(cons 70 0) ; affiché ou non, gelé ou non : facultatif ? sinon on prend une valeur par défaut
			; (cons 62 (atoi couleur)) ; atoi pour convertir en entier -> pas de valeurs ByLayer ou ByBlock possibles
			; (if (/= "" epaisseur) (cons 370 epaisseur)) ; à revoir : convertir en réel atof ? ou utiliser les valeurs d'énumération, type par défaut
			; (cons 6 typeligne)                                            
		; )
	; ); pb de la description que l'on ne sait pas ajouter
	
	; ; ajout de la description avec activeX
	; --> pb de cette méthode = pas d'accès à la description du calque
	
	; Autre méthode possible = celle de Pierre dans ld-ldd : passer par la commande clavier CAD
	
	
	; )


  ;--> DTFieldPos : Recherche la Position du Champs dans une Liste de Champs
	;;		exemple : (DTFieldPos ("A" "B" "C") "B")) retourne 1
	;; repris et adapté de UtFiedPos

(defun DTFieldPos (ListeFields Field / l1 i OneField bNotFound)

  (setq l1 (length ListeFields))
  (setq i 0)
  (setq bNotFound 1)
  (while (and (< i l1) bNotFound)
    (setq OneField (nth i ListeFields))
    (if	(= OneField Field)
      (setq bNotFound Nil)
      (setq i (1+ i))
    )
  )
  (DTReturn i)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fichiers

(defun DTgetpath (prefix chemin / lsrep rep lspath path)

	(setq lsrep (UtSupEltListe "" (UtStr2lst chemin "\\") 0))
	(setq lsprefix (UtSupEltListe "" (UtStr2lst prefix "\\") 0))
	(setq lspath '() )
	(if (= "." (substr (car lsrep) 1 1)) (progn ; il s'agit d'un chemin relatif
		(setq lspath lsprefix)
		(foreach rep lsrep
			(cond 
				((= rep ".") T); répertoire courant : on ne fait rien
				((= rep "..") (setq lspath (UtSupNieme lspath (- (length lspath) 1)))); on remonte d'un cran
				(T (setq lspath (UtAddNieme lspath 9999999 rep))) ; autres cas : on ajoute le nouveau répertoire
			)
		))
	(setq lspath lsrep) ; il s'agit d'un chemin absolu
	)
	
	(DTReturn (UtLst2Str lspath "\\"))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (prompt "\nDocTekus chargement DTut v0.0.5 - licence GNU GPL v3")
  (princ)