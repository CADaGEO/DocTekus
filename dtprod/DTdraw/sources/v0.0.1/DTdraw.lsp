;;Copyright 2014, CADaGEO - Cristel LEGRAND
;;Version 0.0.1 du 21/07/2014
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
;;Aide au dessin en suivant une charte graphique
;;
;;INSTALLATION ET FONCTIONNEMENT *************************************
;*********************************************************************
;;
;; Conserver tous les fichiers du package dans un même répertoire
;; S'assurer que les blocs définis dans les fichiers de paramétrages soient bien présents + fichier ini + fichiers de paramétrages
;;
;; Quand UN SEUL AutoCAD est lancé / Installation :
;; - Ajouter le chemin dudit répertoire dans les chemins de recherche de fichiers de support (_options / fichiers)
;; - charger le lisp DTdraw.lsp systématiquement au démarrage d'AutoCAD (_appload, valisette "contenu" dans "au démarrage" puis redémarrer Autocad)
;;

;; Fonctionnement :
;; Taper "DTd" dans la ligne de commande et se laisser guider 
;;
;;CHANGELOG **********************************************************
;;********************************************************************
;;
;;
;;Version 0.0.1 du 21/07/2012
;;- Mise à disposition initiale de la version 0.0.1
;;
;;
;;DEPENDANCES *******************************************************
;;*******************************************************************
;; Nécessite les fonctions de DTini, DTcsv, ld-ut et dtut
;; Utilise les fonctions vl*
;;
;;TODO, pochaine version *********************************************
;;********************************************************************
;;
;**********************************************************************
;**********************************************************************

;chargement des dépendances
 (if (findfile "DTini.lsp") (load "DTini.lsp") (prompt "\nErreur DTini.lsp non trouvé"))
 (if (findfile "DTcsv.lsp") (load "DTcsv.lsp") (prompt "\nErreur DTcsv.lsp non trouvé"))
 (if (findfile "ld-ut.LSP") (load "ld-ut.LSP") (prompt "\nErreur ld-ut.LSP non trouvé"))
 (if (findfile "DTUt.lsp") (load "DTUt.lsp") (prompt "\nErreur DTUt.lsp non trouvé"))
 (vl-load-com)
 
 ; Commande de dessin
 ;********************************************************************
(defun c:DTd(/ 	)

	; pour que le programme puisse être annulé comme une seule commande
	(setvar "CMDECHO" 0)
	(command "_UNDO" "_BE")
	
	;;Initialisation des variables globales
	(setq $Var '()
		$Sav '()
	)
	
	;;Sauvegarde et initialisation des variables AutoCAD
	; (setq $Var (list (cons "INSUNITS" 0)))
	; (UtSaveVar)
	; (UtInitVar)
	
	; Gestion des erreurs
	(UtInitError)

	;RAZ des messages utilsateurs
	(setq $DTLogUser "")
	
	(F_DTdraw)
	
	;Restauration des variables AutoCAD
	; (UtRestVar)
	
	; pour que le programme puisse être annuél comme une seule commande
	(command "_UNDO" "_E")
	(setvar "CMDECHO" 1)
	
	; Gestion des erreurs
	(UtRestError)
	
	;Affichage des messages utilsateurs
	; (Textscr)
	(princ $DTLogUser)
	(princ)
)

 ; Fonction principale
 ;********************************************************************
(defun F_DTdraw( / INIPath SAVPath T_OBJTYPESPath T_DOCTYPESPath T_SCALESPath ;T_CADLAYERSPath 
					T_OBJCLASSESPath T_CADATTENUMPath ;GABPath
					GABFile LstSection doctype_default scale_default
					T_OBJTYPES T_DOCTYPES T_SCALES ;T_CADLAYERS 
					T_OBJCLASSES T_CADATT T_CADATTENUM V_OBJCLASSES V_OBJTYPES
					lsDOCTYPES lsSCALES lsOBJCLASSES lsOBJTYPES 
					stDOCTYPES stSCALES stOBJCLASSES stOBJTYPES
					staDOCTYPES staSCALES staOBJCLASSES staOBJTYPES criteres doctype scale objclass objtyp LsParamObj LsParamDoc
					OutFile OutScale OutObjClass OutObjTyp
				)


	;;TYPAGE des variables locales
	(setq
		; vrai T
		; point getpoint
		; entier 0
		; numeric 0.0
		; texte ""
		; liste '()

		; Chemin d'accès aux fichiers de paramétrages
		INIPath ""
		SAVPath ""
		T_OBJTYPESPath ""
		T_DOCTYPESPath ""
		T_SCALESPath ""
		; T_CADLAYERSPath ""
		T_OBJCLASSESPath ""
		T_CADATTENUMPath ""
		
		; Chemin d'accès aux gabarits
		; GABPath ""
		GABFile ""

		; Section des fichiers INI ou SAV
		LstSection '()
		
		; Valeurs par défaut
		doctype_default ""
		scale_default ""
		
		; Tables de paramétrage
		T_OBJTYPES '()
		T_DOCTYPES '()
		T_SCALES '()
		; T_CADLAYERS '()
		T_OBJCLASSES '()
		T_CADATT '()
		T_CADATTENUM '()
		
		; "Vues" filtrées
		V_OBJCLASSES '()
		V_OBJTYPES '()
		
		; Listes d'invites pour la saisie utilisateur
		lsDOCTYPES '()
		lsSCALES '()
		lsOBJCLASSES '()
		lsOBJTYPES '()
		; Chaînes pour INITGET
		stDOCTYPES ""
		stSCALES ""
		stOBJCLASSES ""
		stOBJTYPES ""
		; Chaînes pour invit user
		staDOCTYPES ""
		staSCALES ""
		staOBJCLASSES ""
		staOBJTYPES ""		
		
		; critères de filtres
		criteres '()
		
		; Paramètres choisis
		doctype ""
		scale ""
		objclass ""
		objtyp ""
		
		; Liste de paramétrages du type d'objets choisis
		LsParamObj '()
		LsParamDoc '()
		
		; Sorties de boucles
		OutFile nil
		OutScale nil
		OutObjClass nil
		OutObjTyp nil		
		)
		 

	;Chemin d'accès aux fichiers de paramétrages
	(if (findfile "DTdraw.lsp") (setq INIPath (findfile "DTdraw.lsp")) (DTDispError  "Manque les chemins de recherche de fichiers de support - Dtdraw.lsp non trouve"))
  	(setq INIPath (UtRep INIPath))

	; (setq SAVPath (strcat INIPath "DTdraw.sav"))
	(setq T_OBJTYPESPath (strcat INIPath "T_OBJTYPES.csv"))	
	(setq T_DOCTYPESPath (strcat INIPath "T_DOCTYPES.csv"))	
	(setq T_SCALESPath (strcat INIPath "T_SCALES.csv"))	
	; (setq T_CADLAYERSPath (strcat INIPath "T_CADLAYERS.csv"))	
	(setq T_OBJCLASSESPath (strcat INIPath "T_OBJCLASSES.csv"))	
	(setq T_CADATTPath (strcat INIPath "T_CADATT.csv"))	
	(setq T_CADATTENUMPath (strcat INIPath "T_CADATTENUM.csv"))	
	
	(setq INIPath (strcat INIPath "DTdraw.ini"))
		
	; Ouverture et lecture du fichier INI
	(if (not (DTIGetSection INIPath "DTdraw")) (DTDispError "Section par défaut du fichier ini non trouvee"))
	; (if (setq LstSection (DTIGetSection INIPath "DTdraw")) (setq GABPath (DTIGetVal LstSection "GABARITS")))
	;;;;;;;;;;;;;;;;;;; A COMPLETER EVENTUELLEMENT ? V2RIFIER CHEMIN RELATIF FONCTIONNE
	
	; Valeurs par défaut, si elles existent
	; (if (setq LstSection (DTIGetSection SAVPath "DTdraw")) (progn
			; (setq doctype_default (DTIGetVal LstSection "DOCTYPE"))
			; (setq scale_default (DTIGetVal LstSection "SCALE"))

	; ));Type de document
	;;;;;;;;;;;;;;;;;;; A POURSUIVRE AVEC AUTRES Valeurs ? Ou trouver système pour conserver dernière valeurs ?
	
	; Chargement du paramétrage
	(setq T_OBJTYPES (DTCGetCSV T_OBJTYPESPath))
	(setq T_DOCTYPES (DTCGetCSV T_DOCTYPESPath))
	(setq T_SCALES (DTCGetCSV T_SCALESPath))
	; (setq T_CADLAYERS (DTCGetCSV T_CADLAYERSPath))	
	(setq T_OBJCLASSES (DTCGetCSV T_OBJCLASSESPath))
	(setq T_CADATT (DTCGetCSV T_CADATTPath))	
	(setq T_CADATTENUM (DTCGetCSV T_CADATTENUMPath))
	
	; Liste des types de fichiers possibles
	(setq lsDOCTYPES (DTCGetField T_DOCTYPES "INVCAD_DOCTYPE")) ; récupération de tous les types de fichiers
	(if (not lsDOCTYPES) (DTDispError "Erreur paramétrage, champ INVCAD_DOCTYPE non trouvé dans T_DOCTYPES"))
	(setq lsDOCTYPES (UtSupDoublonsList lsDOCTYPES)) ; suppression des éventuels doublons dans la liste
	(setq lsDOCTYPES (UtSupEltListe "##" lsDOCTYPES 0)) ; supression des "entrées vides" : types de documents ne donnant pas lieu à une aide au dessin
	(if (member "" lsDOCTYPES) (DTDispError (strcat "Erreur paramétrage type de document non renseigné"))) ; Test : pas d'entrées vides non voulues
	(setq lsDOCTYPES (UtAddNieme lsDOCTYPES 999999999999999 "annUler")) ; ajout de la possibilité d'annuler
	(setq stDOCTYPES (UtLst2Str lsDOCTYPES " ")) ; création de la chaîne de caractères pour initget
	(setq staDOCTYPES (UtLst2Str lsDOCTYPES "/")) ; création de la chaîne de caractères pour affichage ligne de commande
	
	; Liste des échelles possibles
	(setq lsSCALES (DTCGetField T_SCALES "INVCAD_SCALE"))
	(if (not lsSCALES) (DTDispError "Erreur paramétrage, champ INVCAD_SCALE non trouvé dans T_SCALES"))
	(setq lsSCALES (UtSupDoublonsList lsSCALES))
	(setq lsSCALES (UtSupEltListe "##" lsSCALES 0))
	(if (member "" lsSCALES) (DTDispError (strcat "Erreur paramétrage échelle non renseignée")))
	(setq lsSCALES (UtAddNieme lsSCALES 999999999999999 "annUler"))
	(setq stSCALES (UtLst2Str lsSCALES " "))
	(setq staSCALES (UtLst2Str lsSCALES "/"))
	
	; Première boucle : choix du type de fichier
	; ;;------------------------------------------
	(while (not OutFile)
		
		(if (> (length lsDOCTYPES) 2) (progn ; plusieurs docs possibles, on demande à l'utilisateur son choix
			(initget stDOCTYPES)
			(setq doctype (getkword (strcat "\nType de document [" staDOCTYPES "] <" (car lsDOCTYPES) "> : ")))
			(if (not doctype) (setq doctype (car lsDOCTYPES)))
		)( ; un seul choix possible, on se passe de l'avis de l'utilisateur :)
			setq doctype (car lsDOCTYPES)
		)); end if		
		
		(if (/= doctype "annUler") (progn
		
		;; INSERTION DES CALQUES, BLOCS, TYPES DE LIGNES, STYLES DE COTES ET DE TEXTES : par rechargement du gabarit correspondant à ce type de fichier
		(setq criteres (list (cons "INVCAD_DOCTYPE" doctype)))
		(setq LsParamDoc (DTCGetFilteredLinesAND T_DOCTYPES criteres))
		(setq GABFile (car (DTCGetField LsParamDoc "CAD_GABARIT")))
		(if (not GABFile) (DTDispError (strcat "Erreur paramétrage, champ CAD_GABARIT non trouvé pour : " doctype " dans T_DOCTYPES")))
		(if (not (findfile (strcat (UtRep (findfile "DTdraw.lsp")) "Gabarits\\" GABFile ".dwg"))) (DTDispError (strcat "Erreur gabarit non trouvé : " GABFile ".dwg")) (progn
			(command "_.INSERT" (strcat (UtRep (findfile "DTdraw.lsp")) "Gabarits\\" GABFile ".dwg")) (command)
		))
		
		; Seconde boucle : choix de l'échelle
		; ;;-------------------------------------------------
		(while (not OutScale)
		
			(if (> (length lsSCALES) 2) (progn ; plusieurs échelles possibles, on demande à l'utilisateur son choix
				(initget stSCALES)
				(setq scale (getkword (strcat "\nEchelle du dessin [" staSCALES "] <" (car lsSCALES) "> : ")))
				(if (not scale) (setq scale (car lsSCALES)))
				)( ; un seul choix possible, on se passe de l'avis de l'utilisateur :)
					setq scale (car lsSCALES)
				)); end if

				(if (/= scale "annUler") (progn
			
				; Troisième boucle : choix de la famille d'objets, selon le type de doc et l'échelle
				; ;;---------------------------------------------------------------------------------
				(while (not OutObjClass)
				
					; On récupère la liste des familles qui répondent aux critères choisis
					(setq criteres (list (cons "INVCAD_DOCTYPE" doctype) (cons "INVCAD_SCALE" scale)))
					(setq V_OBJCLASSES(DTCGetFilteredLinesAND T_OBJCLASSES criteres))
					(setq lsOBJCLASSES (DTCGetField V_OBJCLASSES "INVCAD_OBJCLASS"))
					(if (not lsOBJCLASSES) (DTDispError "Erreur paramétrage, champ INVCAD_OBJCLASS non trouvé pour : " doctype " dans T_OBJCLASSES"))
					(setq lsOBJCLASSES (UtSupDoublonsList lsOBJCLASSES))
					(setq lsOBJCLASSES (UtSupEltListe "##" lsOBJCLASSES 0))
					(if (member "" lsOBJCLASSES) (DTDispError (strcat "Erreur paramétrage classe d'objet non renseignée")))
					(setq lsOBJCLASSES (UtAddNieme lsOBJCLASSES 999999999999999 "annUler"))
					(setq stOBJCLASSES (UtLst2Str lsOBJCLASSES " "))
					(setq staOBJCLASSES (UtLst2Str lsOBJCLASSES "/"))
					
					; Choix de la famille d'objets
					(if (> (length lsOBJCLASSES) 2) (progn ; plusieurs familles d'objets possibles, on demande à l'utilisateur son choix
						(initget stOBJCLASSES)
						(setq objclass (getkword (strcat "\nFamille d'objets à dessiner [" staOBJCLASSES "] <" (car lsOBJCLASSES) "> : ")))
						(if (not objclass) (setq objclass (car lsOBJCLASSES)))
					)( ; un seul choix possible, on se passe de l'avis de l'utilisateur :)
						setq objclass (car lsOBJCLASSES)
					)); end if
					
					(if (/= objclass "annUler") (progn
					
						; Quatrième boucle : choix de l'objet, selon les choix précédents
						; ;;---------------------------------------------------------------------------------------------------
						(while (not OutObjTyp)
						
							; On récupère la liste des objets qui répondent aux critères choisis
							(setq criteres (list (cons "INVCAD_DOCTYPE" doctype) (cons "INVCAD_SCALE" scale) (cons "INVCAD_OBJCLASS" objclass)))
							(setq V_OBJTYPES (DTCGetFilteredLinesAND T_OBJTYPES criteres))
							(setq lsOBJTYPES (DTCGetField V_OBJTYPES "INVCAD_OBJTYPE"))
							(if (not lsOBJTYPES) (DTDispError "Erreur paramétrage, champ INVCAD_OBJTYPE non trouvé pour : " objclass " dans T_OBJTYPES"))
							(setq lsOBJTYPES (UtSupDoublonsList lsOBJTYPES))
							(setq lsOBJTYPES (UtSupEltListe "##" lsOBJTYPES 0))
							(if (member "" lsOBJTYPES) (DTDispError (strcat "Erreur paramétrage type objet non renseigné")))
							(setq lsOBJTYPES (UtAddNieme lsOBJTYPES 999999999999999 "annUler"))
							(setq stOBJTYPES (UtLst2Str lsOBJTYPES " "))
							(setq staOBJTYPES (UtLst2Str lsOBJTYPES "/"))
							
							; Choix du type d'objet
							(if (> (length lsOBJTYPES) 2) (progn ; plusieurs objets possibles, on demande à l'utilisateur son choix
								(initget stOBJTYPES)
								(setq objtyp (getkword (strcat "\nType d'objet à dessiner [" staOBJTYPES "] <" (car lsOBJTYPES) "> : ")))
								(if (not objtyp) (setq objtyp (car lsOBJTYPES)))
							)( ; un seul choix possible, on se passe de l'avis de l'utilisateur :)
								setq objtyp (car lsOBJTYPES)
							)); end if	
								
							(if (/= objtyp "annUler") (progn
								
								; récupérer le paramétrage de l'objet
								(setq criteres (list (cons "INVCAD_DOCTYPE" doctype) (cons "INVCAD_SCALE" scale) (cons "INVCAD_OBJCLASS" objclass) (cons "INVCAD_OBJTYPE" objtyp)))
								(setq LsParamObj (DTCGetFilteredLinesAND V_OBJTYPES criteres))
								(if (/= 2 (length LsParamObj)) (DTDispError (strcat "Erreur paramétrage non trouvé ou double, type objet " objtyp)))
								
								; dessin de l'objet
								; A POURSUIVRE
								;(F_DTdobj LsParamObj T_CADLAYERS T_CADATTVAL)
								(F_DTdobj LsParamObj T_CADATT T_CADATTVAL)
															
							)
							(setq OutObjTyp T)
							); end if
						); FIN Quatrième boucle choix du type d'objet
						;;-------------------------------------------------------------------------------------------------------
					(setq OutObjTyp nil)				
					)
					(setq OutObjClass T)
					); end if
				); FIN Troisième boucle choix de la famille d'objets
				;;--------------------------------------------------
			(setq OutObjClass nil)
			)
			(setq OutScale T)
			); end if
		); FIN Seconde boucle choix de l'échelle
		;;--------------------------------------	
		(setq OutScale nil)
		)
		(setq OutFile T)
		); end if
	); FIN Première boucle choix du type de fichier
	;;---------------------------------------------
	(setq OutFile nil)
	(prompt "\n Quelle patience :)")
	
); FIN Fonction principale


;; Fonction générale de dessin des objets
 ;********************************************************************
 
 (defun F_DTdobj ( LsParamObj T_CADATT T_CADATTVAL 
					/ entitytype celtype cecolor clayer ltype couleur calque epaisseur scale alt pt1 pt2 pt4)
 
	(setq 
		entitytype ""
		celtype ""
		cecolor ""
		clayer ""
		celweight 0
		ltype ""
		couleur ""
		calque ""
		epaisseur ""
		scale 0
		alt ""
		pt1 '()
		pt2 '()
		pt4 '()
		)
 
	(setq celtype (getvar "CELTYPE"))
	(setq cecolor (getvar "CECOLOR"))
	(setq clayer (getvar "CLAYER"))
	(setq celweight (getvar "CELWEIGHT"))
	
	; on rend courant le bon type de ligne
	(setq ltype (car (DTCGetField LsParamObj "CADLINETYPES")))
	(if (or (not ltype) (= ltype "")) (setq ltype celtype)); si le type de ligne n'est pas indiqué, on prend le type courant
	(if (not (tblsearch "LTYPE" ltype)) (DTDispError (strcat "erreur type de ligne objet manquant : " (UtLst2Str (cadr LsParamObj) "/") ))) ; le type de ligne doit déjà exister et être valide
	(setvar "CELTYPE" ltype)
	
	; idem couleur
	(setq couleur (car (DTCGetField LsParamObj "CADCOLORS")))
	(if (or (not couleur) (= couleur "")) (setq couleur cecolor)); si la couleur n'est pas indiquée, on prend la couleur courante
	(if (or (> '-249 (atoi couleur)) (< 249 (atoi couleur))) (DTDispError (strcat "erreur couleur objet non valide : " (UtLst2Str (cadr LsParamObj) "/")))) ; la couleur doit être soit un texte, soit un entier valide
	(cond 
		((or (= "ByLayer" couleur) (= "ByBlock" couleur)) (setvar "CECOLOR" couleur))
		(T (setvar "CECOLOR" (atoi couleur)))
	)
		
	; idem calque
	(F_DTdlayer LsParamObj)	
	
	; idem épaisseur
	(setq epaisseur (car (DTCGetField LsParamObj "CADLWEIGHT")))
	(if (or (not epaisseur) (= epaisseur "")) (setq epaisseur (itoa celweight))); si l'épaisseur n'est pas indiquée, on prend l'épaisseur courante
	(if (not (member (atoi epaisseur) (list -3 -2 -1 0 5 9 13 15 18 20 25 30 35 40 50 53 60 70 80 90 100 106 120 140 158 200 211)))
		(DTDispError (strcat "erreur epaisseur objet non valide : " (UtLst2Str (cadr LsParamObj) "/")))) ; l'épaisseur doit être un entier valide (voir aide de la variable CELWEIGHT)
	(setvar "CELWEIGHT" (atoi epaisseur))
	 
	; puis on lance la bonne fonction de dessin, selon le type d'objet à dessiner
	(setq entitytype (car (DTCGetField LsParamObj "CADENTITYTYPE")))
	(cond 
		((= entitytype "Polyline") (F_DTdpolyline LsParamObj))
		((= entitytype "DimAligned") (F_DTddimaligned LsParamObj))
		((= entitytype "Rectangle") (F_DTdrectangle LsParamObj))
		((= entitytype "Text") (F_DTdtext LsParamObj)) 
		((= entitytype "DTGrid") (progn
			(setq scale (atof (car (DTCGetField LsParamObj "INVCAD_SCALE"))))
			(setq alt (substr (car (DTCGetField LsParamObj "INVCAD_OBJCLASS")) 4 1))
			;Saisie du point de base
			(initget 1)
			(setq pt1 (getpoint "\nPoint bas gauche : "))
			;Saisie de la direction horizontale
			(setq pt2 (getpoint "\nPoint bas droit : "))
			;Saisie de la direction verticale
			(setq pt4 (getpoint "\nPoint haut gauche : "))
			(F_DTGrid pt1 pt2 pt4 scale alt)
		))
		((= entitytype "BlockRef") (F_DTdblock nil LsParamObj T_CADATT T_CADATTVAL )) 
		((= entitytype "CADBLOCKATT") (F_DTdblock "T" LsParamObj T_CADATT T_CADATTVAL )) 
		( T (DTDispError (strcat "Type d'objet invalide : " (UtLst2Str (cadr LsParamObj) "/"))))
	)
 
 
	; avant de quitter : on remet les paramètres par défaut en place
	(setvar "CELTYPE" celtype)	
 	(setvar "CECOLOR" cecolor)
  	(setvar "CLAYER" clayer)
	(setvar "CELWEIGHT" celweight)
 
 ); FIN Fonction générale de dessin des objets
 
 
;; Fonction de dessin d'une polyligne
 ;********************************************************************
 (defun F_DTdpolyline ( LsParamObj / epf epc msg )
	
	(setq 
		epf 0
		epc 0
		msg ""
		)
 
	(setq ep (car (DTCGetField LsParamObj "CADLINETHICKNESS")))
	(cond
		((or (not ep) (= ep "")); épaisseur non spécifiée, on conserve celle par défaut du dessin
			(setq epf (getvar "THICKNESS")))
		((= "get:" (substr ep 1 4)) (progn ; épaisseur à demander à l'utilisateur
			(initget (+ 1 4))
			(setq epf (getreal (substr ep 5 (- (strlen ep) 4))))))
		(T ; autres cas : on convertit l'épaisseur saisie en réel et on applique
			(setq epf (atof ep)))
	)
	
	(setq epc (getvar "THICKNESS"))
	(setvar "THICKNESS" epf)
	
	(command "_PLINE") ; dessin de la polyligne
	(while (= (getvar "CMDACTIVE") 1) (command pause)) ; on laisse la main à l'utilisateur tant qu'il n'a pas fini
	
	; pour terminer on affiche le message s'il y en a un
	(setq msg (car (DTCGetField LsParamObj "CADMSG")))
	(if (and msg (/= "" msg)) (alert msg))	
	
	(setvar "THICKNESS" epc); et on remet l'épaisseur à sa valeur par défaut
	
  ); fin fonction F_DTdpolyline
  
  
  ;; Fonction de dessin d'une côte alignée
   ;********************************************************************
 (defun F_DTddimaligned ( LsParamObj / dimstyle dimscale stylecote echcote 
						)
	
	(setq 
		dimstyle ""
		dimscale 0.0
		stylecote ""
		echcote ""		
		)
 
	(setq dimstyle (getvar "DIMSTYLE"))
	(setq dimscale (getvar "DIMSCALE"))
 
	; On récupère et on rend courant le style de côte
 	(setq stylecote (car (DTCGetField LsParamObj "CADDIMSTYLES")))
	(if (or (not stylecote) (= stylecote "")) (setq stylecote dimstyle)); si le style de côte n'est pas indiqué, on prend le style courant
	(if (not (tblsearch "DIMSTYLE" stylecote)) (DTDispError (strcat "erreur style de côte objet manquant : " (UtLst2Str (cadr LsParamObj) "/") ))) ; le style de côte doit déjà exister et être valide
	(command "_DIM" "_RES" stylecote "_EXIT"); remplace setvar dimstyle car cette variable est en lecture seule
 
	; On récupère et on rend courant l'échelle de côtes
	(setq echcote (car (DTCGetField LsParamObj "CADDIMSCALE")))
	(cond
		((or (not echcote) (= echcote "")); échelle non spécifiée, on conserve celle par défaut du dessin
			(setq echcote (rtos (getvar "DIMSCALE") 2)))
		(T ; autres cas : on conserve l'échelle saisie en réel
			)
	)
	(setvar "DIMSCALE" (atof echcote))

	(command "_DIMALIGNED") ; dessin de la côte
	(while (= (getvar "CMDACTIVE") 1) (command pause)) ; on laisse la main à l'utilisateur tant qu'il n'a pas fini
	
	; pour terminer on affiche le message s'il y en a un
	(setq msg (car (DTCGetField LsParamObj "CADMSG")))
	(if (and msg (/= "" msg)) (alert msg))	
	
	(command "_DIM" "_RES" dimstyle "_EXIT"); et on remet le style de côte et l'échelle à leur valeur par défaut
	(setvar "DIMSCALE" dimscale)
	
  ); fin fonction F_DTddimaligned
  
  
  ;; Fonction de dessin d'un rectangle
 ;********************************************************************
 (defun F_DTdrectangle ( LsParamObj / epf epc msg )
	
	(setq 
		epf 0
		epc 0
		msg ""
		)
 
	(setq ep (car (DTCGetField LsParamObj "CADLINETHICKNESS")))
	(cond
		((or (not ep) (= ep "")); épaisseur non spécifiée, on conserve celle par défaut du dessin
			(setq epf (getvar "THICKNESS")))
		((= "get:" (substr ep 1 4)) (progn ; épaisseur à demander à l'utilisateur
			(initget (+ 1 4))
			(setq epf (getreal (substr ep 5 (- (strlen ep) 4))))))
		(T ; autres cas : on convertit l'épaisseur saisie en réel et on applique
			(setq epf (atof ep)))
	)
	
	(setq epc (getvar "THICKNESS"))
	(setvar "THICKNESS" epf)
	
	(command "_RECTANG") ; dessin du rectangle
	(while (= (getvar "CMDACTIVE") 1) (command pause)) ; on laisse la main à l'utilisateur tant qu'il n'a pas fini
	
	; pour terminer on affiche le message s'il y en a un
	(setq msg (car (DTCGetField LsParamObj "CADMSG")))
	(if (and msg (/= "" msg)) (alert msg))	
	
	(setvar "THICKNESS" epc); et on remet l'épaisseur à sa valeur par défaut
	
  ); fin fonction F_DTdrectangle
  
  
    ;; Fonction de dessin d'un texte
 ;********************************************************************
 (defun F_DTdtext ( LsParamObj / style size textstyle textsize pos orient index aunits )
	
	(setq 
		style ""
		size ""
		textstyle ""
		textsize 0.0
		pos '()
		orient 0.0
		index ""
		aunits 0
		)
 
	; On récupère et on rend courant le style de texte
	(setq style (car (DTCGetField LsParamObj "CADTEXTSTYLES")))
	(cond
		((or (not style) (= style "")); style non spécifié, on conserve celui par défaut du dessin
			(setq style (getvar "TEXTSTYLE")))
		(T ; autres cas : on applique le style demandé
			)
	)
	(setq textstyle (getvar "TEXTSTYLE"))
	(setvar "TEXTSTYLE" style)
	
	; On récupère et on rend courant la hauteur de texte
	(setq size (car (DTCGetField LsParamObj "CADTEXTSIZES")))
	(cond
		((or (not size) (= size "")); hauteur non spécifiée, on conserve celle par défaut du dessin
			(setq size (rtos (getvar "TEXTSIZE") 2)))
		(T ; autres cas : on conserve la hauteur saisie en réel
			)
	)
	(setq textsize (getvar "TEXTSIZE"))
	(setvar "TEXTSIZE" (atof size))
	
	; spécifique texte : autre méthode ne fonctionne pas
	(initget 1)
	(setq pos (getpoint "\nPosition du texte : "))
	(initget 1)
	(setq orient (getorient pos "\nOrientation du texte : "))
	(setq aunits (getvar "AUNITS")); pour préparer la conversion de l'angle dans les unités du dessin courant
	(initget 1)
	(setq index (getstring T "\nValeur du texte : "))
	
	(command "_TEXT" pos size (angtos orient aunits) index) ; dessin du texte
	; (while (= (getvar "CMDACTIVE") 1) (command pause)) ; on laisse la main à l'utilisateur tant qu'il n'a pas fini
	; (command pause) ; spécifique à la commande texte : il faut laisser le temps de saisir ce texte...
	
	; pour terminer on affiche le message s'il y en a un
	(setq msg (car (DTCGetField LsParamObj "CADMSG")))
	(if (and msg (/= "" msg)) (alert msg))	
	
	(setvar "TEXTSTYLE" textstyle); et on remet le style de texte et l'épaisseur à leur valeur par défaut
	(setvar "TEXTSIZE" textsize);
	
  ); fin fonction F_DTdtext
  
   ;; Fonction de dessin d'un bloc
 ;********************************************************************
 (defun F_DTdblock (CtrlAtt LsParamObj T_CADATT T_CADATTVAL / 
							nom echb blocpath Doc Space Blocobj pos orient LsBlocAtttemp LsBlocAtt BlocDef objdansbloc nomattr LsParamAtt TypSaisie invite
							ok valst mask criteres lsval kword lsenum lskword stkword stakword el attreq
							)
	
	(setq 
		nom ""
		echb 0.0
		blocpath ""
		;Doc
		;Space
		;Blocobj
		pos '()
		orient 0.0
		LsBlocAtttemp '()
		LsBlocAtt '()
		;BlocDef
		;objdansbloc
		nomattr ""
		LsParamAtt '()
		TypSaisie ""
		invite ""
		ok nil
		valst ""
		mask ""
		criteres '()
		lsval '()
		kword ""
		lsenum '()
		lskword '()
		stkword ""
		stakword ""
		el '()
		attreq 0
		)
 
	; On récupère le nom du bloc et son chemin
	(setq nom (car (DTCGetField LsParamObj "CADBLOCKS")))
	(if (or (not nom) (= nom "")) (DTDispError (strcat "Erreur de paramétrage, nom du bloc absent : " (UtLst2Str (cadr LsParamObj) "/"))))
	(setq blocpath (strcat (UtRep (findfile "DTdraw.lsp")) "Bib\\" nom ".dwg"))
	(if (not (findfile blocpath)) (DTDispError (strcat "Erreur de paramétrage, bloc non trouvé : " nom )))
	
	; Et son échelle
	(setq echb (atof (car (DTCGetField LsParamObj "CADSCALEBLOCK"))))
	(if (or (not echb) (= echb 0)) (DTDispError (strcat "Erreur paramétrage échelle bloc invalide, objet : " (UtLst2Str (cadr LsParamObj) "/") )))
	

	(if (not CtrlAtt) (progn ; on ne cherche pas à contrôler la saisie des attributs : cas bloc saisie libre
			(command "_.-INSERT" blocpath "_S" echb) ; insertion du bloc, avec pt d'insertion et rotation
			(while (= (getvar "CMDACTIVE") 1) (command pause)) ; on laisse la main à l'utilisateur tant qu'il n'a pas fini (saisie du point, des attributs, etc.)
		)
		(progn ; il est demandé de contraindre la saisie des attributs
			; on passe en partie par l'API comm car impossible autrement
			
			; récupération du point d'insertion et de la rotation ==> inutile en pasant par la commande
			; (initget 1)
			; (setq pos (getpoint "\nPoint d'insertion du bloc : "))
			; (initget 1)
			; (setq orient (getorient pos "\nRotation : "))
			; initialisations com			
			(setq	Doc (vla-get-ActiveDocument (vlax-get-acad-object))); récupération de l'objet dessin
			(setq	Space (if (= (getvar "CVPORT") 1) (vla-get-PaperSpace Doc) (vla-get-ModelSpace Doc))); espace courant
			; insertion "basique" du bloc
			(setq attreq (getvar "ATTREQ"))
			(setvar "ATTREQ" 0)
			(command "_.-INSERT" blocpath "_S" echb pause pause)
			(setvar "ATTREQ" attreq)
			; (setq	Blocobj (vla-InsertBlock Space (vlax-3d-point pos) blocpath echb echb 1 orient)) ==> ne fonctionne pas avec les blocs dynamiques (deviennent statiques :( )
			(setq	Blocobj  (vlax-ename->vla-object (entlast)))
			
			; Liste des attributs du bloc pour mise à jour  (attributs instanciés du bloc instancié)
			(setq LsBlocAtttemp (vlax-invoke Blocobj 'GetAttributes))
			(foreach el LsBlocAtttemp
				(setq LsBlocAtt (cons (list (vla-get-TagString el) el) LsBlocAtt))
			)
			(setq LsBlocAtt (reverse LsBlocAtt))
			
			; Et définition du bloc, pour accéder aux définitions des attributs (définition dans table des symboles avec objets ATTDEF)
			(setq BlocDef (vla-Item (vla-get-Blocks doc) nom))
			(vlax-for objdansbloc BlocDef
				(if (= (vla-get-ObjectName objdansbloc) "AcDbAttributeDefinition") ; pour chaque objet composant le bloc, on ne traite que les ATTDEF
				(progn
				
					;on récupère l'étiquette du bloc
					(setq nomattr (vlax-get-property objdansbloc 'TagString))
					;l'invite
					(setq invite (vlax-get-property objdansbloc 'PromptString))
					; et les paramètres de l'attribut depuis le paramétrage
					(setq criteres (list (cons "CADBLOCK" nom) (cons "CADATT_NAME" nomattr)))
					(setq LsParamAtt (DTCGetFilteredLinesAND T_CADATT criteres))
					(if (or (not LsParamAtt) (/= (length LsParamAtt) 2)) (DTDispError (strcat "Erreur paramétrage, attributs en double ou absents, bloc : " nom)))
					; Type de saisie voulue pour cet attribut
					(setq TypSaisie (car (DTCGetField LsParamAtt "CADATT_TYP")))
		
					; Traitement selon le type de saisie voulue
					(cond
					
						((= TypSaisie "getstring") ; saisie d'une chaîne de texte selon masque
							(setq mask (car (DTCGetField LsParamAtt "CADATT_LISPWCMATCH")))
							(setq ok nil)
							(while (not ok)
								(setq valst (getstring T invite))
								(if (or (not valst) (= "" valst) (wcmatch valst mask)) (progn ; L'attribut saisi est conforme au masque
									(setq lsval (cons (list nomattr valst) lsval))
									(setq ok T)
								)(progn ; attribut nok
									(alert (car (DTCGetField LsParamAtt "CADATT_ERRMSG")))
								)); end if
							); end while
								
						)((= TypSaisie "getkword") ; saisie en respectant une liste de valeurs
						
							; On récupère la liste des valeurs
							(setq criteres (list (cons "CADATT_ENUM" (car (DTCGetField LsParamAtt "CADATT_ENUM")))))
							(setq lsenum (DTCGetFilteredLinesAND T_CADATTENUM criteres))
							(if (or (not lsenum) (< (length lsenum) 3)) (DTDispError (strcat "Erreur paramétrage, valeurs d'énumération manquantes : " nom "-" nomattr)))
							(setq lskword (DTCGetField lsenum "INVCAD_CADBLOCKATTVAL"))
							(if (not lskword) (DTDispError "Erreur paramétrage, champ INVCAD_CADBLOCKATTVAL non trouvé pour : " nom "-" nomattr " dans T_CADATTENUM"))
							(setq stkword (UtLst2Str lskword " "))
							(setq stakword (UtLst2Str lskword "/"))
							
							; Choix de la valeur
							(initget stkword)
							(setq valst (getkword (strcat "\n" invite " [" stakword "] : ")))
							; on "convertit" le mot clé en valeur d'énumération
							(setq valst (car (DTCGetField (DTCGetFilteredLinesAND lsenum (list (cons "INVCAD_CADBLOCKATTVAL" valst))) "CADBLOCK_ATTVAL")))
							(setq lsval (cons (list nomattr valst) lsval))
						
						)((= TypSaisie "getreal") ; saisie d'un nombre réel
							(setq mask (UtStr2Lst (car (DTCGetField LsParamAtt "CADATT_LISPRTOS")) ",")); on récupère le format à donner au réel
							(initget 0)
							(setq valr (getreal invite))
							(if valr 
								(setq lsval (cons (list nomattr (rtos valr (atoi (car mask)) (atoi (cadr mask)) ) ) lsval))
								(setq lsval (cons (list nomattr "" ) lsval))
							)
							
						)((T ; autres cas
							(DTDispError (strcat "Erreur paramétrage, type de saisie attribut invalide : " nom "-" nomattr))
						))
					); end cond
					
				)); end if ATTDEF
			); end objdansbloc
			
			; mise à jour des attributs
			(setq lsval (reverse lsval))
			(foreach el lsval
				(if (and (cadr el) (/= "" (cadr el)) (/= " " (cadr el)))
					(progn ; on ne traite que les attributs non vides
						(vlax-put-property (cadr (assoc (car el) LsBlocAtt)) 'TextString (cadr el))
					);progn
				); end if
			); fin mise à jour des attributs
			(vla-regen Doc acActiveViewport); mise à jour de l'affichage
			
		); fin insertion bloc avec contrôle attributs
	); end if contrôles attributs
				
	; pour terminer on affiche le message s'il y en a un
	(setq msg (car (DTCGetField LsParamObj "CADMSG")))
	(if (and msg (/= "" msg)) (alert msg))	
	
  ); fin fonction F_DTdblock
 
 
;; Fonction qui se positionne sur le bon layer pour le dessin d'un objet
 ;***********************************************************************
 
 (defun F_DTdlayer ( LsParamObj / cadlayer clayer)
	
	(setq 
		cadlayer ""
		clayer ""
		)
	
	(setq cadlayer (car (DTCGetField LsParamObj "CADLAYERS")))
	(if (or (not cadlayer) (= cadlayer "")) (UtDispError (strcat "Erreur de paramétrage, nom du calque absent : " (UtLst2Str (cadr LsParamObj) "/"))))
	(setq clayer (getvar "CLAYER"))
	
	(if (/= clayer cadlayer) (progn ; le calque courant n'est pas le bon
		(if (not (tblobjname "LAYER" cadlayer)) (progn ; le calque demandé n'existe pas
			(UtDispError (strcat "Erreur paramétrage ou gabarit, calque inexistant : " cadlayer)) ; Message d'erreur à remplacer à terme par la création du calque (voir lignes ci dessous
			; (setq LsParamLayer (DTCGetFilteredLinesAND T_CADLAYERS (cons 'CADLAYER_NAME" cadlayer)))
			; Erreur si inexistant ou mal renseigné
			;(setq couleur (car (DTCGetField ("CADLAYER_COLOR" LsParamLayer))))
			; Erreur si non renseignée
			; Idem type de ligne, épaisseur et description
			; (DTCreaLay cadlayer couleur typeligne ep desc)
			)
		(progn ; Le calque existe déjà
			(DtInitLay cadlayer)
			(setvar "CLAYER" cadlayer)
		))
	))
	 
 ); fin fonction DTdlayer
 
(prompt  "\nDocTekus chargement DTdraw v0.0.1 - licence GNU GPL v3")
(prompt "\n CADaGEO - DocTekus outil d'aide au dessin chargé, taper DTd pour lancer la commande - licence GNU GPL v3")
(princ)
;Clean chargement