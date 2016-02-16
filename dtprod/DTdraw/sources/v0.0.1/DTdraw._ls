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
(defun F_DTdraw( / INIPath SAVPath T_OBJTYPESPath T_DOCTYPESPath T_SCALESPath T_CADLAYERSPath T_OBJCLASSESPath T_CADATTVALPath ;GABPath
					GABFile LstSection doctype_default scale_default
					T_OBJTYPES T_DOCTYPES T_SCALES T_CADLAYERS T_OBJCLASSES T_CADATTVAL V_OBJCLASSES V_OBJTYPES
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
		T_CADLAYERSPath ""
		T_OBJCLASSESPath ""
		T_CADATTVALPath ""
		
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
		T_CADLAYERS '()
		T_OBJCLASSES '()
		T_CADATTVAL '()
		
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
	(if (findfile "DTdraw.lsp") (setq INIPath (findfile "DTdraw.lsp")) (UtDispError  "Manque les chemins de recherche de fichiers de support - Dtdraw.lsp non trouve"))
  	(setq INIPath (UtRep INIPath))

	(setq SAVPath (strcat INIPath "DTdraw.sav"))
	(setq T_OBJTYPESPath (strcat INIPath "T_OBJTYPES.csv"))	
	(setq T_DOCTYPESPath (strcat INIPath "T_DOCTYPES.csv"))	
	(setq T_SCALESPath (strcat INIPath "T_SCALES.csv"))	
	(setq T_CADLAYERSPath (strcat INIPath "T_CADLAYERS.csv"))	
	(setq T_OBJCLASSESPath (strcat INIPath "T_OBJCLASSES.csv"))	
	(setq T_CADATTVALPath (strcat INIPath "T_CADATTVAL.csv"))	
	
	(setq INIPath (strcat INIPath "DTdraw.ini"))
		
	; Ouverture et lecture du fichier INI
	(if (not (DTIGetSection INIPath "DTdraw")) (UtDispError "Section par défaut du fichier ini non trouvee"))
	; (if (setq LstSection (DTIGetSection INIPath "DTdraw")) (setq GABPath (DTIGetVal LstSection "GABARITS")))
	;;;;;;;;;;;;;;;;;;; A COMPLETER EVENTUELLEMENT ? V2RIFIER CHEMIN RELATIF FONCTIONNE
	
	; Valeurs par défaut, si elles existent
	(if (setq LstSection (DTIGetSection SAVPath "DTdraw")) (progn
			(setq doctype_default (DTIGetVal LstSection "DOCTYPE"))
			(setq scale_default (DTIGetVal LstSection "SCALE"))

	));Type de document
			
	;;;;;;;;;;;;;;;;;;; A POURSUIVRE AVEC AUTRES Valeurs
	
	; Chargement du paramétrage
	(setq T_OBJTYPES (DTCGetCSV T_OBJTYPESPath))
	(setq T_DOCTYPES (DTCGetCSV T_DOCTYPESPath))
	(setq T_SCALES (DTCGetCSV T_SCALESPath))
	(setq T_CADLAYERS (DTCGetCSV T_CADLAYERSPath))	
	(setq T_OBJCLASSES (DTCGetCSV T_OBJCLASSESPath))	
	(setq T_CADATTVAL (DTCGetCSV T_CADATTVALPath))
	
	; Liste des types de fichiers possibles
	(setq lsDOCTYPES (DTCGetField T_DOCTYPES "INVCAD_DOCTYPE")) ; récupération de tous les types de fichiers
	(if (not lsDOCTYPES) (UtDispError "Erreur paramétrage, nom colonne INVCAD types de documents"))
	(setq lsDOCTYPES (UtSupDoublonsList lsDOCTYPES)) ; suppression des éventuels doublons dans la liste
	(setq lsDOCTYPES (UtSupEltListe "##" lsDOCTYPES 0)) ; supression des "entrées vides" : types de documents ne donnant pas lieu à une aide au dessin
	(if (member "" lsDOCTYPES) (UtDispError (strcat "Erreur paramétrage type de document non renseigné"))) ; Test : pas d'entrées vides non voulues
	(setq lsDOCTYPES (UtAddNieme lsDOCTYPES 999999999999999 "annUler")) ; ajout de la possibilité d'annuler
	(setq stDOCTYPES (UtLst2Str lsDOCTYPES " ")) ; création de la chaîne de caractères pour initget
	(setq staDOCTYPES (UtLst2Str lsDOCTYPES "/")) ; création de la chaîne de caractères pour affichage ligne de commande
	
	; Liste des échelles possibles
	(setq lsSCALES (DTCGetField T_SCALES "INVCAD_SCALE"))
	(if (not lsSCALES) (UtDispError "Erreur paramétrage, nom colonne INVCAD échelles"))
	(setq lsSCALES (UtSupDoublonsList lsSCALES))
	(setq lsSCALES (UtSupEltListe "##" lsSCALES 0))
	(if (member "" lsSCALES) (UtDispError (strcat "Erreur paramétrage échelle non renseignée")))
	(setq lsSCALES (UtAddNieme lsSCALES 999999999999999 "annUler"))
	(setq stSCALES (UtLst2Str lsSCALES " "))
	(setq staSCALES (UtLst2Str lsSCALES "/"))
	
	; Première boucle : choix du type de fichier
	; ;;------------------------------------------
	(while (not OutFile)
		
		(initget stDOCTYPES)
		(setq doctype (getkword (strcat "\nType de document [" staDOCTYPES "] (" doctype_default ") : ")))
		; (initget 1 stDOCTYPES)
		; (setq doctype (getkword (strcat "\nType de document [" staDOCTYPES "] : ")))
		(if (not doctype) (setq doctype doctype_default))
		(if (/= doctype "annUler") (progn
		
		;; INSERTION DES CALQUES, BLOCS, TYPES DE LIGNES, STYLES DE COTES ET DE TEXTES : par rechargement du gabarit correspondant à ce type de fichier
		(setq criteres (list (cons "INVCAD_DOCTYPE" doctype)))
		(setq LsParamDoc (DTCGetFilteredLinesAND T_DOCTYPES criteres))
		(setq GABFile (car (DTCGetField LsParamDoc "CAD_GABARIT")))
		(if (not GABFile) (UtDispError (strcat "Erreur paramétrage champ gabarit non trouvé : " doctype))
		(if (not (findfile (strcat GABFile ".dwg"))) (UtDispError (strcat "Erreur gabarit non trouvé : " GABFile ".dwg")) (progn
			(command "_.INSERT" GABFile) (command)
		))
		
		; Seconde boucle : choix de l'échelle
		; ;;-------------------------------------------------
		(while (not OutScale)
		
			(initget stSCALES)
			(setq scale (getkword (strcat "\nEchelle du dessin [" staSCALES "] (" scale_default ") : ")))
			; (initget 1 stSCALES)
			; (setq scale (getkword (strcat "\nEchelle du dessin [" staSCALES "] : ")))
			(if (not scale) (setq scale scale_default))
			(if (/= scale "annUler") (progn
			
				; Troisième boucle : choix de la famille d'objets, selon le type de doc et l'échelle
				; ;;---------------------------------------------------------------------------------
				(while (not OutObjClass)
				
					; On récupère la liste des familles qui répondent aux critères choisis
					(setq criteres (list (cons "INVCAD_DOCTYPE" doctype) (cons "INVCAD_SCALE" scale)))
					(setq V_OBJCLASSES(DTCGetFilteredLinesAND T_OBJCLASSES criteres))
					(setq lsOBJCLASSES (DTCGetField V_OBJCLASSES "INVCAD_OBJCLASS"))
					(if (not lsOBJCLASSES) (UtDispError "Erreur paramétrage, nom colonne INVCAD famille d'objet"))
					(setq lsOBJCLASSES (UtSupDoublonsList lsOBJCLASSES))
					(setq lsOBJCLASSES (UtSupEltListe "##" lsOBJCLASSES 0))
					(if (member "" lsOBJCLASSES) (UtDispError (strcat "Erreur paramétrage classe d'objet non renseignée")))
					(setq lsOBJCLASSES (UtAddNieme lsOBJCLASSES 999999999999999 "annUler"))
					(setq stOBJCLASSES (UtLst2Str lsOBJCLASSES " "))
					(setq staOBJCLASSES (UtLst2Str lsOBJCLASSES "/"))
					
					; Choix de la famille d'objets
					(initget 1 stOBJCLASSES)
					(setq objclass (getkword (strcat "\nFamille d'objets à dessiner [" staOBJCLASSES "] : ")))
					(if (/= objclass "annUler") (progn
					
						; Quatrième boucle : choix de l'objet, selon les choix précédents
						; ;;---------------------------------------------------------------------------------------------------
						(while (not OutObjTyp)
						
							; On récupère la liste des objets qui répondent aux critères choisis
							(setq criteres (list (cons "INVCAD_DOCTYPE" doctype) (cons "INVCAD_SCALE" scale) (cons "INVCAD_OBJCLASS" objclass)))
							(setq V_OBJTYPES (DTCGetFilteredLinesAND T_OBJTYPES criteres))
							(setq lsOBJTYPES (DTCGetField V_OBJTYPES "INVCAD_OBJTYPE"))
							(if (not lsOBJTYPES) (UtDispError "Erreur paramétrage, nom colonne INVCAD objets"))
							(setq lsOBJTYPES (UtSupDoublonsList lsOBJTYPES))
							(setq lsOBJTYPES (UtSupEltListe "##" lsOBJTYPES 0))
							(if (member "" lsOBJTYPES) (UtDispError (strcat "Erreur paramétrage type objet non renseigné")))
							(setq lsOBJTYPES (UtAddNieme lsOBJTYPES 999999999999999 "annUler"))
							(setq stOBJTYPES (UtLst2Str lsOBJTYPES " "))
							(setq staOBJTYPES (UtLst2Str lsOBJTYPES "/"))
							
							; Choix du type d'objet
							(initget 1 stOBJTYPES)
							(setq objtyp (getkword (strcat "\nType d'objet à dessiner [" staOBJTYPES "] : ")))
							(if (/= objtyp "annUler") (progn
								
								; récupérer le paramétrage de l'objet
								(setq criteres (list (cons "INVCAD_DOCTYPE" doctype) (cons "INVCAD_SCALE" scale) (cons "INVCAD_OBJCLASS" objclass) (cons "INVCAD_OBJTYPE" objtyp)))
								(setq LsParamObj (DTCGetFilteredLinesAND V_OBJTYPES criteres))
								(if (/= 2 (length LsParamObj)) (UtDispError (strcat "Erreur paramétrage non trouvé ou double, type objet " objtyp)))
								
								; dessin de l'objet
								; A POURSUIVRE
								;(F_DTdobj LsParamObj T_CADLAYERS T_CADATTVAL)
								(F_DTdobj LsParamObj)
															
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
 
 (defun F_DTdobj ( LsParamObj ;T_CADATT T_CADATTVAL 
					/ entitytype celtype cecolor clayer ltype couleur calque)
 
	(setq 
		entitytype ""
		celtype ""
		cecolor 0
		clayer ""
		ltype ""
		couleur 0
		calque ""
		)
 
	(setq celtype (getvar "CELTYPE"))
	(setq cecolor (getvar "CECOLOR"))
	(setq clayer (getvar "CLAYER"))
	
	; on rend courant le bon type de ligne
	(setq ltype (car (DTCGetField LsParamObj "CADLINETYPES")))
	(if (not (tblsearch "LTYPE" ltype)) (UtDispError (strcat "erreur type de ligne objet manquant : " (UtLst2Str LsParamObj) ))) ; le type de ligne doit déjà exister et être valide
	(setvar "CELTYPE" ltype)
	
	; idem couleur
	(setq couleur (car (DTCGetField LsParamObj "CADCOLORS")))
	(if (/= (type couleur) 'INT) (UtDispError (strcat "erreur couleur objet non valide : " (UtLst2Str LsParamObj) ))) ; la couleur doit être un entier valide
	(if (or (> '-249 couleur) (< 249 couleur)) (UtDispError (strcat "erreur couleur objet non valide : " (UtLst2Str LsParamObj))))
	(setvar "CECOLOR" couleur)
	
	; idem calque
	(F_DTdlayer LsParamObj)	
 
	; puis on lance la bonne fonction de dessin, selon le type d'objet à dessiner
	(setq entitytype (car (DTCGetField LsParamObj "CADENTITYTYPE")))
	(cond 
		((= entitytype "Polyline") (F_DTdpolyline))
		( T (UtDispError (strcat "Type d'objet invalide : " (UtLst2Str LsParamObj))))
	)
 
 
 ; (command "_DIMALIGNED" pause pause pause)
 
	; avant de quitter : on remet les paramètres par défaut en place
	(setvar "CELTYPE" celtype)	
 	(setvar "CECOLOR" cecolor)
  	(setvar "CLAYER" clayer)
 
 ); FIN Fonction générale de dessin des objets
 
 
;; Fonction de dessin d'une polyligne
 (defun F_DTdpolyline ( LsParamObj / epf epc msg )
	
	(setq 
		epf 0
		epc 0
		msg ""
		)
 
	(setq ep (car (DTCGetField LsParamObj "CADLINETHICKNESS")))
	(if (/= (type ep) 'REAL) ; La valeur de l'épaisseur n'est pas numérique : faut-il la récupérer ?
		(if (= "get:" (substr ep 1 4)) (progn ; Il faut la demander
			(initget (+ 1 4))
			(setq epf (getreal (substr ep 5 (- (strlen ep) 4))))
		)
		(; si l'épaisseur n'est pas numérique et ne commence pas par get:, on considère qu'il faut conserver celle par défaut du dessin
			setq epf (getvar "THICKNESS")
		))
	(; l'épaisseur est numérique : on l'utilise
		setq epf ep
	))
	
	(setq epc (getvar "THICKNESS"))
	(setvar "THICKNESS" epf)
	
	(command "_PLINE") ; dessin de la polyligne
	(while (= (getvar "CMDACTIVE") 1) (command pause)) ; on laisse la main à l'utilisateur tant qu'il n'a pas fini
	
	; pour terminer on affiche le message s'il y en a un
	(setq msg (car (DTCGetField LsParamObj "CADMSG")))
	(if (and (not msg) (/= "" msg)) (princ (strcat "/n " msg)))	
	
	(setvar "THICKNESS" epc); et on remet l'épaisseur à sa valeur par défaut
	
  ); fin fonction F_DTdpolyline
 
;; Fonction qui se positionne sur le bon layer pour le dessin d'un objet
 ;***********************************************************************
 
 (defun F_DTdlayer ( LsParamObj / cadlayer clayer)
	
	(setq 
		cadlayer ""
		clayer ""
		)
	
	(setq cadlayer (car (DTCGetField LsParamObj "CADLAYERS")))
	(if (or (not cadlayer) (= cadlayer "")) (UtDispError (strcat "Erreur de paramétrage, nom du calque absent : " (UtLst2Str LsParamObj))))
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
		( ; Le calque existe déjà
			setvar "CLAYER" cadlayer)
		)
	))
	 
 ); fin fonction DTdlayer
 
(prompt  "\nDocTekus chargement DTdraw v0.0.1 - licence GNU GPL v3")
(prompt "\n CADaGEO - DocTekus outil d'aide au dessin chargé, taper DTd pour lancer la commande - licence GNU GPL v3")
(princ)
;Clean chargement