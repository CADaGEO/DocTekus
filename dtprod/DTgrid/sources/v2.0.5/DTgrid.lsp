;;Copyright 2012-2015, CADaGEO - Guillaume BERSON, Couskou, Cristel LEGRAND
;;Version 2.0.5 du 09/11/2015
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
;;Cr�ation de carroyage
;;
;;INSTALLATION ET FONCTIONNEMENT *************************************
;*********************************************************************
;;
;; Conserver tous les fichiers du package dans un m�me r�pertoire
;; S'assurer que les blocs Nord et Echelle d�finis dans le fichier ini soient bien pr�sents
;;
;; Quand UN SEUL AutoCAD est lanc� / Installation pour un usage courant :
;; - Ajouter le chemin dudit r�pertoire dans les chemins de recherche de fichiers de support (_options / fichiers)
;; - charger le lisp DTgrid.lsp syst�matiquement au d�marrage d'AutoCAD (_appload, valisette "contenu" dans "au d�marrage" puis red�marrer Autocad)
;;
;; Utilisation occasionnelle (et partielle : pas de fl�che nord et �chelle par d�faut) :
;; - taper (load "DTgrid.lsp") � partir de la ligne de commande d'AutoCAD(c) avant de l'utiliser
;;
;; Fonctionnement :
;; Taper "DTGrid" dans la ligne de commande et se laisser guider 
;;
;;CHANGELOG **********************************************************
;;********************************************************************
;;
;;Version 2.0.5 du 09/11/2015
;;- Fix bug blocs �chelle et nord qui s'ins�rent avec un angle faux dans l'environnement SNCF : passage par les commandes vl- au lieu de la commande INSERT
;;
;;Version 2.0.4 du 27/07/2014
;;- restructuration pour pouvoir encapsuler DTgrid dans DTdraw (commande dtd)
;;
;;Version 2.0.3 du 31/12/2013
;;- fix bug (insunitSdefsource et deftarget)
;;
;;Version 2.0.2 du 16/12/2013
;;- forcage des variables d'�chelle d'insertion des blocs � 0 (insunits, insunitdefsource et deftarget)
;;
;;Version 2.0.1 du 09/12/2013
;;- fix message d'erreur syst�matique insertion bloc �chelle
;;- fix d�g�le et activation syst�matique des calques utilis�s / puis restauration � l'�tat d'origine
;;- version pour diffusion
;;- fix placement des r�f�rences de planches adjacentes alt2
;;
;;Version 2.0.0alpha3 du 02/12/2013
;;- Int�gration au projet DocTekus : barbacar devient DTgrid
;;- Ajout d'un fichier ini avec options de param�trages suppl�mentaires (calques, alternatives, styles de textes, limites adjacentes...)
;;- Ajout des insertions de blocs nord et �chelles
;;- Correction bug orientation textes lorsque Nord vers le bas
;;- Ajout de contr�les de saisie
;;- Optimisation et nettoyage du code
;;
;;Versions 2.0.0alpha1 (28/11/2013), alpha2 (couskou 31/11/2013) et alpha3 (02/12/2013) 
;;- pour tests et debugs
;;
;;Version 1.5 du 31/10/2013
;;- Ajout de la fonction "Undo"
;;
;;Version 1.0.4 du 02/06/2013
;;- Correction de nom de commande
;;
;;Versions 1.0.2 et 1.0.3 du 18/01/2013
;;- Suppression des chiffres apr�s la virgule dans les coordonn�es (concerne uniquement certaines versions d�Autocad)
;;
;;Version 1.0.1 du 06/11/2012
;;- Correction du nom de la commande (qui �tait Carroyage au lieu de Car)
;;
;;Version 1.0.0 du 30/10/2012
;;- Diffusion sous licence GNU
;;
;;Version 0.2.1 du 30/09/2012
;;- Passage en SCU G�n�ral forc�
;;- Ecart entre croix forc� (10cm quand inf�rieur � 1/1000, 5cm quand sup�rieur)
;;
;;Version 0.2 du 19/09/2012
;;- Prise en compte du carroyage � l�envers (plans ayant le nord vers le bas)
;;- Saisie utilisateur pour la distance entre deux croix
;;- Passage du SCU en g�n�ral apr�s question utilisateur
;;
;;Version 0.1 du 10/09/2012
;;- Mise � disposition initiale de la version 0.1
;;
;;
;;DEPENDANCES *******************************************************
;;*******************************************************************
;; N�cessite les fonctions de DTini, qui lui-m�me charge ld-ut et dtut
;; N�cessite les fonctions ld-ut DTut, charg�es via DTini
;; Utilise les fonctions vl*
;;
;;TODO, pochaine version *********************************************
;;********************************************************************
;; Revoir les valeurs par d�faut : les s�parer du ini et les mettre dans un fichier sav d�di�
;; r�-�criture du fichier ini pour conserver les derni�res valeurs utilis�es
;; si le rectangle d�fini par les 3 points existe d�j�, ne pas le recr�er
;; permettre l'utilisation des chemins relatifs
;;
;**********************************************************************
;**********************************************************************

;chargement des d�pendances
 (if (findfile "DTini.lsp") (load "DTini.lsp") (prompt "\nErreur DTini.lsp non trouv�"))

 
 ; Commande de dessin
 ;********************************************************************
(defun c:DTGrid(/ 	pt1 pt2 pt4 echelle alt ech_default  alt_default INIPath LstSection stech_default)

	; pour que le programme puisse �tre annul� comme une seule commande
	(setvar "CMDECHO" 0)
	(command "_UNDO" "_BE")
	
	;;Initialisation des variables globales
	(setq $Var '()
		$Sav '()
	)
	
	; Initialisation des variables locales
	;pt1 � pt4 getpoint
	(setq 
		echelle 0.0
		alt ""
		ech_default 0
		stech_default ""
		alt_default ""
		INIPath ""
		LstSection '()
		)
		
	; Gestion des erreurs
	(UtInitError)

	;RAZ des messages utilsateurs
	(setq $DTLogUser "")
	
	
	;Chemin d'acc�s au fichier ini
	(if (findfile "DTgrid.lsp") (setq INIPath (findfile "DTgrid.lsp")) (DTDispError "Erreur chemin courant non trouv�"))
	;;31/11/13 - couskou	: extraction du chemin d'acc�s au fichier INI par la fonction UtRep, plus "souple" si le nom de dossier contenant DTIni.lsp
  	;; n'est pas �gal � "DTgrid"
	;;(setq INIPath (substr INIPath 1 (- (strlen INIPath) 17)))
  	(setq INIPath (UtRep INIPath))
	(setq INIPath (strcat INIPath "DTgrid.ini"))
	
	; Ouverture et lecture test du fichier ini
	(if (not (DTIGetSection INIPath "DTgrid-0")) (DTDispError "Erreur section par d�faut du fichier ini non trouv�e"))
	(if (setq LstSection (DTIGetSection INIPath "DTgrid-default")) (progn
			; valeurs par d�faut
		(setq stech_default (DTIGetVal LstSection "ECHELLE"))
		(if (not stech_default) (DTDispError "Fichier ini mal renseign� (section default)"))
		(if (not (numberp (atoi stech_default))) (DTDispError "Fichier ini mal renseign� (echelle par defaut)"))
		(setq ech_default (atoi stech_default))
		(setq alt_default (DTIGetVal LstSection "ALT"))
		(cond
			((= alt_default "1") ())
			((= alt_default "2") ())
			((= alt_default "3") ())
			(T (DTDispError "Fichier ini mal renseign� (alternative par defaut)")))
	) (DTDispError "Erreur section par d�faut du fichier ini non trouv�e"))
		
	;Saisie du point de base
	(initget 1)
	(setq pt1 (getpoint "\nPoint bas gauche : "))
	;Saisie de la direction horizontale
	(setq pt2 (getpoint "\nPoint bas droit : "))
	;Saisie de la direction verticale
	(setq pt4 (getpoint "\nPoint haut gauche : "))
	
	;Echelle du dessin
	(initget (+ 2 4))
	(setq echelle (getreal (strcat "\nEchelle du dessin <" (itoa ech_default) "> : ")))
	(if echelle 
		() ;(if (/= echelle ech_default) (DTISetVal "DTgrid-default" "ECHELLE" echelle)) ;mise � jour de l'�chelle par d�faut avec la nouvelle �chelle --> ToDo
		(setq echelle ech_default) ;pas d'�chelle saisie : on garde celle par d�faut
	); end if
	
	;Alternative
	(initget "1 2 3")
	(setq alt (getkword (strcat "\nAlternative � utiliser [1/2/3] <" alt_default "> : ")))
	(if alt
		() ;(if (/= alt alt_default) (DTISetVal "DTgrid-default" "ALT" alt)) ;mise � jour de l'alternative par d�faut avec la nouvelle --> ToDo
		(setq alt alt_default) ;pas d'alternative saisie : on garde celle par d�faut
	); end if
			
	
	(F_DTgrid pt1 pt2 pt4 echelle alt)
	

	; pour que le programme puisse �tre annu�l comme une seule commande
	(command "_UNDO" "_E")
	(setvar "CMDECHO" 1)
	
	; Gestion des erreurs
	(UtRestError)
	
	;Affichage des messages utilsateurs
	(princ $DTLogUser)
	(princ)
)
  
 
;Fonction de dessin de Carroyage
(defun F_DTGrid( pt1 pt2 pt4 echelle alt / bolSCU ;pt1 pt2 pt3 pt4 echelle 
					espace taillecroix tailletexte listx listy lx hx ly hy startHz endHz startVt endVt 
					currentHz currentVt INIPath Section LstSection ;alt ech_default alt_default 
					layer_default textsize_default 
					textstyle_default val BlockNord EchNord NordPath BlockEchel EchEchel EchelPath layeradj tailletexteadj styletexte styletexteadj
					layer pttemp ;stech_default 
					lok
					)


	;;TYPAGE des variables locales
	(setq
		bolSCU T
		;pt1 � pt4 getpoint
		; echelle 0.0
		espace 0.0
		taillecroix 0.0
		tailletexte 0.0
		INIPath ""
		Section ""
		LstSection '()
		; alt ""
		; ech_default 0
		; alt_default ""
		layer_default ""
		textsize_default 0.0
		textstyle_default ""
		val ""
		BlockNord ""
		bloc ""
		EchNord ""
		NordPath ""
		BlockEchel ""
		EchEchel ""
		EchelPath ""
		layeradj ""
		tailletexteadj 0.0
		styletexte ""
		styletexteadj ""
		layer ""
		;pttemp inters
		; stech_default ""
		lok '()
		layerEtat '()
		layerAdjEtat '()
		)
		 

	;Chemin d'acc�s au fichier ini
	(if (findfile "DTgrid.lsp") (setq INIPath (findfile "DTgrid.lsp")) (DTDispError "Erreur chemin courant non trouv�"))
	;;31/11/13 - couskou	: extraction du chemin d'acc�s au fichier INI par la fonction UtRep, plus "souple" si le nom de dossier contenant DTIni.lsp
  	;; n'est pas �gal � "DTgrid"
	;;(setq INIPath (substr INIPath 1 (- (strlen INIPath) 17)))
  	(setq INIPath (UtRep INIPath))
	(setq INIPath (strcat INIPath "DTgrid.ini"))
	
	; Ouverture et lecture test du fichier ini
	(if (not (DTIGetSection INIPath "DTgrid-0")) (DTDispError "Erreur section par d�faut du fichier ini non trouv�e"))
	(if (setq LstSection (DTIGetSection INIPath "DTgrid-default")) (progn
	
		; ; valeurs par d�faut
			; (setq stech_default (DTIGetVal LstSection "ECHELLE"))
			; (if (not stech_default) (prompt "\nFichier ini mal renseign� (section default)"))
			; (if (not (numberp (atoi stech_default))) (prompt "\nFichier ini mal renseign� (echelle par defaut)"))
			; (setq ech_default (atoi stech_default))
			; (setq alt_default (DTIGetVal LstSection "ALT"))
			; (cond
				; ((= alt_default "1") ())
				; ((= alt_default "2") ())
				; ((= alt_default "3") ())
				; (T (prompt "\nFichier ini mal renseign� (alternative par defaut)")))
		) (DTDispError "Erreur section par d�faut du fichier ini non trouv�e"))
		
	(setq layer_default (getvar "CLAYER")) ;calque courant
	(setq textsize_default (getvar "TEXTSIZE")) ;hauteur de texte courante
	(setq textstyle_default (getvar "TEXTSTYLE")) ;style de texte courant
	
	;; on cr�� la liste des variables modifi�es et leur nouvelle valeur
	(setq $Var (list (cons "cmdecho" 0) (cons "blipmode" 0) (cons "aunits" 3)  (cons "angbase" (/ pi 2)) (cons "angdir" 1) (cons "clayer" "0") (cons "TEXTSIZE" textsize_default) (cons "TEXTSTYLE" textstyle_default)
						(cons "OSMODE" (getvar "OSMODE")) (cons "INSUNITS" 0) (cons "INSUNITSDEFSOURCE" 0) (cons "INSUNITSDEFTARGET" 0)))

	;; on sauvegarde l'�tat des variables AutoCAD
	(UtSaveVar)
		  
	;; on initialise les variables avec nos valeurs
	(UtInitVar)

	;On passe en SCU G�n�ral si ce n'est pas le cas (passage obligatoire)
	(setq bolSCU T)
	(if (= (getvar "worlducs") 0)
		(progn
			(command "_UCS" "_W")
			(setq bolSCU nil)
		)
	) ;if

	; ;Saisie du point de base
	; (initget 1)
	; (setq pt1 (getpoint "\nPoint bas gauche : "))
	; ;Saisie de la direction horizontale
	; (setq pt2 (getpoint "\nPoint bas droit : "))
	; ;Saisie de la direction verticale
	; (setq pt4 (getpoint "\nPoint haut gauche : "))
	
	;Plus besoin de l'accrochage aux objets, on le d�sactive
	(Setvar "OSMODE" 0)

	;On calcule le dernier point du rectangle
	(setq pt3 (polar pt2 (angle pt1 pt4) (distance pt1 pt4)))

	; ;Echelle du dessin
	; (initget (+ 2 4))
	; (setq echelle (getreal (strcat "\nEchelle du dessin (" (itoa ech_default) ") : ")))
	; (if echelle 
		; () ;(if (/= echelle ech_default) (DTISetVal "DTgrid-default" "ECHELLE" echelle)) ;mise � jour de l'�chelle par d�faut avec la nouvelle �chelle --> ToDo
		; (setq echelle ech_default) ;pas d'�chelle saisie : on garde celle par d�faut
	; ); end if
	
	; ;Alternative
	; (initget "1 2 3")
	; (setq alt (getkword (strcat "\nAlternative � utiliser [1/2/3] (" alt_default ") : ")))
	; (if alt
		; () ;(if (/= alt alt_default) (DTISetVal "DTgrid-default" "ALT" alt)) ;mise � jour de l'alternative par d�faut avec la nouvelle --> ToDo
		; (setq alt alt_default) ;pas d'alternative saisie : on garde celle par d�faut
	; ); end if
		
	; Lecture du fichier INI pour r�cup�ration des valeurs li�es � cette �chelle et � cette alternative
	(setq Section (strcat "DTgrid-" (rtos echelle 2 0)))
	(setq lstSection (DTIGetSection IniPath Section)) ;tentative de recherche d'une section d�di�e � cette �chelle
	(if lstSection () (setq lstSection (DTIGetSection IniPath "DTgrid-0"))) ;sinon : section par d�faut
		
	;Espacement entre deux croix du dessin
	(setq Val (DTIGetVal LstSection "ESPACEMENT"))
	(setq espace (* (atof Val) echelle))
	(if espace () (prompt "\nErreur fichier ini mal renseign� (espacement)"))

	;Taille de la croix sur le dessin
	(setq Val (DTIGetVal LstSection "TAILLE_CROIX"))
	(setq taillecroix (* (atof Val) echelle))
	(if taillecroix () (prompt "\nErreur fichier ini mal renseign� (taille croix)"))
	
	;Taille du texte
	(setq Val (DTIGetVal LstSection "HAUTEUR_TEXTE"))
	(setq tailletexte (* (atof Val) echelle))
	(if tailletexte () (setq tailletexte textsize_default))
	
	;Taille du texte adjacent
	(setq Val (DTIGetVal LstSection "HAUTEUR_TXTADJ"))
	(setq tailletexteadj (* (atof Val) echelle))
	(if tailletexteadj () (setq tailletexte textsize_default))
	
	;Style de texte
	(setq Val (DTIGetVal LstSection "STYLE_TEXTE"))
	(setq styletexte Val)
	(if styletexte () (setq styletexte textstyle_default))
	(if (not (tblobjname "STYLE" styletexte)) (setq styletexte textstyle_default)) ; on ne l'utilise pas s'il n'existe pas d�j�
	
	;Style de texte adjacent
	(setq Val (DTIGetVal LstSection "STYLE_TXTADJ"))
	(setq styletexteadj Val)
	(if styletexteadj () (setq styletexteadj textstyle_default))
	(if (not (tblobjname "STYLE" styletexteadj)) (setq styletexteadj textstyle_default)) ; on ne l'utilise pas s'il n'existe pas d�j�
	
	;Calque
	(setq Val (DTIGetVal LstSection (strcat "LAYER_ALT" alt)))
	(setq layer Val)
	(if layer () (setq layer layer_default))
	; Cr�ation du calque si inexistant (avec les valeurs courantes du dessin)
	(if (not (tblobjname "LAYER" layer)) (progn
			(setq lok (entmake (list '(0 . "LAYER")
				   '(100 . "AcDbSymbolTableRecord")
				   '(100 . "AcDbLayerTableRecord")
				   (cons 2 layer)
				   '(70 . 0)
				 )
			   ))
			  (if (not lok) (prompt (strcat "\nErreur cr�ation du calque impossible " layer)))
     ))
	 ; On sauvegarde l'�tat actif du calque (g�l� ? visible ?) puis on le d�g�le et l'active
	(setq layerEtat (DtSaveLay layer))
	(DtInitLay layer)	 
    (setvar "CLAYER" layer); active le calque s�lectionn�
	
	;On dessine le cadre
	(Command "_PLINE" pt1 pt2 pt3 pt4 "_C")
	
	;On dessine les limites adjacentes et textes adjacents, si demand�
	(cond 
		((= alt "1") (progn ; cadre principal sur l'alernative 1 --> on ins�re sur l'adj2
			(setq Val (DTIGetVal LstSection (strcat "LAYER_ADJ2")))
			(setq layeradj Val)
			(if layeradj (progn ; calque indiqu� : il est demand� d'ins�rer les limites adjacentes
				; Cr�ation du calque si inexistant (avec les valeurs courantes du dessin)
				(if (not (tblobjname "LAYER" layeradj)) (progn
					(setq lok (entmake (list '(0 . "LAYER")
						   '(100 . "AcDbSymbolTableRecord")
						   '(100 . "AcDbLayerTableRecord")
						   (cons 2 layeradj)
						   '(70 . 0)
						 )
					   ))
					  (if (not lok) (prompt (strcat "\nErreur cr�ation du calque impossible " layeradj)))
				)); fin crea calque		
				 ; On sauvegarde l'�tat actif du calque (g�l� ? visible ?) puis on le d�g�le et l'active
				(setq layerAdjEtat (DtSaveLay layeradj))
				(DtInitLay layeradj)	 
				(setvar "CLAYER" layeradj)
				(Command "_PLINE" pt1 pt2 pt3 pt4 "_C") ; dessin de la limite
				(setq PtTemp (polar pt1 (angle pt1 pt4) (/ (distance pt1 pt4) 2)))
				(setq PtTemp (polar PtTemp (angle pt1 pt2) (* echelle 0.006)))
				(DTDrawTextO pttemp pttemp (angle pt1 pt4) "R�f�rence planche adjacente" Layeradj styletexteadj "0" "0" tailletexteadj); insertion du premier texte de limites adjacentes
				(setq PtTemp (polar pt2 (angle pt2 pt3) (/ (distance pt2 pt3) 2)))
				(setq PtTemp (polar PtTemp (angle pt2 pt1) (* echelle 0.006)))
				(DTDrawTextO pttemp pttemp (angle pt2 pt3) "R�f�rence planche adjacente" Layeradj styletexteadj "0" "0" tailletexteadj); insertion du second texte de limites adjacentes
				(setvar "CLAYER" layer)
			))
		))
		((= alt "2") (progn ; cadre principal sur l'alernative 2 --> on ins�re sur l'adj1
			(setq Val (DTIGetVal LstSection (strcat "LAYER_ADJ1")))
			(setq layeradj Val)
			(if layeradj (progn ; calque indiqu� : il est demand� d'ins�rer les limites adjacentes
				; Cr�ation du calque si inexistant (avec les valeurs courantes du dessin)
				(if (not (tblobjname "LAYER" layeradj)) (progn
					(setq lok (entmake (list '(0 . "LAYER")
						   '(100 . "AcDbSymbolTableRecord")
						   '(100 . "AcDbLayerTableRecord")
						   (cons 2 layeradj)
						   '(70 . 0)
						 )
					   ))
					  (if (not lok) (DTDispError (strcat "Erreur cr�ation du calque impossible " layeradj)))
				)); fin crea calque	
				; On sauvegarde l'�tat actif du calque (g�l� ? visible ?) puis on le d�g�le et l'active
				(setq layerAdjEtat (DtSaveLay layeradj))
				(DtInitLay layeradj)
				(setvar "CLAYER" layeradj)
				(Command "_PLINE" pt1 pt2 pt3 pt4 "_C") ; dessin de la limite
				(setq PtTemp (polar pt1 (angle pt1 pt4) (/ (distance pt1 pt4) 2)))
				(setq PtTemp (polar PtTemp (angle pt1 pt2) (* echelle 0.006)))
				(DTDrawTextO pttemp pttemp (angle pt1 pt4) "R�f�rence planche adjacente" Layeradj styletexteadj "0" "0" tailletexteadj); insertion du premier texte de limites adjacentes
				(setq PtTemp (polar pt2 (angle pt2 pt3) (/ (distance pt2 pt3) 2)))
				(setq PtTemp (polar PtTemp (angle pt2 pt1) (* echelle 0.006)))
				(DTDrawTextO pttemp pttemp (angle pt2 pt3) "R�f�rence planche adjacente" Layeradj styletexteadj "0" "0" tailletexteadj); insertion du second texte de limites adjacentes
				(setvar "CLAYER" layer)
			))
		))); fin limites adjacentes
				
	
	;Insertion du bloc Nord
	(setq Val (DTIGetVal LstSection (strcat "BLOC_NORD")))
	(setq BlockNord Val)
	(if BlockNord (progn ; Bloc Nord saisi : on doit l'ins�rer
		(if (null (findfile BlockNord)) (DTDispError "Erreur bloc nord non trouv�") (setq NordPath (findfile BlockNord)))
		(setq Val (DTIGetVal LstSection "ECH_NORD"))
		(setq EchNord Val)
		(if (null EchNord) (DTDispError "Erreur fichier ini mal renseign� (�chelle nord)"))
		(if (not (zerop (atof EchNord))) (progn
			; Echelle saisie correcte : num�rique et non nulle / Insertion du bloc Nord
			(setq PtTemp (polar pt4 (+ (angle pt4 pt1) (/ pi 4)) (* echelle 0.06)))
			;(command "_-INSERT" NordPath PtTemp (atof EchNord) (atof EchNord) 0))
			; 09/11/2015 remplacement de la commande _INSERT pat entmake :
			(setq bloc (substr BlockNord 1 ( - (strlen BlockNord) 4)))
			(command "_-INSERT" NordPath ) (command) ; mise � jour de la d�finition du bloc
			(entmake (list (cons 0 "INSERT") (cons  2 bloc) (cons 10 PtTemp)
                         (cons 41 (atof EchNord)) ; facteur echelle X
                         (cons 42 (atof EchNord)) ; facteur echelle Y
                         (cons 43 (atof EchNord)) ; facteur echelle Z
                         (cons 50 0) ; angle de rotation = 0
			)))			
			; Echelle non num�rique
			(DTDispError "Erreur fichier ini mal renseign� (�chelle nord)") 
		); end if EchNord
	)); fin block nord
	
	;Insertion du bloc Echelle
	(setq Val (DTIGetVal LstSection "BLOC_ECHELLE"))
	(setq BlockEch Val)
	(if BlockEch (progn ; Bloc �chelle saisi : on doit l'ins�rer
		(if (null (findfile BlockEch)) (DTDispError "Erreur bloc �chelle non trouv�") (setq EchelPath (findfile BlockEch)))
		(setq Val (DTIGetVal LstSection (strcat "ECH_ECHELLE")))
		(setq EchEchel Val)
		(if (null EchEchel) (DTDispError "Erreur fichier ini mal renseign� (�chelle bloc �chelle)"))
		(if (not (zerop (atof EchEchel))) (progn
			; Echelle saisie correcte : num�rique et non nulle / Insertion du bloc
			; attention : la fonction angle ram�ne un angle mesur� depuis l'axe des x sens antihoraire, mais notre angle de base est au sud sens horaire (voir angbase et angdir)
			; l'insertion avec command se fait depuis l'axe x sens courant, soit ici sens horaire
			(setq PtTemp (polar pt1 (+ (angle pt1 pt2) (/ pi 4)) (* echelle 0.03)))
			;(command "_-INSERT" EchelPath PtTemp (atof EchEchel) (atof EchEchel) (- (angle pt1 pt2))))
			; 09/11/2015 remplacement de la commande _INSERT pat entmake :
			(setq bloc (substr BlockEch 1 ( - (strlen BlockEch) 4)))
			(command "_-INSERT" EchelPath ) (command) ; mise � jour de la d�finition du bloc
			(entmake (list (cons 0 "INSERT") (cons  2 bloc) (cons 10 PtTemp)
                         (cons 41 (atof EchEchel)) ; facteur echelle X
                         (cons 42 (atof EchEchel)) ; facteur echelle Y
                         (cons 43 (atof EchEchel)) ; facteur echelle Z
                         (cons 50 (angle pt1 pt2)) ; angle de rotation = axe de rotation du plan
			)))	
			; Echelle non num�rique
			(DTDispError "Erreur fichier ini mal renseign� (�chelle bloc �chelle)") 
		); end if EchEchel
	)); fin block �chelle

	;On cherche les points extr�mes, on les arrondis et on enl�ve 1 espace
	(setq listy (list (cadr pt1) (cadr pt2) (cadr pt3) (cadr pt4)))
	(setq listx (list (car pt1) (car pt2) (car pt3) (car pt4)))
	(setq ly (DTListLow listy))
	(setq startHz (- (DTround ly espace) espace))
	(setq hy (DTListHigh listy))
	(setq endHz (+ (DTround hy espace) espace))
	(setq lx (DTListLow listx))
	(setq startVt (- (DTround lx espace) espace))
	(setq hx (DTListHigh listx))
	(setq endVt (+ (DTround hx espace) espace))

	;On boucle sur chaque ligne pour dessiner les croix de carroyage
	(setq currentHz startHz)
	(while (<= currentHz endHz)

		(DTGSetCroix currentHz pt1 pt2 pt3 pt4 taillecroix tailleTexte espace)
		(setq currentHz (+ currentHz espace))

	) ;while

	;On boucle sur chaque ligne Verticale pour dessiner les lignes de rappel verticales
	(setq currentVt startVt)
	(while (<= currentVt endVt)

		(DTGSetRappelVt currentVt pt1 pt2 pt3 pt4 taillecroix tailleTexte layer styletexte)
		(setq currentVt (+ currentVt espace))

	) ;while

	;; on r�tablit les variables � leur valeur d'origine
	(UtRestVar)
  
	(if (= bolSCU nil)
	  (progn
	    (command "_UCS" "_P")
	    (alert "Le carroyage a �t� dessin� dans le SCU G�n�ral")
	    ) ;progn
	  ) ;If
	  
	;; Idem pour les calques
	(DtRestLay layerEtat)
	(DtRestLay layerAdjEtat)	
	  
	(princ)
	;Clean fonction

)

;******************|MODULES|********************

;;Fonction qui traite les traits de rappel Verticaux
(defun DTGSetRappelVt (coord pt1 pt2 pt3 pt4 taillecroix tailleTexte layer styletexte / ptHz1 ptHz2 ptInter1 ptInter2 ptInter3 ptInter4 listptInter ptVtMin ptVtMax)

	;On cr�e la ligne horizontale au X d�fini
	(setq ptHz1 (list coord -999999999))
	(setq ptHz2 (list coord 999999999999))

	;On cherche toutes les intersections
	(setq ptInter1 (inters ptHz1 ptHz2 pt1 pt2 1))
	(setq ptInter2 (inters ptHz1 ptHz2 pt2 pt3 1))
	(setq ptInter3 (inters ptHz1 ptHz2 pt3 pt4 1))
	(setq ptInter4 (inters ptHz1 ptHz2 pt4 pt1 1))

	;On cr�e une liste avec tous les points d'intersection
	(setq listptInter (list ptInter1 ptInter2 ptInter3 ptInter4))
	(setq listptInter (vl-remove nil listptInter))

	(if (= (length listptInter) 2)
	;On en sort ensuite le point d'intersection de d�part et d'arriv�e
	(cond
		((< (cadr(nth 0 listptInter)) (cadr(nth 1 listptInter)))
		
				(setq ptVtMin (nth 0 listptInter))
				(setq ptVtMax (nth 1 listptInter))
				(DTGDrawRappelVt ptVtMin ptVtMax pt1 pt2 pt3 pt4 taillecroix tailleTexte layer styletexte)
		);Premier cas
		((> (cadr(nth 0 listptInter)) (cadr(nth 1 listptInter)))
				(setq ptVtMin (nth 1 listptInter))
				(setq ptVtMax (nth 0 listptInter))
				(DTGDrawRappelVt ptVtMin ptVtMax pt1 pt2 pt3 pt4 taillecroix tailleTexte layer styletexte)
		);Deuxi�me cas
	) ;cond
	) ;If

);Function


;;Fonction qui dessine les traits de rappel verticaux
(defun DTGDrawRappelVt (ptVtMin ptVtMax pt1 pt2 pt3 pt4 taillecroix tailleTexte layer styletexte / ang ptTemp i)

	;Traits du bas
	(setq ang (/ pi 2))
	(setq ptTemp (polar ptVtMin ang taillecroix))
	(setq i (DTInRectangle ptTemp pt1 pt2 pt3 pt4))
	(if (= i 1) (DTDrawLineO ptVtMin ptTemp Layer))
	(if (< (car pt1) (car pt2)) ;On g�re le cas du cadre � l'envers
		(DTDrawTextO (list (car ptTemp) (+ (cadr ptTemp) tailleTexte)) PtTemp (/ pi 2) (rtos (car ptTemp) 2 0) Layer Styletexte "2" "0" tailletexte)
		(DTDrawTextO (list (car ptTemp) (+ (cadr ptTemp) tailleTexte)) PtTemp (/ (* 3 pi) 2) (rtos (car ptTemp) 2 0) Layer Styletexte "2" "2" tailletexte)
		)

	;Traits du haut
	(setq ang (/ (* 3 pi) 2))
	(setq ptTemp (polar ptVtMax ang taillecroix))
	(setq i (DTInRectangle ptTemp pt1 pt2 pt3 pt4))
	(if (= i 1) (DTDrawLineO ptVtMax ptTemp Layer))
	(if (< (car pt1) (car pt2)) ;On g�re le cas du cadre � l'envers
		(DTDrawTextO (list (car ptTemp) (- (cadr ptTemp) tailleTexte)) PtTemp (/ pi 2) (rtos (car ptTemp) 2 0) Layer Styletexte "2" "2" tailletexte)
		(DTDrawTextO (list (car ptTemp) (- (cadr ptTemp) tailleTexte)) PtTemp (/ (* 3 pi) 2) (rtos (car ptTemp) 2 0) Layer Styletexte "2" "0" tailletexte)
		)  

);Function


;;Fonction qui traite les croix de carroyage
(defun DTGSetCroix (coord pt1 pt2 pt3 pt4 taillecroix tailleTexte espace / ptHz1 ptHz2 ptinter1 ptinter2 ptinter3 ptinter4 listptInter ptHzMin ptHzMax)

	;On cr�e la ligne horizontale au Y d�fini
	(setq ptHz1 (list -999999999 coord))
	(setq ptHz2 (list 999999999999 coord))

	;On cherche toutes les intersections
	(setq ptInter1 (inters ptHz1 ptHz2 pt1 pt2 1))
	(setq ptInter2 (inters ptHz1 ptHz2 pt2 pt3 1))
	(setq ptInter3 (inters ptHz1 ptHz2 pt3 pt4 1))
	(setq ptInter4 (inters ptHz1 ptHz2 pt4 pt1 1))

	;On cr�e une liste avec tous les points d'intersection
	(setq listptInter (list ptInter1 ptInter2 ptInter3 ptInter4))
	(setq listptInter (vl-remove nil listptInter))

	(if (= (length listptInter) 2)
	;On en sort ensuite le point d'intersection de d�part et d'arriv�e
	(cond
		((< (car(nth 0 listptInter)) (car(nth 1 listptInter)))
		
				(setq ptHzMin (nth 0 listptInter))
				(setq ptHzMax (nth 1 listptInter))
				(DTGDrawCroix ptHzMin ptHzMax pt1 pt2 pt3 pt4 taillecroix tailleTexte espace)
		);Premier cas
		((> (car(nth 0 listptInter)) (car(nth 1 listptInter)))
				(setq ptHzMin (nth 1 listptInter))
				(setq ptHzMax (nth 0 listptInter))
				(DTGDrawCroix ptHzMin ptHzMax pt1 pt2 pt3 pt4 taillecroix tailleTexte espace)
		);Deuxi�me cas
	) ;cond
	) ;If

) ;Function


;;Fonction qui dessine toutes les croix d'un point � un autre
(defun DTGDrawCroix (ptHzMin ptHzMax pt1 pt2 pt3 pt4 taillecroix tailleTexte espace / ang ptTemp HzMinIn ptBaseTemp i)

	;Dessin du trait horizontal sur le bord gauche
	(setq ang 0)
	(setq ptTemp (polar ptHzMin ang taillecroix))
	(setq i (DTInRectangle ptTemp pt1 pt2 pt3 pt4))
	(if (= i 1) (DTDrawLineO ptHzMin ptTemp layer))
	(if (< (car pt1) (car pt2)) ;On g�re le cas du cadre � l'envers
		(DTDrawTextO (list (+ (car ptTemp) tailleTexte) (cadr ptTemp)) PtTemp 0 (rtos (cadr ptTemp) 2 0) Layer Styletexte "2" "0" tailletexte)
		(DTDrawTextO (list (+ (car ptTemp) tailleTexte) (cadr ptTemp)) PtTemp pi (rtos (cadr ptTemp) 2 0) Layer Styletexte "2" "2" tailletexte)
	) ;if

	;On dessine ensuite les croix � l'int�rieur
	(setq ang 0)
	(setq HzMinIn (- (DTround (car ptHzMin) espace) espace))
	(setq ptBaseTemp (list HzMinIn (cadr ptHzMin)))

	(While (< (car ptBaseTemp) (car ptHzMax))
	(setq i (DTInRectangle ptBaseTemp pt1 pt2 pt3 pt4))
		(if (= i 1)
			(Repeat 4
			(setq ptTemp (polar ptBaseTemp ang taillecroix))
			(setq i (DTInRectangle ptTemp pt1 pt2 pt3 pt4))
			(if (= i 1) (DTDrawLineO ptBaseTemp ptTemp Layer))
				(setq ang (+ (/ pi 2) ang))
			) ;Repeat
		);If
	(setq ptBaseTemp (list (+ (car ptBaseTemp) espace) (cadr ptBaseTemp)))
	);Repeat


	;Dessin sur le bord droit
	(setq ang pi)
	(setq ptTemp (polar ptHzMax ang taillecroix))
	(setq i (DTInRectangle ptTemp pt1 pt2 pt3 pt4))
	(if (= i 1) (DTDrawLineO ptHzMax ptTemp Layer))
	(if (< (car pt1) (car pt2)) ;On g�re le cas du cadre � l'envers
		(DTDrawTextO (list (- (car ptTemp) tailleTexte) (cadr ptTemp)) PtTemp 0 (rtos (cadr ptTemp) 2 0) Layer Styletexte "2" "2" tailletexte)
		(DTDrawTextO (list (- (car ptTemp) tailleTexte) (cadr ptTemp)) PtTemp pi (rtos (cadr ptTemp) 2 0) Layer Styletexte "2" "0" tailletexte)
	) ;if

) ;Function

(prompt  "\nDocTekus chargement DTgrid v2.0.5 - licence GNU GPL v3")
(prompt "\n CADaGEO - DocTekus outil de carroyage charg�, taper DTGrid pour lancer la commande - licence GNU GPL v3")
(princ)
;Clean chargement