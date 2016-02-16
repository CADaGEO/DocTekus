;;Copyright 2014, CADaGEO - Guillaume Berson, Stephane Byache, Cristel Legrand
;;Version 2.0.0 du 03/12/2014
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
;;
;; DTcloop boucle des commandes AutoCAD unitaires (effectives sur une entit�) sur plusieurs entit�s. 
;; Pour l'instant DTcloop ne traite que la commande _MEASURE (MESURER) et BUFFER et ne tient pas compte du fichier ini pour les valeurs par d�faut. 
;;
;;INSTALLATION ET FONCTIONNEMENT *************************************
;*********************************************************************
;;
;; _appload DTcloop.lsp
;;  dtcloop
;;
;; Pour utiliser la commande Buffer, il faut l'avoir charg�e auparavant
;;
;;CHANGELOG **********************************************************
;;********************************************************************
;;
;; Version 2.0.0 du 03/12/2014
;; - Ajout de la commande Buffer par CLE
;; - Filtrage des types d'entit�s
;;
;; Version 1.0.0 du 18/04/2013
;; - Renommage en DTcloop par SBY pour mise � disposition dans DocTekus
;;
;; Version 0.1.0 du 18/04/2013
;; - Cr�ation de la premi�re version par GBE. 
;;
;;
;;DEPENDANCES *******************************************************
;;*******************************************************************
;; N�cessite les fonctions de ld-ut et dtut
;; N�cessite le chargement de Buffer
;;
;;TODO, pochaine version *********************************************
;;********************************************************************
;;
;; - Prise en compte de plusieurs commandes, comme par exemple DIVISER. 
;; - Prise en compte du DTcloop.ini pour les valeurs par d�faut. 
;**********************************************************************
;**********************************************************************


;chargement des d�pendances
 (if (findfile "ld-ut.LSP") (load "ld-ut.LSP") (prompt "\nErreur ld-ut.LSP non trouv�"))
 (if (findfile "DTUt.lsp") (load "DTUt.lsp") (prompt "\nErreur DTUt.lsp non trouv�"))
 (if (findfile "Buffer.lsp") (load "Buffer.lsp") (prompt "\nErreur Buffer.lsp non trouv�"))


(defun c:DTcloop(/ 	)

	; pour que le programme puisse �tre annul� comme une seule commande
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
	
	(F_DTcloop)
	
	;Restauration des variables AutoCAD
	; (UtRestVar)
	
	; pour que le programme puisse �tre annu�l comme une seule commande
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

(defun F_DTcloop( / commande intDist strBloc strAlign sset)

	;;TYPAGE des variables locales
	(setq
		commande ""
		intDist 0.0
		strBloc ""
		strAlign ""
		sset nil
	)

	;On r�cup�re le nom de la commande � faire boucler
	(initget "Measure Buffer")
	(setq commande (getkword "\nNom de la commande � r�p�ter (Measure Buffer) :"))

	; On r�cup�re la saisie utilisateur selon la commande
	(cond 
		((= "Measure" commande) (progn
			;On r�cup�re la distance choisie par l'utilisateur
			(setq intDist (getreal "\nDistance : "))
			(setq strBloc (getstring "\nNom du Bloc : "))
			(setq strAlign (getstring "\nAlign� (O/N) : "))
		))
		((= "Buffer" commande) (progn
			(setq intDist (getreal "\nDistance : "))
		))
	); fin cond		
		
	;On r�cup�re la s�lection de l'utilisateur
	(setq sset (ssget))

	; et on filtre pour ne conserver que ce qui convient � notre commande
	(cond 
		((or (= "Measure" commande) (= "Buffer" commande)) (progn ;dans ce cas on ne conserve que les polylignes de l'espace courant
			(setq sset
					(ssget "P" ; entit�s de la pr�c�dente s�lection 
					  (list
						(cons 0 "*LINE") ; de type polyligne, ligne, etc.
						(cons 67 (if (eq (getvar "CVPORT") 2) 0 1)) ; de l'esapce courant
						(cons 410 (if (eq (getvar "CVPORT") 2) "Model" (getvar "CTAB"))) ; de l'esapce courant (tient compte si espace papier de la pr�sentation courante uniquement)
						;(cons -4 "<NOT")
						; (cons -4 "&") (cons 70 113) ; concerne le flag des polylignes qui peuvent �tre ouvertes ou ferm�es, concerne �galement plinegen => sens ici ???
						;(cons -4 "NOT>")
					  )
					)
			)
		))
	); fin cond

	;On d�fini la variable qui boucle
	(setq i 0)

	;On boucle sur chaque �l�ment
	(repeat (sslength sset)
	(setq item (ssname sset i))
		(cond 
			((= "Measure" commande)	(command "_MEASURE" item "B" strBloc strAlign intDist))
			((= "Buffer" commande) (F_buffer item intDist))
		)	
	(setq i (1+ i))
	);repeat
		
	(princ)

) ;defun

(prompt  "\nDocTekus chargement DTcloop v2.0.0 - licence GNU GPL v3")
(prompt "\n CADaGEO - DocTekus outil de bouclage de commandes unitaires, taper DTcloop pour lancer la commande - licence GNU GPL v3")
(princ)
;Clean chargement