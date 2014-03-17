;;Copyright 2014, CADaGEO - Stephane Byache et Cristel LEGRAND
;;
;;This file is part of DocTekus.
;;
;;    DocTekus is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    DocTekus is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with Foobar.  If not, see <http://www.gnu.org/licenses/>
;;
;;DESCRIPTION ********************************************************
;;********************************************************************
;;Commandes et fonctions de nettoyage/correction de fichiers autocad, 
;;en préparation à la conversion vers des formats SIG 
;;
;;INSTALLATION ET FONCTIONNEMENT *************************************
;*********************************************************************
;;
;; Quand UN SEUL AutoCAD est lancé :
;; - s'assurer que ce fichier et le fichier DTcorr2gis.ini sont situés dans le même répertoire. Modifier le fichier ini si nécessaire.
;; - Le cas échéant, ajouter les chemins d'accès aux fichier dans les chemins de recherche de fichiers de support (_options / fichiers)
;;		Doivent être situés dans l'un de ces répertoires : le fichier présent, le fichier ini, les fichiers des dépendances (DTini.lsp, ld-ut.lsp et DTut.lsp), et le fichier dpfsi utilisé
;; - charger le lisp DTgrid.lsp 
;;
;; Fonctionnement :
;; Taper "DTcorr2gis" dans la ligne de commande
;;
;;
;;CHANGELOG **********************************************************
;;********************************************************************
;;
;;Version 0.1 du 20/01/2014 :
;;	- version initiale
;;
;;DEPENDANCES *******************************************************
;;*******************************************************************
;; Nécessite les fonctions DTini + ld-ut et DTut, chargées via DTini
;; Utilise les variables globales $DTLogUser, $DTLogUserW et $DTLogUserE
;;
;;TODO, pochaine version *********************************************
;;********************************************************************
;; Revoir le traitement des erreurs ?
;; Convertir les MLINES en PLINES
;; Transformer les Splines
;; Divers, voir commentaires code
;; Envisager la création d'un vrai fichier Log
;*********************************************************************
;*********************************************************************

;chargement d'utilitaires utilisés dans ce code
 (if (findfile "DTini.lsp") (load "DTini.lsp") (prompt "\nErreur DTini.lsp non trouvé"))
 
 ; DTcorr2gis : nettoyage général
 ;********************************************************************
 (defun C:DTcorr2gis()
 	; pour que le programme puisse être annulé comme une seule commande
	(setvar "CMDECHO" 0)
	(command "_UNDO" "_BE")

	;RAZ des messages utilsateurs
	(setq $DTLogUser "")
	(setq $DTLogUserW "")
	(setq $DTLogUserE "")
	
	(F_DTcorr2gis)
	
	; pour que le programme puisse être annuél comme une seule commande
	(command "_UNDO" "_E")
	(setvar "CMDECHO" 1)
	
	;Affichage des messages utilsateurs
	(Textscr)
	(princ $DTLogUser)
	(princ $DTLogUserW)
	(princ $DTLogUserE)
	(princ)
)

(defun F_DTcorr2gis(/ NetCotDis NetHachDis LsTypObjSup ssHachDis eln i Typ ssTyp NetMap NetPath RetrouvPoly Conv2DPoly ConvMLines ConvSplines NetApps NetPurg NetAudit INIPath LstSection)
	
	;;TYPAGE des variables locales
		(setq NetCotDis nil) ; Dissocier les côtes associatives ?
		(setq NetHachDis nil) ; Dissocier les hacures ?
		(setq LsTypObjSup nil) ; Liste des types d'objets à supprimer de l'espace objet
		; attention, prévoir de tester ces types d'objets pour cévifier qu'ils correspondent bien à un type DXF code 0
		(setq NetMap nil) ; Nettoyage map ?
		(setq NetPath "") ; chemin d'accès au fichier de nettoyage map (chemin absolu ou nom du fichier seul si dans un répertoire définit dans les chemins de recherche de fichiers de support)
		(setq RetrouvPoly nil) ; "Retrouver" les polylignes (Supprimer les sommets supplémentaires insérés par une courbe de lissage ou une spline et transformer tous les segments de la polyligne en droites) ?
		(setq Conv2DPoly nil) ; convertir les polylignes des anciens formats AutoCAD (2DPOLYLIGNE) en polylignes actuelles (LWPOLYLINE) ?
		(setq ConvMLines nil) ; Transformer les MLINES en POLYLINES (attention : non traité à ce jour / détection des MLINES uniquement)
		(setq ConvSplines nil) ; Transformer les SPLINES en POLYLINES (attention : non traité à ce jour / détection des MLINES uniquement)
		(setq NetApps nil) ; Purger les données des applications - Attention, non conseillé dans AutoCADMap (risque de suppression de données map ?)
		(setq NetPurg nil) ; Purge complète, hors données d'applications
		(setq NetAudit nil) ; Audit du fichier (peu parfois être très long et/ou provoquer des plantages)
		

		(setq INIPath "")
		(setq LstSection nil)
		(setq ssHachDis nil) ; jeu de sélection des hachures
		(setq eln nil) ; ssname d'un élément
		(setq i 0) ; indice boucles
		(setq Typ "") ; Type d'objet (string)
		(setq ssTyp nil) ; jeu de sélection pour un type d'objet
		
	;;Chemin d'accès au fichier ini
	(if (findfile "DTcorr2gis.lsp") (setq INIPath (findfile "DTcorr2gis.lsp")) (setq $DTLogUserE (strcat $DTLogUserE "\nERREUR DTcorr2gis : Chemin d'accès au fichier lisp non trouvé")))
  	(setq INIPath (UtRep INIPath))
	(setq INIPath (strcat INIPath "DTcorr2gis.ini"))
	
	; Ouverture et lecture du fichier ini
	(if (setq LstSection (DTIGetSection INIPath "DTcorr2gis-default")) (progn
		
		(if (= (strcase (DTIGetVal LstSection "NETCOTDIS")) "OUI") (setq NetCotDis T) (setq NetCotDis nil))
		(if (= (strcase (DTIGetVal LstSection "NETHACHDIS")) "OUI") (setq NetHachDis T) (setq NetHachDis nil))
		(setq LsTypObjSup (UtCaseStrLst (UtStr2lst (DTIGetVal LstSection "LSTYPOBJSUP") ";"))) ; validité de la saisie contrôlée plus bas
		(if (= (strcase (DTIGetVal LstSection "NETMAP")) "OUI") (setq NetMap T) (setq NetMap nil))
		(setq NetPath (DTIGetVal LstSection "NETPATH")) ; validité de la saisie contrôlée plus bas
		(if (= (strcase (DTIGetVal LstSection "RETROUVPOLY")) "OUI") (setq RetrouvPoly T) (setq RetrouvPoly nil))
		(if (= (strcase (DTIGetVal LstSection "CONV2DPOLY")) "OUI") (setq Conv2DPoly T) (setq Conv2DPoly nil))
		(if (= (strcase (DTIGetVal LstSection "CONVMILNES")) "OUI") (setq ConvMLines T) (setq ConvMLines nil))
		(if (= (strcase (DTIGetVal LstSection "CONVSPLINES")) "OUI") (setq ConvSplines T) (setq ConvSplines nil))
		(if (= (strcase (DTIGetVal LstSection "NETAPPS")) "OUI") (setq NetApps T) (setq NetApps nil))
		(if (= (strcase (DTIGetVal LstSection "NETPURG")) "OUI") (setq NetPurg T) (setq NetPurg nil))
		(if (= (strcase (DTIGetVal LstSection "NETAUDIT")) "OUI") (setq NetAudit T) (setq NetAudit nil))
		
	) 
	(setq $DTLogUserE (strcat $DTLogUserE "\nERREUR DTcorr2gis : Section par défaut du fichier INI non trouvée"))
	); /if
		
	;Dissocier les côtes associatives = indispensable avant de pouvoir travailler proprement
	(if NetCotDis (progn
		(command "_dimdisassociate" "_all" "")
		(setq $DTLogUser (strcat $DTLogUser "\nDTcorr2gis : dissociation des côtes"))
	));/if NetCotDis
	
	;Dissocier les hachures = indispensable avant de pouvoir travailler proprement
	(if NetHachDis (progn
		; On créé un jeu de sélection des hachures associatives de l'espace objet du dessin (filtrage par type d'objet hachures AND  associatives AND espace objet)
		(setq  ssHachDis(ssget  "X" (list (cons  0 "HATCH") (cons  71  1) (cons 67 0))))
		; On dissocie les hachures du jeu de sélection si il en existe au moins une
		(if ssHachDis (progn
			(setq i 0)
			(repeat (sslength ssHachDis)
				(setq eln (ssname ssHachDis i))
				(command "_-HATCHEDIT" eln "_DI")
				(setq i (+ i 1))
			); /repeat
			(setq $DTLogUser (strcat $DTLogUser "\nDTcorr2gis : " (itoa (sslength ssHachDis)) " hachures dissociées dans l'espace objet"))
		)
		(setq $DTLogUser (strcat $DTLogUser "\nDTcorr2gis : aucune hachure dissociée dans l'espace objet"))
		);/if
		
	));/if NetHachDis
	
	;Supression des types d'objets indésirables
	(if LsTypObjSup  (progn
		(foreach Typ LsTypObjSup
			(if (DTTypEntitie Typ) (progn ; Le type d'entité défini est valable
				(setq ssTyp nil)
				(setq  ssTyp(ssget  "X" (list (cons  0 Typ) (cons 67 0)))) ; sélection de ces types d'objets dans l'espace objet
				(if ssTyp (progn
					(setq i 0)
					(repeat (sslength ssTyp)
						(setq eln (ssname ssTyp i))
						(entdel eln)
						(setq i (+ i 1))
					); /repeat
					(setq $DTLogUser (strcat $DTLogUser "\nDTcorr2gis : " (itoa (sslength ssTyp)) " " Typ " supprimé(e)s dans l'espace objet"))
				)
				(setq $DTLogUser (strcat $DTLogUser "\nDTcorr2gis : aucun(e) " Typ " supprimé(e) dans l'espace objet"))
				);/if ssTyp
			)
			(setq $DTLogUserW (strcat $DTLogUserW "\nDTcorr2gis : Type d'entité à supprimer " Typ " invalide et non traité"))
			);/if DTTypEntitie
		); / foreach
	));/if LsTypObjSup
	
	;Nettoyage map (minimaliste : pas de traitement de topologie)
	; à améliorer : chemin relatif par rapport au fichier ini, à indiquer dans le fichier ini ?
	(if NetMap (progn
		(if (findfile NetPath) (progn
			(command "_-mapclean" NetPath)
			(setq $DTLogUser (strcat $DTLogUser "\nDTcorr2gis : Nettoyage map fichier " (findfile NetPath)))
		)
		(setq $DTLogUserE (strcat $DTLogUserE "\nERREUR DTcorr2gis : Fichier de nettoyage Map non trouvé " NetPath)) ; Améliorer : rendre plus visible
		)
	));/if NetMap
	
	; "Retrouver" les polylignes : supprimer les sommets supplémentaires insérés par une courbe de lissage ou une spline et transformer tous les segments de la polyligne en droites
	(if RetrouvPoly (progn
		(setq  ssTyp(ssget  "X" (list (cons  0 "*LINE") (cons 67 0)))) ; sélection de tous les types de LINE dans l'espace objet
		(if ssTyp (command "_pedit" "_m" "_all" "" "_D" ""))
		(setq $DTLogUser (strcat $DTLogUser "\nDTcorr2gis : Polylignes 'retrouvées', simplifiées"))
	));/if RetrouvPoly
	
	; Conversion des 2DPolylines en LWPolylines
	(if Conv2DPoly (progn
		(setq  ssTyp(ssget  "X" (list (cons  0 "*LINE") (cons 67 0)))) ; sélection de tous les types de LINE dans l'espace objet
		(if ssTyp (command "_convert" "_p" "_all"))
		(setq $DTLogUser (strcat $DTLogUser "\nDTcorr2gis : Polylignes 2D converties en LWPOLYLINE"))
	));/if Conv2DPoly
	
	; Détection des MLines --> Todo : convertir les MLines
	(if ConvMLines  (progn
		(setq  ssTyp(ssget  "X" (list (cons  0 "MLINE") (cons 67 0)))) ; sélection des MLINE dans l'espace objet
		(if ssTyp (progn
			(setq $DTLogUserW (strcat $DTLogUserW "\nATTENTION DTcorr2gis : " (itoa (sslength ssTyp)) " " MLINE présente(s) dans l'espace objet, à convertir en POLYLINES par l'utilisateur"))
		)
		(setq $DTLogUser (strcat $DTLogUser "\nDTcorr2gis : aucune MLINE présente dans l'espace objet"))
		);/if
	));/if ConvMLines
	
	; Détection des Splines --> Todo : convertir les MLines
	(if ConvSplines  (progn
		(setq  ssTyp(ssget  "X" (list (cons  0 "SPLINE") (cons 67 0)))) ; sélection des SPLINE dans l'espace objet
		(if ssTyp (progn
			(setq $DTLogUserW (strcat $DTLogUserW "\nATTENTION DTcorr2gis : " (itoa (sslength ssTyp)) " " SPLINE présente(s) dans l'espace objet, à convertir en POLYLINES par l'utilisateur"))
		)
		(setq $DTLogUser (strcat $DTLogUser "\nDTcorr2gis : aucune SPLINE présente dans l'espace objet"))
		);/if
	));/if ConvSplines
	
	;Purger les données des applications
	(if NetApps (progn
		(command "_-purge" "appsenreg" "*" "n")
		(setq $DTLogUser (strcat $DTLogUser "\nDTcorr2gis : Purge des données d'application"))
	))
	
	; Purge complete
	(if NetPurg (progn
		(command "_-purge" "_all" "*" "n")
		(command "_-purge" "_all" "*" "n")
		(command "_-purge" "_all" "*" "n")
		(command "_-purge" "_all" "*" "n")
		(command "_-purge" "_all" "*" "n")
		(command "_-purge" "_all" "*" "n")
		(setq $DTLogUser (strcat $DTLogUser "\nDTcorr2gis : Purge complète du dessin"))
	))
		
	; Audit
	(if NetAudit (progn
		(command "_audit" "_y")
		(setq $DTLogUser (strcat $DTLogUser "\nDTcorr2gis : Audit du dessin"))
	))
	
		
) ; /F_DTcorr2gisste

(prompt "\n DocTekus chargement DTcorr2gis v0.1 - licence GNU GPL v3")
(princ)
;Clean chargement
