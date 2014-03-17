;;Copyright 2013 - CADaGEO Cristel LEGRAND
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
;;Fonctions AutoCAD de lecture/écriture de fichiers ini
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
;;LD-Ut et DT-Ut
;;
;;
;;TODO, pochaine version *********************************************
;;********************************************************************
;;- fonctions de réécriture du fichier ini pour mise à jour :
;;		création d'un fichier temporaire
;;		lecture de l'ancien fichier, ré-écriture de ce qui n'est pas modifié
;;		pour les clés modifiées, écriture depuis liste d'entrée
;;		suppression de l'ancien fichier et enregistrement du fichier temporaire à la place
;;		pour aide, voir http://docs.autodesk.com/ACD/2011/FRA/filesALG/WS73099cc142f4875516d84be10ebc87a53f-7b52.htm#WS4b0506698c46277ad70d7aff1e063494-7ff1
;**********************************************************************
;**********************************************************************

;chargement des dépendances
 (if (findfile "ld-ut.LSP") (load "ld-ut.LSP") (prompt "\nErreur ld-ut.LSP non trouvé"))
 (if (findfile "DTUt.lsp") (load "DTUt.lsp") (prompt "\nErreur DTUt.lsp non trouvé"))
  
;Récupération de la liste des clés et valeurs liées à un code section (ouverture du fichier en lecture)
(defun DTIGetSection(INIPath Section / INIFile lst)
	
	(if (findfile INIPath) () (prompt "\nErreur fichier ini non trouvé"))
	(setq INIFile (open INIPath "r"));; Ouverture en lecture
	(setq lst (DTIReadSection INIFile Section))
	(close INIFile);; Fermeture du fichier INI
	(DTReturn lst)
	
); fin fonction

; Lecture de fichier pour récupération de la liste correspondant à une section (sans ouverture de fichier)
(defun DTIReadSection(INIFile Section / Line lst s c el)
	
	(setq Line (read-line INIFile))
	(setq lst ())
	(setq s nil)
	(while Line
		(setq c (substr Line 1 1))
		(if (= c "[") ; ligne de section
				(progn
				(if (= (DTISectionName Line) Section) (setq s T) (setq s nil) ); lorsque c'est la bonne section, il faut garder les lignes suivantes / sinon non
			); fin traitement ligne de section
			( if (/= c ";") ; ligne clé valeur
				(if s (progn; ligne à conserver : c'est la bonne section
					(setq el (DTISplitLine Line))
					(if el (setq lst (cons el lst)))
		)))); fin traitement ligne
		(setq Line (read-line INIFile))
	); end while
	
	(if lst (setq lst (reverse lst)))
	(if (equal lst '(""))(setq lst nil))
	(DTReturn lst)
	
); fin fonction


;Mise à jour de la liste des clés et valeurs liées à un code section
; (defun DTIRewriteSection(INIPath Section LstSection / INIFile OldLst)
	
	; (if (findfile INIPath) () (prompt "\nErreur fichier ini non trouvé"))
	; (setq INIFile (open INIPath "w"))
	; (setq OldLst (DTIReadSection INIFile Section))
	
	
	
	; (DTReturn lst)
	
; ); fin fonction

;; Split d'une ligne pour création d'une liste (cle valeur)
(defun DTISplitLine (Line / i len c ch lst)
	
	(setq	i   1)
	(setq len (strlen Line))
   	(setq ch "")
	
	(while (<= i len)
		(setq c (substr Line i 1))
		(setq i (1+ i))
		(if (= c "=") 
			(progn ; on a trouvé le séparateur
			(UtSupLeftSpaces ch)
			(UtSupRightSpaces ch)
			(if (not (= ch ""))	(setq lst (cons ch lst)))
			(setq ch "")) 
			; ce n'est pas le séparateur
			(setq ch (strcat ch c))
		); end if c=
	); end while
	
	(UtSupLeftSpaces ch)
	(UtSupRightSpaces ch)
	(if (not (= ch ""))	(setq lst (cons ch lst)))
	(if lst (setq lst (reverse lst)))
	
	(if (equal lst '(""))(setq lst nil))
	(DTReturn lst)
  )
  
;; Split d'une ligne [section] pour récupération du nom de la section
(defun DTISectionName (Line / i c ch)
	
	(setq i 2)
  	(setq ch "")
	(setq c (substr Line i 1))
 
	(while (/= c "]")
		(setq ch (strcat ch c))
		(setq i (1+ i))
		(setq c (substr Line i 1))
	); end while
	
	(if (equal ch '(""))(setq ch nil))
	(DTReturn ch)
  )

;; Récupération de la valeur d'une clé depuis la liste de la section (caractère)
(defun DTIGetVal (lstSection Cle / l1)
	(setq l1 ())
	(setq l1 (assoc (strcase cle T) lstsection))
	(if (not l1) (setq l1 (assoc (strcase cle nil) lstsection)))
	(if l1 (setq l1 (cadr l1)))
	(if (equal l1 '(""))(setq l1 nil))
	(DTReturn l1)
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (prompt "\n DocTekus chargement DTini v0.0.1 - licence GNU GPL v3")
  (princ)
  
