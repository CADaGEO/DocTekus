;;Copyright 2014 - CADaGEO Cristel LEGRAND
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
;;Fonctions AutoCAD de lecture/écriture de fichiers csv
;;
;;INSTALLATION ET FONCTIONNEMENT *************************************
;*********************************************************************
;; Non autonome
;;
;;CHANGELOG **********************************************************
;;********************************************************************
;;
;;Version 0.0.1 du 21/07/2014
;;- version initiale
;;
;;
;;DEPENDANCES *******************************************************
;;*******************************************************************
;;LD-Ut et DT-Ut (à charger dans le programme maître)
;;
;;
;;TODO, pochaine version *********************************************
;;********************************************************************
;;- fonctions d'écriture
;**********************************************************************
;**********************************************************************

  
;Récupération d'un CSV sous forme d'une liste de listes (ouverture du fichier en lecture)
(defun DTCGetCSV(CSVPath / CSVFile line lst)
	
	;;TYPAGE des variables locales
	(setq
		CSVFile nil
		line ""
		lcsv '()
	)
	
	(if (findfile CSVPath) () (UtDispError (strcat "Fichier csv non trouvé : " CSVPath)))
	(setq CSVFile (open CSVPath "r"));; Ouverture en lecture
	(setq line (read-line CSVFile)) ;; Lecture première ligne
	; (setq lscsv (list line) ;; initialisation de la liste
	
	(while line
		(setq ls (UtStr2lst line ";"))
		(setq lcsv (cons ls lcsv))
		(setq line (read-line CSVFile))
	); end while
	
	(if lcsv (setq lcsv (reverse lcsv)))
	(if (equal lcsv '(""))(setq lcsv nil))
	(DTReturn lcsv)

); fin fonction

;Récupération de la liste des valeurs d'un champs (y compris les doublons)
(defun DTCGetField (lcsv FieldName / pos newls line)
	
	;;TYPAGE des variables locales
	(setq 
		pos 0
		newls '()
		line '()
	)
	
	(setq pos (DTFieldPos (car lcsv) FieldName))
	(if (< pos (length (car lcsv))) (progn ; ce nom de champ existe
		(setq lcsv (cdr lcsv))
		(foreach line lcsv
			(setq newls (cons (nth pos line) newls))
		)
		(setq newls (reverse newls))
	)
	(setq newls nil)); ce champ n'existe pas		
	
	(DTReturn newls)

); fin fonction
  
;Récupération d'un pool de lignes filtrées selon une liste de critères (OR)
; Le caractère * est accepté comme caractère générique à condition d'être seul
 (defun DTCGetFilteredLinesOR (lcsv criteres / titre line garder critere pos newls)
 	
	;;TYPAGE des variables locales
	(setq 
		titre '()
		line '()
		garder nil
		critere '()
		pos 0
		newls '()
	)
	
	; Extraction de la ligne de titre
	(setq titre (car lcsv))
	(setq lcsv (cdr lcsv))
	
	; Boucle sur chaque ligne pour tests par rapport aux critères
	(foreach line lcsv
		(setq garder T)
		; boucle pour chaque critère sur cette ligne
		(foreach critere criteres
			(setq pos (DTFieldPos titre (car critere)))
			(if (/= (nth pos line) (cdr critere)) (progn
				(setq garder nil)
				(if (= (nth pos line) "*") (setq garder T))
				(if (= (cdr critere) "*") (setq garder T))
			))
		)
		(if garder (setq newls (cons line newls)))
	)
	; remise en ordre de la liste
	(setq newls (reverse newls))
	; rajout du titre
	(setq newls (cons titre newls))
	(DTReturn newls)
	
  ); fin fonction  
  
  ;Récupération d'un pool de lignes filtrées selon une liste de critères (AND)
; Le caractère * est accepté comme caractère générique à condition d'être seul
 (defun DTCGetFilteredLinesAND (lcsv criteres / titre line garder critere pos newls i nc)
 	
	;;TYPAGE des variables locales
	(setq 
		titre '()
		line '()
		garder nil
		critere '()
		pos 0
		newls '()
		i 0
		nc 0
	)
	
	; Extraction de la ligne de titre
	(setq titre (car lcsv))
	(setq lcsv (cdr lcsv))
	
	; Boucle sur chaque ligne pour tests par rapport aux critères
	(foreach line lcsv
		(setq garder T)
		(setq i 0)
		(setq nc (length criteres))
		; boucle pour chaque critère sur cette ligne, tant que l'on respecte bien tous les critères
		(while (and garder (< i nc))
			(setq critere (nth i criteres))
			(setq pos (DTFieldPos titre (car critere)))
			(if (/= (nth pos line) (cdr critere)) (progn
				(setq garder nil)
				(if (= (nth pos line) "*") (setq garder T))
				(if (= (cdr critere) "*") (setq garder T))
			))
			(setq i (+ i 1))
		)
		(if garder (setq newls (cons line newls)))
	)
	; remise en ordre de la liste
	(setq newls (reverse newls))
	; rajout du titre
	(setq newls (cons titre newls))
	(DTReturn newls)
	
  ); fin fonction  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (prompt "\n DocTekus chargement DTcsv v0.0.1 - licence GNU GPL v3")
  (princ)
  
