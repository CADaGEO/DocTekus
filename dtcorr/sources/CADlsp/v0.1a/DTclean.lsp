;;Copyright 2013, CADaGEO - Cristel LEGRAND
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
;;DESCRIPTION ***************************************************************************************************************************
;****************************************************************************************************************************************
;;Commandes de nettoyage de fichiers autocad
;; Utilise une fonction utilitaire ld-ut.lsp sous licence GPL GNU
;;
;;ROADMAP *******************************************************************************************************************************
;****************************************************************************************************************************************
;;Version 0.1a du 30/10/2013
;;
;;TODO, pochaine version ****************************************************************************************************************
;;***************************************************************************************************************************************
;; fichier ini pour paramétrer ce qui est à nettoyer ou non / et le choix du fichier mapclean
;; supression des élévation, au moins pour les pricnipaux objets
;***************************************************************************************************************************************
;***************************************************************************************************************************************

;chargement d'utilitaires utilisés dans ce code
 (if (findfile "ld-ut.lsp") (load "ld-ut.lsp") (prompt "\nErreur ld-ut.lsp non trouvé"))
 

(defun c:DTclean()
	
		
	;Nettoyage map (minimaliste)
	; à améliorer : chemin relatif par rapport au fichier ini, à indiquer dans le fichier ini ?
	(command "_-mapclean" "D:\cglocal\Tools\env_SFR\outils\DocTekus-SFR\DTmapclean-SFR.dpf")
	(princ "\nFin du nettoyage map")
	
	; mode silencieux
	(setvar "CMDECHO" 0)
	
	;Supression des élévations
	(princ "\nSupression des élévations à faire à la mano pour l'instant...")
	
	;Purger les données des applications
	(command "_-purge" "appsenreg" "*" "n")
	(princ "\nPurge des données d'applications")
	
	;Dissocier les côtes associatives
	(command "_dimdisassociate" "_all" "")
	(princ "\nDissociation des cotes")
	
	;Passer le calque 0 en calque courant
	(setvar "CLAYER" "0")
	
	; Se mettre dans le SCU courant, en zoom étendu
	(command "_ucs" "_w")
	(command "_zoom" "_e")
	
	; Purge et audit complet
	(command "_-purge" "_all" "*" "n")
	(command "_-purge" "_all" "*" "n")
	(command "_-purge" "_all" "*" "n")
	(command "_-purge" "_all" "*" "n")
	(command "_-purge" "_all" "*" "n")
	(command "_-purge" "_all" "*" "n")
	(command "_audit" "_y")
	(princ "\nPurge et audit complet")
	
	(setvar "CMDECHO" 1)
)

(defun c:DTcorxpath()
	;Supression des chemins enregistrés dans les Xréfs
	;
	; pour trouver les références externes et changer les chemins : prendre toutes les entités de la table des blocs, 
	; rechercher celles qui ont un code 1 (les xrefs) et modifier le chemin stocké
	   
		; extraction de la première définition de bloc contenue dans le dessin
		(setq bl (tblnext "BLOCK" T))
		(while bl ; on boucle sur toutes les definitions de blocs du dessin
			(setq xr (assoc 1 bl))
			; si xr n'est pas null c'est une Xréf (un chemin externe est trouvé sur le bloc / code DXF 1)
			(if xr (progn
				; si c'est une xref, on supprime le chemin enregistré : remplacement du chemin complet par le nom du fichier uniquement
				(setq nch (UtFname (cdr xr)))
				(command "_-xref" "_path" (cdr (assoc 2 bl)) nch)
			)) ; fin traitement xref
			
			; passage au bloc suivant
			(setq bl (tblnext "BLOCK"))
			) ; fin boucle while
(princ)
)

(princ)
;Clean chargement