;;Copyright 2013-2014, CADaGEO - Cristel LEGRAND
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
;;Commandes et fonctions de nettoyage/correction de fichiers autocad : 
;;	- DTclean (nettoyage général) et 
;;	- DTcorxpath (supression des chemin enregistrés dans les XRéfs)
;;	- DTcoripath (supression des chemin enregistrés dans les IRéfs)
;;	- DTcoriref (suppression des images et PDF en références externes)
;;	- DTblkmaj (mise à jour des blocs par rechargements)
;; Utilise une fonction utilitaire ld-ut.lsp sous licence GPL GNU
;;
;;CHANGELOG **********************************************************
;;********************************************************************
;;Version 0.1c du 03/12/2014 :
;;  - ajout de la fonction DTupblk
;;
;;Version 0.1b du 04/11/2013 :
;;	- Changement de nom du fichier lsp : DTclean devient DTcor
;;	- prise en compte des images attachées
;;  - correction bug quand chemin Xréf non trouvé
;;	- distinction fonction/commande
;;	- prise en compte variables locales
;;  - revue des message utilsateurs (positionnés en fin de commande)
;;	- ajout DTcoriref et DTcoripath
;;
;; Version 0.1b du 02/12/2013 :
;; - fix message erreur quand pas d'images dtcoripath
;;
;;Version 0.1a du 30/10/2013 :
;;	- version initiale
;;
;;DEPENDANCES *******************************************************
;;*******************************************************************
;; Utilise une fonction utilitaire ld-ut.lsp sous licence GPL GNU
;; Utilise la variable globale $DTLogUser
;;
;;TODO, pochaine version *********************************************
;;********************************************************************
;; Voir issue GitHub
;; Revoir le respect des conventions de prog DocTekus
;; Traitement des erreurs
;; fichier ini pour paramétrer ce qui est à nettoyer ou non / et le choix du fichier mapclean
;; supression des élévation, au moins pour les pricnipaux objets
;; Reprendre le code LDCheck pour le contrôle du calque 0 --> beaucoup plus fiable
;; avant le démarrage, se mettre en style de texte standard, style de côtes standard et styles de tableaux
;*********************************************************************
;*********************************************************************

;chargement d'utilitaires utilisés dans ce code
 (if (findfile "ld-ut.lsp") (load "ld-ut.lsp") (prompt "\nErreur ld-ut.lsp non trouvé"))
 (if (findfile "DTut.lsp") (load "DTut.lsp") (prompt "\nErreur DTut.lsp non trouvé"))
 
 ; DTclean : nettoyage général
 ;********************************************************************
 (defun C:DTclean()
 	; pour que le programme puisse être annulé comme une seule commande
	(setvar "CMDECHO" 0)
	(command "_UNDO" "_BE")

	;RAZ des messages utilsateurs
	(setq $DTLogUser "")
	
	(F_DTclean)
	
	; pour que le programme puisse être annuél comme une seule commande
	(command "_UNDO" "_E")
	(setvar "CMDECHO" 1)
	
	;Affichage des messages utilsateurs
	(Textscr)
	(princ $DTLogUser)
	(princ)
)

(defun F_DTclean(/ NetMap NetEl NetApps NetCot NetPurg NetAudit NetZero ss0 nb0)
	
	; Paramétrage - à remplacer par la lecture du fichier ini
	(setq NetMap T) ; Nettoyage Map ?
	(setq NetPath (findfile "DTmapclean.dpf")) ; chemin d'accès au fichier de nettoyage
	(setq NetEl T) ; Supression des élévations ?
	(setq NetApps T) ; Purger les données des applications ?
	(setq NetCot T) ; Dissocier les côtes associatives ?
	(setq NetPurg T) ; Purge complète ?
	(setq NetAudt nil) ; Audit avec correction des erreurs ?
	(setq NetZero T); Vérification du calque 0 ?
	(setq NetBlk nil); Rechargement des blocs ? => à compléter, ne fonctionne pas en l'état
		
	;Passer le calque 0 en calque courant
	(setvar "CLAYER" "0")
	
	; Se mettre dans le SCU courant, en zoom étendu
	(command "_ucs" "_w")
	(command "_zoom" "_e")
		
	;Nettoyage map (minimaliste)
	; à améliorer : chemin relatif par rapport au fichier ini, à indiquer dans le fichier ini ?
	(if NetMap (progn
		(if (findfile NetPath)
			(command "_-mapclean" NetPath)
		)
	))
	
	;Supression des élévations
	
	;Purger les données des applications
	(if NetApps (progn
		(command "_-purge" "appsenreg" "*" "n")
	))
	
	;Dissocier les côtes associatives
	(if NetCot (progn
		(command "_dimdisassociate" "_all" "")
	))		
	
	; Purge complete
	(if NetPurg (progn
		(command "_-purge" "_all" "*" "n")
		(command "_-purge" "_all" "*" "n")
		(command "_-purge" "_all" "*" "n")
		(command "_-purge" "_all" "*" "n")
		(command "_-purge" "_all" "*" "n")
		(command "_-purge" "_all" "*" "n")
	))
		
	; Audit
	(if NetAudit (progn
		(command "_audit" "_y")
	))
	
	; Messages d'erreurs : à reprendre en s'inspirant de DRcorr2gis
	(if NetMap (setq $DTLogUser (strcat $DTLogUser "\nDTclean : Nettoyage map ok, profil " NetPath)))
	(if NetEl (setq $DTLogUser (strcat $DTLogUser "\nDTclean : Supression des élévations à faire à la mano pour l'instant...")))
	(if NetApps (setq $DTLogUser (strcat $DTLogUser "\nDTclean : Purge des données d'applications")))
	(if NetCot (setq $DTLogUser (strcat $DTLogUser "\nDTclean : Dissociation des cotes")))
	(if NetPurg (setq $DTLogUser (strcat $DTLogUser "\nDTclean : Purge complète")))
	(if NetAudit (setq $DTLogUser (strcat $DTLogUser "\nDTclean : Fichier contrôlé, erreurs corrigées")))	
	
	;Contrôle calque 0
	(if NetZero (progn
		(setq ss0 (ssget "X" (list '(8 . "0"))))
		(if ss0 (setq nb0 (sslength ss0)) (setq nb0 0))
		(if (> nb0 0) (setq $DTLogUser (strcat $DTLogUser "\nDTClean : ATTENTION " (itoa nb0) " entités présentes sur le calque 0")))
	))
	
	; Rechargement des blocs => à compléter, ne fonctionne pas en l'état
	(if NetBlk (progn
		(F_DTupblk)
	))
	
)

; DTcorxpath : supression des chemins enregistrés dans les XRéfs
 ;********************************************************************
 (defun C:DTcorxpath()
 	; pour que le programme puisse être annulé comme une seule commande
	(setvar "CMDECHO" 0)
	(command "_UNDO" "_BE")
	
	;RAZ des messages utilsateurs
	(setq $DTLogUser "")
	
	(F_DTcorxpath)
	
	; pour que le programme puisse être annuél comme une seule commande
	(command "_UNDO" "_E")
	(setvar "CMDECHO" 1)
	
	;Affichage des messages utilsateurs
	(princ $DTLogUser)
	(princ)
 )
(defun F_DTcorxpath(/ bl xr nch)

	;Supression des chemins enregistrés dans les Xréfs, pour celles dont le chemin est effectiviement trouvé
	;
	; pour trouver les références externes et changer les chemins : prendre toutes les entités de la table des blocs, 
	; rechercher celles qui ont un code 1 (les xrefs) et modifier le chemin stocké
	   
	; extraction de la première définition de bloc contenue dans le dessin
	(setq bl (tblnext "BLOCK" T))
	(while bl ; on boucle sur toutes les definitions de blocs du dessin
		(setq xr (assoc 1 bl))
		; si xr n'est pas null c'est une Xréf (un chemin externe est trouvé sur le bloc / code DXF 1)
		(if xr (progn
			; c'est une xref, on regarde si le fichier est trouvé malgré la suppression du chemin
			(setq nch (UtFname (cdr xr)))
			(if (findfile nch) (progn
				; chemin trouvé, on supprime le chemin enregistré : remplacement du chemin complet par le nom du fichier uniquement
				(command "_-xref" "_path" (cdr (assoc 2 bl)) nch)
				(setq $DTLogUser (strcat $DTLogUser "\nDTcorxpath : suppression chemin enregistré " nch))				
			))
		)) ; fin traitement xref
		
		; passage au bloc suivant
		(setq bl (tblnext "BLOCK"))
	) ; fin boucle while		
)

; DTcoripath : supression des chemins enregistrés dans les Images
 ;********************************************************************
 (defun C:DTcoripath()
	; pour que le programme puisse être annulé comme une seule commande
	(setvar "CMDECHO" 0)
	(command "_UNDO" "_BE")
	
	;RAZ des messages utilsateurs
	(setq $DTLogUser "")
	
	(F_DTcoripath)
	
	; pour que le programme puisse être annuél comme une seule commande
	(command "_UNDO" "_E")
	(setvar "CMDECHO" 1)
	
	;Affichage des messages utilsateurs
	(princ $DTLogUser)
	(princ)
 )
(defun F_DTcoripath(/ ssim nbim i imn imsn ims ir nch nir nims nlims nim)
	
	;Supression des chemins enregistrés dans les Images, pour celles dont le chemin est effectiviement trouvé
	;
	; pour trouver les images externes et changer les chemins : prendre toutes les entités de type "IMAGE", 
	; et modifier le chemin stocké (code 1 dans le sous code 340)
	   
	; sélection des images
	(setq ssim (ssget "X" (list '(0 . "IMAGE"))))
	(if ssim (setq nbim (sslength ssim)) (setq nbim 0))
	(setq i 0)
	
	;pour chaque image
	(repeat nbim
		; pour chaque image, sélection de la sous-entité 340 qui nous intéresse ici (celle qui contient le chemin : IMAGEDEF)
		(setq imn (ssname ssim i)) ;nom de l'entité principale
		(setq im (entget imn)); entité principale
		(setq lims (assoc 340 im)); paire pointée contenant l'entité secondaire
		(setq imsn (cdr lims)) ; nom de la sous entité 340
		(setq ims (entget imsn)) ; liste qui définit la sous entité associée au code 340
		(setq ir (assoc 1 ims)) ; liste pointée qui contient le chemin de l'iref
		(setq nch (UtFname (cdr ir))) ; calcul du nouveau chemin
		(if (findfile nch) (progn
				; chemin trouvé, on supprime le chemin enregistré : remplacement du chemin complet par le nom du fichier uniquement
				(setq nir (cons (car ir) nch)); construction de la nouvelle paire pointée contenant le nouveau chemin
				(setq nims (subst nir ir ims)); nouvelle liste de sous-entité mise à jour
				(entmod nims); mise à jour de la base du dessin avec la nouvelle sous-entité
				(setq $DTLogUser (strcat $DTLogUser "\nDTcoripath : suppression chemin enregistré " nch)); message utilisateur
		))
	)

)

; DTcoriref : supression des Images attachées
 ;********************************************************************
 (defun C:DTcoriref()
	; pour que le programme puisse être annulé comme une seule commande
	(setvar "CMDECHO" 0)
	(command "_UNDO" "_BE")
	
	;RAZ des messages utilsateurs
	(setq $DTLogUser "")
	
	(F_DTcoriref)
	
	; pour que le programme puisse être annuél comme une seule commande
	(command "_UNDO" "_E")
	(setvar "CMDECHO" 1)
	
	;Affichage des messages utilsateurs
	(princ $DTLogUser)
	(princ)
 )
(defun F_DTcoriref()
	
	;Supression des images
	(command "_-image" "_d" "*")
	(setq $DTLogUser (strcat $DTLogUser "\nDTcoriref : suppression des images attachées")); message utilisateur

)

; DTblkmaj : Mise à jour des blocs depuis un répertoire donné (chemin)
 ;********************************************************************
 (defun C:DTupblk( / ch)
	; pour que le programme puisse être annulé comme une seule commande
	(setvar "CMDECHO" 0)
	(command "_UNDO" "_BE")
	
	;RAZ des messages utilsateurs
	(setq $DTLogUser "")
	
	;Récupération du chemin des blocs
	(setq ch (getstring "Chemin d'accès au répertoire contenant les blocs ? "))
	
	(F_DTupblk ch)
	
	; pour que le programme puisse être annuél comme une seule commande
	(command "_UNDO" "_E")
	(setvar "CMDECHO" 1)
	
	;Affichage des messages utilsateurs
	(princ $DTLogUser)
	(princ)
 )
(defun F_DTupblk(chemin / lsb b bname)

	(setq lsb (DTLsBlkUsed)); liste des noms de blocs utilisés dans le dessin
	(foreach b lsb
		(setq bname (strcat (DTgetpath (getvar "DWGPREFIX") chemin) "\\" b ".dwg")); nom du fichier bloc à recharger (get path permet de traiter que ce soit un chemin absolu ou relatif)
		(if (findfile bname) (progn ; le bloc n'est rechargé que si il est effectivement trouvé
			(command "_-INSERT" (strcat b "=" bname) nil)
			(setq $DTLogUser (strcat $DTLogUser "\nF_DTupblk : rechargement du bloc " bname)); message utilisateur
		)
			(setq $DTLogUser (strcat $DTLogUser "\nF_DTupblk : bloc non rechargé " bname)); message utilisateur
		)
	)
)



(prompt "\n DocTekus chargement DTcor v0.1c - licence GNU GPL v3")
(princ)
;Clean chargement