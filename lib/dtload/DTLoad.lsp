;;Copyright 2015, CADaGEO - Cristel LEGRAND
;;Version 0.0.2 du 09/11/2015
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
;;Chargement des programmes DocTekus à utiliser
;;
;;INSTALLATION ET FONCTIONNEMENT *************************************
;*********************************************************************
;;
;; Conserver tous les fichiers du package dans un même répertoire
;; S'assurer que les blocs définis dans les fichiers de paramétrages soient bien présents + fichier ini + fichiers de paramétrages
;;
;; Quand UN SEUL AutoCAD est lancé / Installation :
;; - Ajouter le chemin dudit répertoire dans les chemins de recherche de fichiers de support (_options / fichiers) et éventuellement dans les chemins approuvés
;; - charger le lisp DTload.lsp systématiquement au démarrage d'AutoCAD (_appload, valisette "contenu" dans "au démarrage" puis redémarrer Autocad)
;;

;; Fonctionnement :
;; Selon les commandes chargées, à personnaliser éventuellement

;;Fonctionnalités Doctekus
 (if (findfile "DTcloop.lsp") (load "DTcloop.lsp"))
 (if (findfile "DTcor.lsp") (load "DTcor.lsp")) 
 (if (findfile "DTcorr2gis.lsp") (load "DTcorr2gis.lsp"))
 (if (findfile "DTTL.lsp") (load "DTTL.lsp")) 
 (if (findfile "Script-cancel.lsp") (load "Script-cancel.lsp"))
 (if (findfile "DTcsv.lsp") (load "DTcsv.lsp"))
 (if (findfile "DTGrid.lsp") (load "DTGrid.lsp"))
 (if (findfile "DTDraw.lsp") (load "DTDraw.lsp"))
 (if (findfile "DTImages.lsp") (load "DTImages.lsp"))
 
;;Fonctionnalités BarbaTatou
 (if (findfile "ch_attribut.lsp") (load "ch_attribut.lsp"))
 (if (findfile "buffer.lsp") (load "buffer.lsp"))
 (if (findfile "copy_od.lsp") (load "copy_od.lsp")) 
 (if (findfile "Jav.lsp") (load "Jav.lsp"))
 (if (findfile "PTEXPORT.lsp") (load "PTEXPORT.lsp")) 
 
 
 
 
 