;;Copyright 2012-2013, CADaGEO - Guillaume BERSON, Couskou, Cristel LEGRAND
;;Version 2.0.1 du 09/12/2013
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
;;Création de carroyage
;;
;;INSTALLATION ET FONCTIONNEMENT *************************************
;*********************************************************************
;;
;; Conserver tous les fichiers du package dans un même répertoire
;;
;; Quand UN SEUL AutoCAD est lancé / Installation pour un usage courant :
;; - Ajouter le chemin dudit répertoire dans les chemins de recherche de fichiers de support (_options / fichiers)
;; - charger le lisp DTgrid.lsp systématiquement au démarrage d'AutoCAD (_appload, valisette "contenu" dans "au démarrage" puis redémarrer Autocad)
;;
;; Fonctionnement :
;; Taper "DTGrid" dans la ligne de commande et se laisser guider 
;;
;;CHANGELOG **********************************************************
;;********************************************************************
;;
;;Version 2.0.1 du 09/12/2013
;;- fix message d'erreur systématique insertion bloc échelle
;;- fix dégèle et activation systématique des calques utilisés / puis restauration à l'état d'origine
;;- version pour diffusion
;;- fix placement des références de planches adjacentes alt2
;;
;;Version 2.0.0alpha3 du 02/12/2013
;;- Intégration au projet DocTekus : barbacar devient DTgrid
;;- Ajout d'un fichier ini avec options de paramétrages supplémentaires (calques, alternatives, styles de textes, limites adjacentes...)
;;- Ajout des insertions de blocs nord et échelles
;;- Correction bug orientation textes lorsque Nord vers le bas
;;- Ajout de contrôles de saisie
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
;;- Suppression des chiffres après la virgule dans les coordonnées (concerne uniquement certaines versions d’Autocad)
;;
;;Version 1.0.1 du 06/11/2012
;;- Correction du nom de la commande (qui était Carroyage au lieu de Car)
;;
;;Version 1.0.0 du 30/10/2012
;;- Diffusion sous licence GNU
;;
;;Version 0.2.1 du 30/09/2012
;;- Passage en SCU Général forcé
;;- Ecart entre croix forcé (10cm quand inférieur à 1/1000, 5cm quand supérieur)
;;
;;Version 0.2 du 19/09/2012
;;- Prise en compte du carroyage à l’envers (plans ayant le nord vers le bas)
;;- Saisie utilisateur pour la distance entre deux croix
;;- Passage du SCU en général après question utilisateur
;;
;;Version 0.1 du 10/09/2012
;;- Mise à disposition initiale de la version 0.1
;; 
;;
;;DEPENDANCES *******************************************************
;;*******************************************************************
;; Nécessite les fonctions de DTini, qui lui-même charge ld-ut et dtut
;; Nécessite les fonctions ld-ut DTut, chargées via DTini
;; Utilise les fonctions vl*
;;

