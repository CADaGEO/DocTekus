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
;;Cr�ation de carroyage
;;
;;INSTALLATION ET FONCTIONNEMENT *************************************
;*********************************************************************
;;
;; Conserver tous les fichiers du package dans un m�me r�pertoire
;;
;; Quand UN SEUL AutoCAD est lanc� / Installation pour un usage courant :
;; - Ajouter le chemin dudit r�pertoire dans les chemins de recherche de fichiers de support (_options / fichiers)
;; - charger le lisp DTgrid.lsp syst�matiquement au d�marrage d'AutoCAD (_appload, valisette "contenu" dans "au d�marrage" puis red�marrer Autocad)
;;
;; Fonctionnement :
;; Taper "DTGrid" dans la ligne de commande et se laisser guider 
;;
;;CHANGELOG **********************************************************
;;********************************************************************
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

