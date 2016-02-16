# DocTekUs
DocTekus est un projet de boîtes à outils dédiées à la gestion des bibliothèques de plans CAO/DAO.
Il permet de développer et réunir des utilitaires et briques logicielles ouvertes, modulaires et paramétrables.

Ces boîtes à outils ont pour but de faciliter :
- la production de plans structurés, dans le respect de spécifications CAO/DAO imposées (dtprod)
- le contrôle de la production (dtcheck)
- les corrections (dtcorr)
- les conversions et la collaboration avec les autres outils du SI, notamment SIG et GED (dtconv)
- la publication de la documentation et des données (dtpub)

Début 2013 : DocTekus est initié par CADaGEO (http://www.cadageo.com).

Début 2014 : Début de publication du code des premiers outils "en état de l'être" et lancement des premiers outils de collaboration. 

Plusieurs contributeurs ont rejoints le projet en 2015 et 2016 (voir liste des contributeurs plus bas).

### Caractéristiques de la version actuelle
Début 2016, DocTekUs contient principalement des outils pour AutoCAD :
* DTgrid : un outils de création de carroyage
* DTdraw : un outils d'aide au dessin
* DTpompe : un outils d'analyse du contenu d'un fichier dwg
* Dtcor : des outils de correction/préparation de fichiers avant traitements de masse ou imports SIG
 
### Contenu du dépôt
* changelog.txt : historique des modifications
* \doc\ : schéma d'architecture, description des focntionnalités actuellement disponibles, charte de développement
* \[BoiteOutils]\ [Outil]\sources\ [Plateforme]\ : fichiers sources des différents outils, répartis selon les cas par boîtes à outils, outils et plateforme de développement
* \[BoiteOutils]\ [Outil]\packages\ : lorsqu'ils sont disponibles, fichiers archives contenant des packages pour diffusion (nommés [outil]'_'[version]'_'[plateforme].zip)
* \ packages \ : contient un package standard regroupant les principaux outils

### Support or Contact
Une plateforme de gestion de projet [Redmine](http://redmine.cadageo.com/) destinée à remonter les demandes d'évolutions, les anomalies et les demandes d'assistance est disponible. 

### Contributeurs
Développement : ALENO - G BERSON - CADAGEO - JC MICHELIN - E PARIS - Techniques Topos

Autres sponsors : COVAGE - SNCF

Certaines parties de codes ont été recopiées ou inspirées de plusieurs auteurs de forums que nous remercions ici, sans pouvoir tous les citer : fabcad, bonuscad, MNT Reponse No 18...