Traitement des données du programme PME
==========================================

Ce répertoire contient une grande partie du travail effectué de janvier à juin 2014 dans le cadre du programme PME.

>L'objectif du programme était d'étudier les PME (cours d'eau situés en tête de bassin versant et de faible dimensions) d'un point de vue écologique et aussi d’évaluer les effets de perturbations d’origine anthropique sur les communautés aquatiques de ces têtes de bassin. 
>Plusieurs milliers de poissons ont été prélevés en divers points de Guyane. Une base de données répertoriant les résultats des analyses effectuées sur ces individus a donc été créée et a fait l'objet d'analyses.





Les dossiers et fichiers sont présentés ci-dessous


Script_final_PME.R
-------------------

Ce script utilise les fonctions contenues dans le dossier `Scripts` et produit les résultats graphiques stockés dans le dossier `Graph` en utilisant les données issues du répertoire `Data`.
Il est commenté dans la mesure du possible.

Dossier `Scripts`
-------------------

Contient :

* data_cleaning.R : 

    Mise en forme des données brutes issues du répertoire `Data` afin de réaliser les analyses.
    
* functions.R : 

   Quelques fonctions utilisées dans le cadre des analyses.

Dossier `Graph`
-------------------

Contient 3 sous dossiers :

* Elements_traces :

   Contient des visualisations obtenues après le dosage des éléments traces

* Hg_isotopie :

    Contient des visualisations concernant le mercure, l'isotopie et l'organotropisme
    
* Pression_anthropique :

   Contient des visualisations sur l'impact des pressions anthropiques sur la contamination mercurielle

Dossier `Data`
-------------------

Contient :

* Sous format .xlsx : 

  * heavymetals_fishes

          un fichier regroupant les recherches bibliographiques sur la contamination des poissons en éléments traces
          
  * les bases de données brutes
  
* Sous format .csv : 

  * BDD_PME

          base de données complète importable sous `R`
         
  * subset_BDD

         sélection de la base de données importable sous `R`. Contient les informations provenant de 3 stations uniquement



BDD_SIG.R
-------------------

Script permettant de créer une base de données synthétique exploitable sous SIG

EDA_PME.R
-------------------

Fichier brouillon. Inutile mais conservé au cas où
