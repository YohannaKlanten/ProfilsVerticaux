setwd("C:/Users/klayo01/OneDrive - Ministère de l'Environnement et la Lutte contre les changements climatiques/Documents/GitHub/Profils_verticaux")

# Chargement des fonctions et bibliothèques
devtools::document()
devtools::load_all()

annee_fiche <- "2025" #Changer l'année en fonction des données à extraires

#Repertoire
template_path <- "C:/Users/klayo01/OneDrive - Ministère de l'Environnement et la Lutte contre les changements climatiques/Documents/GitHub/Profils_verticaux/template_vide_fiche.docx"
repo <- "X:/DOCUM/4300_Gest_donnees/Eau_Suivi/Lacs/Profils/Extractions"

# Importer la BD
BD <- importBD()

BD_annee <- BD[format(BD$Date, "%Y") == annee_fiche, ] 

# Calculer DO mg/L si manquant
BD_annee <- calc_DO_mgl(BD_annee)

# Production des fiches graphiques
graphFicheLot(db=BD_annee, dossier_out = file.path(repo, 'Par lac'))

# Production des fichiers excel de données physico
extract_parLac(db=BD_annee, dossier_out = file.path(repo, 'Par lac'))
