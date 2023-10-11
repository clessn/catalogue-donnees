# Charger le package dplyr pour la manipulation des données
library(dplyr)

# Créer une liste de noms de source pour les fichiers
source_list <- c("ces65", "ces68", "ces74", "ces79", "ces84", "ces88",
                 "ces93", "ces97", "ces2000", "ces2004", "ces2006", "ces2008",
                 "ces2011", "ces2015", "ces2019", "ces2021", "datagotchi_pilot1_2021",
                 "january", "february", "march", "april", "may", "june",
                 "datagotchi_pilot2_2022", "sondage_nationalisme_2022", "quorum_mcq_pilote",
                 "pes_elxn_2022_text", "pco")

# Spécifier les extensions de fichier possibles
exts <- c(".csv", ".xlsx", ".Sav")

# Créer toutes les combinaisons possibles de noms de source et d'extensions de fichiers
possible_files <- expand.grid(source=source_list,
                              ext=exts,
                              stringsAsFactors = FALSE) %>% 
  mutate(col = paste0(source, ext)) %>% # Concaténer les noms de source et les extensions
  pull(., col) # Sélectionner les noms de fichier complets

# Récupérer tous les chemins de fichier dans le répertoire spécifié et ses sous-répertoires
all_paths <- list.files("_SharedFolder_catalogue-donnees/merging-souverainete/raw",
                        recursive = TRUE, full.names = TRUE)

# Filtrer les chemins pour ne conserver que ceux dont le nom de fichier est dans possible_files
paths <- all_paths[basename(all_paths) %in% possible_files]

# Message pour l'utilisateur pour indiquer le début de la génération des IDs uniques
message("Starting: generate list of unique ids")

# Boucle à travers chaque chemin de fichier dans `paths`
for (i in paths){
  # Message pour indiquer le fichier en cours de traitement
  message(paste0("Trying: ", basename(i)))
  
  # Extraire le nom de fichier sans extension pour l'utiliser comme ID source
  source_id <- tools::file_path_sans_ext(basename(i))
  
  # Lire les données du fichier avec la fonction read_survey du package sondr
  d <- sondr::read_survey(i)
  
  # Générer des IDs uniques pour chaque ligne du fichier de données
  idsi <- sondr::generate_survey_ids(nrow(d), source_id)
  
  # Concaténer les nouveaux IDs avec ceux déjà générés dans les itérations précédentes
  if (i == paths[1]){
    ids <- idsi
  } else {
    ids <- c(ids, idsi)
  }
  
  # Message pour indiquer la fin du traitement du fichier actuel
  message(paste0(basename(i), " done"))
}

# Supprimer tous les objets de l'environnement sauf ids
rm(list = setdiff(ls(), "ids"))

# Message pour l'utilisateur pour indiquer que les IDs uniques se trouvent dans le vecteur ids
message("The unique ids are now in the ids vector")


