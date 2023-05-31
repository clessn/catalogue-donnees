library(tidyverse)

#### Data ####

Omnibus <- read.csv("_SharedFolder_catalogue-donnees/Data/omnibus/omnibusclessn.csv")
Pilot2_2021 <- read.csv("_SharedFolder_catalogue-donnees/Data/datagotchi/CleanData-Lifestyle2.csv")
GlobalES <- read.csv("_SharedFolder_catalogue-donnees/Data/GlobalEs/Canada/CES_MERGED_1968-2019_2021-11-16.csv")
Quorum <- read.csv("_SharedFolder_catalogue-donnees/Data/quorum/QRM3_2021-01-20.csv")
INSPQ <- read.csv("_SharedFolder_catalogue-donnees/Data/sondage_INSPQ/MainlineData_CleanData_2020-06-01.csv")

#### Data Wrangling ####

main <- data.frame(mainID = 1:nrows, sourceID = vec) 

## rows
Nrows_df <- data.frame(do.call("rbind", lapply(ls(), function(x) { # Créer un data frame qui indique le nombre d'observations pour chaque jeu de données
  obj = get(x)
  if (class(obj) == "data.frame" & x != "Nrows_df")
    c(name = x, rows = nrow(obj))
}))) %>%
  mutate(rows = as.numeric(rows))
nrows <- sum(Nrows_df$rows) # Nombre d'observations totales

## sourceID
for (i in 1:nrow(Nrows_df)) {
  veci <- rep(Nrows_df$name[i], Nrows_df$rows[i])
  if (i == 1) {
    vec <- veci
  } else {
    vec <- c(vec, veci) # vecteur qui indique la source de chaque observation
  }
}

#
sourceID <- "GlobalES"
varNameSource <- "female"
varNameMain <- "ses_female"

sourceID_index <- main[main$sourceID == sourceID, ]$mainID # Fonction pour prendre la variable source et l'ajouter avec le bon nom dans main.

if (varNameMain %in% names(main)) {
  
}





