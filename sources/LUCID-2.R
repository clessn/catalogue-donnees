# 0.1 - Packages ####
library(tidyverse)

# 0.2 - Data ####
Data <- read.csv("_SharedFolder_catalogue-donnees/Data/LUCID/LUCID_ligneBleue_December-31-2021_14.csv")

# 0.3 - Cleaning ####

  # Mettre 'ses' comme préfixe aux variables ses

# 1 - Manipulations ####
vars <- names(Data)
bd <- "Nom de la bd"
projets <- toString(c("Projet associé 1", "Projet associé 2"))

Export <- data.frame(nom = vars,
                     bd = rep(bd, length(vars)),
                     projets = rep(projets, length(vars)),
                     label = rep(NA, length(vars)),
                     description = rep(NA, length(vars))) %>%
  mutate(ses = ifelse(substr(nom, 1, 3) == "ses", 1, 0))

# Colonne type
vec_type <- c()
for (i in 1:length(vars)){
  var <- vars[i]
  type <- typeof(Data[[var]])
  vec_type[i] <- type
}
Export$type <- vec_type


# Colonne unique et nVals
Export$unique <- NA
Export$nVals <- NA
for (i in 1:length(vars)){
  var <- vars[i]
  unique <- sort(unique(Data[[var]]))
  Export$unique[i] <- toString(unique)
  Export$nVals[i] <- length(unique)
}

# Associer le export au nom de la bd pour quand on va les bind ensemble
Export_NAMEOFBD <- Export