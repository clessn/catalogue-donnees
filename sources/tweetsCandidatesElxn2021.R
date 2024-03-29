# 0.1 - Packages ####
library(tidyverse)

# 0.2 - Data ####
Data <- readRDS("_SharedFolder_catalogue-donnees/Data/Twitter/tweets_candidates.rds")

# 0.3 - Cleaning ####

  # Mettre 'ses' comme préfixe aux variables ses

# 1 - Manipulations ####
vars <- names(Data)
bd <- "tweetsCandidatesElxn2021"
projets <- "Twitter"

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
#for (i in 1:length(vars)){
#  var <- vars[i]
#  unique <- sort(unique(Data[[var]]))
#  Export$unique[i] <- toString(unique)
#  Export$nVals[i] <- length(unique)
#}

# Associer le export au nom de la bd pour quand on va les bind ensemble
Export_TwCandidates2021 <- Export
