# Packages ----------------------------------------------------------------
library(tidyverse)
library(stringdist)

# Config ------------------------------------------------------------------

### source config file to generate all the respondent_ids
source("merging-souverainete/config.R")

sample(ids, 20) ### 20 ids random
## empty vector where the clean values will go. same length as the n of ids.
output_geoloc <- rep(NA, length(ids))
### each element in output is a respondent with a unique respondent id
names(output_geoloc) <- ids

# Merging and cleaning ----------------------------------------------------

df_ridings <- read.csv("_SharedFolder_catalogue-donnees/merging-souverainete/aux_data/quebec_fed_ridings.csv")

## ces65 -------------------------------------------------------------------

# Load variable
raw_ces65 <- sondr::load_variable(
    file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1965/ces65.csv", 
    variable_name = "v6")

table(raw_ces65, useNA = "always")
ces65_ridings <- unique(raw_ces65)
df_ridings_1965 <- df_ridings %>%
    filter(year == 1952)

df_ridings_1965$circonscription_ces <- NA

for (i in 1:nrow(df_ridings_1965)) {
    min_dist <- 1
    for (j in 1:length(ces65_ridings)) {
        distance <- stringdist(df_ridings_1965$circonscription[i], 
                               ces65_ridings[j], method = "jw")
        if (distance < min_dist) {
            min_dist <- distance
            df_ridings_1965$circonscription_ces[i] <- ces65_ridings[j]
        }
    }
}

df_ridings_1965$distance <- NA

for (i in seq_along(df_ridings_1965$circonscription_ces)) {
   df_ridings_1965$distance[i] <- stringdist(df_ridings_1965$circonscription[i], 
                               df_ridings_1965$circonscription_ces[i], method = "jw")
}

# Vérfier à partir de où les résultats ne sont plus satisfaisants
df_ridings_1965 <- df_ridings_1965 %>%
    filter(distance < 0.2678) 

# Ajustements manuels
df_ridings_1965 <- df_ridings_1965 %>%
  filter(!(circonscription %in% c("Labelle", "Laurier", "Papineau", "Saint-Henri", "Quebec East")))
  
clean_ces65 <- c(NA)

geolocs <- df_ridings_1965$geoloc
names(geolocs) <- df_ridings_1965$circonscription_ces
clean_ces65 <- geolocs[raw_ces65]

names(clean_ces65) <- sondr::generate_survey_ids(n_respondents = length(clean_ces65), ## number of respondents
                                                 source_id = "ces65") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_ces65) ## vector with updates

    
table(sondr::extract_elements_with_prefix(output_geoloc, "ces65"))

## ces68 -------------------------------------------------------------------

# NA

## ces74 -------------------------------------------------------------------

# Load variable
raw_ces74 <- sondr::load_variable(
    file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1974/ces74.csv", 
    variable_name = "v7")

table(raw_ces79, useNA = "always")

## ces79 -------------------------------------------------------------------

# Load variable
raw_ces79 <- sondr::load_variable(
    file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1979/ces79.csv", 
    variable_name = "v5")

table(raw_ces79, useNA = "always")

## ces84 -------------------------------------------------------------------

# Load variable
raw_ces84 <- sondr::load_variable(
    file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1984/ces84.csv", 
    variable_name = "var006")

table(raw_ces79, useNA = "always")

## ces88 -------------------------------------------------------------------

raw_ces88 <- sondr::load_variable(
    file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1988/ces88.csv", 
    variable_name = "intnum")

table(raw_ces79, useNA = "always")

## ces93 -------------------------------------------------------------------#

# NA

## ces97 -------------------------------------------------------------------

# NA

## ces2000 -------------------------------------------------------------------

# NA

## ces2004 -------------------------------------------------------------------

# NA

## ces2006 -------------------------------------------------------------------

# NA

## ces2008 -------------------------------------------------------------------

# NA

## ces2011 -------------------------------------------------------------------

# NA

## ces2015 -------------------------------------------------------------------

# NA

## ces2019 -------------------------------------------------------------------

# NA

## ces2021 -------------------------------------------------------------------

# NA

## datagotchi_pilot1_2021 -------------------------------------------------------------------

# NA

## january -------------------------------------------------------------------

# NA

## february -------------------------------------------------------------------

# NA

## march -------------------------------------------------------------------

## april -------------------------------------------------------------------

## may -------------------------------------------------------------------

## june -------------------------------------------------------------------

## datagotchi_pilot2_2022 -------------------------------------------------------------------

### ICI IL FAUT MERGER FRANCAIS ET ANGLAIS

#### 1. Get raw gender variable vector
raw_fr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                  variable_name = "income_fr")
table(raw_fr, useNA = "always")

raw_en <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                               variable_name = "income_en")
table(raw_en, useNA = "always")

raw <- coalesce(raw_fr, raw_en)

#### 2. clean variable


## sondage_nationalisme_2022 -------------------------------------------------------------------

## quorum_mcq_pilote -------------------------------------------------------------------

### ICI IL FAUT MERGER FRANCAIS ET ANGLAIS

#### 1. Get raw gender variable vector
raw_fr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                               variable_name = "income_fr")
table(raw_fr, useNA = "always")

raw_en <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                               variable_name = "income_en")
table(raw_en, useNA = "always")

raw <- coalesce(raw_fr, raw_en)

#### 2. clean variable

## pes_elxn_2022_text -------------------------------------------------------------------

## pco -------------------------------------------------------------------


# Output ------------------------------------------------------------------

### FACTORISE, LEVELS, etc.

##### SAVE VECTOR WHERE??