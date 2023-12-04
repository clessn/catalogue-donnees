# Packages ----------------------------------------------------------------
library(tidyverse)
library(stringdist)

# Config ------------------------------------------------------------------

### source config file to generate all the respondent_ids
source("merging-souverainete/config.R")

sample(ids, 20) ### 20 ids random
## empty vector where the clean values will go. same length as the n of ids.
output <- rep(NA, length(ids))
### each element in output is a respondent with a unique respondent id
names(output) <- ids

# Merging and cleaning ----------------------------------------------------

df_ridings <- read.csv("merging-souverainete/merging_files/data/quebec_fed_ridings.csv")

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
  
for (i in seq_along(df_ridings_1965$circonscription)) { 
    if (df_ridings_1965$circonscription[i] == "Quebec South") {
        df_ridings_1965$circonscription_ces[i] <- "quebec-sud"
    }
}

# Assuming clean_ces65 is your new vector and it's already initialized with the correct length
# and 'NA' or some default values

# Find the matching indices of raw_ces65 in df_ridings_1965$circonscription_ces
indices <- match(raw_ces65, df_ridings_1965$circonscription_ces)

# 'indices' will be NA for values in raw_ces65 that don't match any value in df_ridings_1965$circonscription_ces
# Replace the NA's with 0 or some other value that cannot be an index
indices[is.na(indices)] <- 0

# Use the indices to replace corresponding entries in clean_ces65 with geoloc from df_ridings_1965
clean_ces65 <- ifelse(indices > 0, df_ridings_1965$geoloc[indices], NA)


#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces65) <- sondr::generate_survey_ids(n_respondents = length(clean_ces65), ## number of respondents
                                                 source_id = "ces65") ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                         updates = clean_ces65) ## vector with updates

## ces68 -------------------------------------------------------------------

# NA

## ces74 -------------------------------------------------------------------

# NA

## ces79 -------------------------------------------------------------------

# Load variable
raw_ces79 <- sondr::load_variable(
    file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1979/ces79.csv", 
    variable_name = "v5")

table(raw_ces79, useNA = "always")

constituency_numbers <- c(401L, 402L, 403L, 404L, 405L, 406L, 407L, 408L, 409L, 410L, 
                          411L, 412L, 413L, 414L, 415L, 416L, 417L, 418L, 419L, 420L, 
                          421L, 422L, 423L, 424L, 425L, 426L, 427L, 428L)

# Vector of constituency names
constituency_names <- c("ARGENTEUIL-DEUX-MONTAGNES", "CHICOUTIMI", "FRONTENAC", "GASPE", 
                        "HULL", "LAPOINTE", "LAPRAIRIE", "LEVIS", "LONGUEUIL", "LOTBINIERE", 
                        "AHUNTSIC", "MONTREAL-BOURASSA", "DOLLARD", "GAMELIN", "LACHINE", 
                        "LAFONTAINE", "MAISONNEUVE-ROSEMONT", "MOUNT ROYAL", "PORTNEUF", 
                        "QUEBEC EAST", "RICHMOND", "ROBERVAL", "SAINT-HYACINTHE", "SAINT-MAURICE", 
                        "SHERBROOKE", "TERREBONNE", "TROIS-RIVIERES-METROPOLITAIN", "VAUDREUIL")

# Create the dataframe
df_constituency_1976 <- data.frame(
  constituency_number = constituency_numbers,
  constituency_name = constituency_names,
  stringsAsFactors = FALSE
)


## ces84 -------------------------------------------------------------------

## ces88 -------------------------------------------------------------------

## ces93 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw_ces93 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1993/ces93.csv",
                                  variable_name = "cpspost")
table(raw_ces93, useNA = "always")


## ces97 -------------------------------------------------------------------

## ces2000 -------------------------------------------------------------------

## ces2004 -------------------------------------------------------------------

## ces2006 -------------------------------------------------------------------

## ces2008 -------------------------------------------------------------------

## ces2011 -------------------------------------------------------------------

## ces2015 -------------------------------------------------------------------

## ces2019 -------------------------------------------------------------------

## ces2021 -------------------------------------------------------------------

## datagotchi_pilot1_2021 -------------------------------------------------------------------

## january -------------------------------------------------------------------

## february -------------------------------------------------------------------

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

output <- factor(output)

##### SAVE VECTOR WHERE??