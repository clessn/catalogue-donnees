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
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
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

poll_to_constituency <- c(
  `030` = "ARGENTEUIL-DEUX-MONTAGNES",
  `123` = "ARGENTEUIL-DEUX-MONTAGNES",
  `162` = "ARGENTEUIL-DEUX-MONTAGNES",
  `037` = "CHICOUTIMI",
  `084` = "CHICOUTIMI",
  `085` = "CHICOUTIMI",
  `149` = "CHICOUTIMI",
  `051` = "FRONTENAC",
  `061` = "FRONTENAC",
  `148` = "FRONTENAC",
  `165` = "FRONTENAC",
  `032` = "GASPE",
  `124` = "GASPE",
  `129` = "GASPE",
  `043` = "HULL",
  `180` = "HULL",
  `281` = "HULL",
  `058` = "LAPOINTE",
  `091` = "LAPOINTE",
  `101` = "LAPOINTE",
  `033` = "LAPRAIRIE",
  `222` = "LAPRAIRIE",
  `236` = "LAPRAIRIE",
  `288` = "LAPRAIRIE",
  `074` = "LEVIS",
  `131` = "LEVIS",
  `167` = "LEVIS",
  `182` = "LEVIS",
  `024` = "LONGUEUIL",
  `047` = "LONGUEUIL",
  `062` = "LONGUEUIL",
  `147` = "LONGUEUIL",
  `087` = "LOTBINIERE",
  `161` = "LOTBINIERE",
  `221` = "LOTBINIERE",
  `125` = "AHUNTSIC",
  `177` = "AHUNTSIC",
  `203` = "AHUNTSIC",
  `219` = "AHUNTSIC",
  `077` = "MONTREAL-BOURASSA",
  `089` = "MONTREAL-BOURASSA",
  `286` = "MONTREAL-BOURASSA",
  `130` = "DOLLARD",
  `193` = "DOLLARD",
  `200` = "DOLLARD",
  `274` = "DOLLARD",
  `066` = "GAMELIN",
  `137` = "GAMELIN",
  `150` = "GAMELIN",
  `003` = "LACHINE",
  `046` = "LACHINE",
  `064` = "LACHINE",
  `097` = "LAFONTAINE",
  `098` = "LAFONTAINE",
  `119` = "LAFONTAINE",
  `001` = "MAISONNEUVE-ROSEMONT",
  `143` = "MAISONNEUVE-ROSEMONT",
  `160` = "MAISONNEUVE-ROSEMONT",
  `207` = "MAISONNEUVE-ROSEMONT",
  `004` = "MOUNT ROYAL",
  `014` = "MOUNT ROYAL",
  `114` = "MOUNT ROYAL",
  `178` = "MOUNT ROYAL",
  `096` = "PORTNEUF",
  `171` = "PORTNEUF",
  `197` = "PORTNEUF",
  `095` = "QUEBEC EAST",
  `164` = "QUEBEC EAST",
  `189` = "QUEBEC EAST",
  `049` = "RICHMOND",
  `144` = "RICHMOND",
  `152` = "RICHMOND",
  `007` = "ROBERVAL",
  `050` = "ROBERVAL",
  `111` = "ROBERVAL",
  `073` = "SAINT-HYACINTHE",
  `108` = "SAINT-HYACINTHE",
  `117` = "SAINT-HYACINTHE",
  `118` = "SAINT-HYACINTHE",
  `094` = "SAINT-MAURICE",
  `104` = "SAINT-MAURICE",
  `202` = "SHERBROOKE",
  `237` = "SHERBROOKE",
  `110` = "TERREBONNE",
  `218` = "TERREBONNE",
  `267` = "TERREBONNE",
  `335` = "TERREBONNE",
  `035` = "TROIS-RIVIERES-METROPOLITAIN",
  `048` = "TROIS-RIVIERES-METROPOLITAIN",
  `138` = "TROIS-RIVIERES-METROPOLITAIN",
  `190` = "TROIS-RIVIERES-METROPOLITAIN",
  `065` = "VAUDREUIL",
  `081` = "VAUDREUIL",
  `090` = "VAUDREUIL",
  `157` = "VAUDREUIL",
  `185` = "VAUDREUIL"
)

# Assume df is your dataframe and `poll_number` is the column with the poll numbers
df$constituency <- poll_to_constituency[as.character(df$poll_number)]

# If there are poll numbers with no matching constituency, they will become NA. 
# You might want to handle these cases as well.

## ces84 -------------------------------------------------------------------

## ces88 -------------------------------------------------------------------

## ces93 -------------------------------------------------------------------

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

##### SAVE VECTOR WHERE??