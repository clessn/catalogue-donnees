# Packages ----------------------------------------------------------------
library(tidyverse)
library(stringdist)
library(haven)

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
df_postal_codes <- read.csv("_SharedFolder_catalogue-donnees/merging-souverainete/aux_data/rta_geoloc.csv")
## ces65 -------------------------------------------------------------------

# Load variable
raw_ces65 <- sondr::load_variable(
    file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1965/ces65.csv", 
    variable_name = "v6")

table(raw_ces65, useNA = "always")
ces65_ridings <- unique(raw_ces65)
df_ridings_2021 <- df_ridings %>%
    filter(year == 1952)

df_ridings_2021$circonscription_ces <- NA

for (i in 1:nrow(df_ridings_2021)) {
    min_dist <- 1
    for (j in 1:length(ces65_ridings)) {
        distance <- stringdist(df_ridings_2021$circonscription[i], 
                               ces65_ridings[j], method = "jw")
        if (distance < min_dist) {
            min_dist <- distance
            df_ridings_2021$circonscription_ces[i] <- ces65_ridings[j]
        }
    }
}

df_ridings_2021$distance <- NA

for (i in seq_along(df_ridings_2021$circonscription_ces)) {
   df_ridings_2021$distance[i] <- stringdist(df_ridings_2021$circonscription[i], 
                               df_ridings_2021$circonscription_ces[i], method = "jw")
}

# Vérfier à partir de où les résultats ne sont plus satisfaisants
df_ridings_2021 <- df_ridings_2021 %>%
    filter(distance < 0.2678) 

# Ajustements manuels
df_ridings_2021 <- df_ridings_2021 %>%
  filter(!(circonscription %in% c("Labelle", "Laurier", "Papineau", "Saint-Henri", "Quebec East")))
  
clean_ces65 <- c(NA)

geolocs <- df_ridings_2021$geoloc
names(geolocs) <- df_ridings_2021$circonscription_ces
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

table(raw_ces74, useNA = "always")

df_ridings_1974 <- df_ridings %>%
    filter(year == 1966)

ridings_id_1974 <- as.vector(na.omit(unique(df_ridings_1974$id_ces74_79_80)))

clean_ces74 <- c(NA)

for (i in 1:length(ridings_id_1974)) {
    for (j in 1:length(raw_ces74)) {
        if (is.na(raw_ces74[j])) {
            clean_ces74[j] <- NA
            next
        }
        found_match <- FALSE
        for (k in 1:nrow(df_ridings_1974)) {
            if (!is.na(df_ridings_1974$id_ces74_79_80[k]) && df_ridings_1974$id_ces74_79_80[k] == raw_ces74[j]) {
                clean_ces74[j] <- df_ridings_1974$geoloc[k]
                found_match <- TRUE
                break
            }
        }
        if (!found_match) {
            clean_ces74[j] <- NA
        }
    }
}

names(clean_ces74) <- sondr::generate_survey_ids(n_respondents = length(clean_ces74), ## number of respondents
                                                 source_id = "ces74") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_ces74) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "ces74"))

## ces79 -------------------------------------------------------------------

# Load variable
raw_ces79 <- sondr::load_variable(
    file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1979/ces79.csv", 
    variable_name = "v5")

table(raw_ces79, useNA = "always")


df_ridings_1979 <- df_ridings %>%
    filter(year == 1966)

ridings_id_1979 <- as.vector(na.omit(unique(df_ridings_1979$id_ces74_79_80)))

clean_ces79 <- c(NA)

for (i in 1:length(ridings_id_1979)) {
    for (j in 1:length(raw_ces79)) {
        if (is.na(raw_ces79[j])) {
            clean_ces79[j] <- NA
            next
        }
        found_match <- FALSE
        for (k in 1:nrow(df_ridings_1979)) {
            if (!is.na(df_ridings_1979$id_ces74_79_80[k]) && df_ridings_1979$id_ces74_79_80[k] == raw_ces79[j]) {
                clean_ces79[j] <- df_ridings_1979$geoloc[k]
                found_match <- TRUE
                break
            }
        }
        if (!found_match) {
            clean_ces79[j] <- NA
        }
    }
}

names(clean_ces79) <- sondr::generate_survey_ids(n_respondents = length(clean_ces79), ## number of respondents
                                                 source_id = "ces79") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_ces79) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "ces79"))


## ces84 -------------------------------------------------------------------

# Load variable
raw_ces84 <- sondr::load_variable(
    file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1984/ces84.csv", 
    variable_name = "var006")

table(raw_ces84, useNA = "always")

df_ridings_1984 <- df_ridings %>%
    filter(year == 1976)

ridings_id_1984 <- as.vector(na.omit(unique(df_ridings_1984$id_ces_84)))

clean_ces84 <- c(NA)

for (i in 1:length(ridings_id_1984)) {
    for (j in 1:length(raw_ces84)) {
        if (is.na(raw_ces84[j])) {
            clean_ces84[j] <- NA
            next
        }
        found_match <- FALSE
        for (k in 1:nrow(df_ridings_1984)) {
            if (!is.na(df_ridings_1984$id_ces_84[k]) && df_ridings_1984$id_ces_84[k] == raw_ces84[j]) {
                clean_ces84[j] <- df_ridings_1984$geoloc[k]
                found_match <- TRUE
                break
            }
        }
        if (!found_match) {
            clean_ces84[j] <- NA
        }
    }
}

names(clean_ces84) <- sondr::generate_survey_ids(n_respondents = length(clean_ces84), ## number of respondents
                                                 source_id = "ces84") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_ces84) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "ces84"))

## ces88 -------------------------------------------------------------------

raw_ces88 <- sondr::load_variable(
    file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1988/ces88.csv", 
    variable_name = "intnum")

table(raw_ces88, useNA = "always")

df_ridings_1988 <- df_ridings %>%
    filter(year == 1987)

ridings_id_1988 <- as.vector(na.omit(unique(df_ridings_1988$id_ces_88)))

clean_ces88 <- c(NA)

for (i in 1:length(ridings_id_1988)) {
    for (j in 1:length(raw_ces88)) {
        if (is.na(raw_ces88[j])) {
            clean_ces88[j] <- NA
            next
        }
        found_match <- FALSE
        for (k in 1:nrow(df_ridings_1988)) {
            if (!is.na(df_ridings_1988$id_ces_88[k]) && df_ridings_1988$id_ces_88[k] == raw_ces88[j]) {
                clean_ces88[j] <- df_ridings_1988$geoloc[k]
                found_match <- TRUE
                break
            }
        }
        if (!found_match) {
            clean_ces88[j] <- NA
        }
    }
}

names(clean_ces88) <- sondr::generate_survey_ids(n_respondents = length(clean_ces88), ## number of respondents
                                                 source_id = "ces88") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_ces88) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "ces88"))

## ces93 -------------------------------------------------------------------#

# NA

#### 1. Get raw gender variable vector
raw_ces93 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1993/ces93.csv",
                                  variable_name = "cpspost")
table(raw_ces93, useNA = "always")

geolocs <- df_postal_codes$geoloc.1
names(geolocs) <- tolower(df_postal_codes$rta)
clean_ces93 <- geolocs[raw_ces93]

table(clean_ces93)

names(clean_ces93) <- sondr::generate_survey_ids(n_respondents = length(clean_ces93), ## number of respondents
                                                 source_id = "ces93") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_ces93) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "ces93"))

## ces97 -------------------------------------------------------------------

# NA

## ces2000 -------------------------------------------------------------------

# NA

## ces2004 -------------------------------------------------------------------

raw_ces2004 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2004/ces2004.csv",
                                  variable_name = "ces04_cps_fsa")
table(raw_ces2004, useNA = "always")

geolocs <- df_postal_codes$geoloc.1
names(geolocs) <- df_postal_codes$rta
clean_ces2004 <- geolocs[raw_ces2004]
table(clean_ces2004)

names(clean_ces2004) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2004), ## number of respondents
                                                 source_id = "ces2004") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_ces2004) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "ces2004"))

## ces2006 -------------------------------------------------------------------

raw_ces2006 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2006/ces2006.csv",
                                  variable_name = "cps_fsa")
table(raw_ces2006, useNA = "always")

geolocs <- df_postal_codes$geoloc.1
names(geolocs) <- df_postal_codes$rta
clean_ces2006 <- geolocs[raw_ces2006]

table(clean_ces2006)

names(clean_ces2006) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2006), ## number of respondents
                                                 source_id = "ces2006") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_ces2006) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "ces2006"))
## ces2008 -------------------------------------------------------------------

raw_ces2008 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2008/ces2008.csv",
                                  variable_name = "cps_fsa")
table(raw_ces2008, useNA = "always")

geolocs <- df_postal_codes$geoloc.1
names(geolocs) <- df_postal_codes$rta
clean_ces2008 <- geolocs[raw_ces2008]

table(clean_ces2008)

names(clean_ces2008) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2008), ## number of respondents
                                                 source_id = "ces2008") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_ces2008) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "ces2008"))

## ces2011 -------------------------------------------------------------------

# NA

## ces2015 -------------------------------------------------------------------

raw_ces2015 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2015/ces2015.csv",
                                  variable_name = "post3")
table(raw_ces2015, useNA = "always")

geolocs <- df_postal_codes$geoloc.1
names(geolocs) <- df_postal_codes$rta
clean_ces2015 <- geolocs[raw_ces2015]

table(clean_ces2015)

names(clean_ces2015) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2015), ## number of respondents
                                                 source_id = "ces2015") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_ces2015) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "ces2015"))


## ces2019 -------------------------------------------------------------------

df_raw_ces2019 <- sondr::read_any_csv("_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2019/ces2019.csv") %>% 
    filter(cps19_province == "Quebec")

raw_ces2019 <- df_raw_ces2019$constituencyname
table(raw_ces2019, useNA = "always")
ces2019_ridings <- unique(raw_ces2019)
df_ridings_2019 <- df_ridings %>%
    filter(year == 2013)

unique_raw_ces2019 <- unique(raw_ces2019)

match_ids <- amatch(unique_raw_ces2019, df_ridings_2019$circonscription, maxDist = 15)

geolocs <- df_ridings_2019$geoloc[match_ids]
names(geolocs) <- unique_raw_ces2019

geolocs <- geolocs[-which(names(geolocs) == "")]
geolocs <- geolocs[-which(names(geolocs) == "York South—Weston")]
geolocs <- geolocs[-which(names(geolocs) == "Windsor West")]
geolocs <- geolocs[-which(names(geolocs) == "Toronto—St. Paul's")]
geolocs <- geolocs[-which(names(geolocs) == "Thornhill")]
geolocs <- geolocs[-which(names(geolocs) == "Skeena—Bulkley Valley")]
geolocs <- geolocs[-which(names(geolocs) == "Richmond Hill")]
geolocs <- geolocs[-which(names(geolocs) == "Regina—Lewvan")]
geolocs <- geolocs[-which(names(geolocs) == "Ottawa Centre")]
geolocs <- geolocs[-which(names(geolocs) == "London West")]
geolocs <- geolocs[-which(names(geolocs) == "Kingston and the Islands")]
geolocs <- geolocs[-which(names(geolocs) == "Etobicoke Centre")]
geolocs <- geolocs[-which(names(geolocs) == "Courtenay—Alberni")]
geolocs <- geolocs[-which(names(geolocs) == "Brampton North")]
geolocs <- geolocs[-which(names(geolocs) == "Bay of Quinte")]
geolocs <- geolocs[-which(names(geolocs) == "Ajax")]
geolocs <- geolocs[-which(names(geolocs) == "Acadie—Bathurst")]

clean_ces2019 <- geolocs[raw_ces2019]

names(clean_ces2019) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2019), ## number of respondents
                                                 source_id = "ces2019") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_ces2019) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "ces2019"))

## ces2021 -------------------------------------------------------------------

df_raw_ces2021 <- sondr::read_any_csv("_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2021/ces2021.csv")  %>% 
    filter(provcode == "Quebec")

raw_ces2021 <- df_raw_ces2021$fedname
Encoding(raw_ces2021) <- "latin1"
table(raw_ces2021, useNA = "always")
ces2021_ridings <- unique(raw_ces2021)
df_ridings_2021 <- df_ridings %>%
    filter(year == 2013)

unique_raw_ces2021 <- unique(raw_ces2021)

match_ids <- amatch(unique_raw_ces2021, df_ridings_2021$circonscription, maxDist = 15)


geolocs <- df_ridings_2021$geoloc[match_ids]
names(geolocs) <- unique_raw_ces2021
geolocs[names(geolocs) == "Montmagny--L'Islet--Kamouraska--Rivière-du-Loup"] <- "region"
geolocs[names(geolocs) == "Abitibi--Baie-James--Nunavik--Eeyou"] <- "region"
geolocs[names(geolocs) == "Avignon--La Mitis--Matane--Matapédia"] <- "region"
geolocs <- geolocs[-which(names(geolocs) == "")]
clean_ces2021 <- geolocs[raw_ces2021]

names(clean_ces2021) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2021), ## number of respondents
                                                 source_id = "ces2021") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_ces2021) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "ces2021"))


## datagotchi_pilot1_2021 -------------------------------------------------------------------

# NA

## january -------------------------------------------------------------------

raw_january <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/january/january_postalcodes.xlsx",
                                  variable_name = "A5")
raw_january <- tolower(raw_january)
table(raw_january, useNA = "always")

geolocs <- df_postal_codes$geoloc.1
names(geolocs) <- tolower(df_postal_codes$rta)
clean_january <- geolocs[raw_january]

table(clean_january)

names(clean_january) <- sondr::generate_survey_ids(n_respondents = length(clean_january), ## number of respondents
                                                 source_id = "january") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_january) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "january"))

## february -------------------------------------------------------------------

raw_february <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/february/february_postalcodes.xlsx",
                                  variable_name = "A5")
raw_february <- tolower(raw_february)
table(raw_february, useNA = "always")

geolocs <- df_postal_codes$geoloc.1
names(geolocs) <- tolower(df_postal_codes$rta)
clean_february <- geolocs[raw_february]

table(clean_february)

names(clean_february) <- sondr::generate_survey_ids(n_respondents = length(clean_february), ## number of respondents
                                                 source_id = "february") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_february) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "february"))

## may -------------------------------------------------------------------

raw_march <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/march/march_postalcodes.xlsx",
                                  variable_name = "A5")
raw_march <- tolower(raw_march)
table(raw_march, useNA = "always")

geolocs <- df_postal_codes$geoloc.1
names(geolocs) <- tolower(df_postal_codes$rta)
clean_march <- geolocs[raw_march]

table(clean_march)

names(clean_march) <- sondr::generate_survey_ids(n_respondents = length(clean_march), ## number of respondents
                                                 source_id = "march") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_march) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "march"))

## april -------------------------------------------------------------------

raw_april <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/april/april_postalcodes.xlsx",
                                  variable_name = "A5")
raw_april <- tolower(raw_april)
table(raw_april, useNA = "always")

geolocs <- df_postal_codes$geoloc.1
names(geolocs) <- tolower(df_postal_codes$rta)
clean_april <- geolocs[raw_april]

table(clean_april)

names(clean_april) <- sondr::generate_survey_ids(n_respondents = length(clean_april), ## number of respondents
                                                 source_id = "april") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_april) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "april"))

## may -------------------------------------------------------------------

raw_may <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/may/may_postalcodes.xlsx",
                                  variable_name = "A5")
raw_may <- tolower(raw_may)
table(raw_may, useNA = "always")

geolocs <- df_postal_codes$geoloc.1
names(geolocs) <- tolower(df_postal_codes$rta)
clean_may <- geolocs[raw_may]

table(clean_may)

names(clean_may) <- sondr::generate_survey_ids(n_respondents = length(clean_may), ## number of respondents
                                                 source_id = "may") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_may) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "may"))

## june -------------------------------------------------------------------

raw_june <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/june/june_postalcodes.xlsx",
                                  variable_name = "A5")
raw_june <- tolower(raw_june)
table(raw_june, useNA = "always")

geolocs <- df_postal_codes$geoloc.1
names(geolocs) <- tolower(df_postal_codes$rta)
clean_june <- geolocs[raw_june]

table(clean_june)

names(clean_june) <- sondr::generate_survey_ids(n_respondents = length(clean_june), ## number of respondents
                                                 source_id = "june") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_june) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "june"))

## datagotchi_pilot2_2022 -------------------------------------------------

raw_datagotchi_pilot2_2022 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                  variable_name = "postal_code")
raw_datagotchi_pilot2_2022 <- tolower(raw_datagotchi_pilot2_2022)
table(raw_datagotchi_pilot2_2022, useNA = "always")

geolocs <- df_postal_codes$geoloc.1
names(geolocs) <- tolower(df_postal_codes$rta)
clean_datagotchi_pilot2_2022 <- geolocs[raw_datagotchi_pilot2_2022]

table(clean_datagotchi_pilot2_2022)

names(clean_datagotchi_pilot2_2022) <- sondr::generate_survey_ids(n_respondents = length(clean_datagotchi_pilot2_2022), ## number of respondents
                                                 source_id = "datagotchi_pilot2_2022") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_datagotchi_pilot2_2022) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "datagotchi_pilot2_2022"))

## sondage_nationalisme_2022 ----------------------------------------------

raw_sondage_nationalisme_2022 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/sondage_nationalisme_2022/sondage_nationalisme_2022.csv",
                                  variable_name = "postal_code")
raw_sondage_nationalisme_2022 <- tolower(raw_sondage_nationalisme_2022)
table(raw_sondage_nationalisme_2022, useNA = "always")

geolocs <- df_postal_codes$geoloc.1
names(geolocs) <- tolower(df_postal_codes$rta)
clean_sondage_nationalisme_2022 <- geolocs[raw_sondage_nationalisme_2022]

table(clean_sondage_nationalisme_2022)

names(clean_sondage_nationalisme_2022) <- sondr::generate_survey_ids(n_respondents = length(clean_sondage_nationalisme_2022), ## number of respondents
                                                 source_id = "sondage_nationalisme_2022") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_sondage_nationalisme_2022) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "sondage_nationalisme_2022"))

## quorum_mcq_pilote ------------------------------------------------------

raw_quorum_mcq_pilote_en <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                               variable_name = "EN_ses_postal_code_1_TEXT")
table(raw_quorum_mcq_pilote_en, useNA = "always")

raw_quorum_mcq_pilote_fr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                               variable_name = "ses_postal_code_1_TEXT")
table(raw_quorum_mcq_pilote_fr, useNA = "always")

raw_quorum_mcq_pilote <- coalesce(raw_quorum_mcq_pilote_fr, raw_quorum_mcq_pilote_en)
raw_quorum_mcq_pilote <- tolower(raw_quorum_mcq_pilote)
table(raw_quorum_mcq_pilote, useNA = "always")

geolocs <- df_postal_codes$geoloc.1
names(geolocs) <- tolower(df_postal_codes$rta)
clean_quorum_mcq_pilote <- geolocs[raw_quorum_mcq_pilote]

table(clean_quorum_mcq_pilote)

names(clean_quorum_mcq_pilote) <- sondr::generate_survey_ids(n_respondents = length(clean_quorum_mcq_pilote), ## number of respondents
                                                 source_id = "quorum_mcq_pilote") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_quorum_mcq_pilote) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "quorum_mcq_pilote"))

## pes_elxn_2022_text -------------------------------------------------------------------

# NA

## pco ------------------------------------------------------------------

raw_pco <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pco/WholeData_Pco14_2015_01_30.csv",
                                  variable_name = "postalCode")
raw_pco <- tolower(raw_pco)
raw_pco <- substr(raw_pco, 1, 3)
table(raw_pco, useNA = "always")



geolocs <- df_postal_codes$geoloc.1
names(geolocs) <- tolower(df_postal_codes$rta)
clean_pco <- geolocs[raw_pco]

table(clean_pco)

names(clean_pco) <- sondr::generate_survey_ids(n_respondents = length(clean_pco), ## number of respondents
                                                 source_id = "WholeData_Pco14_2015_01_30") ## source_id

## 4. add clean to the master output
output_geoloc <- sondr::match_and_update(main = output_geoloc, ## vector to update
                                         updates = clean_pco) ## vector with updates

table(sondr::extract_elements_with_prefix(output_geoloc, "WholeData_Pco14_2015_01_30"))


# Save everything ---------------------------------------------------------

table(output_geoloc)

## factorize
output_geoloc <- factor(output_geoloc)

saveRDS(output_geoloc, "_SharedFolder_catalogue-donnees/merging-souverainete/clean/vectors/ses_geoloc.1.rds")
