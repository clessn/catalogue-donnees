# Packages ----------------------------------------------------------------
library(tidyverse)

# Config ------------------------------------------------------------------
### source config file to generate all the respondent_ids
source("merging-souverainete/config.R")
sample(ids, 20) ### 20 ids random
## empty vector where the clean values will go. same length as the n of ids.
output <- rep(NA, length(ids))
### each element in output is a respondent with a unique respondent id
names(output) <- ids

parties <- c("pq",
            "plq",
            "un", ## union nationale
            "social_credit", # social creditiste
            "qs",
            "caq",
            "pcq",
            "pvq",
            "on", # option nationale
            "none")

# Merging and cleaning ----------------------------------------------------

## ces65 -------------------------------------------------------------------

### no question

## ces68 -------------------------------------------------------------------

### no question

## ces74 -------------------------------------------------------------------

source_id <- "ces74"

#### 1. Get raw variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1974/ces74.csv",
                            variable_name = "v274")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == "parti quebecois"] <- "pq"
clean[raw == "liberal"] <- "plq"
clean[raw == "social credit"] <- "social_credit"
clean[raw == "union nationale"] <- "un"
clean[raw == "none, independent"] <- "none"
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                                 source_id = "ces74") ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                         updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, "ces74"))

## ces79 -------------------------------------------------------------------

## no question

## ces84 -------------------------------------------------------------------

source_id <- "ces84"

#### 1. Get raw variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1984/ces84.csv",
                            variable_name = "var253")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == "parti quebecois"] <- "pq"
clean[raw == "liberal"] <- "plq"
clean[raw == "social credit"] <- "social_credit"
clean[raw == "union nationale"] <- "un"
clean[raw == "independent/none"] <- "none"
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = "ces84") ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, "ces84"))

## ces88 -------------------------------------------------------------------

# none

## ces93 -------------------------------------------------------------------

source_id <- "ces93"

#### 1. Get raw variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1993/ces93.csv",
                            variable_name = "cpsm7")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == "parti quebecois"] <- "pq"
clean[raw == "liberal"] <- "plq"
clean[raw == "social  credit"] <- "social_credit"
clean[raw %in% c("none of these", "other", "d.k.")] <- "none"
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = "ces93") ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, "ces93"))


## ces97 -------------------------------------------------------------------

# none

## ces2000 -------------------------------------------------------------------

# none

## ces2004 -------------------------------------------------------------------

# none

## ces2006 -------------------------------------------------------------------

# none

## ces2008 -------------------------------------------------------------------

# none

## ces2011 -------------------------------------------------------------------

# none

## ces2015 -------------------------------------------------------------------

# none

## ces2019 -------------------------------------------------------------------

source_id <- "ces2019"

#### 1. Get raw variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2019/ces2019.csv",
                            variable_name = "cps19_prov_id")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == "Parti Qu<e9>b<e9>cois"] <- "pq"
clean[raw == "Liberal"] <- "plq"
clean[raw == "Qu<e9>bec Solidaire"] <- "qs"
clean[raw == "Coalition Avenir Qu<e9>bec"] <- "caq"
clean[raw == "Conservative"] <- "pcq"
clean[raw %in% c("Another party (please specify)",
                 "Don't know/ Prefer not to answer",
                 "None of these")] <- "none"
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = "ces2019") ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, "ces2019"))


## ces2021 -------------------------------------------------------------------

source_id <- "ces2021"

#### 1. Get raw variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2021/ces2021.csv",
                            variable_name = "cps21_prov_id")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == "Parti Qu\xe9b\xe9cois"] <- "pq"
clean[raw == "Liberal"] <- "plq"
clean[raw == "Qu\xe9bec Solidaire"] <- "qs"
clean[raw == "Coalition Avenir Qu\xe9bec"] <- "caq"
clean[raw %in% c("Another party (please specify)",
                 "Don't know/ Prefer not to answer",
                 "None of these")] <- "none"
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = "ces2021") ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, "ces2021"))

## datagotchi_pilot1_2021 -------------------------------------------------------------------

# none

## january -------------------------------------------------------------------

# none

## february -------------------------------------------------------------------

# none

## march -------------------------------------------------------------------

# none

## april -------------------------------------------------------------------

# none

## may -------------------------------------------------------------------

# none

## june -------------------------------------------------------------------

# none

## datagotchi_pilot2_2022 -------------------------------------------------------------------

### ICI IL FAUT MERGER FRANCAIS ET ANGLAIS

#### 1. Get raw gender variable vector
raw_fr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                  variable_name = "party_type")
table(raw_fr, useNA = "always")

raw_en <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                               variable_name = "voterType")
table(raw_en, useNA = "always")

raw <- coalesce(raw_fr, raw_en)

table(raw)

#### 2. clean variable
clean <- NA
clean[raw == 3] <- "pq"
clean[raw == 2] <- "plq"
clean[raw == 4] <- "qs"
clean[raw == 1] <- "caq"
clean[raw == 5] <- "pcq"
clean[raw == 6] <- "pvq"
clean[raw %in% c(7, 8, 9)] <- "none"
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = "datagotchi_pilot2_2022") ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, "datagotchi_pilot2_2022"))

## sondage_nationalisme_2022 -------------------------------------------------------------------

# none

## quorum_mcq_pilote -------------------------------------------------------------------

# none

## pes_elxn_2022_text -------------------------------------------------------------------

# none

## pco -------------------------------------------------------------------

# none

# Output ------------------------------------------------------------------

### FACTORISE, LEVELS, etc.

output <- factor(output)
table(output)

##### SAVE VECTOR WHERE??

saveRDS(output, "_SharedFolder_catalogue-donnees/merging-souverainete/clean/vectors/party_id_prov.rds")
