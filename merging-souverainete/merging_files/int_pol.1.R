# Packages ----------------------------------------------------------------
library(tidyverse)

# Config ------------------------------------------------------------------
source("merging-souverainete/config.R")
sample(ids, 20) ### 20 ids random

## empty vector where the clean values will go. same length as the n of ids.
output_intpol <- rep(NA, length(ids))
### each element in output is a respondent with a unique respondent id
names(output_intpol) <- ids


# Merging and cleaning ----------------------------------------------------

## ces65 ------------------------------------------------------------------
#### 1. Get raw age variable vector

raw_ces65 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1965/ces65.csv",
                                  variable_name = "v36")
table(raw_ces65, useNA = "always")

#### 2. clean variable
clean_ces65 <- NA
clean_ces65[raw_ces65=="a good deal"] <- 1
clean_ces65[raw_ces65=="some"] <- 0.5
clean_ces65[raw_ces65=="not much"] <- 0
table(clean_ces65, useNA = "always")


#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces65) <- sondr::generate_survey_ids(n_respondents = length(clean_ces65), ## number of respondents
                                                 source_id = "ces65") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                       updates = clean_ces65) ## vector with updates


## ces68 -------------------------------------------------------------------


## ces74 -------------------------------------------------------------------

raw_ces74 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1974/ces74.csv",
                                  variable_name = "v12")
table(raw_ces74, useNA = "always")

#### 2. clean variable
clean_ces74 <- NA
clean_ces74[raw_ces74=="very closely"] <- 1
clean_ces74[raw_ces74=="fairly closely"] <- 0.5
clean_ces74[raw_ces74=="not much at all"] <- 0

table(clean_ces74, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces74
names(clean_ces74) <- sondr::generate_survey_ids(n_respondents = length(clean_ces74), ## number of respondents
                                                 source_id = "ces74") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces74) ## vector with updates


## ces79 -------------------------------------------------------------------

raw_ces79 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1979/ces79.csv",
                                  variable_name = "v1022")
table(raw_ces79, useNA = "always")

#### 2. clean variable
clean_ces79 <- NA
clean_ces79[raw_ces79=="1"] <- 1
clean_ces79[raw_ces79=="2"] <- 0.5
clean_ces79[raw_ces79=="3"] <- 0

table(clean_ces79, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces79
names(clean_ces79) <- sondr::generate_survey_ids(n_respondents = length(clean_ces79), ## number of respondents
                                                 source_id = "ces79") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces79) ## vector with updates

## ces84 -------------------------------------------------------------------

raw_ces84 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1984/ces84.csv",
                                  variable_name = "var017")
table(raw_ces84, useNA = "always")

#### 2. clean variable
clean_ces84 <- NA
clean_ces84[raw_ces84=="very closely"] <- 1
clean_ces84[raw_ces84=="fairly closely"] <- 0.5
clean_ces84[raw_ces84=="not much at all"] <- 0

table(clean_ces84, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces84
names(clean_ces84) <- sondr::generate_survey_ids(n_respondents = length(clean_ces84), ## number of respondents
                                                 source_id = "ces84") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces84) ## vector with updates


## ces88 -------------------------------------------------------------------

raw_ces88 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1988/ces88.csv",
                                  variable_name = "a7")
table(raw_ces88, useNA = "always")

#### 2. clean variable
clean_ces88 <- NA
clean_ces88[raw_ces88=="very closely"] <- 1
clean_ces88[raw_ces88=="fairly closely" | raw_ces88=="not very closely"] <- 0.5
clean_ces88[raw_ces88=="not at all"] <- 0

table(clean_ces88, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces88
names(clean_ces88) <- sondr::generate_survey_ids(n_respondents = length(clean_ces88), ## number of respondents
                                                 source_id = "ces88") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces88) ## vector with updates


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

## sondage_nationalisme_2022 -------------------------------------------------------------------

## quorum_mcq_pilote -------------------------------------------------------------------

## pes_elxn_2022_text -------------------------------------------------------------------

## pco -------------------------------------------------------------------


# Output ------------------------------------------------------------------

### FACTORISE, LEVELS, etc.

##### SAVE VECTOR WHERE??

