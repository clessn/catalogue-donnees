# Packages ----------------------------------------------------------------
library(tidyverse)

# Config ------------------------------------------------------------------
source("merging-souverainete/config.R")

## empty vector where the clean values will go. same length as the n of ids.
output_age <- rep(NA, length(ids))
### each element in output is a respondent with a unique respondent id
names(output_age) <- ids


# Merging and cleaning ----------------------------------------------------

## ces65 -------------------------------------------------------------------
#### 1. Get raw age variable vector

raw_ces65 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1965/ces65.csv",
                                  variable_name = "v335")
table(raw_ces65, useNA = "always")

#### 2. clean variable
clean_ces65 <- raw_ces65

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces65) <- sondr::generate_survey_ids(n_respondents = length(clean_ces65), ## number of respondents
                                                 source_id = "ces65") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                         updates = clean_ces65) ## vector with updates


## ces68 -------------------------------------------------------------------
#### 1. Get raw age variable vector
raw_ces68 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1968/ces68.csv",
                                  variable_name = "age")
table(raw_ces68, useNA = "always")

#### 2. clean variable
clean_ces68 <- raw_ces68

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_ces68) <- sondr::generate_survey_ids(n_respondents = length(clean_ces68), ## number of respondents
                                                 source_id = "ces68") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_ces68) ## vector with updates

## ces74 -------------------------------------------------------------------
#### 1. Get raw age variable vector
raw_ces74 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1974/ces74.csv",
                                  variable_name = "v478")
table(raw_ces74, useNA = "always")

#### 2. clean variable
clean_ces74 <- raw_ces74

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_ces74) <- sondr::generate_survey_ids(n_respondents = length(clean_ces74), ## number of respondents
                                                 source_id = "ces74") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_ces74) ## vector with updates
## ces79 -------------------------------------------------------------------
#### 1. Get raw age variable vector
raw_ces79 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1979/ces79.csv",
                                  variable_name = "v1535")
table(raw_ces79, useNA = "always")

#### 2. clean variable
clean_ces79 <- raw_ces79

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_ces79) <- sondr::generate_survey_ids(n_respondents = length(clean_ces79), ## number of respondents
                                                 source_id = "ces79") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_ces79) ## vector with updates

## ces84 -------------------------------------------------------------------

#### 1. Get raw age variable vector
raw_ces84 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1984/ces84.csv",
                                  variable_name = "var437")
table(raw_ces84, useNA = "always")

#### 2. clean variable
clean_ces84 <- raw_ces84

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_ces84) <- sondr::generate_survey_ids(n_respondents = length(clean_ces84), ## number of respondents
                                                 source_id = "ces84") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_ces84) ## vector with updates
## ces88 -------------------------------------------------------------------

#### 1. Get raw age variable vector
raw_ces88 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1988/ces88.csv",
                                  variable_name = "n1")
table(raw_ces88, useNA = "always")

#### 2. clean variable
clean_ces88 <- NA
clean_ces88_yrbirth <- raw_ces88
table(clean_ces88_yrbirth, useNA = "always")
clean_ces88_yrbirth[raw_ces88==9997 | raw_ces88==9998 | raw_ces88==9999] <- NA
table(clean_ces88_yrbirth, useNA = "always")
clean_ces88 <- 1988-clean_ces88_yrbirth
table(clean_ces88, useNA = "always")
#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_ces88) <- sondr::generate_survey_ids(n_respondents = length(clean_ces88), ## number of respondents
                                                 source_id = "ces88") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_ces88) ## vector with updates

## ces93 -------------------------------------------------------------------

raw_ces93 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1993/ces93.csv",
                                  variable_name = "cpsage")
table(raw_ces93, useNA = "always")

#### 2. clean variable
clean_ces93 <- NA
clean_ces93_yrbirth <- raw_ces93
table(clean_ces93_yrbirth, useNA = "always")
clean_ces93_yrbirth[raw_ces93==9997 | raw_ces93==9998 | raw_ces93==9999] <- NA
table(clean_ces93_yrbirth, useNA = "always")
clean_ces93 <- 1993-clean_ces93_yrbirth
table(clean_ces93, useNA = "always")
#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_ces93) <- sondr::generate_survey_ids(n_respondents = length(clean_ces93), ## number of respondents
                                                 source_id = "ces93") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_ces93) ## vector with updates
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


