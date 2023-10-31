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


# Merging and cleaning ----------------------------------------------------

## ces65 -------------------------------------------------------------------

### EXAMPLE WITH GENDER
#### 1. Get raw gender variable vector
raw_ces65 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1965/ces65.csv",
                                  variable_name = "v337")
table(raw_ces65, useNA = "always")

#### 2. clean variable
clean_ces65 <- NA
clean_ces65[raw_ces65 == "female"] <- "female"
clean_ces65[raw_ces65 == "male"] <- "male"
table(clean_ces65)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces65) <- sondr::generate_survey_ids(n_respondents = length(clean_ces65), ## number of respondents
                                                 source_id = "ces65") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_ces65) ## vector with updates
### END EXAMPLE WITH GENDER


## ces68 -------------------------------------------------------------------

## ces74 -------------------------------------------------------------------

## ces79 -------------------------------------------------------------------

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

