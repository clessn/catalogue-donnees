# Packages ----------------------------------------------------------------
library(tidyverse)

# Config ------------------------------------------------------------------
source("merging-souverainete/config.R")
sample(ids, 20) ### 20 ids random
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

raw_ces97 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1997/ces97.csv",
                                  variable_name = "cpsage")
table(raw_ces97, useNA = "always")

#### 2. clean variable
clean_ces97 <- NA
clean_ces97_yrbirth <- raw_ces97
table(clean_ces97_yrbirth, useNA = "always")
clean_ces97_yrbirth[raw_ces97==9997 | raw_ces97==9998 | raw_ces97==9999] <- NA
table(clean_ces97_yrbirth, useNA = "always")
clean_ces97 <- 1993-clean_ces97_yrbirth
table(clean_ces97, useNA = "always")
#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_ces97) <- sondr::generate_survey_ids(n_respondents = length(clean_ces97), ## number of respondents
                                                 source_id = "ces97") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_ces97) ## vector with updates
## ces2000 -------------------------------------------------------------------

raw_ces2000 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2000/ces2000.csv",
                                  variable_name = "cpsage")
table(raw_ces2000, useNA = "always")

#### 2. clean variable
clean_ces2000 <- NA
clean_ces2000_yrbirth <- raw_ces2000
table(clean_ces2000_yrbirth, useNA = "always")
clean_ces2000_yrbirth[raw_ces2000==9997 | raw_ces2000==9998 | raw_ces2000==9999] <- NA
table(clean_ces2000_yrbirth, useNA = "always")
clean_ces2000 <- 2000-clean_ces2000_yrbirth
table(clean_ces2000, useNA = "always")
#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_ces2000) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2000), ## number of respondents
                                                 source_id = "ces2000") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_ces2000) ## vector with updates

## ces2004 -------------------------------------------------------------------

raw_ces2004 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2004/ces2004.csv",
                                  variable_name = "yearofbirth")
table(raw_ces2004, useNA = "always")

#### 2. clean variable
clean_ces2004 <- NA
clean_ces2004_yrbirth <- raw_ces2004
table(clean_ces2004_yrbirth, useNA = "always")
clean_ces2004_yrbirth[raw_ces2004==9997 | raw_ces2004==9998 | raw_ces2004==9999] <- NA
table(clean_ces2004_yrbirth, useNA = "always")
clean_ces2004 <- 2004-clean_ces2004_yrbirth
table(clean_ces2004, useNA = "always")
#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_ces2004) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2004), ## number of respondents
                                                 source_id = "ces2004") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_ces2004) ## vector with updates

## ces2006 -------------------------------------------------------------------

raw_ces2006 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2006/ces2006.csv",
                                    variable_name = "cps_s1")
table(raw_ces2006, useNA = "always")

#### 2. clean variable
clean_ces2006 <- NA
clean_ces2006_yrbirth <- raw_ces2006
table(clean_ces2006_yrbirth, useNA = "always")
clean_ces2006_yrbirth[raw_ces2006==9997 | raw_ces2006==9998 | raw_ces2006==9999] <- NA
table(clean_ces2006_yrbirth, useNA = "always")
clean_ces2006 <- 2006-clean_ces2006_yrbirth
table(clean_ces2006, useNA = "always")
#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_ces2006) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2006), ## number of respondents
                                                   source_id = "ces2006") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_ces2006) ## vector with updates

## ces2008 -------------------------------------------------------------------

#### 1. Get raw age variable vector
raw_ces2008 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2008/ces2008.csv",
                                    variable_name = "cps_s1")
table(raw_ces2008, useNA = "always")

#### 2. clean variable
clean_ces2008 <- NA
clean_ces2008_yrbirth <- raw_ces2008
table(clean_ces2008_yrbirth, useNA = "always")
clean_ces2008_yrbirth[raw_ces2008==9997 | raw_ces2008==9998 | raw_ces2008==9999] <- NA
table(clean_ces2008_yrbirth, useNA = "always")
clean_ces2008 <- 2008-clean_ces2008_yrbirth
table(clean_ces2008, useNA = "always")
#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_ces2008) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2008), ## number of respondents
                                                   source_id = "ces2008") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_ces2008) ## vector with updates

## ces2011 -------------------------------------------------------------------

#### 1. Get raw age variable vector
raw_ces2011 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2011/ces2011.csv",
                                    variable_name = "CPS11_78")
table(raw_ces2011, useNA = "always")

#### 2. clean variable
clean_ces2011 <- NA
clean_ces2011_yrbirth <- raw_ces2011
table(clean_ces2011_yrbirth, useNA = "always")
clean_ces2011_yrbirth[raw_ces2011==9997 | raw_ces2011==9998 | raw_ces2011==9999] <- NA
table(clean_ces2011_yrbirth, useNA = "always")
clean_ces2011 <- 2011-clean_ces2011_yrbirth
table(clean_ces2011, useNA = "always")
#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_ces2011) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2011), ## number of respondents
                                                   source_id = "ces2011") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_ces2011) ## vector with updates

## ces2015 -------------------------------------------------------------------

#### 1. Get raw age variable vector
raw_ces2015 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2015/ces2015.csv",
                                    variable_name = "age")
table(raw_ces2015, useNA = "always")

#### 2. clean variable
clean_ces2015 <- NA
clean_ces2015_yrbirth <- raw_ces2015
table(clean_ces2015_yrbirth, useNA = "always")
clean_ces2015_yrbirth[raw_ces2015==9997 | raw_ces2015==9998 | raw_ces2015==9999] <- NA
table(clean_ces2015_yrbirth, useNA = "always")
clean_ces2015 <- 2015-clean_ces2015_yrbirth
table(clean_ces2015, useNA = "always")
#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_ces2015) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2015), ## number of respondents
                                                   source_id = "ces2015") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_ces2015) ## vector with updates


## ces2019 -------------------------------------------------------------------

#### 1. Get raw age variable vector
raw_ces2019 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2019/ces2019.csv",
                                    variable_name = "cps19_yob")
table(raw_ces2019, useNA = "always")

#### 2. clean variable
clean_ces2019 <- NA
clean_ces2019_yrbirth <- raw_ces2019
table(clean_ces2019_yrbirth, useNA = "always")
clean_ces2019_yrbirth[raw_ces2019==9997 | raw_ces2019==9998 | raw_ces2019==9999] <- NA
table(clean_ces2019_yrbirth, useNA = "always")
clean_ces2019 <- 2019-clean_ces2019_yrbirth
table(clean_ces2019, useNA = "always")
#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_ces2019) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2019), ## number of respondents
                                                   source_id = "ces2019") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_ces2019) ## vector with updates

## ces2021 -------------------------------------------------------------------

#### 1. Get raw age variable vector
raw_ces2021 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2021/ces2021.csv",
                                    variable_name = "cps21_yob_2")
table(raw_ces2021, useNA = "always")

#### 2. clean variable
clean_ces2021 <- NA
clean_ces2021_yrbirth <- raw_ces2021
table(clean_ces2021_yrbirth, useNA = "always")
clean_ces2021_yrbirth[raw_ces2021==9997 | raw_ces2021==9998 | raw_ces2021==9999] <- NA
table(clean_ces2021_yrbirth, useNA = "always")
clean_ces2021 <- 2021-clean_ces2021_yrbirth
table(clean_ces2021, useNA = "always")
#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_ces2021) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2021), ## number of respondents
                                                   source_id = "ces2021") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_ces2021) ## vector with updates

## datagotchi_pilot1_2021 ----------------------------------------------------

#### 1. Get raw age variable vector
raw_datgot_1 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot1_2021/datagotchi_pilot1_2021.Sav",
                                    variable_name = "QAGE")
table(raw_datgot_1, useNA = "always")

#### 2. clean variable

clean_datgot_1 <- raw_datgot_1

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_datgot_1) <- sondr::generate_survey_ids(n_respondents = length(clean_datgot_1), ## number of respondents
                                                   source_id = "datgot_1") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_datgot_1) ## vector with updates


## january -------------------------------------------------------------------

## february -------------------------------------------------------------------

## march -------------------------------------------------------------------

## april -------------------------------------------------------------------

## may -------------------------------------------------------------------

## june -------------------------------------------------------------------

## datagotchi_pilot2_2022 -----------------------------------------------------

#### 1. Get raw age variable vector
raw_datgot_2 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                     variable_name = "age")
table(raw_datgot_2, useNA = "always")

#### 2. clean variable

clean_datgot_2_yrbirth <- raw_datgot_2
clean_datgot_2_yrbirth[!(clean_datgot_2_yrbirth >= 1900 & clean_datgot_2_yrbirth <= 2015)] <- NA
table(clean_datgot_2_yrbirth, useNA = "always")
clean_datgot_2 <- 2021-as.numeric(clean_datgot_2_yrbirth)
table(clean_datgot_2, useNA = "always")
#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_datgot_2) <- sondr::generate_survey_ids(n_respondents = length(clean_datgot_2), ## number of respondents
                                                    source_id = "datgot_2") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_datgot_2) ## vector with updates

## sondage_nationalisme_2022 --------------------------------------------------

#### 1. Get raw age variable vector
raw_sondnat <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/sondage_nationalisme_2022/sondage_nationalisme_2022.csv",
                                     variable_name = "age")
table(raw_sondnat, useNA = "always")

#### 2. clean variable

clean_sondnat <- raw_sondnat

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_sondnat) <- sondr::generate_survey_ids(n_respondents = length(clean_sondnat), ## number of respondents
                                                    source_id = "sondnat") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_sondnat) ## vector with updates

## quorum_mcq_pilote -------------------------------------------------------------------

#### 1. Get raw age variable vector
raw_quorum1 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                                     variable_name = "ses_age")
raw_quorum2 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                                    variable_name = "EN_ses_age")
raw_quorum <- c(raw_quorum1, raw_quorum2)
table(raw_quorum, useNA = "always")

#### 2. clean variable

# Supprimer les valeurs en dehors de la plage [0, 99]
clean_quorum <- ifelse(raw_quorum >= 0 & raw_quorum <= 99, raw_quorum, NA)
table(clean_quorum, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_quorum) <- sondr::generate_survey_ids(n_respondents = length(clean_quorum), ## number of respondents
                                                    source_id = "quorum") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_quorum) ## vector with updates


## pes_elxn_2022_text ---------------------------------------------------------

#### 1. Get raw age variable vector
raw_pes <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pes_elxn_2022/pes_elxn_2022_text.csv",
                                     variable_name = "ses_age")
table(raw_pes, useNA = "always")

#### 2. clean variable

clean_pes <- ifelse(raw_pes >= 0 & raw_pes <= 99, raw_pes, NA)
table(clean_pes, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_pes) <- sondr::generate_survey_ids(n_respondents = length(clean_pes), ## number of respondents
                                                    source_id = "pes") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_pes) ## vector with updates


## pco -------------------------------------------------------------------

#### 1. Get raw age variable vector
raw_pco <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pco/pco.csv",
                                variable_name = "Q20.2.Pco2014")
table(raw_pco, useNA = "always")

#### 2. clean variable

clean_pco_yrbirth <- raw_pco
clean_pco_yrbirth[!(clean_pco_yrbirth >= 1900 & clean_pco_yrbirth <= 2015)] <- NA
table(clean_pco_yrbirth, useNA = "always")
clean_pco <- 2014-as.numeric(clean_pco_yrbirth)
table(clean_pco, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_pco) <- sondr::generate_survey_ids(n_respondents = length(clean_pes), ## number of respondents
                                               source_id = "pes") ## source_id

## 4. add clean to the master output
output_age <- sondr::match_and_update(main = output_age, ## vector to update
                                      updates = clean_pco) ## vector with updates

# Output ------------------------------------------------------------------

### FACTORISE, LEVELS, etc.

##### SAVE VECTOR WHERE??


