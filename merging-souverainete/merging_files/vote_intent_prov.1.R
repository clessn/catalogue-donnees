# Packages ----------------------------------------------------------------
library(tidyverse)

# Config ------------------------------------------------------------------
### source config file to generate all the respondent_ids
source("merging-souverainete/config.R")
sample(ids, 20) ### 20 ids random
## empty vector where the clean values will go. same length as the n of ids.
output_vote_intent_prov <- rep(NA, length(ids))
### each element in output is a respondent with a unique respondent id
names(output_vote_intent_prov) <- ids


# Merging and cleaning ----------------------------------------------------

## ces65 -------------------------------------------------------------------

# NA

## ces68 -------------------------------------------------------------------

raw_ces68 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1968/ces68.csv",
                                  variable_name = "var122")
table(raw_ces68, useNA = "always")

#### 2. clean variable
clean_ces68 <- NA
clean_ces68[raw_ces68 == "female"] <- "female"
clean_ces68[raw_ces68 == "male"] <- "male"
table(clean_ces68)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces68) <- sondr::generate_survey_ids(n_respondents = length(clean_ces68), ## number of respondents
                                                 source_id = "ces68") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces68) ## vector with updates

## ces74 -------------------------------------------------------------------

raw_ces74 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1974/ces74.csv",
                                  variable_name = "v228")
table(raw_ces74, useNA = "always")

#### 2. clean variable
clean_ces74 <- NA
clean_ces74[raw_ces74 == "female"] <- "female"
clean_ces74[raw_ces74 == "male"] <- "male"
table(clean_ces74)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces74) <- sondr::generate_survey_ids(n_respondents = length(clean_ces74), ## number of respondents
                                                 source_id = "ces74") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces74) ## vector with updates

## ces79 -------------------------------------------------------------------

raw_ces79 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1979/ces79.csv",
                                  variable_name = "v228")
table(raw_ces79, useNA = "always")

#### 2. clean variable
clean_ces79 <- NA
clean_ces79[raw_ces79 == "female"] <- "female"
clean_ces79[raw_ces79 == "male"] <- "male"
table(clean_ces79)

##### source_id = ces65
names(clean_ces79) <- sondr::generate_survey_ids(n_respondents = length(clean_ces79), ## number of respondents
                                                 source_id = "ces79") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces79) ## vector with updates

## ces84 -------------------------------------------------------------------

raw_ces84 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1984/ces84.csv",
                                  variable_name = "v265")
table(raw_ces84, useNA = "always")

#### 2. clean variable
clean_ces84 <- NA
clean_ces84[raw_ces84 == "female"] <- "female"
clean_ces84[raw_ces84 == "male"] <- "male"
table(clean_ces84)

##### source_id = ces65
names(clean_ces84) <- sondr::generate_survey_ids(n_respondents = length(clean_ces84), ## number of respondents
                                                 source_id = "ces84") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces84) ## vector with updates

## ces88 -------------------------------------------------------------------

raw_ces88 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1988/ces88.csv",
                                  variable_name = "b9")
table(raw_ces88, useNA = "always")

#### 2. clean variable
clean_ces88 <- NA
clean_ces88[raw_ces88 == "female"] <- "female"
clean_ces88[raw_ces88 == "male"] <- "male"
table(clean_ces88)

##### source_id = ces65
names(clean_ces88) <- sondr::generate_survey_ids(n_respondents = length(clean_ces88), ## number of respondents
                                                 source_id = "ces88") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces88) ## vector with updates

## ces93 -------------------------------------------------------------------

raw_ces93 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1993/ces93.csv",
                                  variable_name = "cpsm13")
table(raw_ces93, useNA = "always")

#### 2. clean variable
clean_ces93 <- NA
clean_ces93[raw_ces93 == "female"] <- "female"
clean_ces93[raw_ces93 == "male"] <- "male"
table(clean_ces93)

##### source_id = ces65
names(clean_ces93) <- sondr::generate_survey_ids(n_respondents = length(clean_ces93), ## number of respondents
                                                 source_id = "ces93") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces93) ## vector with updates

## ces97 -------------------------------------------------------------------

raw_ces97 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1997/ces97.csv",
                                  variable_name = "k13")
table(raw_ces97, useNA = "always")

#### 2. clean variable
clean_ces97 <- NA
clean_ces97[raw_ces97 == "female"] <- "female"
clean_ces97[raw_ces97 == "male"] <- "male"
table(clean_ces97)

##### source_id = ces65
names(clean_ces97) <- sondr::generate_survey_ids(n_respondents = length(clean_ces97), ## number of respondents
                                                 source_id = "ces97") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces97) ## vector with updates

## ces2000 -------------------------------------------------------------------

raw_ces00 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2000/ces00.csv",
                                  variable_name = "n5")
table(raw_ces00, useNA = "always")

#### 2. clean variable
clean_ces00 <- NA
clean_ces00[raw_ces00 == "female"] <- "female"
clean_ces00[raw_ces00 == "male"] <- "male"
table(clean_ces00)

##### source_id = ces65
names(clean_ces00) <- sondr::generate_survey_ids(n_respondents = length(clean_ces00), ## number of respondents
                                                 source_id = "ces00") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces00) ## vector with updates

## ces2004 -------------------------------------------------------------------


raw_ces04 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2004/ces04.csv",
                                  variable_name = "sd5")
table(raw_ces04, useNA = "always")

#### 2. clean variable
clean_ces04 <- NA
clean_ces04[raw_ces04 == "female"] <- "female"
clean_ces04[raw_ces04 == "male"] <- "male"
table(clean_ces04)

##### source_id = ces65
names(clean_ces04) <- sondr::generate_survey_ids(n_respondents = length(clean_ces04), ## number of respondents
                                                 source_id = "ces04") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces00) ## vector with updates


## ces2006 -------------------------------------------------------------------

raw_ces06 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2006/ces06.csv",
                                  variable_name = "pes_sd5")
table(raw_ces06, useNA = "always")

#### 2. clean variable
clean_ces06 <- NA
clean_ces06[raw_ces06 == "female"] <- "female"
clean_ces06[raw_ces06 == "male"] <- "male"
table(clean_ces06)

##### source_id = ces65
names(clean_ces06) <- sondr::generate_survey_ids(n_respondents = length(clean_ces06), ## number of respondents
                                                 source_id = "ces06") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces06) ## vector with updates

## ces2008 -------------------------------------------------------------------

raw_ces08 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2008/ces08.csv",
                                  variable_name = "prov_vote1")
table(raw_ces08, useNA = "always")

#### 2. clean variable
clean_ces08 <- NA
clean_ces08[raw_ces08 == "female"] <- "female"
clean_ces08[raw_ces08 == "male"] <- "male"
table(clean_ces08)

##### source_id = ces65
names(clean_ces08) <- sondr::generate_survey_ids(n_respondents = length(clean_ces08), ## number of respondents
                                                 source_id = "ces08") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces08) ## vector with updates

## ces2011 -------------------------------------------------------------------

raw_ces11 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2011/ces11.csv",
                                  variable_name = "PES11_68")
table(raw_ces11, useNA = "always")

#### 2. clean variable
clean_ces11 <- NA
clean_ces11[raw_ces11 == "female"] <- "female"
clean_ces11[raw_ces11 == "male"] <- "male"
table(clean_ces11)

##### source_id = ces65
names(clean_ces11) <- sondr::generate_survey_ids(n_respondents = length(clean_ces11), ## number of respondents
                                                 source_id = "ces11") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces11) ## vector with updates

## ces2015 -------------------------------------------------------------------

raw_ces15 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2015/ces15.csv",
                                  variable_name = "p_qcvot")
table(raw_ces15, useNA = "always")

#### 2. clean variable
clean_ces15 <- NA
clean_ces15[raw_ces15 == "female"] <- "female"
clean_ces15[raw_ces15 == "male"] <- "male"
table(clean_ces15)

##### source_id = ces65
names(clean_ces15) <- sondr::generate_survey_ids(n_respondents = length(clean_ces15), ## number of respondents
                                                 source_id = "ces15") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces15) ## vector with updates

## ces2019 -------------------------------------------------------------------

raw_ces19 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2019/ces19.csv",
                                  variable_name = "pes19_provvote")
table(raw_ces19, useNA = "always")

#### 2. clean variable
clean_ces19 <- NA
clean_ces19[raw_ces19 == "female"] <- "female"
clean_ces19[raw_ces19 == "male"] <- "male"
table(clean_ces19)

##### source_id = ces65
names(clean_ces19) <- sondr::generate_survey_ids(n_respondents = length(clean_ces19), ## number of respondents
                                                 source_id = "ces19") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces19) ## vector with updates

## ces2021 -------------------------------------------------------------------

raw_ces21 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2021/ces21.csv",
                                  variable_name = "pes21_provvote")
table(raw_ces21, useNA = "always")

#### 2. clean variable
clean_ces21 <- NA
clean_ces21[raw_ces21 == "female"] <- "female"
clean_ces21[raw_ces21 == "male"] <- "male"
table(clean_ces21)

##### source_id = ces65
names(clean_ces21) <- sondr::generate_survey_ids(n_respondents = length(clean_ces21), ## number of respondents
                                                 source_id = "ces21") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces21) ## vector with updates

## datagotchi_pilot1_2021 -------------------------------------------------------------------

# NA

## datagotchi_pilot2_2022 -------------------------------------------------------------------

raw_datagotchi_pilot2_2022 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                  variable_name = "QID41")
table(raw_datagotchi_pilot2_2022, useNA = "always")

#### 2. clean variable
clean_datagotchi_pilot2_2022 <- NA
clean_datagotchi_pilot2_2022[raw_datagotchi_pilot2_2022 == "female"] <- "female"
clean_datagotchi_pilot2_2022[raw_datagotchi_pilot2_2022 == "male"] <- "male"
table(clean_datagotchi_pilot2_2022)

##### source_id = ces65
names(clean_datagotchi_pilot2_2022) <- sondr::generate_survey_ids(n_respondents = length(clean_datagotchi_pilot2_2022), ## number of respondents
                                                 source_id = "datagotchi_pilot2_2022") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_datagotchi_pilot2_2022) ## vector with updates


## january -------------------------------------------------------------------

## february -------------------------------------------------------------------

## march -------------------------------------------------------------------

## april -------------------------------------------------------------------

## may -------------------------------------------------------------------

## june -------------------------------------------------------------------

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

