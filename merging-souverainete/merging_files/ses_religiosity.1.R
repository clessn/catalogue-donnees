# Packages ----------------------------------------------------------------
library(tidyverse)

# Config ------------------------------------------------------------------
### source config file to generate all the respondent_ids
source("merging-souverainete/config.R")
sample(ids, 20) ### 20 ids random
## empty vector where the clean values will go. same length as the n of ids.
output_religiosity <- rep(NA, length(ids))
### each element in output is a respondent with a unique respondent id
names(output_religiosity) <- ids


# Merging and cleaning ----------------------------------------------------

## ces65 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw_ces65 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1965/ces65.csv",
                                  variable_name = "v310")
table(raw_ces65, useNA = "always")

#### 2. clean variable
clean_ces65 <- NA
clean_ces65[raw_ces65 == "never"] <- 0 # pas religieux
clean_ces65[raw_ces65 == "a few times a year or less" | raw_ces65 == "once a month"] <- 0.33 # un peu religieux/pas très religieux
clean_ces65[raw_ces65 == "2 or 3 times monthly"] <- 0.66 # Assez religieux
clean_ces65[raw_ces65 == "at least weekly"] <- 1 # très religieux
table(clean_ces65)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces65) <- sondr::generate_survey_ids(n_respondents = length(clean_ces65), ## number of respondents
                                                 source_id = "ces65") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                         updates = clean_ces65) ## vector with updates

table(sondr::extract_elements_with_prefix(output_religiosity, "ces65"))

## ces68 -------------------------------------------------------------------
#### 1. Get raw gender variable vector
raw_ces68 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1968/ces68.csv",
                                  variable_name = "var341")
table(raw_ces68, useNA = "always")

#### 2. clean variable
clean_ces68 <- NA
clean_ces68[raw_ces68 == 5] <- 0 # pas religieux
clean_ces68[raw_ces68 == 4 | raw_ces68 == 3] <- 0.33 # un peu religieux/pas très religieux
clean_ces68[raw_ces68 == 2] <- 0.66 # Assez religieux
clean_ces68[raw_ces68 == 1] <- 1 # très religieux
table(clean_ces68)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces68) <- sondr::generate_survey_ids(n_respondents = length(clean_ces68), ## number of respondents
                                                 source_id = "ces68") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_ces68) ## vector with updates

## ces74 -------------------------------------------------------------------
#### 1. Get raw gender variable vector
raw_ces74 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1974/ces74.csv",
                                  variable_name = "v454")
table(raw_ces74, useNA = "always")

#### 2. clean variable
clean_ces74 <- NA
clean_ces74[raw_ces74 == "never"] <- 0 # pas religieux
clean_ces74[raw_ces74 == "few times a year or less" | raw_ces74 == "once a month"] <- 0.33 # un peu religieux/pas très religieux
clean_ces74[raw_ces74 == "two or three times a month"] <- 0.66 # Assez religieux
clean_ces74[raw_ces74 == "once a week"] <- 1 # très religieux
table(clean_ces74)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces74) <- sondr::generate_survey_ids(n_respondents = length(clean_ces74), ## number of respondents
                                                 source_id = "ces74") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_ces74) ## vector with updates
## ces79 -------------------------------------------------------------------
#### 1. Get raw gender variable vector
raw_ces79 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1979/ces79.csv",
                                  variable_name = "v1508")
table(raw_ces79, useNA = "always")

#### 2. clean variable
clean_ces79 <- NA
clean_ces79[raw_ces79 == 3] <- 0.33 # un peu religieux/pas très religieux
clean_ces79[raw_ces79 == 2] <- 0.66 # Assez religieux
clean_ces79[raw_ces79 == 1] <- 1 # très religieux
table(clean_ces79)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces79) <- sondr::generate_survey_ids(n_respondents = length(clean_ces79), ## number of respondents
                                                 source_id = "ces79") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_ces79) ## vector with updates
## ces84 -------------------------------------------------------------------
#### 1. Get raw gender variable vector
raw_ces84 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1984/ces84.csv",
                                  variable_name = "var373")
table(raw_ces84, useNA = "always")

#### 2. clean variable
clean_ces84 <- NA
clean_ces84[raw_ces84 == "not very religious"] <- 0.33 # un peu religieux/pas très religieux
clean_ces84[raw_ces84 == "fairly religious"] <- 0.66 # Assez religieux
clean_ces84[raw_ces84 == "very religious"] <- 1 # très religieux
table(clean_ces84)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces84) <- sondr::generate_survey_ids(n_respondents = length(clean_ces84), ## number of respondents
                                                 source_id = "ces84") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_ces84) ## vector with updates
## ces88 -------------------------------------------------------------------
#### 1. Get raw gender variable vector
raw_ces88 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1988/ces88.csv",
                                  variable_name = "n12")
table(raw_ces88, useNA = "always")

#### 2. clean variable
clean_ces88 <- NA
clean_ces88[raw_ces88 == "never"] <- 0 # pas religieux
clean_ces88[raw_ces88 == "less once a year" | raw_ces88 == "once a month" | raw_ces88 == "once/twice a yr"] <- 0.33 # un peu religieux/pas très religieux
clean_ces88[raw_ces88 == "couple a month" | raw_ces88 == "several a year"] <- 0.66 # Assez religieux
clean_ces88[raw_ces88 == "every week" | raw_ces88 == "more once a week" | raw_ces88 == "nearly every wk"] <- 1 # très religieux
table(clean_ces88)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces88) <- sondr::generate_survey_ids(n_respondents = length(clean_ces88), ## number of respondents
                                                 source_id = "ces88") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_ces88) ## vector with updates
## ces93 -------------------------------------------------------------------
#### 1. Get raw gender variable vector
raw_ces93 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1993/ces93.csv",
                                  variable_name = "cpso10")
table(raw_ces93, useNA = "always")

#### 2. clean variable
clean_ces93 <- NA
clean_ces93[raw_ces93 == "not important"] <- 0 # pas religieux
clean_ces93[raw_ces93 == "not very"] <- 0.33 # un peu religieux/pas très religieux
clean_ces93[raw_ces93 == "somewhat"] <- 0.66 # Assez religieux
clean_ces93[raw_ces93 == "very important"] <- 1 # très religieux
table(clean_ces93)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces93) <- sondr::generate_survey_ids(n_respondents = length(clean_ces93), ## number of respondents
                                                 source_id = "ces93") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_ces93) ## vector with updates
## ces97 -------------------------------------------------------------------
#### 1. Get raw gender variable vector
raw_ces97 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1997/ces97.csv",
                                  variable_name = "pesm10b")
table(raw_ces97, useNA = "always")

#### 2. clean variable
clean_ces97 <- NA
clean_ces97[raw_ces97 == "notatall"] <- 0 # pas religieux
clean_ces97[raw_ces97 == "not very"] <- 0.33 # un peu religieux/pas très religieux
clean_ces97[raw_ces97 == "somewhat"] <- 0.66 # Assez religieux
clean_ces97[raw_ces97 == "very important"] <- 1 # très religieux
table(clean_ces68)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces97) <- sondr::generate_survey_ids(n_respondents = length(clean_ces97), ## number of respondents
                                                 source_id = "ces97") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_ces97) ## vector with updates
## ces2000 -------------------------------------------------------------------
#### 1. Get raw gender variable vector
raw_ces2000 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2000/ces2000.csv",
                                  variable_name = "cpsm10b")
table(raw_ces2000, useNA = "always")

#### 2. clean variable
clean_ces2000 <- NA
clean_ces2000[raw_ces2000 == "not important at all"] <- 0 # pas religieux
clean_ces2000[raw_ces2000 == "not very important"] <- 0.33 # un peu religieux/pas très religieux
clean_ces2000[raw_ces2000 == "somewhat important"] <- 0.66 # Assez religieux
clean_ces2000[raw_ces2000 == "very important"] <- 1 # très religieux
table(clean_ces2000)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces2000) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2000), ## number of respondents
                                                 source_id = "ces2000") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_ces2000) ## vector with updates
## ces2004 -------------------------------------------------------------------
#### 1. Get raw gender variable vector
raw_ces2004 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2004/ces2004.csv",
                                  variable_name = "cps_s11")
table(raw_ces2004, useNA = "always")

#### 2. clean variable
clean_ces68 <- NA
clean_ces68[raw_ces68 == 5] <- 0 # pas religieux
clean_ces68[raw_ces68 == 4 | raw_ces68 == 3] <- 0.33 # un peu religieux/pas très religieux
clean_ces68[raw_ces68 == 2] <- 0.66 # Assez religieux
clean_ces68[raw_ces68 == 1] <- 1 # très religieux
table(clean_ces68)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces68) <- sondr::generate_survey_ids(n_respondents = length(clean_ces68), ## number of respondents
                                                 source_id = "ces68") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_ces68) ## vector with updates
## ces2006 -------------------------------------------------------------------
#### 1. Get raw gender variable vector
raw_ces2006 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2006/ces2006.csv",
                                  variable_name = "cps_s11")
table(raw_ces2006, useNA = "always")

#### 2. clean variable
clean_ces2006 <- NA
clean_ces2006[raw_ces2006 == "not important at all"] <- 0 # pas religieux
clean_ces2006[raw_ces2006 == "not very important"] <- 0.33 # un peu religieux/pas très religieux
clean_ces2006[raw_ces2006 == "somewhat important"] <- 0.66 # Assez religieux
clean_ces2006[raw_ces2006 == "very important"] <- 1 # très religieux
table(clean_ces2006)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces2006) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2006), ## number of respondents
                                                 source_id = "ces2006") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_ces2006) ## vector with updates
## ces2008 -------------------------------------------------------------------
#### 1. Get raw gender variable vector
raw_ces2008 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2008/ces2008.csv",
                                  variable_name = "cps_s11")
table(raw_ces2008, useNA = "always")

#### 2. clean variable
clean_ces2008 <- NA
clean_ces2008[raw_ces2008 == "not important at all"] <- 0 # pas religieux
clean_ces2008[raw_ces2008 == "not very important"] <- 0.33 # un peu religieux/pas très religieux
clean_ces2008[raw_ces2008 == "somewhat important"] <- 0.66 # Assez religieux
clean_ces2008[raw_ces2008 == "very important"] <- 1 # très religieux
table(clean_ces2008)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces2008) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2008), ## number of respondents
                                                 source_id = "ces2008") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_ces2008) ## vector with updates
## ces2011 -------------------------------------------------------------------
raw_ces2011 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2011/ces2011.csv",
                                    variable_name = "CPS11_82")
table(raw_ces2011, useNA = "always")

#### 2. clean variable
clean_ces2011 <- NA
clean_ces2011[raw_ces2011 == "not important at all"] <- 0 # pas religieux
clean_ces2011[raw_ces2011 == "not very important  "] <- 0.33 # un peu religieux/pas très religieux
clean_ces2011[raw_ces2011 == "somewhat important  "] <- 0.66 # Assez religieux
clean_ces2011[raw_ces2011 == "very important      "] <- 1 # très religieux
table(clean_ces2011)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces2011) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2011), ## number of respondents
                                                   source_id = "ces2011") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_ces2011) ## vector with updates
## ces2015 -------------------------------------------------------------------
raw_ces2015 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2015/ces2015.csv",
                                    variable_name = "relig_imp")
table(raw_ces2015, useNA = "always")

#### 2. clean variable
clean_ces2015 <- NA
clean_ces2015[raw_ces2015 == 7] <- 0 # pas religieux
clean_ces2015[raw_ces2015 == 5] <- 0.33 # un peu religieux/pas très religieux
clean_ces2015[raw_ces2015 == 3] <- 0.66 # Assez religieux
clean_ces2015[raw_ces2015 == 1] <- 1 # très religieux
table(clean_ces2015)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces2015) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2015), ## number of respondents
                                                   source_id = "ces2015") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_ces2015) ## vector with updates
## ces2019 -------------------------------------------------------------------
raw_ces2019 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2019/ces2019.csv",
                                    variable_name = "cps19_rel_imp")
table(raw_ces2019, useNA = "always")

#### 2. clean variable
clean_ces2019 <- NA
clean_ces2019[raw_ces2019 == "Not important at all"] <- 0 # pas religieux
clean_ces2019[raw_ces2019 == "Not very important"] <- 0.33 # un peu religieux/pas très religieux
clean_ces2019[raw_ces2019 == "Somewhat important"] <- 0.66 # Assez religieux
clean_ces2019[raw_ces2019 == "Very important"] <- 1 # très religieux
table(clean_ces2019)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces2019) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2019), ## number of respondents
                                                   source_id = "ces2019") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_ces2019) ## vector with updates
## ces2021 -------------------------------------------------------------------
raw_ces2021 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2021/ces2021.csv",
                                    variable_name = "cps21_rel_imp")
table(raw_ces2021, useNA = "always")

#### 2. clean variable
clean_ces2021 <- NA
clean_ces2021[raw_ces2021 == "Not important at all"] <- 0 # pas religieux
clean_ces2021[raw_ces2021 == "Not very important"] <- 0.33 # un peu religieux/pas très religieux
clean_ces2021[raw_ces2021 == "Somewhat important"] <- 0.66 # Assez religieux
clean_ces2021[raw_ces2021 == "Very important"] <- 1 # très religieux
table(clean_ces2021)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces2021) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2021), ## number of respondents
                                                   source_id = "ces2021") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_ces2021) ## vector with updates
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
raw_datagotchi_pilot2_2022_fr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                               variable_name = "FR_importantOfRelig")
table(raw_datagotchi_pilot2_2022_fr, useNA = "always")

raw_en <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                               variable_name = "income_en")
table(raw_en, useNA = "always")

raw <- coalesce(raw_fr, raw_en)

#### 2. clean variable


## sondage_nationalisme_2022 -------------------------------------------------------------------
raw_sondage_nationalisme_2022 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/sondage_nationalisme_2022/sondage_nationalisme_2022.csv",
                                    variable_name = "religious_service")
table(raw_sondage_nationalisme_2022, useNA = "always")

#### 2. clean variable
clean_sondage_nationalisme_2022 <- NA
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022 == "Jamais ou pratiquement jamais" | raw_sondage_nationalisme_2022 == "Never or practically never"] <- 0 # pas religieux
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022 == "Seulement lors de certains jours saints" | raw_sondage_nationalisme_2022 == "Only at specific holy days"] <- 0.33 # un peu religieux/pas très religieux
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022 == "Une fois par mois" | raw_sondage_nationalisme_2022 == "Once a month"] <- 0.66 # Assez religieux
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022 == "Une fois par semaine" | raw_sondage_nationalisme_2022 == "Once a week" | raw_sondage_nationalisme_2022 == "Plus d'une fois par semaine" | raw_sondage_nationalisme_2022 == "More than once a week"] <- 1 # très religieux
table(clean_sondage_nationalisme_2022)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_sondage_nationalisme_2022) <- sondr::generate_survey_ids(n_respondents = length(clean_sondage_nationalisme_2022), ## number of respondents
                                                   source_id = "sondage_nationalisme_2022") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_sondage_nationalisme_2022) ## vector with updates
## quorum_mcq_pilote -------------------------------------------------------------------

### ICI IL FAUT MERGER FRANCAIS ET ANGLAIS

#### 1. Get raw gender variable vector
raw_quorum_mcq_pilote_fr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                               variable_name = "iss_religiosity")
table(raw_quorum_mcq_pilote_fr, useNA = "always")

raw_quorum_mcq_pilote_en <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                               variable_name = "EN_iss_religiosity")
table(raw_quorum_mcq_pilote_en, useNA = "always")

raw_quorum_mcq_pilote <- coalesce(raw_quorum_mcq_pilote_fr, raw_quorum_mcq_pilote_en)

table(raw_quorum_mcq_pilote, useNA = "always")

#### 2. clean variable

## pes_elxn_2022_text -------------------------------------------------------------------
raw_pes_elxn_2022_text <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pes_elxn_2022/pes_elxn_2022_text.csv",
                                    variable_name = "ses_religiosity")
table(raw_pes_elxn_2022_text, useNA = "always")

#### 2. clean variable
clean_pes_elxn_2022_text <- NA
clean_pes_elxn_2022_text[raw_pes_elxn_2022_text == "Très faible"] <- 0 # pas religieux
clean_pes_elxn_2022_text[raw_pes_elxn_2022_text == "Plutôt faible" | raw_pes_elxn_2022_text == "Modéré"] <- 0.33 # un peu religieux/pas très religieux
clean_pes_elxn_2022_text[raw_pes_elxn_2022_text == "Plutôt fort"] <- 0.66 # Assez religieux
clean_pes_elxn_2022_text[raw_pes_elxn_2022_text == "Très fort"] <- 1 # très religieux
table(clean_pes_elxn_2022_text)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_pes_elxn_2022_text) <- sondr::generate_survey_ids(n_respondents = length(clean_pes_elxn_2022_text), ## number of respondents
                                                   source_id = "pes_elxn_2022_text") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_pes_elxn_2022_text) ## vector with updates
## pco -------------------------------------------------------------------
raw_pco <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pco/pco.csv",
                                    variable_name = "Q18.2.Pco2014")
table(raw_pco, useNA = "always")

#### 2. clean variable
clean_pco <- NA
clean_pco[raw_pco == "Not important at all"] <- 0 # pas religieux
clean_pco[raw_pco == "Not very important"] <- 0.33 # un peu religieux/pas très religieux
clean_pco[raw_pco == "Somewhat important"] <- 0.66 # Assez religieux
clean_pco[raw_pco == "Very important"] <- 1 # très religieux
table(clean_pco)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_pco) <- sondr::generate_survey_ids(n_respondents = length(clean_pco), ## number of respondents
                                                   source_id = "pco") ## source_id

## 4. add clean to the master output
output_religiosity <- sondr::match_and_update(main = output_religiosity, ## vector to update
                                              updates = clean_pco) ## vector with updates

# Output ------------------------------------------------------------------

### FACTORISE, LEVELS, etc.

##### SAVE VECTOR WHERE??

