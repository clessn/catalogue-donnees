# Packages ----------------------------------------------------------------
library(tidyverse)

# Config ------------------------------------------------------------------
### source config file to generate all the respondent_ids
source("merging-souverainete/config.R")
sample(ids, 20) ### 20 ids random
## empty vector where the clean values will go. same length as the n of ids.
output_idcanqc <- rep(NA, length(ids))
### each element in output is a respondent with a unique respondent id
names(output_idcanqc) <- ids


# Merging and cleaning ----------------------------------------------------

## ces65 -------------------------------------------------------------------


## ces68 -------------------------------------------------------------------

## ces74 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw_ces74 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1974/ces74.csv",
                                  variable_name = "v77")
table(raw_ces74, useNA = "always")

quantiles <- c(0, 1/3, 2/3, 1)
quantile_values <- quantile(raw_ces74, quantiles, na.rm = TRUE)
print(quantile_values)

########## En me référant à ces2021, je choisi le 4e quartile pour lui attribuer la valeur clean de 1.

#### 2. clean variable
clean_ces74 <- NA
clean_ces74[raw_ces74 == "99"] <- 1
clean_ces74[raw_ces74 < "99"] <- 0
table(clean_ces74, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces74
names(clean_ces74) <- sondr::generate_survey_ids(n_respondents = length(clean_ces74), ## number of respondents
                                                 source_id = "ces74") ## source_id

## 4. add clean to the master output
output_idcanqc <- sondr::match_and_update(main = output_idcanqc, ## vector to update
                                          updates = clean_ces74) ## vector with updates

## ces79 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw_ces79 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1979/ces79.csv",
                                  variable_name = "v66")
table(raw_ces79, useNA = "always")


########## En me référant à ces2021, je choisi le 4e quartile pour lui attribuer la valeur clean de 1.

#### 2. clean variable
clean_ces79 <- NA
clean_ces79[raw_ces79 == "99"] <- 1
clean_ces79[raw_ces79 < "99"] <- 0
table(clean_ces79, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces79
names(clean_ces79) <- sondr::generate_survey_ids(n_respondents = length(clean_ces79), ## number of respondents
                                                 source_id = "ces79") ## source_id

## 4. add clean to the master output
output_idcanqc <- sondr::match_and_update(main = output_idcanqc, ## vector to update
                                          updates = clean_ces79) ## vector with updates

## ces84 -------------------------------------------------------------------

## ces88 -------------------------------------------------------------------

## ces93 -------------------------------------------------------------------

## ces97 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw_ces97 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1997/ces97.csv",
                                  variable_name = "pesf11a")
table(raw_ces97, useNA = "always")

#### 2. clean variable
clean_ces97 <- NA
clean_ces97[raw_ces97 <= "98"] <- 0
clean_ces97[raw_ces97 == "100" | raw_ces97 == "99"] <- 1
table(clean_ces97, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces97
names(clean_ces97) <- sondr::generate_survey_ids(n_respondents = length(clean_ces97), ## number of respondents
                                                 source_id = "ces97") ## source_id

## 4. add clean to the master output
output_idcanqc <- sondr::match_and_update(main = output_idcanqc, ## vector to update
                                          updates = clean_ces97) ## vector with updates

## ces2000 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw_ces2000 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2000/ces2000.csv",
                                  variable_name = "pesc3")
table(raw_ces2000, useNA = "always")



#### 2. clean variable
clean_ces2000 <- NA
clean_ces2000[raw_ces2000 <= "98"] <- 0
clean_ces2000[raw_ces2000 == "100" | raw_ces2000 == "99"] <- 1
table(clean_ces2000, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2000
names(clean_ces2000) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2000), ## number of respondents
                                                 source_id = "ces2000") ## source_id

## 4. add clean to the master output
output_idcanqc <- sondr::match_and_update(main = output_idcanqc, ## vector to update
                                          updates = clean_ces2000) ## vector with updates

## ces2004 -------------------------------------------------------------------

## ces2006 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw_ces2006 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2006/ces2006.csv",
                                    variable_name = "pes_c7")
table(raw_ces2006, useNA = "always")



#### 2. clean variable
clean_ces2006 <- NA
clean_ces2006[raw_ces2006 <= "98"] <- 0
clean_ces2006[raw_ces2006 == "100" | raw_ces2006 == "99"] <- 1
table(clean_ces2006, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2006
names(clean_ces2006) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2006), ## number of respondents
                                                   source_id = "ces2006") ## source_id

## 4. add clean to the master output
output_idcanqc <- sondr::match_and_update(main = output_idcanqc, ## vector to update
                                          updates = clean_ces2006) ## vector with updates

## ces2008 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw_ces2008 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2008/ces2008.csv",
                                    variable_name = "pes_c7")
table(raw_ces2008, useNA = "always")



#### 2. clean variable
clean_ces2008 <- NA
clean_ces2008[raw_ces2008 <= "98"] <- 0
clean_ces2008[raw_ces2008 == "100" | raw_ces2008 == "99"] <- 1
table(clean_ces2008, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2008
names(clean_ces2008) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2008), ## number of respondents
                                                   source_id = "ces2008") ## source_id

## 4. add clean to the master output
output_idcanqc <- sondr::match_and_update(main = output_idcanqc, ## vector to update
                                          updates = clean_ces2008) ## vector with updates

## ces2011 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw_ces2011 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2011/ces2011.csv",
                                    variable_name = "PES11_11")
table(raw_ces2011, useNA = "always")



#### 2. clean variable
clean_ces2011 <- NA
clean_ces2011[raw_ces2011 <= "98"] <- 0
clean_ces2011[raw_ces2011 == "100" | raw_ces2011 == "99"] <- 1
table(clean_ces2011, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2011
names(clean_ces2011) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2011), ## number of respondents
                                                   source_id = "ces2011") ## source_id

## 4. add clean to the master output
output_idcanqc <- sondr::match_and_update(main = output_idcanqc, ## vector to update
                                          updates = clean_ces2011) ## vector with updates

## ces2015 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw_ces2015 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2015/ces2015.csv",
                                    variable_name = "p_pos_can")
table(raw_ces2015, useNA = "always")



#### 2. clean variable
clean_ces2015 <- NA
clean_ces2015[raw_ces2015 <= "98"] <- 0
clean_ces2015[raw_ces2015 == "100" | raw_ces2015 == "99"] <- 1
table(clean_ces2015, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2015
names(clean_ces2015) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2015), ## number of respondents
                                                   source_id = "ces2015") ## source_id

## 4. add clean to the master output
output_idcanqc <- sondr::match_and_update(main = output_idcanqc, ## vector to update
                                          updates = clean_ces2015) ## vector with updates

## ces2019 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw_ces2019 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2019/ces2019.csv",
                                    variable_name = "pes19_canid3")
table(raw_ces2019, useNA = "always")
####### J'ai calculé les quartiles en divisant la distribution en 4 quartiles et 1 représente le dernier quartile.
#### 2. clean variable
clean_ces2019 <- NA
clean_ces2019[raw_ces2019 == "Strongly agree"] <- 1
clean_ces2019[raw_ces2019 == "Neither agree nor disagree" | raw_ces2019 == "Somewhat disagree" | raw_ces2019 == "Somewhat agree" | raw_ces2019 == "Strongly disagree"] <- 0
table(clean_ces2019, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2019
names(clean_ces2019) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2019), ## number of respondents
                                                   source_id = "ces2019") ## source_id

## 4. add clean to the master output
output_idcanqc <- sondr::match_and_update(main = output_idcanqc, ## vector to update
                                          updates = clean_ces2019) ## vector with updates

## ces2021 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw_ces2021 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2021/ces2021.csv",
                                  variable_name = "pes21_ethid_1")
table(raw_ces2021, useNA = "always")
####### J'ai calculé les quartiles en divisant la distribution en 4 quartiles et 1 représente le dernier quartile.
#### 2. clean variable
clean_ces2021 <- NA
clean_ces2021[raw_ces2021 == "Very important"] <- 1
clean_ces2021[raw_ces2021 == "Not important at all" | raw_ces2021 == "Fairly important" | raw_ces2021 == "Not very important"] <- 0
table(clean_ces2021, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2021
names(clean_ces2021) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2021), ## number of respondents
                                                 source_id = "ces2021") ## source_id

## 4. add clean to the master output
output_idcanqc <- sondr::match_and_update(main = output_idcanqc, ## vector to update
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
raw_fr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                               variable_name = "FR_canBeforeQc")
table(raw_fr, useNA = "always")

raw_en <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                               variable_name = "EN_canBeforeQc")
table(raw_en, useNA = "always")

raw_datgot22 <- coalesce(raw_fr, raw_en)
table(raw_datgot22, useNA = "always")
#### 2. clean variable
clean_datgot22 <- NA
clean_datgot22[raw_datgot22 == "1"] <- 1
clean_datgot22[raw_datgot22 != "1"] <- 0
table(clean_datgot22, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = datgot22
names(clean_datgot22) <- sondr::generate_survey_ids(n_respondents = length(clean_datgot22), ## number of respondents
                                                  source_id = "datgot22") ## source_id

## 4. add clean to the master output
output_idcanqc <- sondr::match_and_update(main = output_idcanqc, ## vector to update
                                          updates = clean_datgot22) ## vector with updates



## sondage_nationalisme_2022 --------------------------------------------------

#### 1. Get raw gender variable vector
raw_sondnat22 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/sondage_nationalisme_2022/sondage_nationalisme_2022.csv",
                                variable_name = "moreno")
table(raw_sondnat22, useNA = "always")
####### J'ai calculé les quartiles en divisant la distribution en 4 quartiles et 1 représente le dernier quartile.
#### 2. clean variable
clean_sondnat22 <- NA
clean_sondnat22[raw_sondnat22 == "1"| raw_sondnat22 == "2"] <- 1
clean_sondnat22[raw_sondnat22 != "1" & raw_sondnat22 != "2"] <- 0
table(clean_sondnat22, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = sondnat22
names(clean_sondnat22) <- sondr::generate_survey_ids(n_respondents = length(clean_sondnat22), ## number of respondents
                                               source_id = "sondnat22") ## source_id

## 4. add clean to the master output
output_idcanqc <- sondr::match_and_update(main = output_idcanqc, ## vector to update
                                          updates = clean_sondnat22) ## vector with updates

## quorum_mcq_pilote -------------------------------------------------------------------

### ICI IL FAUT MERGER FRANCAIS ET ANGLAIS

#### 1. Get raw gender variable vector
raw_fr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                               variable_name = "iss_can")
table(raw_fr, useNA = "always")

raw_en <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                               variable_name = "EN_iss_can")
table(raw_en, useNA = "always")

raw_quorum <- c(raw_fr, raw_en)
table(raw_quorum, useNA = "always")

#### 2. clean variable
clean_quorum <- NA
clean_quorum[raw_quorum == "Fortement d'accord" | raw_quorum == "Strongly agree"] <- 1
clean_quorum[raw_quorum != "Fortement d'accord" & raw_quorum != "Strongly agree" & raw_quorum != ""] <- 0
table(clean_quorum, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = quorum
names(clean_quorum) <- sondr::generate_survey_ids(n_respondents = length(clean_quorum), ## number of respondents
                                                    source_id = "quorum") ## source_id

## 4. add clean to the master output
output_idcanqc <- sondr::match_and_update(main = output_idcanqc, ## vector to update
                                          updates = clean_quorum) ## vector with updates


## pes_elxn_2022_text --------------------------------------------------------

#### 1. Get raw gender variable vector
raw_pes22 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pes_elxn_2022/pes_elxn_2022_text.csv",
                                      variable_name = "iss_moreno")
table(raw_pes22, useNA = "always")
####### J'ai calculé les quartiles en divisant la distribution en 4 quartiles et 1 représente le dernier quartile.
#### 2. clean variable
clean_pes22 <- NA
clean_pes22[raw_pes22 == "Uniquement comme Canadien(ne)"| raw_pes22 == "D’abord comme Canadien(ne), puis comme Québécois(e)"] <- 1
clean_pes22[raw_pes22 != "Uniquement comme Canadien(ne)" & raw_pes22 != "D’abord comme Canadien(ne), puis comme Québécois(e)" &
              raw_pes22 != "Autre" &  raw_pes22 != "Plutôt en accord" &  raw_pes22 != ""] <- 0
table(clean_pes22, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = pes22
names(clean_pes22) <- sondr::generate_survey_ids(n_respondents = length(clean_pes22), ## number of respondents
                                                     source_id = "pes22") ## source_id

## 4. add clean to the master output
output_idcanqc <- sondr::match_and_update(main = output_idcanqc, ## vector to update
                                          updates = clean_pes22) ## vector with updates


## pco -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw_pco <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pco/pco.csv",
                                    variable_name = "Q14.2_6.Pco2014")
table(raw_pco, useNA = "always")
####### J'ai calculé les quartiles en divisant la distribution en 4 quartiles et 1 représente le dernier quartile.
#### 2. clean variable
clean_pco <- NA
clean_pco[raw_pco == "Very strong"] <- 1
clean_pco[raw_pco != "Very strong" & raw_pco != ""] <- 0
table(clean_pco, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = pco
names(clean_pco) <- sondr::generate_survey_ids(n_respondents = length(clean_pco), ## number of respondents
                                                   source_id = "pco") ## source_id

## 4. add clean to the master output
output_idcanqc <- sondr::match_and_update(main = output_idcanqc, ## vector to update
                                          updates = clean_pco) ## vector with updates


# Output ------------------------------------------------------------------

### FACTORISE, LEVELS, etc.

##### SAVE VECTOR WHERE??
