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
clean_ces68[raw_ces68 == "liberal"] <- "PLQ"
clean_ces68[raw_ces68 != "liberal"] <- "other"
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
                                  variable_name = "v274")
table(raw_ces74, useNA = "always")

#### 2. clean variable
clean_ces74 <- NA
clean_ces74[raw_ces74 == "liberal"] <- "PLQ"
clean_ces74[raw_ces74 == "parti quebecois"] <- "PQ"
clean_ces74[raw_ces74 != "liberal" & raw_ces74 != "parti quebecois"] <- "other"
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
                                  variable_name = "v244")
table(raw_ces79, useNA = "always")

#### 2. clean variable
clean_ces79 <- NA
clean_ces79[raw_ces79 == 1] <- "PLQ"
clean_ces79[raw_ces79 == 7] <- "PQ"
clean_ces79[raw_ces79 != 1 & raw_ces79 != 7] <- "other"
table(clean_ces79)

##### source_id = ces65
names(clean_ces79) <- sondr::generate_survey_ids(n_respondents = length(clean_ces79), ## number of respondents
                                                 source_id = "ces79") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces79) ## vector with updates

## ces84 -------------------------------------------------------------------

raw_ces84 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1984/ces84.csv",
                                  variable_name = "var265")
table(raw_ces84, useNA = "always")

#### 2. clean variable
clean_ces84 <- NA
clean_ces84[raw_ces84 == "liberal"] <- "PLQ"
clean_ces84[raw_ces84 == "parti quebecois"] <- "PQ"
clean_ces84[raw_ces84 != "liberal" & raw_ces84 != "parti quebecois"] <- "other"
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
clean_ces88[raw_ces88 == "liberal"] <- "PLQ"
clean_ces88[raw_ces88 == "parti quebecois"] <- "PQ"
clean_ces88[raw_ces88 != "liberal" & raw_ces88 != "parti quebecois"] <- "other"
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
clean_ces93[raw_ces93 == "liberal"] <- "PLQ"
clean_ces93[raw_ces93 == "parti quebecois"] <- "PQ"
clean_ces93[raw_ces93 != "liberal" & raw_ces93 != "parti quebecois"] <- "other"
table(clean_ces93)

##### source_id = ces65
names(clean_ces93) <- sondr::generate_survey_ids(n_respondents = length(clean_ces93), ## number of respondents
                                                 source_id = "ces93") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces93) ## vector with updates

## ces97 -------------------------------------------------------------------

raw_ces97 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1997/ces97.csv",
                                  variable_name = "cpsk13")
table(raw_ces97, useNA = "always")

#### 2. clean variable
clean_ces97 <- NA
clean_ces97[raw_ces97 == "l'action democrat"] <- "ADQ"
clean_ces97[raw_ces97 == "liberal"] <- "PLQ"
clean_ces97[raw_ces97 == "parti quebecois"] <- "PQ"
clean_ces97[raw_ces97 != "l'action democrat" & raw_ces97 != "liberal" & raw_ces97 != "parti quebecois"] <- "other"
table(clean_ces97)

##### source_id = ces65
names(clean_ces97) <- sondr::generate_survey_ids(n_respondents = length(clean_ces97), ## number of respondents
                                                 source_id = "ces97") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces97) ## vector with updates

## ces2000 -------------------------------------------------------------------

raw_ces2000 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2000/ces2000.csv",
                                  variable_name = "pesn5")
table(raw_ces2000, useNA = "always")

#### 2. clean variable
clean_ces2000 <- NA
clean_ces2000[raw_ces2000 == "female"] <- "female"
clean_ces2000[raw_ces2000 == "male"] <- "male"
table(clean_ces2000)

##### source_id = ces65
names(clean_ces2000) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2000), ## number of respondents
                                                 source_id = "ces2000") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces2000) ## vector with updates

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

raw_january <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/january/january.Sav",
                                  variable_name = "Q2")
table(raw_january, useNA = "always")

#### 2. clean variable
clean_january <- NA
clean_january[raw_january == "female"] <- "female"
clean_january[raw_january == "male"] <- "male"
table(clean_january)

##### source_id = ces65
names(clean_january) <- sondr::generate_survey_ids(n_respondents = length(clean_january), ## number of respondents
                                                 source_id = "january") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_january) ## vector with updates

## february -------------------------------------------------------------------

raw_february <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/february/february.Sav",
                                  variable_name = "Q2")
table(raw_february, useNA = "always")

#### 2. clean variable
clean_february <- NA
clean_february[raw_february == "female"] <- "female"
clean_february[raw_february == "male"] <- "male"
table(clean_february)

##### source_id = ces65
names(clean_february) <- sondr::generate_survey_ids(n_respondents = length(clean_february), ## number of respondents
                                                 source_id = "february") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_february) ## vector with updates

## march -------------------------------------------------------------------

raw_march <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/march/march.Sav",
                                  variable_name = "Q2")
table(raw_march, useNA = "always")

#### 2. clean variable
clean_march <- NA
clean_march[raw_march == "female"] <- "female"
clean_march[raw_march == "male"] <- "male"
table(clean_march)

##### source_id = ces65
names(clean_march) <- sondr::generate_survey_ids(n_respondents = length(clean_march), ## number of respondents
                                                 source_id = "march") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_march) ## vector with updates

## april -------------------------------------------------------------------

raw_april <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/april/april.Sav",
                                  variable_name = "Q2")
table(raw_april, useNA = "always")

#### 2. clean variable
clean_april <- NA
clean_april[raw_april == "female"] <- "female"
clean_april[raw_april == "male"] <- "male"
table(clean_april)

##### source_id = ces65
names(clean_april) <- sondr::generate_survey_ids(n_respondents = length(clean_april), ## number of respondents
                                                 source_id = "april") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_april) ## vector with updates

## may -------------------------------------------------------------------

raw_may <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/may/may.Sav",
                                  variable_name = "Q2")
table(raw_may, useNA = "always")

#### 2. clean variable
clean_may <- NA
clean_may[raw_may == "female"] <- "female"
clean_may[raw_may == "male"] <- "male"
table(clean_may)

##### source_id = ces65
names(clean_may) <- sondr::generate_survey_ids(n_respondents = length(clean_may), ## number of respondents
                                                 source_id = "may") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_may) ## vector with updates

## june -------------------------------------------------------------------

raw_june <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/june/june.Sav",
                                  variable_name = "Q2")
table(raw_june, useNA = "always")

#### 2. clean variable
clean_june <- NA
clean_june[raw_june == "female"] <- "female"
clean_june[raw_june == "male"] <- "male"
table(clean_june)

##### source_id = ces65
names(clean_june) <- sondr::generate_survey_ids(n_respondents = length(clean_june), ## number of respondents
                                                 source_id = "june") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_june) ## vector with updates

## sondage_nationalisme_2022 -------------------------------------------------------------------

raw_sondage_nationalisme_2022 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/sondage_nationalisme_2022/sondage_nationalisme_2022.csv",
                                  variable_name = "vote_choice_qc")
table(raw_sondage_nationalisme_2022, useNA = "always")

#### 2. clean variable
clean_sondage_nationalisme_2022 <- NA
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022 == "female"] <- "female"
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022 == "male"] <- "male"
table(clean_sondage_nationalisme_2022)

##### source_id = ces65
names(clean_sondage_nationalisme_2022) <- sondr::generate_survey_ids(n_respondents = length(clean_sondage_nationalisme_2022), ## number of respondents
                                                 source_id = "sondage_nationalisme_2022") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_sondage_nationalisme_2022) ## vector with updates

## quorum_mcq_pilote -------------------------------------------------------------------

# NA

## pes_elxn_2022_text -------------------------------------------------------------------

raw_pes_elxn_2022 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pes_elxn_2022/pes_elxn_2022_num.csv",
                                  variable_name = "vote_2022")
table(raw_pes_elxn_2022, useNA = "always")

#### 2. clean variable
clean_pes_elxn_2022 <- NA
clean_pes_elxn_2022[raw_pes_elxn_2022 == "female"] <- "female"
clean_pes_elxn_2022[raw_pes_elxn_2022 == "male"] <- "male"
table(clean_pes_elxn_2022)

##### source_id = ces65
names(clean_pes_elxn_2022) <- sondr::generate_survey_ids(n_respondents = length(clean_pes_elxn_2022), ## number of respondents
                                                 source_id = "pes_elxn_2022") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_pes_elxn_2022) ## vector with updates

## pco -------------------------------------------------------------------

raw_pco <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pco/pco.csv",
                                  variable_name = "Q5.12_TEXT.Pco2014")
table(raw_pco, useNA = "always")

#### 2. clean variable
clean_pco <- NA
clean_pco[raw_pco == "female"] <- "female"
clean_pco[raw_pco == "male"] <- "male"
table(clean_pco)

##### source_id = ces65
names(clean_pco) <- sondr::generate_survey_ids(n_respondents = length(clean_pco), ## number of respondents
                                                 source_id = "pco") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_pco) ## vector with updates

# Output ------------------------------------------------------------------

### FACTORISE, LEVELS, etc.

##### SAVE VECTOR WHERE??

