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

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "ces68"))

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

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "ces74"))

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

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "ces79"))

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

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "ces84"))

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

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "ces88"))

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

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "ces93"))

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

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "ces97"))

## ces2000 -------------------------------------------------------------------

raw_ces2000 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2000/ces2000.csv",
                                  variable_name = "pesn5")
table(raw_ces2000, useNA = "always")

#### 2. clean variable
clean_ces2000 <- NA
clean_ces2000[raw_ces2000 == 1] <- "PLQ"
clean_ces2000[raw_ces2000 == 7] <- "PQ"
clean_ces2000[raw_ces2000 == 8] <- "ADQ"
clean_ces2000[raw_ces2000 != 1 & raw_ces2000 != 7 & raw_ces2000 != 8] <- "other"
table(clean_ces2000)

##### source_id = ces65
names(clean_ces2000) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2000), ## number of respondents
                                                 source_id = "ces2000") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces2000) ## vector with updates

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "ces2000"))

## ces2004 -------------------------------------------------------------------

raw_ces2004 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2004/ces2004.csv",
                                  variable_name = "ces04_pes_sd5")
table(raw_ces2004, useNA = "always")

#### 2. clean variable
clean_ces2004 <- NA
clean_ces2004[raw_ces2004 == "l'action democratique"] <- "ADQ"
clean_ces2004[raw_ces2004 == "liberal"] <- "PLQ"
clean_ces2004[raw_ces2004 == "parti quebecois"] <- "PQ"
clean_ces2004[raw_ces2004 != "l'action democratique" & raw_ces2004 != "liberal" & raw_ces2004 != "parti quebecois"] <- "other"
table(clean_ces2004)

##### source_id = ces65
names(clean_ces2004) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2004), ## number of respondents
                                                 source_id = "ces2004") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces2004) ## vector with updates

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "ces2004"))

## ces2006 -------------------------------------------------------------------

raw_ces2006 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2006/ces2006.csv",
                                  variable_name = "pes_sd5")
table(raw_ces2006, useNA = "always")

#### 2. clean variable
clean_ces2006 <- NA
clean_ces2006[raw_ces2006 == "l'action democratique"] <- "ADQ"
clean_ces2006[raw_ces2006 == "parti quebecois"] <- "PQ"
clean_ces2006[raw_ces2006 == "liberal"] <- "PLQ"
clean_ces2006[raw_ces2006 != "l'action democratique" & raw_ces2006 != "parti quebecois" & raw_ces2006 != "liberal"] <- "other"
table(clean_ces2006)

##### source_id = ces65
names(clean_ces2006) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2006), ## number of respondents
                                                 source_id = "ces2006") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces2006) ## vector with updates

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "ces2006"))

## ces2008 -------------------------------------------------------------------

raw_ces2008 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2008/ces2008.csv",
                                  variable_name = "pes_prov_vote1")
table(raw_ces2008, useNA = "always")

#### 2. clean variable
clean_ces2008 <- NA
clean_ces2008[raw_ces2008 == "liberal (grits)"] <- "PLQ"
clean_ces2008[raw_ces2008 == "parti quebecois"] <- "PQ"
clean_ces2008[raw_ces2008 == "l'action democratique"] <- "ADQ"
clean_ces2008[raw_ces2008 != "l'action democratique" & raw_ces2008 != "parti quebecois" & raw_ces2008 != "liberal (grits)"] <- "other"
table(clean_ces2008)

##### source_id = ces65
names(clean_ces2008) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2008), ## number of respondents
                                                 source_id = "ces2008") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces2008) ## vector with updates

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "ces2008"))

## ces2011 -------------------------------------------------------------------

raw_ces2011 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2011/ces2011.csv",
                                  variable_name = "PES11_68")
table(raw_ces2011, useNA = "always")

#### 2. clean variable
clean_ces2011 <- NA
clean_ces2011[raw_ces2011 == 4] <- "PLQ"
clean_ces2011[raw_ces2011 == 5] <- "PQ"
clean_ces2011[raw_ces2011 == 6] <- "ADQ"
clean_ces2011[raw_ces2011 != 4 & raw_ces2011 != 5 & raw_ces2011 != 6] <- "other"
table(clean_ces2011)

##### source_id = ces65
names(clean_ces2011) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2011), ## number of respondents
                                                 source_id = "ces2011") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces2011) ## vector with updates

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "ces2011"))

## ces2015 -------------------------------------------------------------------

## we unfortunately can't use this variable.

#ces2015 <- sondr::read_survey("_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2015/ces2015.csv")
#
#
#table(ces2015$province, ces2015$p_provvt)
#
#raw_ces2015 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2015/ces2015.csv",
#                                  variable_name = "p_provvt")
#table(raw_ces2015, useNA = "always")
#
##### 2. clean variable
#clean_ces2015 <- NA
#clean_ces2015[raw_ces2015 == 1] <- "PLQ"
#clean_ces2015[raw_ces2015 == 2] <- "PQ"
#clean_ces2015[raw_ces2015 == 3] <- "CAQ"
#clean_ces2015[raw_ces2015 == 4] <- 'QS'
#clean_ces2015[raw_ces2015 != 1 & raw_ces2015 != 2 & raw_ces2015 != 3 & raw_ces2015 != 4] <- "other"
#table(clean_ces2015)
#
###### source_id = ces65
#names(clean_ces2015) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2015), ## number of respondents
#                                                 source_id = "ces2015") ## source_id
#
### 4. add clean to the master output
#output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
#                                         updates = clean_ces2015) ## vector with updates
#
#table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "ces2015"))

## ces2019 -------------------------------------------------------------------

raw_ces2019 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2019/ces2019.csv",
                                  variable_name = "pes19_provvote")
table(raw_ces2019, useNA = "always")

#### 2. clean variable
clean_ces2019 <- NA
clean_ces2019[raw_ces2019 == "Coalition Avenir Québec"] <- "CAQ"
clean_ces2019[raw_ces2019 == "Liberal"] <- "PLQ"
clean_ces2019[raw_ces2019 == "Parti Québécois"] <- "PQ"
clean_ces2019[raw_ces2019 == "Québec Solidaire"] <- "QS"
clean_ces2019[raw_ces2019 != "Coalition Avenir Québec" &
              raw_ces2019 != "Liberal" &
              raw_ces2019 != "Parti Québécois" &
              raw_ces2019 != "Québec Solidaire"] <- "other"

table(clean_ces2019)

##### source_id = ces65
names(clean_ces2019) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2019), ## number of respondents
                                                 source_id = "ces2019") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces2019) ## vector with updates

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "ces2019"))

## ces2021 -------------------------------------------------------------------

raw_ces2021 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2021/ces2021.csv",
                                  variable_name = "pes21_provvote")
table(raw_ces2021, useNA = "always")

#### 2. clean variable
clean_ces2021 <- NA
clean_ces2021[raw_ces2021 == "Coalition Avenir Qu\xe9bec"] <- "CAQ"
clean_ces2021[raw_ces2021 == "Liberal"] <- "PLQ"
clean_ces2021[raw_ces2021 == "Parti Qu\xe9b\xe9cois"] <- "PQ"
clean_ces2021[raw_ces2021 == "Qu\xe9bec Solidaire"] <- "QS"
clean_ces2021[raw_ces2021 != "Coalition Avenir Qu\xe9bec" &
              raw_ces2021 != "Liberal" &
              raw_ces2021 != "Parti Qu\xe9b\xe9cois" &
              raw_ces2021 != "Qu\xe9bec Solidaire"] <- "other"
table(clean_ces2021)

##### source_id = ces65
names(clean_ces2021) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2021), ## number of respondents
                                                 source_id = "ces2021") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_ces2021) ## vector with updates

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "ces2021"))

## datagotchi_pilot1_2021 -------------------------------------------------------------------

# NA

## datagotchi_pilot2_2022 -------------------------------------------------------------------

raw_fr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                  variable_name = "Vote.choice")
table(raw_fr, useNA = "always")

raw_en <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                               variable_name = "voteChoice")
table(raw_en, useNA = "always")

raw_datagotchi_pilot2_2022 <- coalesce(raw_fr, raw_en)

table(raw_datagotchi_pilot2_2022, useNA = "always")

#### 2. clean variable
clean_datagotchi_pilot2_2022 <- NA
clean_datagotchi_pilot2_2022[raw_datagotchi_pilot2_2022 == 1] <- "CAQ"
clean_datagotchi_pilot2_2022[raw_datagotchi_pilot2_2022 == 2] <- "PLQ"
clean_datagotchi_pilot2_2022[raw_datagotchi_pilot2_2022 == 3] <- "PQ"
clean_datagotchi_pilot2_2022[raw_datagotchi_pilot2_2022 == 4] <- "QS"
clean_datagotchi_pilot2_2022[raw_datagotchi_pilot2_2022 == 5] <- "PCQ"
clean_datagotchi_pilot2_2022[raw_datagotchi_pilot2_2022 != 1 & raw_datagotchi_pilot2_2022 != 2 & raw_datagotchi_pilot2_2022 != 3 & raw_datagotchi_pilot2_2022 != 4 & raw_datagotchi_pilot2_2022 != 5] <- "other"
table(clean_datagotchi_pilot2_2022)

##### source_id = ces65
names(clean_datagotchi_pilot2_2022) <- sondr::generate_survey_ids(n_respondents = length(clean_datagotchi_pilot2_2022), ## number of respondents
                                                 source_id = "datagotchi_pilot2_2022") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_datagotchi_pilot2_2022) ## vector with updates

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "datagotchi_pilot2_2022"))

## january -------------------------------------------------------------------

raw_january <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/january/january.Sav",
                                  variable_name = "Q2")
table(raw_january, useNA = "always")

#### 2. clean variable
clean_january <- NA
clean_january[raw_january == 1] <- "CAQ"
clean_january[raw_january == 2] <- "PQ"
clean_january[raw_january == 3] <- "PLQ"
clean_january[raw_january == 4] <- "QS"
clean_january[raw_january == 8] <- "PCQ"
clean_january[raw_january != 1 & raw_january != 2 & raw_january != 3 & raw_january != 4 & raw_january != 8] <- "other"
table(clean_january)

##### source_id = ces65
names(clean_january) <- sondr::generate_survey_ids(n_respondents = length(clean_january), ## number of respondents
                                                 source_id = "january") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_january) ## vector with updates

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "january"))

## february -------------------------------------------------------------------

raw_february <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/february/february.Sav",
                                  variable_name = "Q2")
table(raw_february, useNA = "always")

#### 2. clean variable
clean_february <- NA
clean_february[raw_february == 1] <- "CAQ"
clean_february[raw_february == 2] <- "PQ"
clean_february[raw_february == 3] <- "PLQ"
clean_february[raw_february == 4] <- "QS"
clean_february[raw_february == 8] <- "PCQ"
clean_february[raw_february != 1 & raw_february != 2 & raw_february != 3 & raw_february != 4 & raw_february != 8] <- "other"
table(clean_february)

##### source_id = ces65
names(clean_february) <- sondr::generate_survey_ids(n_respondents = length(clean_february), ## number of respondents
                                                 source_id = "february") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_february) ## vector with updates

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "february"))

## march -------------------------------------------------------------------

raw_march <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/march/march.Sav",
                                  variable_name = "Q2")
table(raw_march, useNA = "always")

#### 2. clean variable
clean_march <- NA
clean_march[raw_march == 1] <- "CAQ"
clean_march[raw_march == 2] <- "PQ"
clean_march[raw_march == 3] <- "PLQ"
clean_march[raw_march == 4] <- "QS"
clean_march[raw_march == 8] <- "PCQ"
clean_march[raw_march != 1 & raw_march != 2 & raw_march != 3 & raw_march != 4 & raw_march != 8] <- "other"
table(clean_march)

##### source_id = ces65
names(clean_march) <- sondr::generate_survey_ids(n_respondents = length(clean_march), ## number of respondents
                                                 source_id = "march") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_march) ## vector with updates

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "march"))

## april -------------------------------------------------------------------

raw_april <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/april/april.Sav",
                                  variable_name = "Q2")
table(raw_april, useNA = "always")

#### 2. clean variable
clean_april <- NA
clean_april[raw_april == 1] <- "CAQ"
clean_april[raw_april == 2] <- "PQ"
clean_april[raw_april == 3] <- "PLQ"
clean_april[raw_april == 4] <- "QS"
clean_april[raw_april == 8] <- "PCQ"
clean_april[raw_april != 1 & raw_april != 2 & raw_april != 3 & raw_april != 4 & raw_april != 8] <- "other"
table(clean_april)

##### source_id = ces65
names(clean_april) <- sondr::generate_survey_ids(n_respondents = length(clean_april), ## number of respondents
                                                 source_id = "april") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_april) ## vector with updates

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "april"))

## may -------------------------------------------------------------------

raw_may <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/may/may.Sav",
                                  variable_name = "Q2")
table(raw_may, useNA = "always")

#### 2. clean variable
clean_may <- NA
clean_may[raw_may == 1] <- "CAQ"
clean_may[raw_may == 2] <- "PQ"
clean_may[raw_may == 3] <- "PLQ"
clean_may[raw_may == 4] <- "QS"
clean_may[raw_may == 8] <- "PCQ"
clean_may[raw_may != 1 & raw_may != 2 & raw_may != 3 & raw_may != 4 & raw_may != 8] <- "other"
table(clean_may)

##### source_id = ces65
names(clean_may) <- sondr::generate_survey_ids(n_respondents = length(clean_may), ## number of respondents
                                                 source_id = "may") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_may) ## vector with updates

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "may"))

## june -------------------------------------------------------------------

raw_june <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/june/june.Sav",
                                  variable_name = "Q2")
table(raw_june, useNA = "always")

#### 2. clean variable
clean_june <- NA
clean_june[raw_june == 1] <- "CAQ"
clean_june[raw_june == 2] <- "PQ"
clean_june[raw_june == 3] <- "PLQ"
clean_june[raw_june == 4] <- "QS"
clean_june[raw_june == 8] <- "PCQ"
clean_june[raw_june != 1 & raw_june != 2 & raw_june != 3 & raw_june != 4 & raw_june != 8] <- "other"
table(clean_june)

##### source_id = ces65
names(clean_june) <- sondr::generate_survey_ids(n_respondents = length(clean_june), ## number of respondents
                                                 source_id = "june") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_june) ## vector with updates

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "june"))

## sondage_nationalisme_2022 -------------------------------------------------------------------

raw_sondage_nationalisme_2022 <- read.csv("_SharedFolder_catalogue-donnees/merging-souverainete/raw/sondage_nationalisme_2022/sondage_nationalisme_2022.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE, fileEncoding = "UTF-8", encoding = "UTF-8")  %>% 
    mutate(id = 1:nrow(.)) %>% 
    rename(v1 = vote_intention_1, v2 = vote_intention_2, v3 = vote_intention_3, v4 = vote_intention_4, v5 = vote_intention_5) %>%
    select(id, v1, v2, v3, v4, v5) %>% 
    pivot_longer(cols = c(v1, v2, v3, v4, v5), names_to = "party", values_to = "intention") %>% 
    group_by(id) %>% 
    filter(intention == max(intention))


#### 2. clean variable
clean_sondage_nationalisme_2022 <- NA
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022$party == "v1"] <- "CAQ"
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022$party == "v2"] <- "PLQ"
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022$party == "v3"] <- "PQ"
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022$party == "v4"] <- "QS"
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022$party == "v5"] <- "PCQ"
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022$party != "v1" & 
                                raw_sondage_nationalisme_2022$party != "v2" & 
                                raw_sondage_nationalisme_2022$party != "v3" & 
                                raw_sondage_nationalisme_2022$party != "v4" & 
                                raw_sondage_nationalisme_2022$party != "v5"] <- "other"
table(clean_sondage_nationalisme_2022)

##### source_id = ces65
names(clean_sondage_nationalisme_2022) <- sondr::generate_survey_ids(n_respondents = length(clean_sondage_nationalisme_2022), ## number of respondents
                                                 source_id = "sondage_nationalisme_2022") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_sondage_nationalisme_2022) ## vector with updates

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "sondage_nationalisme_2022"))

## quorum_mcq_pilote -------------------------------------------------------------------

# NA

## pes_elxn_2022_text -------------------------------------------------------------------

raw_pes_elxn_2022 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pes_elxn_2022/pes_elxn_2022_num.csv",
                                  variable_name = "vote_2022")
table(raw_pes_elxn_2022, useNA = "always")

raw_pes_elxn_2022 <- read.csv("_SharedFolder_catalogue-donnees/merging-souverainete/raw/pes_elxn_2022/pes_elxn_2022_text.csv", 
                              header = TRUE, sep = ",", quote = "\"", dec = ".", 
                              fill = TRUE, comment.char = "", 
                              stringsAsFactors = FALSE, fileEncoding = "UTF-8", encoding = "UTF-8") %>%
    mutate(id = 1:nrow(.)) %>%
    rename(CAQ = pot_growth_prov_1, 
           PLQ = pot_growth_prov_2, 
           PQ = pot_growth_prov_3, 
           QS = pot_growth_prov_4, 
           PCQ = pot_growth_prov_5) %>%
    mutate_at(vars(CAQ, PLQ, PQ, QS, PCQ), as.numeric) %>%
    select(id, CAQ, PLQ, PQ, QS, PCQ) %>% 
    pivot_longer(cols = c(CAQ, PLQ, PQ, QS, PCQ), names_to = "party", values_to = "intention") %>% 
    drop_na(intention) %>% 
    group_by(id) %>% 
    mutate(rank = rank(-intention, ties.method = 'random')) %>%
    filter(rank == 1)

#### 2. clean variable
clean_pes_elxn_2022 <- NA
clean_pes_elxn_2022[raw_pes_elxn_2022$party == "CAQ"] <- "CAQ"
clean_pes_elxn_2022[raw_pes_elxn_2022$party == "PLQ"] <- "PLQ"
clean_pes_elxn_2022[raw_pes_elxn_2022$party == "PQ"] <- "PQ"
clean_pes_elxn_2022[raw_pes_elxn_2022$party == "QS"] <- "QS"
clean_pes_elxn_2022[raw_pes_elxn_2022$party == "PCQ"] <- "PCQ"
clean_pes_elxn_2022[raw_pes_elxn_2022$party != "CAQ" & 
                    raw_pes_elxn_2022$party != "PLQ" & 
                    raw_pes_elxn_2022$party != "PQ" & 
                    raw_pes_elxn_2022$party != "QS" & 
                    raw_pes_elxn_2022$party != "PCQ"] <- "other"
table(clean_pes_elxn_2022)

##### source_id = ces65
names(clean_pes_elxn_2022) <- sondr::generate_survey_ids(n_respondents = length(clean_pes_elxn_2022), ## number of respondents
                                                         source_id = "pes_elxn_2022_text") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_pes_elxn_2022) ## vector with updates

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "pes_elxn_2022_text"))

## pco -------------------------------------------------------------------

d <- sondr::read_survey("_SharedFolder_catalogue-donnees/merging-souverainete/raw/pco/WholeData_Pco14_2015_01_30.csv")

table(d$voteIntentCaqQcProv.Pco2014)
table(d$voteIntentQsQcProv.Pco2014)
table(d$voteIntentPqQcProv.Pco2014)
table(d$voteIntentPlqQcProv.Pco2014)

#### 2. clean variable
clean_pco <- NA
clean_pco[d$voteIntentCaqQcProv.Pco2014 == 1] <- "CAQ"
clean_pco[d$voteIntentQsQcProv.Pco2014 == 1] <- "QS"
clean_pco[d$voteIntentPqQcProv.Pco2014 == 1] <- "PQ"
clean_pco[d$voteIntentPlqQcProv.Pco2014 == 1] <- "PLQ"
table(clean_pco)

##### source_id = ces65
names(clean_pco) <- sondr::generate_survey_ids(n_respondents = length(clean_pco), ## number of respondents
                                                 source_id = "WholeData_Pco14_2015_01_30") ## source_id

## 4. add clean to the master output
output_vote_intent_prov <- sondr::match_and_update(main = output_vote_intent_prov, ## vector to update
                                         updates = clean_pco) ## vector with updates

table(sondr::extract_elements_with_prefix(output_vote_intent_prov, "WholeData_Pco14_2015_01_30"))

# Output ------------------------------------------------------------------

table(output_vote_intent_prov)

### factorise
output_vote_intent_prov <- factor(output_vote_intent_prov)

##### SAVE

saveRDS(output_vote_intent_prov, "_SharedFolder_catalogue-donnees/merging-souverainete/clean/vectors/vote_intent_prov.rds")

