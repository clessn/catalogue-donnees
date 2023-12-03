# Packages ----------------------------------------------------------------
library(tidyverse)

# Config ------------------------------------------------------------------
### source config file to generate all the respondent_ids
source("merging-souverainete/config.R")
sample(ids, 20) ### 20 ids random
## empty vector where the clean values will go. same length as the n of ids.
output_souv <- rep(NA, length(ids))
### each element in output is a respondent with a unique respondent id
names(output_souv) <- ids


# Merging and cleaning ----------------------------------------------------

## ces65 -------------------------------------------------------------------

### no question

## ces68 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces68 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1968/ces68.csv",
                                  variable_name = "var306")
table(raw_ces68, useNA = "always")

#### 2. clean variable
clean_ces68 <- NA
clean_ces68[raw_ces68 == "slightly  in favour" | raw_ces68 == "strongly  in favour"] <- 1
clean_ces68[raw_ces68 == "slightly  opposed" | raw_ces68 == "strongly opposed"] <- 0
table(clean_ces68)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces68) <- sondr::generate_survey_ids(n_respondents = length(clean_ces68), ## number of respondents
                                                 source_id = "ces68") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                         updates = clean_ces68) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "ces68"))
## ces74 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces74 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1974/ces74.csv",
                                  variable_name = "v125")
table(raw_ces74, useNA = "always")

#### 2. clean variable
clean_ces74 <- NA
clean_ces74[raw_ces74 == "in favour"] <- 1
clean_ces74[raw_ces74 == "opposed"] <- 0
table(clean_ces74)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces74) <- sondr::generate_survey_ids(n_respondents = length(clean_ces74), ## number of respondents
                                                 source_id = "ces74") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_ces74) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "ces74"))
## ces79 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces79 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1979/ces79.csv",
                                  variable_name = "v1188")
table(raw_ces79, useNA = "always")

#### 2. clean variable
clean_ces79 <- NA
clean_ces79[raw_ces79 == 1 | raw_ces79 == 2] <- 1
clean_ces79[raw_ces79 == 3 | raw_ces79 == 4] <- 0
table(clean_ces79)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces79) <- sondr::generate_survey_ids(n_respondents = length(clean_ces79), ## number of respondents
                                                 source_id = "ces79") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_ces79) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "ces79"))
## ces84 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces84 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1984/ces84.csv",
                                  variable_name = "var500")
table(raw_ces84, useNA = "always")

#### 2. clean variable
clean_ces84 <- NA
clean_ces84[raw_ces84 == "voted yes"] <- 1
clean_ces84[raw_ces84 == "voted no"] <- 0
table(clean_ces84)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces84) <- sondr::generate_survey_ids(n_respondents = length(clean_ces84), ## number of respondents
                                                 source_id = "ces84") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_ces84) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "ces84"))
## ces88 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces88 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1988/ces88.csv",
                                  variable_name = "b10")
table(raw_ces88, useNA = "always")

#### 2. clean variable
clean_ces88 <- NA
clean_ces88[raw_ces88 == "support completely" | raw_ces88 == "support somewhat"] <- 1
clean_ces88[raw_ces88 == "oppose  completely" | raw_ces88 == "oppose  somewhat"] <- 0
table(clean_ces88)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces88) <- sondr::generate_survey_ids(n_respondents = length(clean_ces88), ## number of respondents
                                                 source_id = "ces88") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_ces88) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "ces88"))
## ces93 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces93 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1993/ces93.csv",
                                  variable_name = "cpsg11")
table(raw_ces93, useNA = "always")

#### 2. clean variable
clean_ces93 <- NA
clean_ces93[raw_ces93 == "somewhat favour" | raw_ces93 == "very favourable"] <- 1
clean_ces93[raw_ces93 == "somewhat opposed" | raw_ces93 == "very opposed"] <- 0
table(clean_ces93)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces93) <- sondr::generate_survey_ids(n_respondents = length(clean_ces93), ## number of respondents
                                                 source_id = "ces93") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_ces93) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "ces93"))
## ces97 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces97 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1997/ces97.csv",
                                  variable_name = "cpsj3a")
table(raw_ces97, useNA = "always")

#### 2. clean variable
clean_ces97 <- NA
clean_ces97[raw_ces97 == "somewhat favourable" | raw_ces97 == "very favourable"] <- 1
clean_ces97[raw_ces97 == "somewhat opposed" | raw_ces97 == "very opposed"] <- 0
table(clean_ces97)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces97) <- sondr::generate_survey_ids(n_respondents = length(clean_ces97), ## number of respondents
                                                 source_id = "ces97") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_ces97) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "ces97"))
## ces2000 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces2000 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2000/ces2000.csv",
                                  variable_name = "pesc6")
table(raw_ces2000, useNA = "always")

#### 2. clean variable
clean_ces2000 <- NA
clean_ces2000[raw_ces2000 == "somewhat favourable" | raw_ces2000 == "very favourable"] <- 1
clean_ces2000[raw_ces2000 == "somewhat opposed" | raw_ces2000 == "very opposed"] <- 0
table(clean_ces2000)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces2000) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2000), ## number of respondents
                                                 source_id = "ces2000") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_ces2000) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "ces2000"))
## ces2004 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces2004 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2004/ces2004.csv",
                                    variable_name = "ces04_pes_c10")
table(raw_ces2004, useNA = "always")

#### 2. clean variable
clean_ces2004 <- NA
clean_ces2004[raw_ces2004 == "somewhat favourable" | raw_ces2004 == "very favourable"] <- 1
clean_ces2004[raw_ces2004 == "somewhat opposed" | raw_ces2004 == "very opposed"] <- 0
table(clean_ces2004)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces2004) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2004), ## number of respondents
                                                   source_id = "ces2004") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_ces2004) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "ces2004"))
## ces2006 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces2006 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2006/ces2006.csv",
                                    variable_name = "cps_i806")
table(raw_ces2006, useNA = "always")

#### 2. clean variable
clean_ces2006 <- NA
clean_ces2006[raw_ces2006 == "somewhat favourable" | raw_ces2006 == "very favourable"] <- 1
clean_ces2006[raw_ces2006 == "somewhat opposed" | raw_ces2006 == "very opposed"] <- 0
table(clean_ces2006)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces2006) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2006), ## number of respondents
                                                   source_id = "ces2006") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_ces2006) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "ces2006"))
## ces2008 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces2008 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2008/ces2008.csv",
                                    variable_name = "cps_q9")
table(raw_ces2008, useNA = "always")

#### 2. clean variable
clean_ces2008 <- NA
clean_ces2008[raw_ces2008 == "somewhat favourable" | raw_ces2008 == "very favourable"] <- 1
clean_ces2008[raw_ces2008 == "somewhat opposed" | raw_ces2008 == "very opposed"] <- 0
table(clean_ces2008)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces2008) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2008), ## number of respondents
                                                   source_id = "ces2008") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_ces2008) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "ces2008"))
## ces2011 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces2011 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2011/ces2011.csv",
                                    variable_name = "CPS11_75")
table(raw_ces2011, useNA = "always")

#### 2. clean variable
clean_ces2011 <- NA
clean_ces2011[raw_ces2011 == "somewhat favourable" | raw_ces2011 == "very favourable    "] <- 1
clean_ces2011[raw_ces2011 == "somewhat opposed   " | raw_ces2011 == "very opposed       "] <- 0
table(clean_ces2011)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces2011) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2011), ## number of respondents
                                                   source_id = "ces2011") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_ces2011) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "ces2011"))
## ces2015 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces2015 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2015/ces2015.csv",
                                    variable_name = "sov")
table(raw_ces2015, useNA = "always")

#### 2. clean variable
clean_ces2015 <- NA
clean_ces2015[raw_ces2015 == 1 | raw_ces2015 == 3] <- 1
clean_ces2015[raw_ces2015 == 5 | raw_ces2015 == 7] <- 0
table(clean_ces2015)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces2015) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2015), ## number of respondents
                                                   source_id = "ces2015") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_ces2015) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "ces2015"))
## ces2019 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces2019 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2019/ces2019.csv",
                                    variable_name = "cps19_quebec_sov")
table(raw_ces2019, useNA = "always")

#### 2. clean variable
clean_ces2019 <- NA
clean_ces2019[raw_ces2019 == "Somewhat favourable" | raw_ces2019 == "Very favourable"] <- 1
clean_ces2019[raw_ces2019 == "Somewhat opposed" | raw_ces2019 == "Very opposed"] <- 0
table(clean_ces2019)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces2019) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2019), ## number of respondents
                                                   source_id = "ces2019") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_ces2019) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "ces2019"))
## ces2021 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces2021 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2021/ces2021.csv",
                                    variable_name = "cps21_quebec_sov")
table(raw_ces2021, useNA = "always")

#### 2. clean variable
clean_ces2021 <- NA
clean_ces2021[raw_ces2021 == "Somewhat favourable" | raw_ces2021 == "Very favourable"] <- 1
clean_ces2021[raw_ces2021 == "Somewhat opposed" | raw_ces2021 == "Very opposed"] <- 0
table(clean_ces2021)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces2021) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2021), ## number of respondents
                                                   source_id = "ces2021") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_ces2021) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "ces2021"))
## datagotchi_pilot1_2021 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_datagotchi_pilot1_2021 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot1_2021/datagotchi_pilot1_2021.Sav",
                                    variable_name = "E1Q_A1")
table(raw_datagotchi_pilot1_2021, useNA = "always")

#### 2. clean variable
clean_datagotchi_pilot1_2021 <- NA
clean_datagotchi_pilot1_2021[raw_datagotchi_pilot1_2021 == 1 | raw_datagotchi_pilot1_2021 == 2] <- 1
clean_datagotchi_pilot1_2021[raw_datagotchi_pilot1_2021 == 3 | raw_datagotchi_pilot1_2021 == 4] <- 0
table(clean_datagotchi_pilot1_2021)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_datagotchi_pilot1_2021) <- sondr::generate_survey_ids(n_respondents = length(clean_datagotchi_pilot1_2021), ## number of respondents
                                                   source_id = "datagotchi_pilot1_2021") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_datagotchi_pilot1_2021) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "datagotchi_pilot1_2021"))
## january -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_omnibus_1january <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/january/january.Sav",
                                                   variable_name = "Q22")
table(raw_omnibus_1january, useNA = "always")

#### 2. clean variable
clean_omnibus_1january <- NA
clean_omnibus_1january[raw_omnibus_1january == 3 | raw_omnibus_1january == 4] <- 1
clean_omnibus_1january[raw_omnibus_1january == 1 | raw_omnibus_1january == 2] <- 0
table(clean_omnibus_1january)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_omnibus_1january) <- sondr::generate_survey_ids(n_respondents = length(clean_omnibus_1january), ## number of respondents
                                                                  source_id = "january") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_omnibus_1january) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "january"))
## february -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_omnibus_2february <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/february/february.Sav",
                                             variable_name = "Q22")
table(raw_omnibus_2february, useNA = "always")

#### 2. clean variable
clean_omnibus_2february <- NA
clean_omnibus_2february[raw_omnibus_2february == 3 | raw_omnibus_2february == 4] <- 1
clean_omnibus_2february[raw_omnibus_2february == 1 | raw_omnibus_2february == 2] <- 0
table(clean_omnibus_2february)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_omnibus_2february) <- sondr::generate_survey_ids(n_respondents = length(clean_omnibus_2february), ## number of respondents
                                                            source_id = "february") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_omnibus_2february) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "february"))
## march -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_omnibus_3march <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/february/february.Sav",
                                              variable_name = "Q22")
table(raw_omnibus_3march, useNA = "always")

#### 2. clean variable
clean_omnibus_3march <- NA
clean_omnibus_3march[raw_omnibus_3march == 3 | raw_omnibus_3march == 4] <- 1
clean_omnibus_3march[raw_omnibus_3march == 1 | raw_omnibus_3march == 2] <- 0
table(clean_omnibus_3march)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_omnibus_3march) <- sondr::generate_survey_ids(n_respondents = length(clean_omnibus_3march), ## number of respondents
                                                             source_id = "march") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_omnibus_3march) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "march"))
## april -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_omnibus_4april <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/april/april.Sav",
                                           variable_name = "Q22")
table(raw_omnibus_4april, useNA = "always")

#### 2. clean variable
clean_omnibus_4april <- NA
clean_omnibus_4april[raw_omnibus_4april == 3 | raw_omnibus_4april == 4] <- 1
clean_omnibus_4april[raw_omnibus_4april == 1 | raw_omnibus_4april == 2] <- 0
table(clean_omnibus_4april)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_omnibus_4april) <- sondr::generate_survey_ids(n_respondents = length(clean_omnibus_4april), ## number of respondents
                                                          source_id = "april") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_omnibus_4april) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "april"))
## may -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_omnibus_5may <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/may/may.Sav",
                                           variable_name = "Q22")
table(raw_omnibus_5may, useNA = "always")

#### 2. clean variable
clean_omnibus_5may <- NA
clean_omnibus_5may[raw_omnibus_5may == 3 | raw_omnibus_5may == 4] <- 1
clean_omnibus_5may[raw_omnibus_5may == 1 | raw_omnibus_5may == 2] <- 0
table(clean_omnibus_5may)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_omnibus_5may) <- sondr::generate_survey_ids(n_respondents = length(clean_omnibus_5may), ## number of respondents
                                                          source_id = "may") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_omnibus_5may) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "may"))
## june -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_omnibus_6june <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/june/june.Sav",
                                         variable_name = "Q22")
table(raw_omnibus_6june, useNA = "always")

#### 2. clean variable
clean_omnibus_6june <- NA
clean_omnibus_6june[raw_omnibus_6june == 3 | raw_omnibus_6june == 4] <- 1
clean_omnibus_6june[raw_omnibus_6june == 1 | raw_omnibus_6june == 2] <- 0
table(clean_omnibus_6june)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_omnibus_6june) <- sondr::generate_survey_ids(n_respondents = length(clean_omnibus_6june), ## number of respondents
                                                        source_id = "june") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_omnibus_6june) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "june"))
## datagotchi_pilot2_2022 -------------------------------------------------------------------

### ICI IL FAUT MERGER FRANCAIS ET ANGLAIS

#### 1. Get raw gender variable vector
raw_datagotchi_pilot2_2022_fr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                  variable_name = "FR_qcIndependant")
table(raw_datagotchi_pilot2_2022_fr, useNA = "always")

raw_datagotchi_pilot2_2022_en <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                               variable_name = "EN_qcIndependant")
table(raw_datagotchi_pilot2_2022_en, useNA = "always")

raw_datagotchi_pilot2_2022 <- coalesce(raw_datagotchi_pilot2_2022_fr, raw_datagotchi_pilot2_2022_en)
table(raw_datagotchi_pilot2_2022, useNA = "always")

#### 2. clean variable
clean_datagotchi_pilot2_2022 <- NA
clean_datagotchi_pilot2_2022[raw_datagotchi_pilot2_2022 == 1 | raw_datagotchi_pilot2_2022 == 4] <- 1
clean_datagotchi_pilot2_2022[raw_datagotchi_pilot2_2022 == 6 | raw_datagotchi_pilot2_2022 == 7] <- 0
table(clean_datagotchi_pilot2_2022)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_datagotchi_pilot2_2022) <- sondr::generate_survey_ids(n_respondents = length(clean_datagotchi_pilot2_2022), ## number of respondents
                                                                  source_id = "datagotchi_pilot2_2022") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_datagotchi_pilot2_2022) ## vector with updates

table(sondr::extract_elements_with_prefix(output_souv, "datagotchi_pilot2_2022"))
## sondage_nationalisme_2022 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_sondage_nationalisme_2022 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/sondage_nationalisme_2022/sondage_nationalisme_2022.csv",
                                                      variable_name = "independence_1")
table(raw_sondage_nationalisme_2022, useNA = "always")

#### 2. clean variable
clean_sondage_nationalisme_2022 <- NA
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022 == "Fortement en accord" | raw_sondage_nationalisme_2022 == "Plutôt en accord"
                                | raw_sondage_nationalisme_2022 == "Somewhat agree" | raw_sondage_nationalisme_2022 == "Strongly agree"] <- 1
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022 == "Fortement en désaccord" | raw_sondage_nationalisme_2022 == "Plutôt en désaccord"
                                | raw_sondage_nationalisme_2022 == "Somewhat disagree" | raw_sondage_nationalisme_2022 == "Strongly disagree"]  <- 0
table(clean_sondage_nationalisme_2022)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_sondage_nationalisme_2022) <- sondr::generate_survey_ids(n_respondents = length(clean_sondage_nationalisme_2022), ## number of respondents
                                                         source_id = "sondage_nationalisme_2022") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_sondage_nationalisme_2022) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "sondage_nationalisme_2022"))
## quorum_mcq_pilote -------------------------------------------------------------------

### ICI IL FAUT MERGER FRANCAIS ET ANGLAIS

#### 1. Get raw gender variable vector
raw_quorum_mcq_pilote_fr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                               variable_name = "iss_independence")
table(raw_quorum_mcq_pilote_fr, useNA = "always")

## fix labels
labels <- c("1", "2", "3", "4")
names(labels) <- as.character(c("Fortement d'accord", "Plutôt d'accord", "Plutôt en désaccord", "Fortement en désaccord"))
raw_quorum_mcq_pilote_fr <- labels[raw_quorum_mcq_pilote_fr]
table(raw_quorum_mcq_pilote_fr)

raw_quorum_mcq_pilote_en <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                               variable_name = "EN_iss_independence")
table(raw_quorum_mcq_pilote_en, useNA = "always")

## fix labels
labels <- c("1", "2", "3", "4")
names(labels) <- as.character(c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree"))
raw_quorum_mcq_pilote_en <- labels[raw_quorum_mcq_pilote_en]
table(raw_quorum_mcq_pilote_en)

raw_quorum_mcq_pilote <- coalesce(raw_quorum_mcq_pilote_fr, raw_quorum_mcq_pilote_en)
table(raw_quorum_mcq_pilote, useNA = "always")

#### 2. clean variable

#### 2. clean variable
clean_quorum_mcq_pilote <- NA
clean_quorum_mcq_pilote[raw_quorum_mcq_pilote == 1 | raw_quorum_mcq_pilote == 2] <- 1
clean_quorum_mcq_pilote[raw_quorum_mcq_pilote == 3 | raw_quorum_mcq_pilote == 4] <- 0
table(clean_quorum_mcq_pilote)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_quorum_mcq_pilote) <- sondr::generate_survey_ids(n_respondents = length(clean_quorum_mcq_pilote), ## number of respondents
                                                                  source_id = "quorum_mcq_pilote") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_quorum_mcq_pilote) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "quorum_mcq_pilote"))
## pes_elxn_2022_text -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_pes_elxn_2022 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pes_elxn_2022/pes_elxn_2022_num.csv",
                                          variable_name = "iss_independence")
table(raw_pes_elxn_2022, useNA = "always")


#### 2. clean variable
clean_pes_elxn_2022 <- NA
clean_pes_elxn_2022[raw_pes_elxn_2022 == 7 | raw_pes_elxn_2022 == 6 | raw_pes_elxn_2022 == 5] <- 1
clean_pes_elxn_2022[raw_pes_elxn_2022 == 1 | raw_pes_elxn_2022 == 2 | raw_pes_elxn_2022 == 3] <- 0
table(clean_pes_elxn_2022)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_pes_elxn_2022) <- sondr::generate_survey_ids(n_respondents = length(clean_pes_elxn_2022), ## number of respondents
                                                         source_id = "pes_elxn_2022_text") ## source_id

## 4. add clean to the master output
output_souv <- sondr::match_and_update(main = output_souv, ## vector to update
                                       updates = clean_pes_elxn_2022) ## vector with updates
table(sondr::extract_elements_with_prefix(output_souv, "pes_elxn_2022"))
## pco -------------------------------------------------------------------

# Aucune question

# Output ------------------------------------------------------------------

### FACTORISE, LEVELS, etc.

#output_souv <- factor(output_souv)
table(output_souv)

##### SAVE VECTOR WHERE??

saveRDS(output_souv, "_SharedFolder_catalogue-donnees/merging-souverainete/clean/vectors/iss_souv.rds")


