# Packages ----------------------------------------------------------------
library(tidyverse)

# Config ------------------------------------------------------------------

## to get ids of each respondent
source("merging-souverainete/config.R")
sample(ids, 20) ### 20 ids random

## empty vector where the clean values will go. same length as the n of ids.
output_gender <- rep(NA, length(ids))
### each element in output is a respondent with a unique respondent id
names(output_gender) <- ids

# Merging and cleaning ----------------------------------------------------

## ces65 -------------------------------------------------------------------

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

table(sondr::extract_elements_with_prefix(output_gender, "ces65"))

## ces68 -------------------------------------------------------------------
raw_ces68 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1968/ces68.csv",
                                  variable_name = "var401")
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
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_ces68) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "ces68"))

### ces74 -------------------------------------------------------------------
raw_ces74 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1974/ces74.csv",
                                  variable_name = "v480")
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
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_ces74) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "ces74"))

## ces79 -------------------------------------------------------------------
raw_ces79 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1979/ces79.csv",
                                  variable_name = "v1537")
table(raw_ces79, useNA = "always")

#### 2. clean variable
clean_ces79 <- NA
clean_ces79[raw_ces79 == 2] <- "female"
clean_ces79[raw_ces79 == 1] <- "male"
table(clean_ces79)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces79) <- sondr::generate_survey_ids(n_respondents = length(clean_ces79), ## number of respondents
                                                 source_id = "ces79") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_ces79) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "ces79"))

## ces84 -------------------------------------------------------------------
raw_ces84 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1984/ces84.csv",
                                  variable_name = "var456")
table(raw_ces84, useNA = "always")

#### 2. clean variable
clean_ces84 <- NA
clean_ces84[raw_ces84 == "female"] <- "female"
clean_ces84[raw_ces84 == "male"] <- "male"
table(clean_ces84)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces84) <- sondr::generate_survey_ids(n_respondents = length(clean_ces84), ## number of respondents
                                                 source_id = "ces84") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_ces84) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "ces84"))

## ces88 -------------------------------------------------------------------
raw_ces88 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1988/ces88.csv",
                                  variable_name = "rsex")
table(raw_ces88, useNA = "always")

#### 2. clean variable
clean_ces88 <- NA
clean_ces88[raw_ces88 == "female"] <- "female"
clean_ces88[raw_ces88 == "male"] <- "male"
table(clean_ces88)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces88) <- sondr::generate_survey_ids(n_respondents = length(clean_ces88), ## number of respondents
                                                 source_id = "ces88") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_ces88) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "ces88"))

## ces93 -------------------------------------------------------------------
raw_ces93 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1993/ces93.csv",
                                  variable_name = "cpsrgen")
table(raw_ces93, useNA = "always")

#### 2. clean variable
clean_ces93 <- NA
clean_ces93[raw_ces93 == "female"] <- "female"
clean_ces93[raw_ces93 == "male"] <- "male"
table(clean_ces93)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces93) <- sondr::generate_survey_ids(n_respondents = length(clean_ces93), ## number of respondents
                                                 source_id = "ces93") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_ces93) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "ces93"))

## ces97 -------------------------------------------------------------------
raw_ces97 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1997/ces97.csv",
                                  variable_name = "cpsrgen")
table(raw_ces97, useNA = "always")

#### 2. clean variable
clean_ces97 <- NA
clean_ces97[raw_ces97 == "female"] <- "female"
clean_ces97[raw_ces97 == "male"] <- "male"
table(clean_ces97)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces97) <- sondr::generate_survey_ids(n_respondents = length(clean_ces97), ## number of respondents
                                                 source_id = "ces97") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_ces97) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "ces97"))

## ces2000 -------------------------------------------------------------------
raw_ces2000 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2000/ces2000.csv",
                                  variable_name = "cpsrgen")
table(raw_ces2000, useNA = "always")

#### 2. clean variable
clean_ces2000 <- NA
clean_ces2000[raw_ces2000 == "female"] <- "female"
clean_ces2000[raw_ces2000 == "male"] <- "male"
table(clean_ces2000)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces2000) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2000), ## number of respondents
                                                 source_id = "ces2000") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_ces2000) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "ces2000"))

## ces2004 -------------------------------------------------------------------
raw_ces2004 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2004/ces2004.csv",
                                  variable_name = "gender")
table(raw_ces2004, useNA = "always")

#### 2. clean variable
clean_ces2004 <- NA
clean_ces2004[raw_ces2004 == "female"] <- "female"
clean_ces2004[raw_ces2004 == "male"] <- "male"
table(clean_ces2004)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces2004) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2004), ## number of respondents
                                                 source_id = "ces2004") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_ces2004) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "ces2004"))

## ces2006 -------------------------------------------------------------------
raw_ces2006 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2006/ces2006.csv",
                                  variable_name = "cps_rgen")
table(raw_ces2006, useNA = "always")

#### 2. clean variable
clean_ces2006 <- NA
clean_ces2006[raw_ces2006 == "female"] <- "female"
clean_ces2006[raw_ces2006 == "male"] <- "male"
table(clean_ces2006)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces2006) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2006), ## number of respondents
                                                 source_id = "ces2006") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_ces2006) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "ces2006"))

## ces2008 -------------------------------------------------------------------
raw_ces2008 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2008/ces2008.csv",
                                  variable_name = "cps_rgender")
table(raw_ces2008, useNA = "always")

#### 2. clean variable
clean_ces2008 <- NA
clean_ces2008[raw_ces2008 == "female"] <- "female"
clean_ces2008[raw_ces2008 == "male"] <- "male"
table(clean_ces2008)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces2008) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2008), ## number of respondents
                                                 source_id = "ces2008") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_ces2008) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "ces2008"))

## ces2011 -------------------------------------------------------------------
raw_ces2011 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2011/ces2011.csv",
                                    variable_name = "RGENDER")
table(raw_ces2011, useNA = "always")

#### 2. clean variable
clean_ces2011 <- NA
clean_ces2011[raw_ces2011 == "Female"] <- "female"
clean_ces2011[raw_ces2011 == "Male"] <- "male"
table(clean_ces2011)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces2011) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2011), ## number of respondents
                                                   source_id = "ces2011") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_ces2011) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "ces2011"))

## ces2015 -------------------------------------------------------------------
raw_ces2015 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2015/ces2015.csv",
                                    variable_name = "sex_r")
table(raw_ces2015, useNA = "always")

#### 2. clean variable
clean_ces2015 <- NA
clean_ces2015[raw_ces2015 == 5] <- "female"
clean_ces2015[raw_ces2015 == 1] <- "male"
table(clean_ces2015)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces2015) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2015), ## number of respondents
                                                   source_id = "ces2015") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_ces2015) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "ces2015"))

## ces2019 -------------------------------------------------------------------
raw_ces2019 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2019/ces2019.csv",
                                    variable_name = "cps19_gender")
table(raw_ces2019, useNA = "always")

#### 2. clean variable
clean_ces2019 <- NA
clean_ces2019[raw_ces2019 == "A woman"] <- "female"
clean_ces2019[raw_ces2019 == "A man"] <- "male"
table(clean_ces2019)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces2019) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2019), ## number of respondents
                                                   source_id = "ces2019") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_ces2019) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "ces2019"))

## ces2021 -------------------------------------------------------------------
raw_ces2021 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2021/ces2021.csv",
                                    variable_name = "cps21_genderid")
table(raw_ces2021, useNA = "always")

#### 2. clean variable
clean_ces2021 <- NA
clean_ces2021[raw_ces2021 == "A woman"] <- "female"
clean_ces2021[raw_ces2021 == "A man"] <- "male"
table(clean_ces2021)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces2021) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2021), ## number of respondents
                                                   source_id = "ces2021") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_ces2021) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "ces2021"))

## datagotchi_pilot1_2021 -------------------------------------------------------------------
raw_datagotchi_pilot1_2021 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot1_2021/datagotchi_pilot1_2021.Sav",
                                    variable_name = "SEXE")
table(raw_datagotchi_pilot1_2021, useNA = "always")

#### 2. clean variable
clean_datagotchi_pilot1_2021 <- NA
clean_datagotchi_pilot1_2021[raw_datagotchi_pilot1_2021 == 2] <- "female"
clean_datagotchi_pilot1_2021[raw_datagotchi_pilot1_2021 == 1] <- "male"
table(clean_datagotchi_pilot1_2021)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_datagotchi_pilot1_2021) <- sondr::generate_survey_ids(n_respondents = length(clean_datagotchi_pilot1_2021), ## number of respondents
                                                   source_id = "datagotchi_pilot1_2021") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_datagotchi_pilot1_2021) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "datagotchi_pilot1_2021"))

## january -------------------------------------------------------------------
raw_january <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/january/january.Sav",
                                                   variable_name = "SEXE")
table(raw_january, useNA = "always")

#### 2. clean variable
clean_january <- NA
clean_january[raw_january == 2] <- "female"
clean_january[raw_january == 1] <- "male"
table(clean_january)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_january) <- sondr::generate_survey_ids(n_respondents = length(clean_january), ## number of respondents
                                                                  source_id = "january") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_january) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "january"))

## february -------------------------------------------------------------------
raw_february <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/february/february.Sav",
                                    variable_name = "SEXE")
table(raw_february, useNA = "always")

#### 2. clean variable
clean_february <- NA
clean_february[raw_february == 2] <- "female"
clean_february[raw_february == 1] <- "male"
table(clean_february)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_february) <- sondr::generate_survey_ids(n_respondents = length(clean_february), ## number of respondents
                                                   source_id = "february") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_february) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "february"))

## march -------------------------------------------------------------------
raw_march <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/march/march.Sav",
                                    variable_name = "SEXE")
table(raw_march, useNA = "always")

#### 2. clean variable
clean_march <- NA
clean_march[raw_march == 2] <- "female"
clean_march[raw_march == 1] <- "male"
table(clean_march)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_march) <- sondr::generate_survey_ids(n_respondents = length(clean_march), ## number of respondents
                                                   source_id = "march") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_march) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "march"))

## april -------------------------------------------------------------------
raw_april <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/april/april.Sav",
                                    variable_name = "SEXE")
table(raw_april, useNA = "always")

#### 2. clean variable
clean_april <- NA
clean_april[raw_april == 2] <- "female"
clean_april[raw_april == 1] <- "male"
table(clean_april)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_april) <- sondr::generate_survey_ids(n_respondents = length(clean_april), ## number of respondents
                                                   source_id = "april") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_april) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "april"))

## may -------------------------------------------------------------------
raw_may <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/may/may.Sav",
                                    variable_name = "SEXE")
table(raw_may, useNA = "always")

#### 2. clean variable
clean_may <- NA
clean_may[raw_may == 2] <- "female"
clean_may[raw_may == 1] <- "male"
table(clean_may)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_may) <- sondr::generate_survey_ids(n_respondents = length(clean_may), ## number of respondents
                                                   source_id = "may") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_may) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "may"))

## june -------------------------------------------------------------------
raw_june <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/june/june.Sav",
                                    variable_name = "SEXE")
table(raw_june, useNA = "always")

#### 2. clean variable
clean_june <- NA
clean_june[raw_june == 2] <- "female"
clean_june[raw_june == 1] <- "male"
table(clean_june)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_june) <- sondr::generate_survey_ids(n_respondents = length(clean_june), ## number of respondents
                                                   source_id = "june") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_june) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "june"))

## datagotchi_pilot2_2022 -------------------------------------------------------------------
raw_datagotchi_pilot2_2022_fr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                               variable_name = "gender")
table(raw_datagotchi_pilot2_2022_fr, useNA = "always")

raw_datagotchi_pilot2_2022_en <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                                   variable_name = "gender.1")
table(raw_datagotchi_pilot2_2022_en, useNA = "always")

raw_datagotchi_pilot2_2022 <- coalesce(raw_datagotchi_pilot2_2022_fr, raw_datagotchi_pilot2_2022_en)
table(raw_datagotchi_pilot2_2022)

#### 2. clean variable
clean_datagotchi_pilot2_2022 <- NA
clean_datagotchi_pilot2_2022[raw_datagotchi_pilot2_2022 == 2] <- "female"
clean_datagotchi_pilot2_2022[raw_datagotchi_pilot2_2022 == 1] <- "male"
table(clean_datagotchi_pilot2_2022)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_datagotchi_pilot2_2022) <- sondr::generate_survey_ids(n_respondents = length(clean_datagotchi_pilot2_2022), ## number of respondents
                                                source_id = "datagotchi_pilot2_2022") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_datagotchi_pilot2_2022) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "datagotchi_pilot2_2022"))

## sondage_nationalisme_2022 -------------------------------------------------------------------
raw_sondage_nationalisme_2022 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/sondage_nationalisme_2022/sondage_nationalisme_2022.csv",
                                 variable_name = "gender")
table(raw_sondage_nationalisme_2022, useNA = "always")

#### 2. clean variable
clean_sondage_nationalisme_2022 <- NA
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022 == "Une femme" | raw_sondage_nationalisme_2022 == "Woman"] <- "female"
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022 == "Un homme" | raw_sondage_nationalisme_2022 == "Man"] <- "male"
table(clean_sondage_nationalisme_2022)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_sondage_nationalisme_2022) <- sondr::generate_survey_ids(n_respondents = length(clean_sondage_nationalisme_2022), ## number of respondents
                                                source_id = "sondage_nationalisme_2022") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_sondage_nationalisme_2022) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "sondage_nationalisme_2022"))

## quorum_mcq_pilote -------------------------------------------------------------------
raw_quorum_mcq_pilote_fr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                               variable_name = "ses_gender")
table(raw_quorum_mcq_pilote_fr, useNA = "always")
### change "" for NA
raw_quorum_mcq_pilote_fr[raw_quorum_mcq_pilote_fr == ""] <- NA

raw_quorum_mcq_pilote_en <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                               variable_name = "EN_ses_gender")
table(raw_quorum_mcq_pilote_en, useNA = "always")
### change "" for NA
raw_quorum_mcq_pilote_en[raw_quorum_mcq_pilote_en == ""] <- NA

raw_quorum_mcq_pilote <- coalesce(raw_quorum_mcq_pilote_fr, raw_quorum_mcq_pilote_en)
table(raw_quorum_mcq_pilote)

#### 2. clean variable
clean_quorum_mcq_pilote <- NA
clean_quorum_mcq_pilote[raw_quorum_mcq_pilote == "Femme" | raw_quorum_mcq_pilote == "Woman"] <- "female"
clean_quorum_mcq_pilote[raw_quorum_mcq_pilote == "Homme" | raw_quorum_mcq_pilote == "Man"] <- "male"
table(clean_quorum_mcq_pilote)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_quorum_mcq_pilote) <- sondr::generate_survey_ids(n_respondents = length(clean_quorum_mcq_pilote), ## number of respondents
                                                                     source_id = "quorum_mcq_pilote") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_quorum_mcq_pilote) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "quorum_mcq_pilote"))

## pes_elxn_2022_text -------------------------------------------------------------------
raw_pes_elxn_2022_text <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pes_elxn_2022/pes_elxn_2022_text.csv",
                                                      variable_name = "ses_gender")
table(raw_pes_elxn_2022_text, useNA = "always")

#### 2. clean variable
clean_pes_elxn_2022_text <- NA
clean_pes_elxn_2022_text[raw_pes_elxn_2022_text == "Féminin"] <- "female"
clean_pes_elxn_2022_text[raw_pes_elxn_2022_text == "Masculin"] <- "male"
table(clean_pes_elxn_2022_text)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_pes_elxn_2022_text) <- sondr::generate_survey_ids(n_respondents = length(clean_pes_elxn_2022_text), ## number of respondents
                                                                     source_id = "pes_elxn_2022_text") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_pes_elxn_2022_text) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "pes_elxn_2022_text"))

## pco -------------------------------------------------------------------
raw_pco <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pco/WholeData_Pco14_2015-01-30.csv",
                                               variable_name = "female")
table(raw_pco, useNA = "always")

#### 2. clean variable
clean_pco <- NA
clean_pco[raw_pco == 1] <- "female"
clean_pco[raw_pco == 0] <- "male"
table(clean_pco)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_pco) <- sondr::generate_survey_ids(n_respondents = length(clean_pco), ## number of respondents
                                                              source_id = "WholeData_Pco14_2015-01-30") ## source_id

## 4. add clean to the master output
output_gender <- sondr::match_and_update(main = output_gender, ## vector to update
                                         updates = clean_pco) ## vector with updates

table(sondr::extract_elements_with_prefix(output_gender, "WholeData_Pco14_2015-01-30"))

# Output ------------------------------------------------------------------

### FACTORISE, LEVELS, etc.

table(output_gender)
output_gender <- factor(output_gender)

##### SAVE VECTOR
saveRDS(output_gender, "_SharedFolder_catalogue-donnees/merging-souverainete/clean/vectors/ses_gender.rds")

