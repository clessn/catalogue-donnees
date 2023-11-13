# Packages ----------------------------------------------------------------
library(tidyverse)

# Config ------------------------------------------------------------------
### source config file to generate all the respondent_ids
source("merging-souverainete/config.R")
sample(ids, 20) ### 20 ids random
## empty vector where the clean values will go. same length as the n of ids.
output_educ <- rep(NA, length(ids))
### each element in output is a respondent with a unique respondent id
names(output_educ) <- ids


# Merging and cleaning ----------------------------------------------------

## ces65 -------------------------------------------------------------------

### no question

## ces68 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces68 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1968/ces68.csv",
                                  variable_name = "var334")
table(raw_ces68, useNA = "always")

#### 2. clean variable
clean_ces68 <- NA
clean_ces68[raw_ces68 >= 13] <- 1
clean_ces68[raw_ces68 >= 0 & raw_ces68 <= 13] <- 0
table(clean_ces68)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces68) <- sondr::generate_survey_ids(n_respondents = length(clean_ces68), ## number of respondents
                                                 source_id = "ces68") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                         updates = clean_ces68) ## vector with updates

## ces74 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces74 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1974/ces74.csv",
                                  variable_name = "v414")
table(raw_ces74, useNA = "always")

#### 2. clean variable
clean_ces74 <- NA
clean_ces74[raw_ces74 == 25] <- 1
clean_ces74[raw_ces74 >= 0 & raw_ces74 <= 13] <- 0
table(clean_ces74)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces74) <- sondr::generate_survey_ids(n_respondents = length(clean_ces74), ## number of respondents
                                                 source_id = "ces74") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_ces74) ## vector with updates

## ces79 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces79 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1979/ces79.csv",
                                  variable_name = "v443")
table(raw_ces79, useNA = "always")

#### 2. clean variable
clean_ces79 <- NA
clean_ces79[raw_ces79 == 25] <- 1
clean_ces79[raw_ces79 >= 0 & raw_ces79 <= 13] <- 0
table(clean_ces79)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces79) <- sondr::generate_survey_ids(n_respondents = length(clean_ces79), ## number of respondents
                                                 source_id = "ces79") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_ces79) ## vector with updates

## ces84 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces84 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1984/ces84.csv",
                                  variable_name = "var362")
table(raw_ces84, useNA = "always")

#### 2. clean variable
clean_ces84 <- NA
clean_ces84[raw_ces84 == "some technical" | raw_ces84 == "grad. technical" | raw_ces84 == "some university" 
            | raw_ces84 == "grad. university"] <- 1
clean_ces84[raw_ces84 == "no formal school" | raw_ces84 == "some elementary" | raw_ces84 == "grad. elementary" 
            | raw_ces84 == "some high school" | raw_ces84 == "high school grad."] <- 0
table(clean_ces84)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces84) <- sondr::generate_survey_ids(n_respondents = length(clean_ces84), ## number of respondents
                                                 source_id = "ces84") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_ces84) ## vector with updates

## ces88 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces88 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1988/ces88.csv",
                                  variable_name = "n3")
table(raw_ces88, useNA = "always")

#### 2. clean variable
clean_ces88 <- NA
clean_ces88[raw_ces88 == "some tech,cc,etc" | raw_ces88 == "complete tech,cc,etc" | raw_ces88 == "some university" 
            | raw_ces88 == "bachelors" | raw_ces88 == "masters" | raw_ces88 == "profesnl,ph.d."] <- 1
clean_ces88[raw_ces88 == "no schooling" | raw_ces88 == "some elementary" | raw_ces88 == "complete elementary" 
            | raw_ces88 == "some sec,high school" | raw_ces88 == "complete sec,hs"] <- 0
table(clean_ces88)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces88) <- sondr::generate_survey_ids(n_respondents = length(clean_ces88), ## number of respondents
                                                 source_id = "ces88") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_ces88) ## vector with updates

## ces93 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces93 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1993/ces93.csv",
                                  variable_name = "cpso3")
table(raw_ces93, useNA = "always")

#### 2. clean variable
clean_ces93 <- NA
clean_ces93[raw_ces93 == "some tech,cc,etc" | raw_ces93 == "complete tech,cc,etc" | raw_ces93 == "some university" 
            | raw_ces93 == "bachelors" | raw_ces93 == "masters" | raw_ces93 == "profesnl,ph.d."] <- 1
clean_ces93[raw_ces93 == "no schooling" | raw_ces93 == "some elementary" | raw_ces93 == "complete elementary" 
            | raw_ces93 == "some sec,high school" | raw_ces93 == "complete sec,hs"] <- 0
table(clean_ces93)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces93) <- sondr::generate_survey_ids(n_respondents = length(clean_ces93), ## number of respondents
                                                 source_id = "ces93") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_ces93) ## vector with updates

## ces97 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces97 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1997/ces97.csv",
                                  variable_name = "cpsm3")
table(raw_ces97, useNA = "always")

#### 2. clean variable
clean_ces97 <- NA
clean_ces97[raw_ces97 == "sometech/caat" | raw_ces97 == "completetechcaat" | raw_ces97 == "some university" 
            | raw_ces97 == "b.a." | raw_ces97 == "m.a." | raw_ces97 == "profesnl/phd"] <- 1
clean_ces97[raw_ces97 == "no schooling" | raw_ces97 == "some elementary" | raw_ces97 == "complete elem." 
            | raw_ces97 == "somesoc/h.s." | raw_ces97 == "complete sec/hs"] <- 0
table(clean_ces97)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces97) <- sondr::generate_survey_ids(n_respondents = length(clean_ces97), ## number of respondents
                                                 source_id = "ces97") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_ces97) ## vector with updates

## ces2000 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces2000 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2000/ces2000.csv",
                                  variable_name = "cpsm3")
table(raw_ces2000, useNA = "always")

#### 2. clean variable
clean_ces2000 <- NA
clean_ces2000[raw_ces2000 == "some technical, caat, cegep,..." | raw_ces2000 == "completed technical, caat, cegep,..." | raw_ces2000 == "some university" 
            | raw_ces2000 == "bachelor's degree" | raw_ces2000 == "master's degree" | raw_ces2000 == "professional degree or doctorate"] <- 1
clean_ces2000[raw_ces2000 == "no schooling" | raw_ces2000 == "some elementary school" | raw_ces2000 == "completed elementary school" 
            | raw_ces2000 == "some secondary / high school" | raw_ces2000 == "completed secondary / high school"] <- 0
table(clean_ces2000)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces2000) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2000), ## number of respondents
                                                 source_id = "ces2000") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_ces2000) ## vector with updates

## ces2004 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces2004 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2004/ces2004.csv",
                                    variable_name = "ces04_cps_s3")
table(raw_ces2004, useNA = "always")

#### 2. clean variable
clean_ces2004 <- NA
clean_ces2004[raw_ces2004 == "some technical, caat, cegep,..." | raw_ces2004 == "completed technical, caat, cegep,..." | raw_ces2004 == "some university" 
              | raw_ces2004 == "bachelor's degree" | raw_ces2004 == "master's degree" | raw_ces2004 == "professional degree or doctorate"] <- 1
clean_ces2004[raw_ces2004 == "no schooling" | raw_ces2004 == "some elementary school" | raw_ces2004 == "completed elementary school" 
              | raw_ces2004 == "some secondary / high school" | raw_ces2004 == "completed secondary / high school"] <- 0
table(clean_ces2004)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces2004) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2004), ## number of respondents
                                                   source_id = "ces2004") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_ces2004) ## vector with updates

## ces2006 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces2006 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2006/ces2006.csv",
                                    variable_name = "cps_s3")
table(raw_ces2006, useNA = "always")

#### 2. clean variable
clean_ces2006 <- NA
clean_ces2006[raw_ces2006 == "some technical, caat, cegep,..." | raw_ces2006 == "completed technical, caat, cegep,..." | raw_ces2006 == "some university" 
              | raw_ces2006 == "bachelor's degree" | raw_ces2006 == "master's degree" | raw_ces2006 == "professional degree or doctorate"] <- 1
clean_ces2006[raw_ces2006 == "no schooling" | raw_ces2006 == "some elementary school" | raw_ces2006 == "completed elementary school" 
              | raw_ces2006 == "some secondary / high school" | raw_ces2006 == "completed secondary / high school"] <- 0
table(clean_ces2006)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces2006) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2006), ## number of respondents
                                                   source_id = "ces2006") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_ces2006) ## vector with updates

## ces2008 -------------------------------------------------------------------

# Pas trouvé

## ces2011 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces2011 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2011/ces2011.csv",
                                    variable_name = "CPS11_79")
table(raw_ces2011, useNA = "always")

#### 2. clean variable
clean_ces2011 <- NA
clean_ces2011[raw_ces2011 == "some technical, community college       " | raw_ces2011 == "completed technical, community college  " | raw_ces2011 == "some university                         " 
              | raw_ces2011 == "bachelor's degree                       " | raw_ces2011 == "master's degree                         " | raw_ces2011 == "professional degree or doctorate        "] <- 1
clean_ces2011[raw_ces2011 == "no schooling                            " | raw_ces2011 == "some elementary school                  " | raw_ces2011 == "completed elementary school             " 
              | raw_ces2011 == "some secondary / high school            " | raw_ces2011 == "completed secondary / high school       "] <- 0
table(clean_ces2011)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces2011) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2011), ## number of respondents
                                                   source_id = "ces2011") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_ces2011) ## vector with updates

## ces2015 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces2015 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2015/ces2015.csv",
                                    variable_name = "education")
table(raw_ces2015, useNA = "always")

#### 2. clean variable
clean_ces2015 <- NA
clean_ces2015[raw_ces2015 >= 6 & raw_ces2015 <= 11] <- 1
clean_ces2015[raw_ces2015 >= 1 & raw_ces2015 <= 5] <- 0
table(clean_ces2015)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces2015) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2015), ## number of respondents
                                                   source_id = "ces2015") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_ces2015) ## vector with updates

## ces2019 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces2019 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2019/ces2019.csv",
                                    variable_name = "cps19_education")
table(raw_ces2019, useNA = "always")

#### 2. clean variable
clean_ces2019 <- NA
clean_ces2019[raw_ces2019 == "Some technical, community college, CEGEP, College Classique" | raw_ces2019 == "Completed technical, community college, CEGEP, College Classique" | raw_ces2019 == "Some university" 
              | raw_ces2019 == "Bachelor's degree" | raw_ces2019 == "Master's degree" | raw_ces2019 == "Professional degree or doctorate"] <- 1
clean_ces2019[raw_ces2019 == "No schooling" | raw_ces2019 == "Some elementary school" | raw_ces2019 == "Completed elementary school" 
              | raw_ces2019 == "Some secondary/ high school" | raw_ces2019 == "Completed secondary/ high school"] <- 0
table(clean_ces2019)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces2019) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2019), ## number of respondents
                                                   source_id = "ces2019") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_ces2019) ## vector with updates

## ces2021 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_ces2021 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2021/ces2021.csv",
                                    variable_name = "cps21_education")
table(raw_ces2021, useNA = "always")

#### 2. clean variable
clean_ces2021 <- NA
clean_ces2021[raw_ces2021 == "Some technical, community college, CEGEP, College Classique" | raw_ces2021 == "Completed technical, community college, CEGEP, College Classique" | raw_ces2021 == "Some university" 
              | raw_ces2021 == "Bachelor's degree" | raw_ces2021 == "Master's degree" | raw_ces2021 == "Professional degree or doctorate"] <- 1
clean_ces2021[raw_ces2021 == "No schooling" | raw_ces2021 == "Some elementary school" | raw_ces2021 == "Completed elementary school" 
              | raw_ces2021 == "Some secondary/ high school" | raw_ces2021 == "Completed secondary/ high school"] <- 0
table(clean_ces2021)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_ces2021) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2021), ## number of respondents
                                                   source_id = "ces2021") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_ces2021) ## vector with updates

## datagotchi_pilot1_2021 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_datagotchi_pilot1_2021 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot1_2021/datagotchi_pilot1_2021.Sav",
                                    variable_name = "SES1")
table(raw_datagotchi_pilot1_2021, useNA = "always")

#### 2. clean variable
clean_datagotchi_pilot1_2021 <- NA
clean_datagotchi_pilot1_2021[raw_datagotchi_pilot1_2021 == 4 | raw_datagotchi_pilot1_2021 == 5 |
                             raw_datagotchi_pilot1_2021 == 6 | raw_datagotchi_pilot1_2021 == 7] <- 1
clean_datagotchi_pilot1_2021[raw_datagotchi_pilot1_2021 == 1 | raw_datagotchi_pilot1_2021 == 3] <- 0
table(clean_datagotchi_pilot1_2021)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_datagotchi_pilot1_2021) <- sondr::generate_survey_ids(n_respondents = length(clean_datagotchi_pilot1_2021), ## number of respondents
                                                   source_id = "datagotchi_pilot1_2021") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_datagotchi_pilot1_2021) ## vector with updates

## january -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_omnibus_1january <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/january/january.Sav",
                                                   variable_name = "S2")
table(raw_omnibus_1january, useNA = "always")

#### 2. clean variable
clean_omnibus_1january <- NA
clean_omnibus_1january[raw_omnibus_1january == 2 | raw_omnibus_1january == 3] <- 1
clean_omnibus_1january[raw_omnibus_1january == 1] <- 0
table(clean_omnibus_1january)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_omnibus_1january) <- sondr::generate_survey_ids(n_respondents = length(clean_omnibus_1january), ## number of respondents
                                                                  source_id = "omnibus_1january") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_omnibus_1january) ## vector with updates

## february -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_omnibus_2february <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/february/february.Sav",
                                             variable_name = "S2")
table(raw_omnibus_2february, useNA = "always")

#### 2. clean variable
clean_omnibus_2february <- NA
clean_omnibus_2february[raw_omnibus_2february == 2 | raw_omnibus_2february == 3] <- 1
clean_omnibus_2february[raw_omnibus_2february == 1] <- 0
table(clean_omnibus_2february)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_omnibus_2february) <- sondr::generate_survey_ids(n_respondents = length(clean_omnibus_2february), ## number of respondents
                                                            source_id = "omnibus_2february") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_omnibus_2february) ## vector with updates

## march -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_omnibus_3march <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/february/february.Sav",
                                              variable_name = "S2")
table(raw_omnibus_3march, useNA = "always")

#### 2. clean variable
clean_omnibus_3march <- NA
clean_omnibus_3march[raw_omnibus_3march == 2 | raw_omnibus_3march == 3] <- 1
clean_omnibus_3march[raw_omnibus_3march == 1] <- 0
table(clean_omnibus_3march)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_omnibus_3march) <- sondr::generate_survey_ids(n_respondents = length(clean_omnibus_3march), ## number of respondents
                                                             source_id = "omnibus_3march") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_omnibus_3march) ## vector with updates

## april -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_omnibus_4april <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/april/april.Sav",
                                           variable_name = "S2")
table(raw_omnibus_4april, useNA = "always")

#### 2. clean variable
clean_omnibus_4april <- NA
clean_omnibus_4april[raw_omnibus_4april == 2 | raw_omnibus_4april == 3] <- 1
clean_omnibus_4april[raw_omnibus_4april == 1] <- 0
table(clean_omnibus_4april)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_omnibus_4april) <- sondr::generate_survey_ids(n_respondents = length(clean_omnibus_4april), ## number of respondents
                                                          source_id = "omnibus_4april") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_omnibus_4april) ## vector with updates

## may -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_omnibus_5may <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/may/may.Sav",
                                           variable_name = "S2")
table(raw_omnibus_5may, useNA = "always")

#### 2. clean variable
clean_omnibus_5may <- NA
clean_omnibus_5may[raw_omnibus_5may == 2 | raw_omnibus_5may == 3] <- 1
clean_omnibus_5may[raw_omnibus_5may == 1] <- 0
table(clean_omnibus_5may)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_omnibus_5may) <- sondr::generate_survey_ids(n_respondents = length(clean_omnibus_5may), ## number of respondents
                                                          source_id = "omnibus_5may") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_omnibus_5may) ## vector with updates

## june -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_omnibus_6june <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/june/june.Sav",
                                         variable_name = "S2")
table(raw_omnibus_6june, useNA = "always")

#### 2. clean variable
clean_omnibus_6june <- NA
clean_omnibus_6june[raw_omnibus_6june == 2 | raw_omnibus_6june == 3] <- 1
clean_omnibus_6june[raw_omnibus_6june == 1] <- 0
table(clean_omnibus_6june)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_omnibus_6june) <- sondr::generate_survey_ids(n_respondents = length(clean_omnibus_6june), ## number of respondents
                                                        source_id = "omnibus_6june") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_omnibus_6june) ## vector with updates

## datagotchi_pilot2_2022 -------------------------------------------------------------------

### ICI IL FAUT MERGER FRANCAIS ET ANGLAIS

# Pas trouvé

## sondage_nationalisme_2022 -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_sondage_nationalisme_2022 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/sondage_nationalisme_2022/sondage_nationalisme_2022.csv",
                                                      variable_name = "education")
table(raw_sondage_nationalisme_2022, useNA = "always")

#### 2. clean variable
clean_sondage_nationalisme_2022 <- NA
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022 == "Technical, community college, CEGEP or College classique" | raw_sondage_nationalisme_2022 == "Collège, CÉGEP ou Collège classique" | clean_sondage_nationalisme_2022 == "Baccalauréat" 
              | raw_sondage_nationalisme_2022 == "Bachelor's degree" | raw_sondage_nationalisme_2022 == "Master's degree" | raw_sondage_nationalisme_2022 == "Maîtrise" | raw_sondage_nationalisme_2022 == "Doctorat"
              | raw_sondage_nationalisme_2022 == "Doctorate"] <- 1
clean_sondage_nationalisme_2022[raw_sondage_nationalisme_2022 == "Aucune scolarité" | raw_sondage_nationalisme_2022 == "No schooling" | raw_sondage_nationalisme_2022 == "École primaire" | raw_sondage_nationalisme_2022 == "Elementary school" 
              | raw_sondage_nationalisme_2022 == "École secondaire" | raw_sondage_nationalisme_2022 == "High school"] <- 0
table(clean_sondage_nationalisme_2022)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_sondage_nationalisme_2022) <- sondr::generate_survey_ids(n_respondents = length(clean_sondage_nationalisme_2022), ## number of respondents
                                                         source_id = "sondage_nationalisme_2022") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_sondage_nationalisme_2022) ## vector with updates

## quorum_mcq_pilote -------------------------------------------------------------------

### ICI IL FAUT MERGER FRANCAIS ET ANGLAIS

#### 1. Get raw gender variable vector
raw_quorum_mcq_pilote_fr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                               variable_name = "ses_education")
table(raw_quorum_mcq_pilote_fr, useNA = "always")

## fix labels
labels <- c("1", "2", "3", "4", "5", "6", "7")
names(labels) <- as.character(c("Aucune scolarité", "École primaire", "École secondaire", "Collège, Cégep ou Collège classique",
                                "Baccalauréat", "Maîtrise", "Doctorat"))
raw_quorum_mcq_pilote_fr <- labels[raw_quorum_mcq_pilote_fr]
table(raw_quorum_mcq_pilote_fr)

raw_quorum_mcq_pilote_en <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                               variable_name = "EN_ses_education")
table(raw_quorum_mcq_pilote_en, useNA = "always")

## fix labels
labels <- c("1", "2", "3", "4", "5", "6", "7")
names(labels) <- as.character(c("No schooling", "Elementary school", "High school", "Technical, community college, CEGEP, or college classique", 
                                "Bachelor's degree", "Master's degree", "Doctorate"))
raw_quorum_mcq_pilote_en <- labels[raw_quorum_mcq_pilote_en]
table(raw_quorum_mcq_pilote_en)

raw_quorum_mcq_pilote <- coalesce(raw_quorum_mcq_pilote_fr, raw_quorum_mcq_pilote_en)
table(raw_quorum_mcq_pilote, useNA = "always")

#### 2. clean variable

#### 2. clean variable
clean_quorum_mcq_pilote <- NA
clean_quorum_mcq_pilote[raw_quorum_mcq_pilote == 4 | raw_quorum_mcq_pilote == 5 
                        | raw_quorum_mcq_pilote == 6 | raw_quorum_mcq_pilote == 7] <- 1
clean_quorum_mcq_pilote[raw_quorum_mcq_pilote == 1 | raw_quorum_mcq_pilote == 2 | raw_quorum_mcq_pilote == 3] <- 0
table(clean_quorum_mcq_pilote)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_quorum_mcq_pilote) <- sondr::generate_survey_ids(n_respondents = length(clean_quorum_mcq_pilote), ## number of respondents
                                                                  source_id = "quorum_mcq_pilote") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_quorum_mcq_pilote) ## vector with updates

## pes_elxn_2022_text -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_pes_elxn_2022 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pes_elxn_2022/pes_elxn_2022_num.csv",
                                          variable_name = "ses_education")
table(raw_pes_elxn_2022, useNA = "always")

#### 2. clean variable
clean_pes_elxn_2022 <- NA
clean_pes_elxn_2022[raw_pes_elxn_2022 == 4 | raw_pes_elxn_2022 == 5 | raw_pes_elxn_2022 == 6 | raw_pes_elxn_2022 == 7] <- 1
clean_pes_elxn_2022[raw_pes_elxn_2022 == 1 | raw_pes_elxn_2022 == 2 | raw_pes_elxn_2022 == 3] <- 0
table(clean_pes_elxn_2022)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_pes_elxn_2022) <- sondr::generate_survey_ids(n_respondents = length(clean_pes_elxn_2022), ## number of respondents
                                                         source_id = "pes_elxn_2022") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_pes_elxn_2022) ## vector with updates

## pco -------------------------------------------------------------------

#### 1. Get raw variable vector
raw_pco <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pco/pco.csv",
                                          variable_name = "Q9.1.Pco2014")
table(raw_pco, useNA = "always")

#### 2. clean variable
clean_pco <- NA
clean_pco[raw_pco == "Some technical, community college, CEGEP, College classique" | raw_pco == "Completed technical, community college, CEGEP, college classique" 
          | raw_pco == "Some university" | raw_pco == "Bachelor's degree" | raw_pco == "Master's degree" | raw_pco == "Professional degree or doctorate"] <- 1
clean_pco[raw_pco == "No schooling" | raw_pco == "Completed elementary school" | raw_pco == "Some secondary / high school" | raw_pco == "Completed secondary / high school"] <- 0
table(clean_pco)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
names(clean_pco) <- sondr::generate_survey_ids(n_respondents = length(clean_pco), ## number of respondents
                                                         source_id = "pco") ## source_id

## 4. add clean to the master output
output_educ <- sondr::match_and_update(main = output_educ, ## vector to update
                                       updates = clean_pco) ## vector with updates

# Output ------------------------------------------------------------------

### FACTORISE, LEVELS, etc.

output_educ <- factor(output_educ)
table(output_educ)

##### SAVE VECTOR WHERE??

saveRDS(output_educ, "_SharedFolder_catalogue-donnees/merging-souverainete/clean/vectors/ses_educ.rds")


