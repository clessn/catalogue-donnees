# Packages ----------------------------------------------------------------
library(tidyverse)

# Config ------------------------------------------------------------------
source("merging-souverainete/config.R")
sample(ids, 20) ### 20 ids random

## empty vector where the clean values will go. same length as the n of ids.
output_intpol <- rep(NA, length(ids))
### each element in output is a respondent with a unique respondent id
names(output_intpol) <- ids


# Merging and cleaning ----------------------------------------------------

###
# General rule
# try to have the top40-70% as 1
###

## ces65 ------------------------------------------------------------------
#### 1. Get raw age variable vector

raw_ces65 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1965/ces65.csv",
                                  variable_name = "v36")
table(raw_ces65, useNA = "always")

#### 2. clean variable
clean_ces65 <- NA
clean_ces65[raw_ces65 %in% c("a good deal", "some")] <- 1
clean_ces65[raw_ces65 == "not much"] <- 0
table(clean_ces65, useNA = "always")


#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces65) <- sondr::generate_survey_ids(n_respondents = length(clean_ces65), ## number of respondents
                                                 source_id = "ces65") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                       updates = clean_ces65) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "ces65"))

## ces68 -------------------------------------------------------------------

# none

## ces74 -------------------------------------------------------------------

raw_ces74 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1974/ces74.csv",
                                  variable_name = "v12")
table(raw_ces74, useNA = "always")

#### 2. clean variable
clean_ces74 <- NA
clean_ces74[raw_ces74=="very closely" | raw_ces74=="fairly closely"] <- 1
clean_ces74[raw_ces74=="not much at all"] <- 0

table(clean_ces74, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces74
names(clean_ces74) <- sondr::generate_survey_ids(n_respondents = length(clean_ces74), ## number of respondents
                                                 source_id = "ces74") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces74) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "ces74"))

## ces79 -------------------------------------------------------------------

raw_ces79 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1979/ces79.csv",
                                  variable_name = "v1022")
table(raw_ces79, useNA = "always")

#### 2. clean variable
clean_ces79 <- NA
clean_ces79[raw_ces79=="1" | raw_ces79=="2"] <- 1
clean_ces79[raw_ces79=="3"] <- 0

table(clean_ces79, useNA = "always")
table(clean_ces79)[2]/sum(table(clean_ces79))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces79
names(clean_ces79) <- sondr::generate_survey_ids(n_respondents = length(clean_ces79), ## number of respondents
                                                 source_id = "ces79") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces79) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "ces79"))

## ces84 -------------------------------------------------------------------

raw_ces84 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1984/ces84.csv",
                                  variable_name = "var017")
table(raw_ces84, useNA = "always")

#### 2. clean variable
clean_ces84 <- NA
clean_ces84[raw_ces84=="very closely" | raw_ces84=="fairly closely"] <- 1
clean_ces84[raw_ces84=="not much at all"] <- 0

table(clean_ces84, useNA = "always")
table(clean_ces84)[2]/sum(table(clean_ces84))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces84
names(clean_ces84) <- sondr::generate_survey_ids(n_respondents = length(clean_ces84), ## number of respondents
                                                 source_id = "ces84") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces84) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "ces84"))

## ces88 -------------------------------------------------------------------

raw_ces88 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1988/ces88.csv",
                                  variable_name = "a7")
table(raw_ces88, useNA = "always")

#### 2. clean variable
clean_ces88 <- NA
clean_ces88[raw_ces88=="very closely" | raw_ces88=="fairly closely"] <- 1
clean_ces88[raw_ces88=="not at all" | raw_ces88=="not very closely"] <- 0

table(clean_ces88, useNA = "always")
table(clean_ces88)[2]/sum(table(clean_ces88))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces88
names(clean_ces88) <- sondr::generate_survey_ids(n_respondents = length(clean_ces88), ## number of respondents
                                                 source_id = "ces88") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces88) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "ces88"))

## ces93 -------------------------------------------------------------------

raw_ces93 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1993/ces93.csv",
                                  variable_name = "cpsb1")
table(raw_ces93, useNA = "always")

#### 2. clean variable
clean_ces93 <- NA
clean_ces93[raw_ces93=="very interested" | raw_ces93=="fairly"] <- 1
clean_ces93[raw_ces93=="not at all" | raw_ces93=="not very"] <- 0

table(clean_ces93, useNA = "always")
table(clean_ces93)[2]/sum(table(clean_ces93))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces93
names(clean_ces93) <- sondr::generate_survey_ids(n_respondents = length(clean_ces93), ## number of respondents
                                                 source_id = "ces93") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces93) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "ces93"))

## ces97 -------------------------------------------------------------------

raw_ces97 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1997/ces97.csv",
                                  variable_name = "cpsb5")
table(raw_ces97, useNA = "always")

#### 2. clean variable
clean_ces97 <- NA
clean_ces97[raw_ces97=="6" | raw_ces97=="7" | raw_ces97=="8" | raw_ces97=="9" | raw_ces97=="10"] <- 1
clean_ces97[raw_ces97=="5" | raw_ces97=="4" | raw_ces97=="3" |
              raw_ces97=="2" | raw_ces97=="1" | raw_ces97=="0"] <- 0

table(clean_ces97, useNA = "always")
table(clean_ces97)[2]/sum(table(clean_ces97))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces97
names(clean_ces97) <- sondr::generate_survey_ids(n_respondents = length(clean_ces97), ## number of respondents
                                                 source_id = "ces97") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces97) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "ces97"))

## ces2000 -------------------------------------------------------------------
raw_ces2000 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2000/ces2000.csv",
                                  variable_name = "cpsb5")
table(raw_ces2000, useNA = "always")

#### 2. clean variable
clean_ces2000 <- NA
clean_ces2000[raw_ces2000=="7" | raw_ces2000=="6" | raw_ces2000=="8" | raw_ces2000=="9" | raw_ces2000=="10"] <- 1
clean_ces2000[raw_ces2000=="5" | raw_ces2000=="4" | raw_ces2000=="3" |
              raw_ces2000=="2" | raw_ces2000=="1" | raw_ces2000=="0"] <- 0

table(clean_ces2000, useNA = "always")
table(clean_ces2000)[2]/sum(table(clean_ces2000))


#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2000
names(clean_ces2000) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2000), ## number of respondents
                                                 source_id = "ces2000") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces2000) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "ces2000"))

## ces2004 -------------------------------------------------------------------

raw_ces2004 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2004/ces2004.csv",
                                    variable_name = "ces04_cps_a6")
table(raw_ces2004, useNA = "always")

#### 2. clean variable
clean_ces2004 <- NA
clean_ces2004[raw_ces2004=="7" | raw_ces2004=="6" | raw_ces2004=="8" | raw_ces2004=="9" | raw_ces2004=="10"] <- 1
clean_ces2004[raw_ces2004=="5" | raw_ces2004=="4" | raw_ces2004=="3" |
                raw_ces2004=="2" | raw_ces2004=="1" | raw_ces2004=="0"] <- 0

table(clean_ces2004, useNA = "always")
table(clean_ces2004)[2]/sum(table(clean_ces2004))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2004
names(clean_ces2004) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2004), ## number of respondents
                                                   source_id = "ces2004") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces2004) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "ces2004"))

## ces2006 -------------------------------------------------------------------

raw_ces2006 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2006/ces2006.csv",
                                    variable_name = "cps_a806")
table(raw_ces2006, useNA = "always")

#### 2. clean variable
clean_ces2006 <- NA
clean_ces2006[raw_ces2006=="7" | raw_ces2006=="6" | raw_ces2006=="8" | raw_ces2006=="9" | raw_ces2006=="10"] <- 1
clean_ces2006[raw_ces2006=="5" | raw_ces2006=="4" | raw_ces2006=="3" |
                raw_ces2006=="2" | raw_ces2006=="1" | raw_ces2006=="0"] <- 0

table(clean_ces2006, useNA = "always")
table(clean_ces2006)[2]/sum(table(clean_ces2006))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2006
names(clean_ces2006) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2006), ## number of respondents
                                                   source_id = "ces2006") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces2006) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "ces2006"))

## ces2008 -------------------------------------------------------------------

raw_ces2008 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2008/ces2008.csv",
                                    variable_name = "cps_a4")
table(raw_ces2008, useNA = "always")

#### 2. clean variable
clean_ces2008 <- NA
clean_ces2008[raw_ces2008=="7" | raw_ces2008=="6" | raw_ces2008=="8" | raw_ces2008=="9" | raw_ces2008=="10"] <- 1
clean_ces2008[raw_ces2008=="5" | raw_ces2008=="4" | raw_ces2008=="3" |
                raw_ces2008=="2" | raw_ces2008=="1" | raw_ces2008=="0"] <- 0

table(clean_ces2008, useNA = "always")
table(clean_ces2008)[2]/sum(table(clean_ces2008))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2008
names(clean_ces2008) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2008), ## number of respondents
                                                   source_id = "ces2008") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces2008) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "ces2008"))

## ces2011 -------------------------------------------------------------------

raw_ces2011 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2011/ces2011.csv",
                                    variable_name = "PES11_60")
table(raw_ces2011, useNA = "always")

#### 2. clean variable
clean_ces2011 <- NA
clean_ces2011[raw_ces2011=="7" | raw_ces2011=="6" | raw_ces2011=="8" | raw_ces2011=="9" | raw_ces2011=="10"] <- 1
clean_ces2011[raw_ces2011=="5" | raw_ces2011=="4" | raw_ces2011=="3" |
                raw_ces2011=="2" | raw_ces2011=="1" | raw_ces2011=="0"] <- 0

table(clean_ces2011, useNA = "always")
table(clean_ces2011)[2]/sum(table(clean_ces2011))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2011
names(clean_ces2011) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2011), ## number of respondents
                                                   source_id = "ces2011") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces2011) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "ces2011"))

## ces2015 -------------------------------------------------------------------

raw_ces2015 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2015/ces2015.csv",
                                    variable_name = "p_intpol")
table(raw_ces2015, useNA = "always")

#### 2. clean variable
clean_ces2015 <- NA
clean_ces2015[raw_ces2015=="7" | raw_ces2015=="6" | raw_ces2015=="8" | raw_ces2015=="9" | raw_ces2015=="10"] <- 1
clean_ces2015[raw_ces2015=="5" | raw_ces2015=="4" | raw_ces2015=="3" |
                raw_ces2015=="2" | raw_ces2015=="1" | raw_ces2015=="0"] <- 0

table(clean_ces2015, useNA = "always")
table(clean_ces2015)[2]/sum(table(clean_ces2015))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2015
names(clean_ces2015) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2015), ## number of respondents
                                                   source_id = "ces2015") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces2015) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "ces2015"))

## ces2019 -------------------------------------------------------------------

raw_ces2019 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2019/ces2019.csv",
                                    variable_name = "cps19_interest_gen_1")
table(raw_ces2019, useNA = "always")

#### 2. clean variable
clean_ces2019 <- NA
clean_ces2019[raw_ces2019=="7" | raw_ces2019=="6" | raw_ces2019=="8" | raw_ces2019=="9" | raw_ces2019=="10"] <- 1
clean_ces2019[raw_ces2019=="5" | raw_ces2019=="4" | raw_ces2019=="3" |
                raw_ces2019=="2" | raw_ces2019=="1" | raw_ces2019=="0"] <- 0

table(clean_ces2019, useNA = "always")
table(clean_ces2019)[2]/sum(table(clean_ces2019))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2019
names(clean_ces2019) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2019), ## number of respondents
                                                   source_id = "ces2019") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces2019) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "ces2019"))

## ces2021 -------------------------------------------------------------------

raw_ces2021 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2021/ces2021.csv",
                                    variable_name = "cps21_interest_gen_1")
table(raw_ces2021, useNA = "always")

#### 2. clean variable
clean_ces2021 <- NA
clean_ces2021[raw_ces2021=="7" | raw_ces2021=="6" | raw_ces2021=="8" | raw_ces2021=="9" | raw_ces2021=="10"] <- 1
clean_ces2021[raw_ces2021=="5" | raw_ces2021=="4" | raw_ces2021=="3" |
                raw_ces2021=="2" | raw_ces2021=="1" | raw_ces2021=="0"] <- 0

table(clean_ces2021, useNA = "always")
table(clean_ces2021)[2]/sum(table(clean_ces2021))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2021
names(clean_ces2021) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2021), ## number of respondents
                                                   source_id = "ces2021") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_ces2021) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "ces2021"))

## datagotchi_pilot1_2021 ----------------------------------------------------

## january -------------------------------------------------------------------

## february -------------------------------------------------------------------

## march -------------------------------------------------------------------

## april -------------------------------------------------------------------

## may -------------------------------------------------------------------

raw_may <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/may/may.Sav",
                                    variable_name = "C25")
table(raw_may, useNA = "always")

#### 2. clean variable
clean_may <- NA
clean_may[raw_may=="1" | raw_may=="2"] <- 1
clean_may[raw_may=="3" | raw_may=="4" | raw_may=="5"] <- 0

table(clean_may, useNA = "always")
table(clean_may)[2]/sum(table(clean_may))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = may
names(clean_may) <- sondr::generate_survey_ids(n_respondents = length(clean_may), ## number of respondents
                                                   source_id = "may") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_may) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "may"))

## june -------------------------------------------------------------------

raw_june <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/june/june.Sav",
                                variable_name = "C25")
table(raw_june, useNA = "always")

#### 2. clean variable
clean_june <- NA
clean_june[raw_june=="1" | raw_june=="2"] <- 1
clean_june[raw_june=="3" | raw_june=="4" | raw_june=="5"] <- 0

table(clean_june, useNA = "always")
table(clean_june)[2]/sum(table(clean_june))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = june
names(clean_june) <- sondr::generate_survey_ids(n_respondents = length(clean_june), ## number of respondents
                                               source_id = "june") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_june) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "june"))

## datagotchi_pilot2_2022 ---------------------------------------------------------------

raw_datgot22.1 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                 variable_name = "FR_scalePol")
table(raw_datgot22.1, useNA = "always")
raw_datgot22.2 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                       variable_name = "EN_knowPol")
table(raw_datgot22.2, useNA = "always")
raw_datgot22 <- coalesce(raw_datgot22.1, raw_datgot22.2)

table(raw_datgot22, useNA = "always")

#### 2. clean variable
clean_datgot22 <- NA
clean_datgot22[raw_datgot22=="1" | raw_datgot22=="4"] <- 1
clean_datgot22[raw_datgot22=="5" | raw_datgot22=="6" | raw_datgot22=="7"] <- 0

table(clean_datgot22, useNA = "always")
table(clean_datgot22)[2]/sum(table(clean_datgot22))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = datgot22
names(clean_datgot22) <- sondr::generate_survey_ids(n_respondents = length(clean_datgot22), ## number of respondents
                                                source_id = "datagotchi_pilot2_2022") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_datgot22) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "datagotchi_pilot2_2022"))

## sondage_nationalisme_2022 --------------------------------------------------

raw_sondnat22 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/sondage_nationalisme_2022/sondage_nationalisme_2022.csv",
                                    variable_name = "interest_politics_1")
table(raw_sondnat22, useNA = "always")

#### 2. clean variable
clean_sondnat22 <- NA
clean_sondnat22[raw_sondnat22=="7" | raw_sondnat22=="6" | raw_sondnat22=="8" | raw_sondnat22=="9" | raw_sondnat22=="10"] <- 1
clean_sondnat22[raw_sondnat22=="5" | raw_sondnat22=="4" | raw_sondnat22=="3" |
                raw_sondnat22=="2" | raw_sondnat22=="1" | raw_sondnat22=="0"] <- 0

table(clean_sondnat22, useNA = "always")

table(clean_sondnat22)[2]/sum(table(clean_sondnat22))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = sondnat22
names(clean_sondnat22) <- sondr::generate_survey_ids(n_respondents = length(clean_sondnat22), ## number of respondents
                                                   source_id = "sondage_nationalisme_2022") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_sondnat22) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "sondage_nationalisme_2022"))

## quorum_mcq_pilote -------------------------------------------------------------------

## pes_elxn_2022_text ---------------------------------------------------------

raw_pes22 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pes_elxn_2022/pes_elxn_2022_text.csv",
                                      variable_name = "pol_knowledge")
table(raw_pes22, useNA = "always")

#### 2. clean variable
clean_pes22 <- NA
clean_pes22[raw_pes22=="Très élevé" | raw_pes22=="Élevé"] <- 1
clean_pes22[raw_pes22=="Ni élevé ni faible" | raw_pes22=="Très faible"] <- 0

table(clean_pes22, useNA = "always")
table(clean_pes22)[2]/sum(table(clean_pes22))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = pes22
names(clean_pes22) <- sondr::generate_survey_ids(n_respondents = length(clean_pes22), ## number of respondents
                                                     source_id = "pes_elxn_2022_text") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_pes22) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "pes_elxn_2022_text"))

## pco -------------------------------------------------------------------

raw_pco <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pco/pco.csv",
                                  variable_name = "Q6.1.Pco2014")
table(raw_pco, useNA = "always")

#### 2. clean variable
clean_pco <- NA
clean_pco[raw_pco=="Very interested"] <- 1
clean_pco[raw_pco=="Not interested at all" | raw_pco=="Not very interested" | raw_pco=="Somewhat interested"] <- 0

table(clean_pco, useNA = "always")
table(clean_pco)[2]/sum(table(clean_pco))

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = pco
names(clean_pco) <- sondr::generate_survey_ids(n_respondents = length(clean_pco), ## number of respondents
                                                 source_id = "pco") ## source_id

## 4. add clean to the master output
output_intpol <- sondr::match_and_update(main = output_intpol, ## vector to update
                                         updates = clean_pco) ## vector with updates

table(sondr::extract_elements_with_prefix(output_intpol, "pco"))

# Output ------------------------------------------------------------------

### FACTORISE, LEVELS, etc.

##### SAVE VECTOR WHERE??
saveRDS(output_intpol, "_SharedFolder_catalogue-donnees/merging-souverainete/clean/vectors/int_pol.rds")
