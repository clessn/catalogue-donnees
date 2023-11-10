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


output_gender["ces65.2118"]
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

main <- output_gender
updates <- clean_ces68

main[names(main) %in% names(updates)] <- updates[names(main)[4500:4885] %in% 
                                                   names(updates)]


output_gender["ces68.1288"]

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

output_gender["ces74.250"]

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


output_gender["ces79.218"]
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


output_gender["ces84.18"]
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


output_gender["ces88.2118"]
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


output_gender["ces93.211"]
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


output_gender["ces97.2118"]
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


output_gender["ces2000.211"]
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


output_gender["ces2004.2118"]
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


output_gender["ces2006.2118"]
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


output_gender["ces2008.211"]
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


output_gender["ces2011.211"]
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


output_gender["ces2015.211"]
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


output_gender["ces2019.211"]
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


output_gender["ces2021.211"]
## datagotchi_pilot1_2021 -------------------------------------------------------------------
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


output_gender["ces2011.211"]
## january -------------------------------------------------------------------

## february -------------------------------------------------------------------

## march -------------------------------------------------------------------

## april -------------------------------------------------------------------

## may -------------------------------------------------------------------

## june -------------------------------------------------------------------

## datagotchi_pilot2_2022 -------------------------------------------------------------------

## sondage_nationalisme_2022 -------------------------------------------------------------------

## quorum_mcq_pilote -------------------------------------------------------------------

## pes_elxn_2022_text -------------------------------------------------------------------

## pco -------------------------------------------------------------------


# Output ------------------------------------------------------------------

### FACTORISE, LEVELS, etc.

##### SAVE VECTOR WHERE??

