# Packages ----------------------------------------------------------------
library(tidyverse)

# Config ------------------------------------------------------------------
### source config file to generate all the respondent_ids
source("merging-souverainete/config.R")
sample(ids, 20) ### 20 ids random
## empty vector where the clean values will go. same length as the n of ids.
output <- rep(NA, length(ids))
### each element in output is a respondent with a unique respondent id
names(output) <- ids


# Merging and cleaning ----------------------------------------------------

## ces65 -------------------------------------------------------------------

source_id <- "ces65"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1965/ces65.csv",
                                  variable_name = "v313")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == "before 1939"] <- 1920
clean[raw == "between 1939 and 1945"] <- 1942
clean[raw == "between 1946 and 1960"] <- 1953
clean[raw == "after 1960"] <- 1962
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                                 source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                         updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, "ces65"), useNA = "always")

## ces68 -------------------------------------------------------------------

# none

## ces74 -------------------------------------------------------------------

source_id <- "ces74"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1974/ces74.csv",
                            variable_name = "v457")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean <- raw + 1900
clean[clean %in% c(1988, 1999)] <- NA
table(clean)
hist(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id), useNA = "always")

## ces79 -------------------------------------------------------------------

source_id <- "ces79"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1979/ces79.csv",
                            variable_name = "v1518")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean <- raw + 1900
clean[clean %in% c(1988, 1999)] <- NA
table(clean)
hist(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id), useNA = "always")

## ces84 -------------------------------------------------------------------

source_id <- "ces84"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1984/ces84.csv",
                            variable_name = "var402")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean <- raw + 1900
table(clean)
hist(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id), useNA = "always")

## ces88 -------------------------------------------------------------------

source_id <- "ces88"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1988/ces88.csv",
                            variable_name = "n14")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean <- raw
clean[clean %in% c(9998, 9999)] <- NA
table(clean)
hist(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id), useNA = "always")

## ces93 -------------------------------------------------------------------

source_id <- "ces93"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1993/ces93.csv",
                            variable_name = "cpso12")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean <- raw
clean[clean %in% c(9998, 9999)] <- NA
table(clean)
hist(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id), useNA = "always")

## ces97 -------------------------------------------------------------------

source_id <- "ces97"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1997/ces97.csv",
                            variable_name = "cpsm12")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean <- raw
clean[clean %in% c(9998, 9999)] <- NA
table(clean)
hist(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id), useNA = "always")

## ces2000 -------------------------------------------------------------------

source_id <- "ces2000"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2000/ces2000.csv",
                            variable_name = "cpsm12")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean <- raw
clean[clean %in% c(9998, 9999)] <- NA
table(clean)
hist(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id), useNA = "always")

## ces2004 -------------------------------------------------------------------

source_id <- "ces2004"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2004/ces2004.csv",
                            variable_name = "ces04_cps_s13")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean <- raw
clean[clean %in% c(9998, 9999)] <- NA
table(clean)
hist(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id), useNA = "always")

## ces2006 -------------------------------------------------------------------

source_id <- "ces2006"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2006/ces2006.csv",
                            variable_name = "cps_s13")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean <- raw
clean[clean %in% c(9998, 9999)] <- NA
table(clean)
hist(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id), useNA = "always")

## ces2008 -------------------------------------------------------------------

source_id <- "ces2008"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2008/ces2008.csv",
                            variable_name = "cps_s13")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean <- raw
clean[clean %in% c(9998, 9999)] <- NA
table(clean)
hist(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id), useNA = "always")

## ces2011 -------------------------------------------------------------------

source_id <- "ces2011"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2011/ces2011.csv",
                            variable_name = "CPS11_84")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean <- raw
clean[clean %in% c(9998, 9999)] <- NA
table(clean)
hist(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id), useNA = "always")

## ces2015 -------------------------------------------------------------------

source_id <- "ces2015"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2015/ces2015.csv",
                            variable_name = "year_come")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean <- raw
clean[clean %in% c(9998, 9999)] <- NA
table(clean)
hist(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id), useNA = "always")

## ces2019 -------------------------------------------------------------------

source_id <- "ces2019"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2019/ces2019.csv",
                            variable_name = "cps19_imm_year")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean <- as.numeric(raw)
table(clean)
hist(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id), useNA = "always")

## ces2021 -------------------------------------------------------------------

source_id <- "ces2021"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2021/ces2021.csv",
                            variable_name = "cps21_imm_year")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean <- as.numeric(raw)
table(clean)
hist(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id), useNA = "always")

## datagotchi_pilot1_2021 -------------------------------------------------------------------

# none

## january -------------------------------------------------------------------

# none

## february -------------------------------------------------------------------

# none

## march -------------------------------------------------------------------

# none

## april -------------------------------------------------------------------

# none

## may -------------------------------------------------------------------

# none

## june -------------------------------------------------------------------

# none

## datagotchi_pilot2_2022 -------------------------------------------------------------------

# none

## sondage_nationalisme_2022 -------------------------------------------------------------------

# none

## quorum_mcq_pilote -------------------------------------------------------------------

# none

## pes_elxn_2022_text -------------------------------------------------------------------

# none

## pco -------------------------------------------------------------------

source_id <- "pco"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pco/pco.csv",
                            variable_name = "Q13.5.Pco2014")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean <- raw
table(clean)
hist(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id), useNA = "always")

# Output ------------------------------------------------------------------

saveRDS(output, "_SharedFolder_catalogue-donnees/merging-souverainete/clean/vectors/ses_year_canada.rds")
