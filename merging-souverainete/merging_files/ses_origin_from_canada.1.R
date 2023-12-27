# Packages ----------------------------------------------------------------
library(tidyverse)

## 1 = from Canada
## 0 = else

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
                                  variable_name = "v312")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == "canada"] <- 1
clean[raw != "canada"] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                                 source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                         updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## ces68 -------------------------------------------------------------------

# none

## ces74 -------------------------------------------------------------------

source_id <- "ces74"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1974/ces74.csv",
                            variable_name = "v456")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == "canada"] <- 1
clean[raw != "canada"] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## ces79 -------------------------------------------------------------------

source_id <- "ces79"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1979/ces79.csv",
                            variable_name = "v1517")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == 1] <- 1
clean[raw != 1] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## ces84 -------------------------------------------------------------------

source_id <- "ces84"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1984/ces84.csv",
                            variable_name = "var400")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == "canada"] <- 1
clean[raw != "canada"] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## ces88 -------------------------------------------------------------------

source_id <- "ces88"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1988/ces88.csv",
                            variable_name = "n13")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == "canada"] <- 1
clean[raw != "canada"] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## ces93 -------------------------------------------------------------------

source_id <- "ces93"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1993/ces93.csv",
                            variable_name = "cpso11")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == "canada"] <- 1
clean[raw != "canada"] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

table(output)

## ces97 -------------------------------------------------------------------

source_id <- "ces97"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1997/ces97.csv",
                            variable_name = "cpsm11")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == "canada"] <- 1
clean[raw != "canada"] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## ces2000 -------------------------------------------------------------------

source_id <- "ces2000"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2000/ces2000.csv",
                            variable_name = "cpsm11")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == "canada"] <- 1
clean[raw != "canada"] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## ces2004 -------------------------------------------------------------------

source_id <- "ces2004"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2004/ces2004.csv",
                            variable_name = "ces04_cps_s12")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == "canada"] <- 1
clean[raw != "canada"] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## ces2006 -------------------------------------------------------------------

source_id <- "ces2006"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2006/ces2006.csv",
                            variable_name = "cps_s12")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == "canada"] <- 1
clean[raw != "canada"] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## ces2008 -------------------------------------------------------------------

source_id <- "ces2008"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2008/ces2008.csv",
                            variable_name = "cps_s12")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw %in% c("canada", "quebec")] <- 1
clean[!(raw %in% c("canada", "quebec"))] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## ces2011 -------------------------------------------------------------------

source_id <- "ces2011"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2011/ces2011.csv",
                            variable_name = "CPS11_83")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw %in% c("Canada                                       ",
                 "Quebec                                       ")] <- 1
clean[!(raw %in% c("Canada                                       ",
                   "Quebec                                       "))] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## ces2015 -------------------------------------------------------------------

source_id <- "ces2015"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2015/ces2015.csv",
                            variable_name = "cntry_born")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw %in% c(1, 2)] <- 1
clean[!(raw %in% c(1, 2))] <- 0
clean[is.na(raw)] <- NA
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))


## ces2019 -------------------------------------------------------------------

source_id <- "ces2019"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2019/ces2019.csv",
                            variable_name = "cps19_bornin_other")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[is.na(raw)] <- 1
clean[!is.na(raw)] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## ces2021 -------------------------------------------------------------------

source_id <- "ces2021"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2021/ces2021.csv",
                            variable_name = "cps21_bornin_other")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[is.na(raw)] <- 1
clean[!is.na(raw)] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## datagotchi_pilot1_2021 -------------------------------------------------------------------

source_id <- "datagotchi_pilot1_2021"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot1_2021/datagotchi_pilot1_2021.Sav",
                            variable_name = "SES9")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == 1] <- 1
clean[raw != 1] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## january -------------------------------------------------------------------

# none

## february -------------------------------------------------------------------

# none

## march -------------------------------------------------------------------

source_id <- "march"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/march/march.Sav",
                            variable_name = "C2")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == 1] <- 1
clean[raw != 1] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## april -------------------------------------------------------------------

source_id <- "april"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/april/april.Sav",
                            variable_name = "C2")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == 1] <- 1
clean[raw != 1] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## may -------------------------------------------------------------------

source_id <- "may"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/may/may.Sav",
                            variable_name = "C2")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == 1] <- 1
clean[raw != 1] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## june -------------------------------------------------------------------

source_id <- "june"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/june/june.Sav",
                            variable_name = "C2")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == 1] <- 1
clean[raw != 1] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))


## datagotchi_pilot2_2022 -------------------------------------------------------------------

source_id <- "datagotchi_pilot2_2022"

### ICI IL FAUT MERGER FRANCAIS ET ANGLAIS

#### 1. Get raw gender variable vector
raw_fr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                  variable_name = "born")
table(raw_fr, useNA = "always")

raw_en <- as.numeric(sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                               variable_name = "born.1"))
table(raw_en, useNA = "always")

raw <- coalesce(raw_fr, raw_en)
table(raw)

#### 2. clean variable

#### 2. clean variable
clean <- NA
clean[raw %in% c(33, 44)] <- 1
clean[!(raw %in% c(33, 44))] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## sondage_nationalisme_2022 -------------------------------------------------------------------

source_id <- "sondage_nationalisme_2022"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/sondage_nationalisme_2022/sondage_nationalisme_2022.csv",
                            variable_name = "immigration")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw %in% c("Oui", "Yes")] <- 1
clean[raw %in% c("Non", "No")] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## quorum_mcq_pilote -------------------------------------------------------------------

source_id <- "quorum_mcq_pilote"

### ICI IL FAUT MERGER FRANCAIS ET ANGLAIS

#### 1. Get raw gender variable vector
raw_fr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                               variable_name = "ses_birth_country")
table(raw_fr, useNA = "always")
raw_fr[raw_fr == ""] <- NA

raw_en <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                               variable_name = "EN_ses_birth_country")
table(raw_en, useNA = "always")
raw_en[raw_en == ""] <- NA

raw <- coalesce(raw_fr, raw_en)
table(raw)

#### 2. clean variable
clean <- NA
clean[raw == "Canada"] <- 1
clean[raw != "Canada"] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

## pes_elxn_2022_text -------------------------------------------------------------------

source_id <- "pes_elxn_2022_text"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pes_elxn_2022/pes_elxn_2022_text.csv",
                            variable_name = "ses_birth_country")
table(raw, useNA = "always")

#### 2. clean variable
clean <- NA
clean[raw == "Canada"] <- 1
clean[raw != "Canada"] <- 0
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))


## pco -------------------------------------------------------------------

source_id <- "WholeData_Pco14_2015_01_30"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pco/WholeData_Pco14_2015_01_30.csv",
                            variable_name = "countryBornCanada")

table(raw, useNA = "always")

#### 2. clean variable
clean <- raw
table(clean)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean) <- sondr::generate_survey_ids(n_respondents = length(clean), ## number of respondents
                                           source_id = source_id) ## source_id

## 4. add clean to the master output
output <- sondr::match_and_update(main = output, ## vector to update
                                  updates = clean) ## vector with updates

table(sondr::extract_elements_with_prefix(output, source_id))

# Output ------------------------------------------------------------------

table(output)

saveRDS(output, "_SharedFolder_catalogue-donnees/merging-souverainete/clean/vectors/ses_origin_from_canada.1.rds")
