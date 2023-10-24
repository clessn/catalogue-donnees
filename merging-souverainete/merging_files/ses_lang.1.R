# Packages ----------------------------------------------------------------
library(tidyverse)

# Config ------------------------------------------------------------------
source("merging-souverainete/config.R")

## empty vector where the clean values will go. same length as the n of ids.
output_lang <- rep(NA, length(ids))
### each element in output is a respondent with a unique respondent id
names(output_lang) <- ids


# Merging and cleaning ----------------------------------------------------

## ces65 -------------------------------------------------------------------
#### 1. Get raw age variable vector

raw_ces65 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1965/ces65.csv",
                                  variable_name = "v314")
table(raw_ces65, useNA = "always")

#### 2. clean variable
clean_ces65 <- NA
clean_ces65[raw_ces65=="english"] <- "english"
clean_ces65[raw_ces65=="french"] <- "french"
clean_ces65[raw_ces65=="german, dutch, latvian, belgian, lithuanian," | raw_ces65=="italian, spanish, greek, portuguese, maltese"
            | raw_ces65=="other non-european, chinese, japanese, lebanese," | raw_ces65=="scandanavian, finnish, swedish, norwegian, danish," |
              raw_ces65=="slavic, russian, ukranian, polish, armenian, czech,"] <- "other"

table(clean_ces65, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_ces65) <- sondr::generate_survey_ids(n_respondents = length(clean_ces65), ## number of respondents
                                                 source_id = "ces65") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                      updates = clean_ces65) ## vector with updates

## ces68 -------------------------------------------------------------------

#### 1. Get raw age variable vector

raw_ces68 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1968/ces68.csv",
                                  variable_name = "var357")
table(raw_ces68, useNA = "always")

#### 2. clean variable
clean_ces68 <- NA
clean_ces68[raw_ces68=="english"] <- "english"
clean_ces68[raw_ces68=="french"] <- "french"
clean_ces68[raw_ces68=="germ-dutch-oth ger'c" | raw_ces68=="hebrew    jewish "
            | raw_ces68=="hung-finn-estonian" | raw_ces68=="italian   spanish" |
              raw_ces68=="other comb" | raw_ces68=="other sing mentions" | raw_ces68=="ukran pole-oth slavc"
            | raw_ces68=="engl-fren equally"] <- "other"
table(clean_ces68, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces68
names(clean_ces68) <- sondr::generate_survey_ids(n_respondents = length(clean_ces68), ## number of respondents
                                                 source_id = "ces68") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_ces68) ## vector with updates

## ces74 -------------------------------------------------------------------

raw_ces74 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1974/ces74.csv",
                                  variable_name = "v471")
table(raw_ces74, useNA = "always")

#### 2. clean variable
clean_ces74 <- NA
clean_ces74[raw_ces74=="english"] <- "english"
clean_ces74[raw_ces74=="french"] <- "french"
clean_ces74[raw_ces74=="all other combinations" | raw_ces74=="english and french"
            | raw_ces74=="english and one or more other" | raw_ces74=="french and one or more other" |
              raw_ces74=="other"] <- "other"
table(clean_ces74, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces74
names(clean_ces74) <- sondr::generate_survey_ids(n_respondents = length(clean_ces74), ## number of respondents
                                                 source_id = "ces74") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_ces74) ## vector with updates

## ces79 -------------------------------------------------------------------
raw_ces79 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1979/ces79.csv",
                                  variable_name = "v1509")
table(raw_ces79, useNA = "always")

#### 2. clean variable
clean_ces79 <- NA
clean_ces79[raw_ces79=="1"] <- "english"
clean_ces79[raw_ces79=="2"] <- "french"
clean_ces79[raw_ces79!="1" & raw_ces79!="2"] <- "other"
table(clean_ces79, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces79
names(clean_ces79) <- sondr::generate_survey_ids(n_respondents = length(clean_ces79), ## number of respondents
                                                 source_id = "ces79") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_ces79) ## vector with updates

## ces84 -------------------------------------------------------------------

raw_ces84 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1984/ces84.csv",
                                  variable_name = "var374")
table(raw_ces84, useNA = "always")

#### 2. clean variable
clean_ces84 <- NA
clean_ces84[raw_ces84=="english"] <- "english"
clean_ces84[raw_ces84=="french"] <- "french"
clean_ces84[raw_ces84!="english" & raw_ces84!="french" & raw_ces84!="<NA>"] <- "other"
table(clean_ces84, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces84
names(clean_ces84) <- sondr::generate_survey_ids(n_respondents = length(clean_ces84), ## number of respondents
                                                 source_id = "ces84") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_ces84) ## vector with updates

## ces88 -------------------------------------------------------------------

raw_ces88 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1988/ces88.csv",
                                  variable_name = "n16")
table(raw_ces88, useNA = "always")

#### 2. clean variable
clean_ces88 <- NA
clean_ces88[raw_ces88=="english"] <- "english"
clean_ces88[raw_ces88=="french"] <- "french"
clean_ces88[raw_ces88=="other"] <- "other"
table(clean_ces88, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces88
names(clean_ces88) <- sondr::generate_survey_ids(n_respondents = length(clean_ces88), ## number of respondents
                                                 source_id = "ces88") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_ces88) ## vector with updates

## ces93 -------------------------------------------------------------------

## ces97 -------------------------------------------------------------------

## ces2000 -------------------------------------------------------------------

## ces2004 -------------------------------------------------------------------

## ces2006 -------------------------------------------------------------------

## ces2008 -------------------------------------------------------------------

## ces2011 -------------------------------------------------------------------

## ces2015 -------------------------------------------------------------------

## ces2019 -------------------------------------------------------------------

## ces2021 -------------------------------------------------------------------

## datagotchi_pilot1_2021 -------------------------------------------------------------------

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

