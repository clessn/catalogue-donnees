# Packages ----------------------------------------------------------------
library(tidyverse)

# Config ------------------------------------------------------------------
source("merging-souverainete/config.R")
sample(ids, 20) ### 20 ids random
## empty vector where the clean values will go. same length as the n of ids.
output_num <- rep(NA, length(ids))
output_centile <- rep(NA, length(ids))
### each element in output is a respondent with a unique respondent id
names(output_num) <- ids
names(output_centile) <- ids

### THIS FILE WILL CREATE A VECTOR CONTAINING RAW INCOME AND RELATIVE INCOME (in quantiles)

# Custom function ---------------------------------------------------------

parse_money_vector <- function(values, sep, floor, ceiling, ceiling_increment = 10000) {
  sapply(values, function(value) {
    if (is.na(value) || value == "") {
      return(NA)
    }
    if (value == floor) {
      round(mean(sondr::parse_money_range(value, limit = "floor")))
    } else if (value == ceiling) {
      round(mean(sondr::parse_money_range(value, limit = "ceiling", ceiling_increment = ceiling_increment)))
    } else {
      round(mean(sondr::parse_money_range(value, sep)))
    }
  })
}

# Merging and cleaning ----------------------------------------------------

## ces65 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1965/ces65.csv",
                                  variable_name = "v336")
table(raw, useNA = "always")

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "to",
                                floor = "under $1,000",
                                ceiling = "$15,000 or over",
                                ceiling_increment = 5000)
table(clean_num)

## to quantile
clean_centile <- round(ecdf(clean_num)(clean_num)*100)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_num) <- sondr::generate_survey_ids(n_respondents = length(clean_num), ## number of respondents
                                                      source_id = "ces65") ## source_id
names(clean_centile) <- sondr::generate_survey_ids(n_respondents = length(clean_centile), ## number of respondents
                                                        source_id = "ces65") ## source_id

## 4. add clean to the master output
output_num <- sondr::match_and_update(main = output_num, ## vector to update
                                         updates = clean_num) ## vector with updates
output_centile <- sondr::match_and_update(main = output_centile, ## vector to update
                                      updates = clean_centile) ## vector with updates


## ces68 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1968/ces68.csv",
                                  variable_name = "incom2")
table(raw, useNA = "always")

## fix labels
labels <- c("UNDER $1,000", "$1,000 to $1,999", "$2,000 to $2,999", "$3,000 to $3,999", "$4,000 to $4,999", "$5,000 to $5,999", "$6,000 to $6,999", "$7,000 to $7,999", "$8,000 to $8,999", "$10,000 to $14,999", "$15,000 or over")
names(labels) <- as.character(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
raw <- labels[raw]
table(raw)

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "to",
                                floor = "UNDER $1,000",
                                ceiling = "$15,000 or over",
                                ceiling_increment = 5000)
table(clean_num)

## to quantile
clean_centile <- round(ecdf(clean_num)(clean_num)*100)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_num) <- sondr::generate_survey_ids(n_respondents = length(clean_num), ## number of respondents
                                                     source_id = "ces68") ## source_id
names(clean_centile) <- sondr::generate_survey_ids(n_respondents = length(clean_centile), ## number of respondents
                                                         source_id = "ces68") ## source_id

## 4. add clean to the master output
output_num <- sondr::match_and_update(main = output_num, ## vector to update
                                      updates = clean_num) ## vector with updates
output_centile <- sondr::match_and_update(main = output_centile, ## vector to update
                                          updates = clean_centile) ## vector with updates

hist(output_num)
hist(output_centile)

## ces74 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1974/ces74.csv",
                            variable_name = "v479")
table(raw, useNA = "always")

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "-",
                                floor = "less than $3,000 per year",
                                ceiling = "$20,000 or more per year",
                                ceiling_increment = 3000)
table(clean_num)

## to quantile
clean_centile <- round(ecdf(clean_num)(clean_num)*100)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_num) <- sondr::generate_survey_ids(n_respondents = length(clean_num), ## number of respondents
                                               source_id = "ces74") ## source_id
names(clean_centile) <- sondr::generate_survey_ids(n_respondents = length(clean_centile), ## number of respondents
                                                   source_id = "ces74") ## source_id

## 4. add clean to the master output
output_num <- sondr::match_and_update(main = output_num, ## vector to update
                                      updates = clean_num) ## vector with updates
output_centile <- sondr::match_and_update(main = output_centile, ## vector to update
                                          updates = clean_centile) ## vector with updates

table(output_num)
table(output_centile)

## ces79 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1979/ces79.csv",
                            variable_name = "v1516")
table(raw, useNA = "always")

## fix labels
labels <- c("LESS THAN $3000 PER YEAR", "$3000 - $4999 PER YEAR", "$5000 - $7499 PER YEAR", "$7500 - $9999 PER YEAR", "$10000 - $14999 PER YEAR", "$15000 - $16999 PER YEAR", "$17000 - $19999 PER YEAR", "$20000 OR MORE PER YEAR")
names(labels) <- as.character(c(1, 2, 3, 4, 5, 6, 7, 8))
raw <- labels[raw]
table(raw)


#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "-",
                                floor = "LESS THAN $3000 PER YEAR",
                                ceiling = "$20000 OR MORE PER YEAR",
                                ceiling_increment = 3000)
table(clean_num)

## to quantile
clean_centile <- round(ecdf(clean_num)(clean_num)*100)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_num) <- sondr::generate_survey_ids(n_respondents = length(clean_num), ## number of respondents
                                               source_id = "ces79") ## source_id
names(clean_centile) <- sondr::generate_survey_ids(n_respondents = length(clean_centile), ## number of respondents
                                                   source_id = "ces79") ## source_id

## 4. add clean to the master output
output_num <- sondr::match_and_update(main = output_num, ## vector to update
                                      updates = clean_num) ## vector with updates
output_centile <- sondr::match_and_update(main = output_centile, ## vector to update
                                          updates = clean_centile) ## vector with updates

table(output_num)
table(output_centile)

## ces84 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw1 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1984/ces84.csv",
                            variable_name = "var442")
table(raw1, useNA = "always")

raw2 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1984/ces84.csv",
                            variable_name = "var443")
table(raw2, useNA = "always")

table(raw1, raw2, useNA = "always")

raw1[raw1 %in% c("dont know", "nothing", "refused")] <- NA
raw2[raw2 %in% c("na", "nothing", "unable to estimate")] <- NA

raw <- coalesce(raw1, raw2)

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "-",
                                floor = "under $5,000",
                                ceiling = "$100,000 & over",
                                ceiling_increment = 30000)
table(clean_num)

## to quantile
clean_centile <- round(ecdf(clean_num)(clean_num)*100)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_num) <- sondr::generate_survey_ids(n_respondents = length(clean_num), ## number of respondents
                                               source_id = "ces84") ## source_id
names(clean_centile) <- sondr::generate_survey_ids(n_respondents = length(clean_centile), ## number of respondents
                                                   source_id = "ces84") ## source_id

## 4. add clean to the master output
output_num <- sondr::match_and_update(main = output_num, ## vector to update
                                      updates = clean_num) ## vector with updates
output_centile <- sondr::match_and_update(main = output_centile, ## vector to update
                                          updates = clean_centile) ## vector with updates

table(output_num)
hist(output_num)
table(output_centile)


## ces88 -------------------------------------------------------------------

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1988/ces88.csv",
                             variable_name = "n19")
table(raw, useNA = "always")

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "-",
                                floor = "<$10,000",
                                ceiling = "$80,000 or more",
                                ceiling_increment = 10000)
table(clean_num)

## to quantile
clean_centile <- round(ecdf(clean_num)(clean_num)*100)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_num) <- sondr::generate_survey_ids(n_respondents = length(clean_num), ## number of respondents
                                               source_id = "ces88") ## source_id
names(clean_centile) <- sondr::generate_survey_ids(n_respondents = length(clean_centile), ## number of respondents
                                                   source_id = "ces88") ## source_id

## 4. add clean to the master output
output_num <- sondr::match_and_update(main = output_num, ## vector to update
                                      updates = clean_num) ## vector with updates
output_centile <- sondr::match_and_update(main = output_centile, ## vector to update
                                          updates = clean_centile) ## vector with updates

table(output_num)
hist(output_num)
table(output_centile)



## ces93 -------------------------------------------------------------------

source_id <- "ces93"

#### 1. Get raw gender variable vector
raw1 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1993/ces93.csv",
                            variable_name = "cpso18")
table(raw1, useNA = "always")

## remove 998 999 who are dont know refused
raw1[raw1 %in% c(998, 999)] <- NA
raw1 <- raw1*1000
table(raw1)

raw2 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1993/ces93.csv",
                             variable_name = "cpso18a")
table(raw2, useNA = "always")
raw2[raw2 %in% c("d.k.", "refused")] <- NA

#### 2. clean variable
raw2 <- parse_money_vector(values = raw2,
                                sep = "-",
                                floor = "<$20,000",
                                ceiling = "$100,000+",
                                ceiling_increment = 10000)
table(raw2)

clean_num <- coalesce(raw1, raw2)
hist(clean_num)

## to quantile
clean_centile <- round(ecdf(clean_num)(clean_num)*100)
hist(clean_centile)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_num) <- sondr::generate_survey_ids(n_respondents = length(clean_num), ## number of respondents
                                               source_id = source_id) ## source_id
names(clean_centile) <- sondr::generate_survey_ids(n_respondents = length(clean_centile), ## number of respondents
                                                   source_id = source_id) ## source_id

## 4. add clean to the master output
output_num <- sondr::match_and_update(main = output_num, ## vector to update
                                      updates = clean_num) ## vector with updates
output_centile <- sondr::match_and_update(main = output_centile, ## vector to update
                                          updates = clean_centile) ## vector with updates

table(output_num)
hist(output_num)
table(output_centile)

## ces97 -------------------------------------------------------------------

source_id <- "ces97"

#### 1. Get raw gender variable vector
raw1 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1997/ces97.csv",
                             variable_name = "cpsm16")
table(raw1, useNA = "always")

## remove 998 999 who are dont know refused
raw1[raw1 %in% c(998, 999)] <- NA
raw1 <- raw1*1000
table(raw1)

raw2 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1997/ces97.csv",
                             variable_name = "cpsm16a")
table(raw2, useNA = "always")
raw2[raw2 %in% c("d.k.", "refused")] <- NA

#### 2. clean variable
raw2 <- parse_money_vector(values = raw2,
                           sep = "-",
                           floor = "<$20,000",
                           ceiling = ">100,000",
                           ceiling_increment = 10000)
table(raw2)

clean_num <- coalesce(raw1, raw2)
hist(clean_num)

## to quantile
clean_centile <- round(ecdf(clean_num)(clean_num)*100)
hist(clean_centile)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_num) <- sondr::generate_survey_ids(n_respondents = length(clean_num), ## number of respondents
                                               source_id = source_id) ## source_id
names(clean_centile) <- sondr::generate_survey_ids(n_respondents = length(clean_centile), ## number of respondents
                                                   source_id = source_id) ## source_id

## 4. add clean to the master output
output_num <- sondr::match_and_update(main = output_num, ## vector to update
                                      updates = clean_num) ## vector with updates
output_centile <- sondr::match_and_update(main = output_centile, ## vector to update
                                          updates = clean_centile) ## vector with updates

table(output_num)
hist(output_num)
table(output_centile)
hist(output_centile)

## ces2000 -------------------------------------------------------------------

source_id <- "ces2000"

#### 1. Get raw gender variable vector
raw1 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2000/ces2000.csv",
                             variable_name = "cpsm16")
table(raw1, useNA = "always")

## remove 998 999 who are dont know refused
raw1[raw1 %in% c(998, 999)] <- NA
raw1 <- raw1*1000
table(raw1)

raw2 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2000/ces2000.csv",
                             variable_name = "cpsm16a")
table(raw2, useNA = "always")
raw2[raw2 %in% c("don't know", "refused")] <- NA

#### 2. clean variable
raw2 <- parse_money_vector(values = raw2,
                           sep = "-",
                           floor = "less than $20,000",
                           ceiling = "more than $100,000",
                           ceiling_increment = 10000)
table(raw2)

clean_num <- coalesce(raw1, raw2)
hist(clean_num)

## to quantile
clean_centile <- round(ecdf(clean_num)(clean_num)*100)
hist(clean_centile)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_num) <- sondr::generate_survey_ids(n_respondents = length(clean_num), ## number of respondents
                                               source_id = source_id) ## source_id
names(clean_centile) <- sondr::generate_survey_ids(n_respondents = length(clean_centile), ## number of respondents
                                                   source_id = source_id) ## source_id

## 4. add clean to the master output
output_num <- sondr::match_and_update(main = output_num, ## vector to update
                                      updates = clean_num) ## vector with updates
output_centile <- sondr::match_and_update(main = output_centile, ## vector to update
                                          updates = clean_centile) ## vector with updates

table(output_num)
hist(output_num)
table(output_centile)
hist(output_centile)

## ces2004 -------------------------------------------------------------------

source_id <- "ces2004"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2004/ces2004.csv",
                            variable_name = "ces04_cps_s18")
table(raw, useNA = "always")

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "-",
                                floor = "less than $20,000",
                                ceiling = "more than $100,000",
                                ceiling_increment = 10000)
table(clean_num)

## to quantile
clean_centile <- round(ecdf(clean_num)(clean_num)*100)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_num) <- sondr::generate_survey_ids(n_respondents = length(clean_num), ## number of respondents
                                               source_id = source_id) ## source_id
names(clean_centile) <- sondr::generate_survey_ids(n_respondents = length(clean_centile), ## number of respondents
                                                   source_id = source_id) ## source_id

## 4. add clean to the master output
output_num <- sondr::match_and_update(main = output_num, ## vector to update
                                      updates = clean_num) ## vector with updates
output_centile <- sondr::match_and_update(main = output_centile, ## vector to update
                                          updates = clean_centile) ## vector with updates

table(output_num)
hist(output_num)
table(output_centile)
hist(output_centile)


## ces2006 -------------------------------------------------------------------

source_id <- "ces2006"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2006/ces2006.csv",
                            variable_name = "cps_s18")
table(raw, useNA = "always")

## fix labels
labels <- c("less than $20,000", "between $20,000 and $30,000", "between $30,000 and $40,000", "between $40,000 and $50,000", "between $50,000 and $60,000", "between $60,000 and $70,000", "between $70,000 and $80,000", "between $80,000 and $90,000", "between $90,000 and $100,000", "more than $100,000")
names(labels) <- as.character(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
raw <- labels[raw]
table(raw)

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "and",
                                floor = "less than $20,000",
                                ceiling = "more than $100,000",
                                ceiling_increment = 10000)
table(clean_num)

## to quantile
clean_centile <- round(ecdf(clean_num)(clean_num)*100)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_num) <- sondr::generate_survey_ids(n_respondents = length(clean_num), ## number of respondents
                                               source_id = source_id) ## source_id
names(clean_centile) <- sondr::generate_survey_ids(n_respondents = length(clean_centile), ## number of respondents
                                                   source_id = source_id) ## source_id

## 4. add clean to the master output
output_num <- sondr::match_and_update(main = output_num, ## vector to update
                                      updates = clean_num) ## vector with updates
output_centile <- sondr::match_and_update(main = output_centile, ## vector to update
                                          updates = clean_centile) ## vector with updates

table(output_num)
hist(output_num)
table(output_centile)
hist(output_centile)

## ces2008 -------------------------------------------------------------------

source_id <- "ces2008"

#### 1. Get raw gender variable vector
raw1 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2008/ces2008.csv",
                             variable_name = "cps_s18a")
table(raw1, useNA = "always")

## remove 998 999 who are dont know refused
raw1[raw1 %in% c(998, 999)] <- NA
raw1 <- raw1*1000
table(raw1)

raw2 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2008/ces2008.csv",
                             variable_name = "cps_s18b")
table(raw2, useNA = "always")
raw2[raw2 %in% c(98, 99)] <- NA

### fix labels
labels <- c("less than $20,000", "between $20,000 and $30,000", 
            "between $30,000 and $40,000", "between $40,000 and $50,000", 
            "between $50,000 and $60,000", "between $60,000 and $70,000", 
            "between $70,000 and $80,000", "between $80,000 and $90,000", 
            "between $90,000 and $100,000", "between $100,000 and $110,000", 
            "between $110,000 and $120,000", "more than $120,000")
names(labels) <- as.character(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
raw2 <- labels[raw2]
table(raw2)

#### 2. clean variable
raw2 <- parse_money_vector(values = raw2,
                           sep = "and",
                           floor = "less than $20,000",
                           ceiling = "more than $120,000",
                           ceiling_increment = 10000)
table(raw2)

clean_num <- coalesce(raw1, raw2)
hist(clean_num)

## to quantile
clean_centile <- round(ecdf(clean_num)(clean_num)*100)
hist(clean_centile)

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces65
names(clean_num) <- sondr::generate_survey_ids(n_respondents = length(clean_num), ## number of respondents
                                               source_id = source_id) ## source_id
names(clean_centile) <- sondr::generate_survey_ids(n_respondents = length(clean_centile), ## number of respondents
                                                   source_id = source_id) ## source_id

## 4. add clean to the master output
output_num <- sondr::match_and_update(main = output_num, ## vector to update
                                      updates = clean_num) ## vector with updates
output_centile <- sondr::match_and_update(main = output_centile, ## vector to update
                                          updates = clean_centile) ## vector with updates

table(output_num)
hist(output_num)
table(output_centile)
hist(output_centile)

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

