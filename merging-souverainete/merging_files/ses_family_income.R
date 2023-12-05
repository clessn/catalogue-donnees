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

table(sondr::extract_elements_with_prefix(output_centile, "ces65"))

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

table(sondr::extract_elements_with_prefix(output_centile, "ces68"))

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

table(sondr::extract_elements_with_prefix(output_centile, "ces74"))

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

table(sondr::extract_elements_with_prefix(output_centile, "ces79"))
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

table(sondr::extract_elements_with_prefix(output_centile, "ces84"))


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

table(sondr::extract_elements_with_prefix(output_centile, "ces88"))

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
table(sondr::extract_elements_with_prefix(output_centile, "ces93"))

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
table(sondr::extract_elements_with_prefix(output_centile, "ces97"))

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

table(sondr::extract_elements_with_prefix(output_centile, "ces2000"))


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
table(sondr::extract_elements_with_prefix(output_centile, "ces2004"))

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
table(sondr::extract_elements_with_prefix(output_centile, "ces2006"))
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
table(sondr::extract_elements_with_prefix(output_centile, "ces2008"))
## ces11 -------------------------------------------------------------------

source_id <- "ces2011"

#### 1. Get raw gender variable vector
raw1 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2011/ces2011.csv",
                             variable_name = "CPS11_92")
table(raw1, useNA = "always")

## remove 998 999 who are dont know refused
raw1[raw1 %in% c(998, 999)] <- NA
raw1 <- raw1*1000
table(raw1)

raw2 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2011/ces2011.csv",
                             variable_name = "CPS11_93")
table(raw2, useNA = "always")
raw2[raw2 %in% c("refused", "don't know")] <- NA


#### 2. clean variable
raw2 <- parse_money_vector(values = raw2,
                           sep = "and",
                           floor = "less than $29,999            ",
                           ceiling = "more than $110,000           ",
                           ceiling_increment = 20000)
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
table(sondr::extract_elements_with_prefix(output_centile, "ces2011"))
## ces2015 -------------------------------------------------------------------

source_id <- "ces2015"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2015/ces2015.csv",
                            variable_name = "income_full")
table(raw, useNA = "always")

## fix labels
labels <- c("Less than $30,000", "$30,000 to $60,000", "$60,000 to $90,000", "$90,000 to $110,000", "More than $110,000")
names(labels) <- as.character(c(1, 2, 3, 4, 5))
raw <- labels[raw]
table(raw)

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "to",
                                floor = "Less than $30,000",
                                ceiling = "More than $110,000",
                                ceiling_increment = 20000)
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
table(sondr::extract_elements_with_prefix(output_centile, "ces2015"))
## ces2019 -------------------------------------------------------------------

source_id <- "ces2019"

#### 1. Get raw gender variable vector
raw1 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2019/ces2019.csv",
                             variable_name = "cps19_income_number")
table(raw1, useNA = "always")

raw2 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2019/ces2019.csv",
                             variable_name = "cps19_income_cat")
table(raw2, useNA = "always")
raw1[raw2 == "No income"] <- 0
raw2[raw2 %in% c("Don't know/ Prefer not to answer", "No income")] <- NA
raw2[raw2 == "$1 to $30,000"] <- "less $30,000"

#### 2. clean variable
raw2 <- parse_money_vector(values = raw2,
                           sep = "to",
                           floor = "less $30,000",
                           ceiling = "More than $200,000",
                           ceiling_increment = 50000)
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
table(sondr::extract_elements_with_prefix(output_centile, "ces2019"))

## ces2021 -------------------------------------------------------------------

source_id <- "ces2021"

#### 1. Get raw gender variable vector
clean_num <- as.numeric(sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2021/ces2021.csv",
                             variable_name = "cps21_income_number"))
table(clean_num, useNA = "always")

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
table(sondr::extract_elements_with_prefix(output_centile, "ces2021"))

## datagotchi_pilot1_2021 -------------------------------------------------------------------

source_id <- "datagotchi_pilot1_2021"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot1_2021/datagotchi_pilot1_2021.Sav",
                            variable_name = "SES2")
table(raw, useNA = "always")

## fix labels
labels <- c("Aucun revenu", "1$ to 30,000$", "30,001$ to 60,000$", "60,001$ to 90,000$", "90,001$ to 110,000$", "110,001$ to 150,000$", "150,001$ to 200,000$", "More than 200,000$")
names(labels) <- as.character(c(1, 2, 3, 4, 5, 6, 7, 8))
raw <- labels[raw]
raw[raw == "Aucun revenu"] <- "0$ to 0$"
table(raw)

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "to",
                                floor = "0$ to 0$",
                                ceiling = "More than 200,000$",
                                ceiling_increment = 50000)
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
table(sondr::extract_elements_with_prefix(output_centile, "datagotchi_pilot1_2021"))
## january -------------------------------------------------------------------

source_id <- "january"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/january/january.Sav",
                            variable_name = "S7")
table(raw, useNA = "always")

## fix labels
labels <- c("Moins de 20,000$", "20,000$ to 39,999$", "40,000$ to 59,999$", "60,000$ to 79,999$", "80,000$ to 99,999$", "100,000$ to 119,999$", "120,000$ to 139,999$", "140,000$ et plus")
names(labels) <- as.character(c(1, 2, 3, 4, 5, 6, 7, 8))
raw <- labels[raw]
table(raw)

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "to",
                                floor = "Moins de 20,000$",
                                ceiling = "140,000$ et plus",
                                ceiling_increment = 20000)
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

table(sondr::extract_elements_with_prefix(output_centile, "january"))
## february -------------------------------------------------------------------

source_id <- "february"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/february/february.Sav",
                            variable_name = "S7")
table(raw, useNA = "always")

## fix labels
labels <- c("Moins de 20,000$", "20,000$ to 39,999$", "40,000$ to 59,999$", "60,000$ to 79,999$", "80,000$ to 99,999$", "100,000$ to 119,999$", "120,000$ to 139,999$", "140,000$ et plus")
names(labels) <- as.character(c(1, 2, 3, 4, 5, 6, 7, 8))
raw <- labels[raw]
table(raw)

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "to",
                                floor = "Moins de 20,000$",
                                ceiling = "140,000$ et plus",
                                ceiling_increment = 20000)
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

table(sondr::extract_elements_with_prefix(output_centile, "february"))
## march -------------------------------------------------------------------

source_id <- "march"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/march/march.Sav",
                            variable_name = "S7")
table(raw, useNA = "always")

## fix labels
labels <- c("Moins de 20,000$", "20,000$ to 39,999$", "40,000$ to 59,999$", "60,000$ to 79,999$", "80,000$ to 99,999$", "100,000$ to 119,999$", "120,000$ to 139,999$", "140,000$ et plus")
names(labels) <- as.character(c(1, 2, 3, 4, 5, 6, 7, 8))
raw <- labels[raw]
table(raw)

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "to",
                                floor = "Moins de 20,000$",
                                ceiling = "140,000$ et plus",
                                ceiling_increment = 20000)
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

table(sondr::extract_elements_with_prefix(output_centile, "march"))
## april -------------------------------------------------------------------

source_id <- "april"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/april/april.Sav",
                            variable_name = "S7")
table(raw, useNA = "always")

## fix labels
labels <- c("Moins de 20,000$", "20,000$ to 39,999$", "40,000$ to 59,999$", "60,000$ to 79,999$", "80,000$ to 99,999$", "100,000$ to 119,999$", "120,000$ to 139,999$", "140,000$ et plus")
names(labels) <- as.character(c(1, 2, 3, 4, 5, 6, 7, 8))
raw <- labels[raw]
table(raw)

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "to",
                                floor = "Moins de 20,000$",
                                ceiling = "140,000$ et plus",
                                ceiling_increment = 20000)
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
table(sondr::extract_elements_with_prefix(output_centile, "april"))
## may -------------------------------------------------------------------

source_id <- "may"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/may/may.Sav",
                            variable_name = "S7")
table(raw, useNA = "always")

## fix labels
labels <- c("Moins de 20,000$", "20,000$ to 39,999$", "40,000$ to 59,999$", "60,000$ to 79,999$", "80,000$ to 99,999$", "100,000$ to 119,999$", "120,000$ to 139,999$", "140,000$ et plus")
names(labels) <- as.character(c(1, 2, 3, 4, 5, 6, 7, 8))
raw <- labels[raw]
table(raw)

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "to",
                                floor = "Moins de 20,000$",
                                ceiling = "140,000$ et plus",
                                ceiling_increment = 20000)
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
table(sondr::extract_elements_with_prefix(output_centile, "may"))
## june -------------------------------------------------------------------

source_id <- "june"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/june/june.Sav",
                            variable_name = "S7")
table(raw, useNA = "always")

## fix labels
labels <- c("Moins de 20,000$", "20,000$ to 39,999$", "40,000$ to 59,999$", "60,000$ to 79,999$", "80,000$ to 99,999$", "100,000$ to 119,999$", "120,000$ to 139,999$", "140,000$ et plus")
names(labels) <- as.character(c(1, 2, 3, 4, 5, 6, 7, 8))
raw <- labels[raw]
table(raw)

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "to",
                                floor = "Moins de 20,000$",
                                ceiling = "140,000$ et plus",
                                ceiling_increment = 20000)
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
table(sondr::extract_elements_with_prefix(output_centile, "june"))
## datagotchi_pilot2_2022 -------------------------------------------------------------------

source_id <- "datagotchi_pilot2_2022"

#### 1. Get raw gender variable vector
rawfr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                            variable_name = "income")
table(rawfr, useNA = "always")

rawen <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                              variable_name = "income.1")
table(rawen, useNA = "always")

table(rawfr, rawen, useNA = "always")

raw <- coalesce(rawfr, rawen)
table(raw, useNA = "always")

## fix labels
labels <- c("0 à 0", "1$ à 30 000$", "30 001$ à 60 000$", "60 001$ à 90 000$", "90 001 à 110 000$", "110 001$ à 150 000$", "150 001$ à 200 000$", "Plus de 200 000$")
names(labels) <- as.character(c(1, 2, 3, 4, 5, 6, 7, 8))
raw <- labels[raw]
table(raw)

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "à",
                                floor = "0 à 0",
                                ceiling = "Plus de 200 000$",
                                ceiling_increment = 50000)
table(clean_num)

## to quantile
clean_centile <- round(ecdf(clean_num)(clean_num)*100)
table(clean_centile)
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
output_centile["datagotchi_pilot2_2022.189"]
table(sondr::extract_elements_with_prefix(output_centile, "datagotchi_pilot2_2022"))
## sondage_nationalisme_2022 -------------------------------------------------------------------

source_id <- "sondage_nationalisme_2022"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/sondage_nationalisme_2022/sondage_nationalisme_2022.csv",
                            variable_name = "income")
table(raw, useNA = "always")

raw[raw %in% c("", "Don't know/Prefer not to say",
               "Ne sait pas/préfère ne pas répondre")] <- NA
raw[raw %in% c("No income", "Aucun revenu")] <- "0 to 0"
raw[raw %in% c("Plus de 200 000$", "More than $200,000")] <- "more than 200 000"
raw <- gsub("à", "to", raw)
table(raw)

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "to",
                                floor = "0 to 0",
                                ceiling = "more than 200 000",
                                ceiling_increment = 50000)
table(clean_num)

## to quantile
clean_centile <- round(ecdf(clean_num)(clean_num)*100)
table(clean_centile)

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

table(sondr::extract_elements_with_prefix(output_centile, "sondage_nationalisme_2022"))

## quorum_mcq_pilote -------------------------------------------------------------------

source_id <- "quorum_mcq_pilote"

#### 1. Get raw gender variable vector
rawfr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                            variable_name = "ses_income")
table(rawfr, useNA = "always")

rawfr[rawfr == ""] <- NA
rawfr[rawfr == "Aucun revenu"] <- "0 à 0"
table(rawfr)

#### 2. clean variable
clean_numfr <- parse_money_vector(values = rawfr,
                                  sep = "à",
                                  floor = "0 à 0",
                                  ceiling = "Plus de 200 000$",
                                  ceiling_increment = 50000)
table(clean_numfr)

## english
#### 1. Get raw gender variable vector
rawen <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                              variable_name = "EN_ses_income")
table(rawen, useNA = "always")

rawen[rawen == ""] <- NA
rawen[rawen == "No income"] <- "0 à 0"
table(rawen)

#### 2. clean variable
clean_numen <- parse_money_vector(values = rawen,
                                  sep = "to",
                                  floor = "0 à 0",
                                  ceiling = "More than $200 000",
                                  ceiling_increment = 50000)
table(clean_numen)

### merge fr et en

clean_num <- coalesce(clean_numfr, clean_numen)
table(clean_num)

## to quantile
clean_centile <- round(ecdf(clean_num)(clean_num)*100)
table(clean_centile)

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
table(sondr::extract_elements_with_prefix(output_centile, "quorum_mcq_pilote"))
## pes_elxn_2022_text -------------------------------------------------------------------

source_id <- "pes_elxn_2022_text"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pes_elxn_2022/pes_elxn_2022_text.csv",
                            variable_name = "ses_income")
table(raw, useNA = "always")

raw[raw == ""] <- NA
raw[raw == "Aucun revenu"] <- "0 à 0"
table(raw)

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "à",
                                floor = "0 à 0",
                                ceiling = "Plus de 200 000$",
                                ceiling_increment = 50000)
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
table(sondr::extract_elements_with_prefix(output_centile, "pes_elxn_2022_text"))

## pco -------------------------------------------------------------------

source_id <- "WholeData_Pco14_2015_01_30"

#### 1. Get raw gender variable vector
raw <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pco/WholeData_Pco14_2015_01_30.csv",
                            variable_name = "income")
table(raw, useNA = "always")

raw[raw == ""] <- NA
raw[raw == "$500k-1 million"] <- "more than $500,000"
table(raw, useNA = "always")

#### 2. clean variable
clean_num <- parse_money_vector(values = raw,
                                sep = "-",
                                floor = "$0-20k",
                                ceiling = "more than $500,000",
                                ceiling_increment = 500000)
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
table(sondr::extract_elements_with_prefix(output_centile, source_id))
# Output ------------------------------------------------------------------

### save num
#saveRDS(output_num, "_SharedFolder_catalogue-donnees/merging-souverainete/clean/vectors/ses_family_income_num.rds")

### save centile
#saveRDS(output_centile, "_SharedFolder_catalogue-donnees/merging-souverainete/clean/vectors/ses_family_income_centile.rds")

### Categorise in centiles
output_centile_cat <- factor(cut(
  output_centile,
  breaks = c(0, 10, 25, 50, 75, 90, 100),
  labels = c("1_10", "11_25", "26_50", "51_75", "76_90", "91_100")
),
levels = c("1_10", "11_25", "26_50", "51_75", "76_90", "91_100"))

### save centile
saveRDS(output_centile_cat, "_SharedFolder_catalogue-donnees/merging-souverainete/clean/vectors/ses_family_income_centile_cat.rds")
