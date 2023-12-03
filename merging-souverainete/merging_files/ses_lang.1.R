# Packages ----------------------------------------------------------------
library(tidyverse)

# Config ------------------------------------------------------------------
source("merging-souverainete/config.R")
sample(ids, 20) ### 20 ids random
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

table(sondr::extract_elements_with_prefix(output_lang, "ces65"))

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

table(sondr::extract_elements_with_prefix(output_lang, "ces68"))

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

table(sondr::extract_elements_with_prefix(output_lang, "ces74"))

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

table(sondr::extract_elements_with_prefix(output_lang, "ces79"))

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

table(sondr::extract_elements_with_prefix(output_lang, "ces84"))

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

table(sondr::extract_elements_with_prefix(output_lang, "ces88"))

## ces93 -------------------------------------------------------------------

raw_ces93 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1993/ces93.csv",
                                  variable_name = "refn16")
table(raw_ces93, useNA = "always")

#### 2. clean variable
clean_ces93 <- NA
clean_ces93[raw_ces93=="english"] <- "english"
clean_ces93[raw_ces93=="french"] <- "french"
clean_ces93[raw_ces93=="other"] <- "other"
table(clean_ces93, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces93
names(clean_ces93) <- sondr::generate_survey_ids(n_respondents = length(clean_ces93), ## number of respondents
                                                 source_id = "ces93") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_ces93) ## vector with updates

table(sondr::extract_elements_with_prefix(output_lang, "ces93"))
## ces97 -------------------------------------------------------------------

raw_ces97 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/1997/ces97.csv",
                                  variable_name = "cpsm15")
table(raw_ces97, useNA = "always")

#### 2. clean variable
clean_ces97 <- NA
clean_ces97[raw_ces97=="english"] <- "english"
clean_ces97[raw_ces97=="french"] <- "french"
clean_ces97[raw_ces97!="english" & raw_ces97!="french" & raw_ces97!="refused" & raw_ces97!="d.k."] <- "other"
table(clean_ces97, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces97
names(clean_ces97) <- sondr::generate_survey_ids(n_respondents = length(clean_ces97), ## number of respondents
                                                 source_id = "ces97") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_ces97) ## vector with updates

table(sondr::extract_elements_with_prefix(output_lang, "ces97"))
## ces2000 -------------------------------------------------------------------

raw_ces2000 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2000/ces2000.csv",
                                  variable_name = "cpsm15")
table(raw_ces2000, useNA = "always")

#### 2. clean variable
clean_ces2000 <- NA
clean_ces2000[raw_ces2000=="1"] <- "english"
clean_ces2000[raw_ces2000=="5"] <- "french"
clean_ces2000[raw_ces2000!="1" & raw_ces2000!="5" & raw_ces2000!="98" & raw_ces2000!="99"] <- "other"
table(clean_ces2000, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2000
names(clean_ces2000) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2000), ## number of respondents
                                                 source_id = "ces2000") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_ces2000) ## vector with updates

table(sondr::extract_elements_with_prefix(output_lang, "ces2000"))
## ces2004 -------------------------------------------------------------------

raw_ces2004 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2004/ces2004.csv",
                                    variable_name = "ces04_cps_s17")
table(raw_ces2004, useNA = "always")

#### 2. clean variable
clean_ces2004 <- NA
clean_ces2004[raw_ces2004=="english"] <- "english"
clean_ces2004[raw_ces2004=="french"] <- "french"
clean_ces2004[raw_ces2004!="english" & raw_ces2004!="french" & raw_ces2004!="don't know" & raw_ces2004!="refused" & raw_ces2004!="<NA>"] <- "other"
table(clean_ces2004, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2004
names(clean_ces2004) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2004), ## number of respondents
                                                   source_id = "ces2004") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_ces2004) ## vector with updates

table(sondr::extract_elements_with_prefix(output_lang, "ces2004"))

## ces2006 -------------------------------------------------------------------
raw_ces2006 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2006/ces2006.csv",
                                    variable_name = "cps_s17")
table(raw_ces2006, useNA = "always")

#### 2. clean variable
clean_ces2006 <- NA
clean_ces2006[raw_ces2006=="english"] <- "english"
clean_ces2006[raw_ces2006=="french"] <- "french"
clean_ces2006[raw_ces2006!="english" & raw_ces2006!="french" & raw_ces2006!="don't know" & raw_ces2006!="refused" & raw_ces2006!="<NA>"] <- "other"
table(clean_ces2006, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2006
names(clean_ces2006) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2006), ## number of respondents
                                                   source_id = "ces2006") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_ces2006) ## vector with updates

table(sondr::extract_elements_with_prefix(output_lang, "ces2006"))

## ces2008 -------------------------------------------------------------------

raw_ces2008 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2008/ces2008.csv",
                                    variable_name = "cps_s17")
table(raw_ces2008, useNA = "always")

#### 2. clean variable
clean_ces2008 <- NA
clean_ces2008[raw_ces2008== 1] <- "english"
clean_ces2008[raw_ces2008== 5] <- "french"
clean_ces2008[(5 < raw_ces2008 & raw_ces2008 < 55) | (1 < raw_ces2008 & raw_ces2008 < 5)] <- "other"
table(clean_ces2008, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2008
names(clean_ces2008) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2008), ## number of respondents
                                                   source_id = "ces2008") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_ces2008) ## vector with updates

table(sondr::extract_elements_with_prefix(output_lang, "ces2008"))

## ces2011 -------------------------------------------------------------------

raw_ces2011 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2011/ces2011.csv",
                                    variable_name = "CPS11_90")
table(raw_ces2011, useNA = "always")

#### 2. clean variable
clean_ces2011 <- NA
clean_ces2011[raw_ces2011=="English                   "] <- "english"
clean_ces2011[raw_ces2011=="French                    "] <- "french"
clean_ces2011[raw_ces2011!="English                   " & raw_ces2011!="French                    " & raw_ces2011!="don't know              " & raw_ces2011!="refused                   "] <- "other"
table(clean_ces2011, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2011
names(clean_ces2011) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2011), ## number of respondents
                                                   source_id = "ces2011") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_ces2011) ## vector with updates
table(sondr::extract_elements_with_prefix(output_lang, "ces2011"))

## ces2015 -------------------------------------------------------------------

raw_ces2015 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2015/ces2015.csv",
                                    variable_name = "first_lang")
table(raw_ces2015, useNA = "always")

#### 2. clean variable
clean_ces2015 <- NA
clean_ces2015[raw_ces2015=="1"] <- "english"
clean_ces2015[raw_ces2015=="5"] <- "french"
clean_ces2015[raw_ces2015!="1" & raw_ces2015!="5" & as.numeric(raw_ces2015<=31)] <- "other"
table(clean_ces2015, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2015
names(clean_ces2015) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2015), ## number of respondents
                                                   source_id = "ces2015") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_ces2015) ## vector with updates
table(sondr::extract_elements_with_prefix(output_lang, "ces2015"))

## ces2019 -------------------------------------------------------------------

raw_ces2019 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2019/ces2019.csv",
                                    variable_name = "cps19_Q_Language")
table(raw_ces2019, useNA = "always")

#### 2. clean variable
clean_ces2019 <- NA
clean_ces2019[raw_ces2019=="EN"] <- "english"
clean_ces2019[raw_ces2019=="FR-CA"] <- "french"
table(clean_ces2019, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2019
names(clean_ces2019) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2019), ## number of respondents
                                                   source_id = "ces2019") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_ces2019) ## vector with updates
table(sondr::extract_elements_with_prefix(output_lang, "ces2019"))

## ces2021 -------------------------------------------------------------------

raw_ces2021 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/ces/2021/ces2021.csv",
                                    variable_name = "pes21_lang")
table(raw_ces2021, useNA = "always")

#### 2. clean variable
clean_ces2021 <- NA
clean_ces2021[raw_ces2021=="English"] <- "english"
clean_ces2021[raw_ces2021=="French"] <- "french"
clean_ces2021[raw_ces2021!="English" & raw_ces2021!="French" & raw_ces2021!="Don't know/ Prefer not to answer"] <- "other"
table(clean_ces2021, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = ces2021
names(clean_ces2021) <- sondr::generate_survey_ids(n_respondents = length(clean_ces2021), ## number of respondents
                                                   source_id = "ces2021") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_ces2021) ## vector with updates

table(sondr::extract_elements_with_prefix(output_lang, "ces2021"))
## datagotchi_pilot1_2021 ----------------------------------------------------

raw_datgot21 <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot1_2021/datagotchi_pilot1_2021.Sav",
                                    variable_name = "LANGM")
table(raw_datgot21, useNA = "always")

#### 2. clean variable
clean_datgot21 <- NA
clean_datgot21[raw_datgot21=="1"] <- "english"
clean_datgot21[raw_datgot21=="2"] <- "french"
clean_datgot21[raw_datgot21=="3"] <- "other"
table(clean_datgot21, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = datgot21
names(clean_datgot21) <- sondr::generate_survey_ids(n_respondents = length(clean_datgot21), ## number of respondents
                                                   source_id = "datagotchi_pilot1_2021") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_datgot21) ## vector with updates

table(sondr::extract_elements_with_prefix(output_lang, "datagotchi_pilot1_2021"))

## january -------------------------------------------------------------------

raw_january <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/january/january.Sav",
                                     variable_name = "LANGUE")
table(raw_january, useNA = "always")

#### 2. clean variable
clean_january <- NA
clean_january[raw_january=="1"] <- "french"
clean_january[raw_january=="2"] <- "english"
clean_january[raw_january=="3"] <- "other"
table(clean_january, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = january
names(clean_january) <- sondr::generate_survey_ids(n_respondents = length(clean_january), ## number of respondents
                                                    source_id = "january") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_january) ## vector with updates

table(sondr::extract_elements_with_prefix(output_lang, "january"))

## february -------------------------------------------------------------------

raw_february <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/february/february.Sav",
                                    variable_name = "LANGUE")
table(raw_february, useNA = "always")

#### 2. clean variable
clean_february <- NA
clean_february[raw_february=="1"] <- "french"
clean_february[raw_february=="2"] <- "english"
clean_february[raw_february=="3"] <- "other"
table(clean_february, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = february
names(clean_february) <- sondr::generate_survey_ids(n_respondents = length(clean_february), ## number of respondents
                                                   source_id = "february") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_february) ## vector with updates

table(sondr::extract_elements_with_prefix(output_lang, "february"))
## march -------------------------------------------------------------------

raw_march <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/march/march.Sav",
                                     variable_name = "LANGUE")
table(raw_march, useNA = "always")

#### 2. clean variable
clean_march <- NA
clean_march[raw_march=="1"] <- "french"
clean_march[raw_march=="2"] <- "english"
clean_march[raw_march=="3"] <- "other"
table(clean_march, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = march
names(clean_march) <- sondr::generate_survey_ids(n_respondents = length(clean_march), ## number of respondents
                                                    source_id = "march") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_march) ## vector with updates

table(sondr::extract_elements_with_prefix(output_lang, "march"))

## april -------------------------------------------------------------------

raw_april <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/april/april.Sav",
                                  variable_name = "LANGUE")
table(raw_april, useNA = "always")

#### 2. clean variable
clean_april <- NA
clean_april[raw_april=="1"] <- "french"
clean_april[raw_april=="2"] <- "english"
clean_april[raw_april=="3"] <- "other"
table(clean_april, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = april
names(clean_april) <- sondr::generate_survey_ids(n_respondents = length(clean_april), ## number of respondents
                                                 source_id = "april") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_april) ## vector with updates
table(sondr::extract_elements_with_prefix(output_lang, "april"))
## may -------------------------------------------------------------------

raw_may <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/may/may.Sav",
                                  variable_name = "LANGUE")
table(raw_may, useNA = "always")

#### 2. clean variable
clean_may <- NA
clean_may[raw_may=="1"] <- "french"
clean_may[raw_may=="2"] <- "english"
clean_may[raw_may=="3"] <- "other"
table(clean_may, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = may
names(clean_may) <- sondr::generate_survey_ids(n_respondents = length(clean_may), ## number of respondents
                                                 source_id = "may") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_may) ## vector with updates

table(sondr::extract_elements_with_prefix(output_lang, "may"))
## june -------------------------------------------------------------------

raw_june <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/june/june.Sav",
                                variable_name = "LANGUE")
table(raw_june, useNA = "always")

#### 2. clean variable
clean_june <- NA
clean_june[raw_june=="1"] <- "french"
clean_june[raw_june=="2"] <- "english"
clean_june[raw_june=="3"] <- "other"
table(clean_june, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = june
names(clean_june) <- sondr::generate_survey_ids(n_respondents = length(clean_june), ## number of respondents
                                               source_id = "june") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_june) ## vector with updates

table(sondr::extract_elements_with_prefix(output_lang, "june"))

## datagotchi_pilot2_2022 -------------------------------------------------------------------

raw_datgot22<- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                     variable_name = "language")
table(raw_datgot22, useNA = "always")

#### 2. clean variable
clean_datgot22 <- NA
clean_datgot22[raw_datgot22=="1"] <- "english"
clean_datgot22[raw_datgot22=="2"] <- "french"
clean_datgot22[raw_datgot22=="3"] <- "other"
table(clean_datgot22, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = datgot22
names(clean_datgot22) <- sondr::generate_survey_ids(n_respondents = length(clean_datgot22), ## number of respondents
                                                    source_id = "datagotchi_pilot2_2022") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_datgot22) ## vector with updates

table(sondr::extract_elements_with_prefix(output_lang, "datagotchi_pilot2_2022"))
## sondage_nationalisme_2022 ------------------------------------------------

raw_sond_nat<- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/sondage_nationalisme_2022/sondage_nationalisme_2022.csv",
                                    variable_name = "language")
table(raw_sond_nat, useNA = "always")

#### 2. clean variable
clean_sond_nat <- NA
clean_sond_nat[raw_sond_nat=="English" | raw_sond_nat=="Anglais"] <- "english"
clean_sond_nat[raw_sond_nat=="French" | raw_sond_nat=="Français"] <- "french"
clean_sond_nat[raw_sond_nat=="Other" | raw_sond_nat=="Autre"] <- "other"
table(clean_sond_nat, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = sond_nat
names(clean_sond_nat) <- sondr::generate_survey_ids(n_respondents = length(clean_sond_nat), ## number of respondents
                                                    source_id = "sondage_nationalisme_2022") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_sond_nat) ## vector with updates

table(sondr::extract_elements_with_prefix(output_lang, "sondage_nationalisme_2022"))

## quorum_mcq_pilote ---------------------------------------------------------

raw_quorum_mcq_pilote_fr <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                                                 variable_name = "ses_lang")
table(raw_quorum_mcq_pilote_fr, useNA = "always")
### change "" for NA
raw_quorum_mcq_pilote_fr[raw_quorum_mcq_pilote_fr == ""] <- NA

raw_quorum_mcq_pilote_en <- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/quorum_mcq_pilote/quorum_mcq_pilote.csv",
                                                 variable_name = "EN_ses_lang")
table(raw_quorum_mcq_pilote_en, useNA = "always")
### change "" for NA
raw_quorum_mcq_pilote_en[raw_quorum_mcq_pilote_en == ""] <- NA

raw_quorum_mcq_pilote <- coalesce(raw_quorum_mcq_pilote_fr, raw_quorum_mcq_pilote_en)
table(raw_quorum_mcq_pilote)

#### 2. clean variable
clean_quorum <- NA
clean_quorum[raw_quorum_mcq_pilote=="English" | raw_quorum_mcq_pilote=="Anglais"] <- "english"
clean_quorum[raw_quorum_mcq_pilote=="French" |  raw_quorum_mcq_pilote=="Français"] <- "french"
clean_quorum[raw_quorum_mcq_pilote=="Other" |   raw_quorum_mcq_pilote=="Autre"] <- "other"
table(clean_quorum, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = quorum
names(clean_quorum) <- sondr::generate_survey_ids(n_respondents = length(clean_quorum), ## number of respondents
                                                    source_id = "quorum_mcq_pilote") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_quorum) ## vector with updates

table(sondr::extract_elements_with_prefix(output_lang, "quorum_mcq_pilote"))

## pes_elxn_2022_text ----------------------------------------------------
### NA pour PES

## pco -------------------------------------------------------------------

raw_pco<- sondr::load_variable(file = "_SharedFolder_catalogue-donnees/merging-souverainete/raw/pco/pco.csv",
                                    variable_name = "Q16.4.Pco2014")
table(raw_pco, useNA = "always")

#### 2. clean variable
clean_pco <- NA
clean_pco[raw_pco=="English"] <- "english"
clean_pco[raw_pco=="French"] <- "french"
clean_pco[raw_pco!="English" & raw_pco!="French" & raw_pco!="What language do you speak most often at home?"] <- "other"
table(clean_pco, useNA = "always")

#### 3. name each element in clean (assign the respondent id to each person in the vector)
##### source_id = pco
names(clean_pco) <- sondr::generate_survey_ids(n_respondents = length(clean_pco), ## number of respondents
                                                    source_id = "pco") ## source_id

## 4. add clean to the master output
output_lang <- sondr::match_and_update(main = output_lang, ## vector to update
                                       updates = clean_pco) ## vector with updates

table(sondr::extract_elements_with_prefix(output_lang, "pco"))
# Output ------------------------------------------------------------------

### FACTORISE, LEVELS, etc.

table(output_lang)
output_lang <- factor(output_lang)

##### SAVE VECTOR
saveRDS(output_lang, "_SharedFolder_catalogue-donnees/merging-souverainete/clean/vectors/ses_lang.1.rds")
