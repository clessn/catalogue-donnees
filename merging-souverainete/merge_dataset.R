# Packages ----------------------------------------------------------------
library(tidyverse)

# Config ------------------------------------------------------------------

### source config file to generate all the respondent_ids
source("merging-souverainete/config.R")

### get year of each survey
source_ids <- unique(sapply(strsplit(ids, "\\."), `[`, 1))

years <- c(ces65 = 1965,
           ces68 = 1968,
           ces74 = 1974,
           ces79 = 1979,
           ces84 = 1984,
           ces88 = 1988,
           ces93 = 1993,
           ces97 = 1997,
           ces2000 = 2000,
           ces2004 = 2004,
           ces2006 = 2006,
           ces2008 = 2008,
           ces2011 = 2011,
           ces2015 = 2015,
           ces2019 = 2019,
           ces2021 = 2021,
           datagotchi_pilot1_2021 = 2021,
           datagotchi_pilot2_2022 = 2022,
           april = 2022,
           february = 2022,
           january = 2022,
           june = 2022,
           march = 2022,
           may = 2022,
           pco = 2014,
           pes_elxn_2022_text = 2023,
           quorum_mcq_pilote = 2023,
           sondage_nationalisme_2022 = 2022)


### Create dataframe skeleton
data <- data.frame(
  id = ids,
  source_id = sapply(strsplit(ids, "\\."), `[`, 1),
  respondent_id = sapply(strsplit(ids, "\\."), `[`, 2)) %>% 
  mutate(year = years[source_id])

# Load all vectors and add to data --------------------------------------------------------

files <- list.files(path = "_SharedFolder_catalogue-donnees/merging-souverainete/clean/vectors",
                    full.names = TRUE)

for (i in 1:length(files)){
  vector <- readRDS(files[i])
  variable_name <- gsub("\\.rds", "", basename(files[i])) 
  data[[variable_name]] <- vector
}


# Reorder variables -------------------------------------------------------

data <- data %>% 
  select(id, source_id, respondent_id, year, ses_age, ses_gender, ses_lang.1,
         ses_educ, ses_family_income_centile_cat, ses_origin_from_canada.1, ses_year_canada,
         ses_religiosity, int_pol, iss_idcan, iss_souv, party_id_prov)

# Save it -----------------------------------------------------------------

saveRDS(data, "_SharedFolder_catalogue-donnees/merging-souverainete/clean/merged_v1.rds")


# Describe the data -------------------------------------------------------

n_by_source_id <- data %>% 
  group_by(source_id) %>% 
  summarise(n_survey = n())

na_count <- data %>%
  select(-id, -respondent_id) %>% 
  group_by(year, source_id) %>% 
  summarise_all(~sum(is.na(.))) %>% 
  pivot_longer(., cols = names(.)[!(names(.) %in% c("year", "source_id"))],
               names_to = "variable", values_to = "na_count") %>% 
  left_join(., n_by_source_id, by = "source_id")

#### Graph of missing data
na_count %>% 
  mutate(prop = na_count / n_survey) %>%
  ggplot(aes(x = variable, y = source_id)) +
  facet_grid(rows = vars(year), scales = "free_y",
             switch = "y") +
  geom_point(aes(color = prop),
             shape = 15, size = 15) +
  clessnverse::theme_clean_light() +
  scale_color_gradient(low = "#32CD32", high = "black",
                       name = "Proportion de données manquantes (%)") +
  theme(strip.placement = "inside",
        axis.text.x = element_text(angle = 90, hjust = 1),
        panel.spacing.y = unit(0.5, "lines"))

ggsave("_SharedFolder_catalogue-donnees/merging-souverainete/graphs/missing_data.png",
       width = 9, height = 18)
