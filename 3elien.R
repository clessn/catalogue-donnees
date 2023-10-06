library(tidyverse)

cp <- readxl::read_xlsx("_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/february/february_postalcodes.xlsx") %>% 
  mutate(quest = as.numeric(quest))
d <- haven::read_sav("_SharedFolder_catalogue-donnees/merging-souverainete/raw/omnibus/february/february.Sav") %>% 
  left_join(., cp, by = c("QUEST" = "quest")) %>% 
  mutate(A5 = tolower(A5))
attributes(d$REGION)
attributes(d$L1)
table(d$L1)
table(d$A5)


# Codes postaux
qc_codes <- c("g1a", "g1j", "g1k", "g1l", "g1m", "g1p", "g1n", "g1r", "g1s", "g1t", "g1v", "g1w", "g1x", "g1y")
banlieue_nord_codes <- c("g2a", "g3a", "g2b", "g1c", "g2c", "g3c", "g1e", "g2e", "g3e", "g1g", "g2g", "g3g", "g1h", "g3h", "g2j", "g3j", "g2k", "g3k", "g2l", "g2m", "g3n", "g3s")
levis_codes <- c("g7a", "g3b", "g6c", "g6j", "g6k", "g6v", "g6w", "g6x", "g6y", "g6z")

# Création du vecteur avec les codes postaux comme noms et les lieux comme valeurs
locations <- c(
  setNames(rep("qc", length(qc_codes)), qc_codes),
  setNames(rep("banlieue_nord", length(banlieue_nord_codes)), banlieue_nord_codes),
  setNames(rep("levis", length(levis_codes)), levis_codes)
)

data <- d %>% 
  mutate(region = locations[A5]) %>% 
  replace_na(list(region = "autre"))


graph <- data %>% 
  group_by(region, L1) %>% 
  summarise(n = n()) %>% 
  group_by(region) %>% 
  mutate(nregion = sum(n),
         prop = n/nregion,
         accord = ifelse(L1 %in% c(1,2), 1, 0),
         force = ifelse(L1 %in% c(1,4), 1, 0),
         region = case_when(
           region == "autre" ~ "Reste du Québec",
           region == "banlieue_nord" ~ "Banlieue de Québec\nrive-nord",
           region == "levis" ~ "Lévis, rive-sud",
           region == "qc" ~ "Québec"
         )) %>% 
  group_by(region, accord) %>% 
  mutate(txt = round(sum(prop)*100))


ggplot(graph, aes(x = accord, y = prop*100)) +
  facet_wrap(~region) +
  geom_bar(
    aes(group = as.factor(force),
        alpha = as.factor(force),
        fill = as.factor(accord)),
    stat = "identity",
    color = NA
  ) +
  clessnverse::theme_clean_light() +
  scale_alpha_manual(name = "Force de la position",
                     values = c("0" = 0.4,
                                "1" = 1),
                     labels = c("0" = "Légèrement",
                                "1" = "Fortement")) +
  scale_fill_manual(name = "",
                    values = c("0" = "#FFB3B3",
                               "1" = "#99E6B3"),
                    labels = c("0" = "En désaccord",
                               "1" = "En accord")) +
  xlab("") +
  ylab("Proportion dans la région") +
  labs(title = "Accord au projet de 3e lien, février 2022") +
  geom_text(aes(label = paste0(txt, "%"), y = txt + 3)) +
  theme(legend.title = element_text(),
        axis.text.x = element_blank())

ggsave("_SharedFolder_catalogue-donnees/graph_3elien.png",
       width = 10, height = 8)
