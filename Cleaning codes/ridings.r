# Create vectors for each column
region <- c(
  "Abitibi-Témiscamingue", "Bas-Saint-Laurent", "Bas-Saint-Laurent", "Bas-Saint-Laurent",
  "Gaspésie–Îles-de-la-Madeleine", "Gaspésie–Îles-de-la-Madeleine", "Capitale-Nationale",
  "Capitale-Nationale", "Capitale-Nationale", "Capitale-Nationale", "Capitale-Nationale",
  "Capitale-Nationale", "Capitale-Nationale", "Centre-du-Québec", "Centre-du-Québec",
  "Centre-du-Québec", "Centre-du-Québec", "Chaudière-Appalaches", "Chaudière-Appalaches",
  "Chaudière-Appalaches", "Chaudière-Appalaches", "Chaudière-Appalaches", "Côte-Nord",
  "Estrie", "Estrie", "Estrie", "Estrie", "Estrie", "Lanaudière", "Lanaudière",
  "Lanaudière", "Lanaudière", "Lanaudière", "Mauricie", "Mauricie", "Mauricie",
  "Laurentides", "Laurentides", "Laurentides", "Laurentides", "Laurentides", "Laurentides",
  "Laval", "Laval", "Laval", "Laval", "Montérégie", "Montérégie", "Montérégie", "Montérégie",
  "Montérégie", "Montérégie", "Montérégie", "Montérégie", "Montérégie", "Montérégie",
  "Montérégie", "Montérégie", "Montérégie", "Montérégie", "Montréal", "Montréal",
  "Montréal", "Montréal", "Montréal", "Montréal", "Montréal", "Montréal", "Montréal",
  "Montréal", "Montréal", "Montréal", "Montréal", "Montréal", "Montréal", "Montréal",
  "Montréal", "Nord-du-Québec", "Outaouais", "Outaouais", "Outaouais", "Outaouais",
  "Saguenay–Lac-Saint-Jean", "Saguenay–Lac-Saint-Jean", "Saguenay–Lac-Saint-Jean"
)

ridings <- c(
  "Abitibi—Témiscamingue", "Avignon—La Mitis—Matane—Matapédia (en partie)",
  "Montmagny—L'Islet—Kamouraska—Rivière-du-Loup (principalement)",
  "Rimouski-Neigette—Témiscouata—Les Basques", "Avignon—La Mitis—Matane—Matapédia (en partie)",
  "Gaspésie–Les Îles-de-la-Madeleine", "Beauport—Côte-de-Beaupré—Île d'Orléans—Charlevoix",
  "Beauport—Limoilou", "Charlesbourg—Haute-Saint-Charles", "Louis-Hébert", "Louis-Saint-Laurent",
  "Portneuf—Jacques-Cartier", "Québec", "Bécancour—Nicolet—Saurel", "Drummond",
  "Richmond—Arthabaska", "Mégantic—L'Érable (principalement)", "Beauce",
  "Bellechasse–Les Etchemins–Lévis", "Lévis—Lotbinière", "Mégantic—L'Érable (en partie)",
  "Montmagny—L'Islet—Kamouraska—Rivière-du-Loup (en partie)", "Manicouagan",
  "Brome—Missisquoi (en partie)", "Compton—Stanstead", "Richmond—Arthabaska (en partie)",
  "Shefford (en partie)", "Sherbrooke", "Berthier—Maskinongé (en partie)", "Joliette",
  "Montcalm", "Repentigny", "Terrebonne", "Berthier—Maskinongé (en partie)",
  "Saint-Maurice—Champlain", "Trois-Rivières", "Argenteuil—La Petite-Nation (en partie)",
  "Laurentides—Labelle", "Mirabel", "Rivière-des-Mille-Îles", "Rivière-du-Nord",
  "Thérèse-De Blainville", "Alfred-Pellan", "Laval—Les Îles", "Marc-Aurèle-Fortin",
  "Vimy", "Beloeil—Chambly", "Brome—Missisquoi (en partie)", "Brossard—Saint-Lambert",
  "Châteauguay—Lacolle", "La Prairie", "Longueuil—Charles-LeMoyne", "Longueuil—Saint-Hubert",
  "Montarville", "Salaberry—Suroît", "Saint-Hyacinthe—Bagot", "Saint-Jean",
  "Shefford (en partie)", "Pierre-Boucher—Les Patriotes—Verchères", "Vaudreuil-Soulanges",
  "Ahuntsic-Cartierville", "Bourassa", "Hochelaga", "Honoré-Mercier", "LaSalle—Émard—Verdun",
  "Lac-Saint-Louis", "La Pointe-de-l'Île", "Dorval—Lachine—LaSalle", "Laurier—Sainte-Marie",
  "Mont-Royal", "Notre-Dame-de-Grâce—Westmount", "Outremont", "Papineau",
  "Pierrefonds—Dollard", "Rosemont—La Petite-Patrie", "Saint-Laurent",
  "Saint-Léonard—Saint-Michel", "Ville-Marie—Le Sud-Ouest—Île-des-Sœurs",
  "Abitibi—Baie-James—Nunavik—Eeyou (en partie)", "Argenteuil—La Petite-Nation (en partie)",
  "Gatineau", "Hull—Aylmer", "Pontiac", "Chicoutimi—Le Fjord", "Jonquière", "Lac-Saint-Jean"
)

# Create a data frame
df <- data.frame(ridings, region)

# Print out the first few rows of the dataframe
print(head(df))
