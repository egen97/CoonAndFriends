pacman::p_load(tidyverse)

# Hand-coded classification of the 59 Telegram channels in
# putin_mention_population_2022_2025.rds into a rough typology: state/official
# media, PMC-affiliated, military correspondents ("milbloggers"), aggregator/
# entertainment/meme channels, or uncategorized where the source's identity
# wasn't clear enough to classify with any confidence. Based on publicly
# documented channel identities, not on anything in the post content itself --
# treat as a first draft to be corrected, not ground truth.

channel_categories <- tribble(
  ~source, ~category,

  # -- State/official media & officials --
  "rt_russian", "State/official media",
  "sputniklive", "State/official media",
  "epoddubny", "State/official media",
  "Sladkov_plus", "State/official media",
  "RKadyrov_95", "State/official media",
  "vrogov", "State/official media",
  "aleksandr_skif", "State/official media",
  "marochkolive", "State/official media",
  "SolovievLive", "State/official media",

  # -- PMC/paramilitary-affiliated --
  "wagnernew", "PMC/paramilitary-affiliated",
  "vagner_group", "PMC/paramilitary-affiliated",
  "grey_zone", "PMC/paramilitary-affiliated",
  "orchestra_w", "PMC/paramilitary-affiliated",

  # -- Military correspondents / milbloggers --
  "boris_rozhin", "Military correspondent/milblogger",
  "rybar", "Military correspondent/milblogger",
  "wargonzo", "Military correspondent/milblogger",
  "dva_majors", "Military correspondent/milblogger",
  "vladlentatarsky", "Military correspondent/milblogger",
  "sashakots", "Military correspondent/milblogger",
  "DmitriySteshin", "Military correspondent/milblogger",
  "annashafran", "Military correspondent/milblogger",
  "infantmilitario", "Military correspondent/milblogger",
  "voenkorKotenok", "Military correspondent/milblogger",
  "bolshiepushki", "Military correspondent/milblogger",
  "Abbasdjuma", "Military correspondent/milblogger",
  "brussinf", "Military correspondent/milblogger",
  "rusich_army", "Military correspondent/milblogger",
  "evgeniy_lisitsyn", "Military correspondent/milblogger",
  "Ratnik2nd", "Military correspondent/milblogger",
  "ghost_of_novorossia", "Military correspondent/milblogger",
  "strelkovii", "Military correspondent/milblogger",

  # -- Aggregators, entertainment & meme channels --
  "swodki", "Aggregator/entertainment",
  "readovkanews", "Aggregator/entertainment",
  "SIL0VIKI", "Aggregator/entertainment",
  "opersvodki", "Aggregator/entertainment",
  "kremlinprachka", "Aggregator/entertainment",
  "rlz_the_kraken", "Aggregator/entertainment",
  "heroesofZ", "Aggregator/entertainment",
  "zradaperemoga", "Aggregator/entertainment",
  "MedvedevVesti", "Aggregator/entertainment",
  "militarymaps", "Aggregator/entertainment",
  "warhistoryalconafter", "Aggregator/entertainment",
  "warfakes", "Aggregator/entertainment",
  "vityzeva", "Aggregator/entertainment",

  # -- Uncategorized: identity not clear enough to classify --
  "anna_news", "Uncategorized",
  "Oleg_Blokhin", "Uncategorized",
  "milinfolive", "Uncategorized",
  "rsotmdivision", "Uncategorized",
  "mardanaka", "Uncategorized",
  "Doninside", "Uncategorized",
  "vysokygovorit", "Uncategorized",
  "zola_of_renovation", "Uncategorized",
  "astramilitarum", "Uncategorized",
  "romagolovanov", "Uncategorized",
  "russ_orientalist", "Uncategorized",
  "neoficialniybezsonov", "Uncategorized",
  "wehearfromyanina", "Uncategorized",
  "new_militarycolumnist", "Uncategorized",
  "nm_dnr", "Uncategorized"
)

stopifnot(!anyDuplicated(channel_categories$source))

saveRDS(channel_categories, "2_Descriptives/channel_categories.rds")
