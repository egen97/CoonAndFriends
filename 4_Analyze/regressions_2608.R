### PRE AMBLE  ####

pacman::p_load(tidyverse, fixest, marginaleffects, ggthemes)

complete <- readRDS("Data/complete_data.rds")

complete$impression_putin <- as.numeric(complete$impression_putin) #Many of the dependent variabels are in character fromat

complete$support_putin <- as.numeric(complete$support_putin)

######## Deviations from 14-day rolling mean ####



#################################################
# 1️⃣ Impression Models
#################################################

# Main effects
m_imp_ler  <- feols(impression_putin ~ russian_ler_deviation, data = complete, vcov = "hetero")
m_imp_occ  <- feols(impression_putin ~ occupied_deviation, data = complete, vcov = "hetero")

# Interaction model
m_imp_int  <- feols(
  impression_putin ~ russian_ler_deviation * occupied_deviation,
  data = complete, vcov = "hetero"
)

etable(m_imp_ler, m_imp_occ, m_imp_int)


#################################################
# 2️⃣ Plot Interaction (Impression)
#################################################

plot_predictions(
  m_imp_int,
  by = c("occupied_deviation", "russian_ler_deviation"),
  newdata = datagrid(
    occupied_deviation     = c(-10, 0, 5),
    russian_ler_deviation  = c(-3.8, -0.08, 5)
  ),
  vcov = FALSE # Marginaleffects cannot use FEOLS models
) +
  theme_excel_new() +
  labs(
    x = "Deviation from 14-day mean of occupied territory",
    y = "Impression of Putin",
    colour = "Deviation from 14-day mean of Russian LER",
    fill   = "Deviation from 14-day mean of Russian LER"
  ) +
  theme(
    legend.position = "right",
    text = element_text(size = 14),
    strip.text = element_text(size = 15)
  )


#################################################
# 3️⃣ Support Models
#################################################

# Main effects
m_sup_ler  <- feols(support_putin ~ russian_ler_deviation, data = complete)
m_sup_occ  <- feols(support_putin ~ occupied_deviation, data = complete)

# Interaction model
m_sup_int  <- feols(
  support_putin ~ russian_ler_deviation * occupied_deviation,
  data = complete
)

etable(m_sup_ler, m_sup_occ, m_sup_int)


#################################################
# 4️⃣ SD-Flag Models (Support)
#################################################

m_sup_sd_ler  <- feols(support_putin ~ russian_ler_sd_flag, data = complete)
m_sup_sd_occ  <- feols(support_putin ~ occupied_sd_flag, data = complete)

m_sup_sd_int  <- feols(
  support_putin ~ occupied_sd_flag * russian_ler_sd_flag,
  data = complete
)

etable(m_sup_sd_ler, m_sup_sd_occ, m_sup_sd_int)

