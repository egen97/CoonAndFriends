### PRE AMBLE  ####

pacman::p_load(tidyverse, fixest, marginaleffects, ggthemes)

day_data <- readRDS("Data/complete_data.rds")

day_data$impression_putin <- as.numeric(day_data$impression_putin) #Many of the dependent variabels are in character fromat

day_data$support_putin <- as.numeric(day_data$support_putin)

###### Impression Models #####

# Model 1: Effect of Russian loss-exchange ratio on impressions of Putin
ler_imp <- feols(impression_putin ~ log_russian_ler, data = day_data)
summary(ler_imp)
# No statistically significant association between battlefield efficiency
# (log LER) and impressions of Putin.

# Model 2: Equipment loss-exchange ratio
equip_imp <- feols(impression_putin ~ eqipment_ler, data = day_data)
summary(equip_imp)
# Positive association: when Russia destroys relatively more equipment
# than it loses, impressions of Putin improve.

# Model 3: Territorial control
area_imp <- feols(impression_putin ~ occupied, data = day_data)
summary(area_imp)
# Unexpected negative association: greater occupied area is associated
# with lower impressions of Putin. This runs counter to the expectation
# that territorial gains increase leader approval.

# Model 4: Interaction between territorial control and battlefield efficiency
area_loss <- lm(impression_putin ~ occupied * log_russian_ler, data = day_data)
summary(area_loss)
# Both main effects are significant:
# - Occupied area: negative
# - Log LER: positive
# The interaction term suggests that the negative effect of occupied area
# becomes stronger as the LER increases. This is contrary to expectations,
# as one might anticipate battlefield efficiency to reinforce the
# positive political value of territorial control.


plot_predictions(
  area_loss,
  by = c("occupied", "log_russian_ler"),
  newdata = datagrid(occupied = c(0, 5, 13.2, 20),
                     log_russian_ler = c(-9, 0, 0.1, 1.09, 8.9))
  ) +
  theme_excel_new() +
  theme(
    axis.title.x = element_text(),
    axis.title.y = element_text(),
    legend.title = element_text()
  ) +
  labs(x = "Occupied area",
       y = "Impression of Putin",
       colour = "Ln Russian Loss-Exchange ratio",
       fill = "Ln Russian Loss-Exchange ratio"
  ) +
  theme(
    plot.caption = element_text(size = 14),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size = 16)
  )

#### Support Putin


# Model 1: LER only
ler_supp <- feols(support_putin ~ log_russian_ler, data = day_data)
summary(ler_supp)

# Significant positive but unsubstantial effect of LER

# Model 2: Equipment LER
equip_supp <- feols(support_putin ~ eqipment_ler, data = day_data)
summary(equip_supp)
# Positive relationship: greater relative equipment destruction
# is associated with higher support for Putin.

# Model 3: Territorial control
area_support <- feols(support_putin ~ occupied, data = day_data)
summary(area_support)
# Positive association: increases in occupied territory correspond
# with higher support for Putin, consistent with expectations. However, effect is very weak :(

# Model 4: Interaction between territory and battlefield efficiency
area_int_support <- lm(support_putin ~ occupied * log_russian_ler, data = day_data)
summary(area_int_support)

# Interpretation:
# - Occupied area has a positive effect on support.
# - Log LER increases baseline support.
# - The interaction suggests that battlefield efficiency amplifies
#   the positive effect of territorial control on support

plot_predictions(
  area_int_support,
  by = c("occupied", "log_russian_ler"),
  newdata = datagrid(occupied = c(0, 5, 13.2, 20),
                     log_russian_ler = c(-9, 0, 0.1, 1.09, 8.9))
) +
  theme_excel_new() +
  theme(
    axis.title.x = element_text(),
    axis.title.y = element_text(),
    legend.title = element_text()
  ) +
  labs(x = "Occupied area",
       y = "Support of Putin",
       colour = "Ln Russian Loss-Exchange ratio",
       fill = "Ln Russian Loss-Exchange ratio"
  ) +
  theme(
    plot.caption = element_text(size = 14),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size = 16)
  )
