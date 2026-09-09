pacman::p_load(tidyverse, fixest)

# Machine has only 4GB RAM -- keep objects lean and drop what's no longer
# needed as we go (rm + gc) rather than holding everything at once.

merged_data <- readRDS("Data/complete_data.rds")

# merged_data is already restricted to posts that mention Putin (impression_putin
# etc. are coded on that subsample), so it has no variation in "is Putin mentioned".
# What we actually want from it is the day-level explanatory panel (occupied area,
# LER, losses, and their rolling/deviation versions) -- these are constant within
# date, so we can pull one row per date and drop the rest.
day_panel <- merged_data %>%
  distinct(date, .keep_all = TRUE) %>%
  select(date, week,
         starts_with("occupied"),
         starts_with("russian_ler"), starts_with("log_russian_ler"),
         starts_with("deaths_russia"), starts_with("deaths_ukraine"),
         Russia_Total, Ukraine_Total,
         starts_with("equipment_ler"), starts_with("log_equipment_ler"))

rm(merged_data); gc()

# putin_mention is the full post population (2022-2025) with a putin_mention 0/1
# flag, so it has the actual outcome variation we need for the DV. Only keep the
# columns the models actually use.
putin_mention <- readRDS("Data/putin_mention_population_2022_2025.rds") %>%
  transmute(date = as.Date(date), source, putin_mention)

# --- Primary model: day-level aggregate ---------------------------------
# Collapse to one row per day (share of posts mentioning Putin) before doing
# any joining/modeling -- 1,461 rows, trivial on 4GB regardless of how large
# the raw post population is.
day_level <- putin_mention %>%
  group_by(date) %>%
  summarise(n_posts = n(), n_mentioned = sum(putin_mention, na.rm = TRUE)) %>%
  mutate(share_mentioned = n_mentioned / n_posts) %>%
  left_join(day_panel, by = "date")

m_day <- feols(
  share_mentioned ~ occupied_deviation + russian_ler_deviation +
    deaths_russia + deaths_ukraine,
  data = day_level,
  weights = ~n_posts
)

etable(m_day)

# --- Optional: post-level logistic regression (case-control subsample) ------
# Full post-level join is ~2M rows and a source fixed effect adds ~80 dummy
# levels on top -- that combination is what OOM'd before. Two changes here
# keep it light:
#   1. No `| source` fixed effect, just clustered SEs (clustering is cheap,
#      the FE demeaning over many levels was the expensive part).
#   2. Downsample the putin_mention == 0 rows. For logistic regression this
#      is a valid case-control design: subsampling controls only biases the
#      intercept, the slope coefficients stay consistent.
# set.seed(1)
# putin_mention_cc <- putin_mention %>%
#   filter(putin_mention == 1 | runif(n()) < 0.15)
#
# model_data_cc <- putin_mention_cc %>%
#   left_join(day_panel, by = "date")
#
# m_post <- feglm(
#   putin_mention ~ occupied_deviation + russian_ler_deviation +
#     deaths_russia + deaths_ukraine,
#   data = model_data_cc,
#   family = binomial(),
#   cluster = ~source
# )
