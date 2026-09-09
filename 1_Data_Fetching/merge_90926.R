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

# --- Week-level aggregate (n-week averaging) --------------------------------
# Same idea as day_level, but collapsed to a continuous week index (not raw
# ISO week number, so 2022/2023/... don't collide) instead of date, to test
# whether posters react to the broader trend rather than day-to-day noise.
# Trade-off: ~1,300 days becomes ~185 weeks, so less power to pick up
# short-lived reactions, but plausibly a better match to what posters notice.
week_start <- min(day_panel$date)

week_panel <- day_panel %>%
  mutate(week_id = as.integer(floor((date - week_start) / 7))) %>%
  group_by(week_id) %>%
  summarise(across(
    c(occupied, occupied_deviation, occupied_rollmean,
      russian_ler, russian_ler_deviation, russian_ler_rollmean,
      deaths_russia, deaths_ukraine, Russia_Total, Ukraine_Total, equipment_ler),
    ~mean(.x, na.rm = TRUE)
  ))

week_level <- putin_mention %>%
  mutate(week_id = as.integer(floor((date - week_start) / 7))) %>%
  group_by(week_id) %>%
  summarise(n_posts = n(), n_mentioned = sum(putin_mention, na.rm = TRUE)) %>%
  mutate(share_mentioned = n_mentioned / n_posts) %>%
  left_join(week_panel, by = "week_id")

m_week <- feols(
  share_mentioned ~ occupied_deviation + russian_ler_deviation +
    deaths_russia + deaths_ukraine,
  data = week_level,
  weights = ~n_posts
)

etable(m_day, m_week)

# --- Optional: post-level logistic regression (case-control subsample) ------
# Full post-level join is ~2M rows and a source fixed effect adds ~80 dummy
# levels on top -- that combination is what OOM'd before. Two changes here
# keep it light:
#   1. No `| source` fixed effect, just clustered SEs (clustering is cheap,
#      the FE demeaning over many levels was the expensive part).
#   2. Downsample the putin_mention == 0 rows. For logistic regression this
#      is a valid case-control design: subsampling controls only biases the
#      intercept, the slope coefficients stay consistent.
 set.seed(1)
 putin_mention_cc <- putin_mention %>%
   filter(putin_mention == 1 | runif(n()) < 0.15)

 model_data_cc <- putin_mention_cc %>%
   left_join(day_panel, by = "date")

# The regressors only vary by date (every post on the same day gets the same
# occupied_deviation/russian_ler_deviation/... value), so clustering on
# `source` alone understates SEs -- posts sharing a day aren't independent
# draws given the regressors (Moulton problem). Two-way cluster on date and
# source instead; this is just an SE computation (Cameron-Gelbach-Miller),
# not additional fixed-effect demeaning, so it doesn't reintroduce the OOM risk.
 m_post <- feglm(
   putin_mention ~ occupied_deviation + russian_ler_deviation +
     deaths_russia + deaths_ukraine,
   data = model_data_cc,
   family = binomial(),
   cluster = ~date + source
 )

summary(m_post)

# ==========================================================================
# Part 2: how is Putin discussed, conditional on being mentioned?
# ==========================================================================
# Unlike Part 1, complete_data.rds *is* the right analysis sample here: it's
# already restricted to posts that mention Putin, coded for tone, and already
# has the day-level war covariates joined onto each post. No population data
# or case-control downsampling needed.
rm(putin_mention, putin_mention_cc, model_data_cc, day_level, week_level); gc()

tone_data <- readRDS("Data/complete_data.rds") %>%
  filter(!is.na(impression_putin)) %>%
  mutate(
    impression_putin = as.numeric(impression_putin),
    support_putin = as.numeric(support_putin),
    criticism_putin = as.numeric(criticism_putin),
    war_mention = as.numeric(war_mention),
    post_type = as.numeric(post_type),
    t = as.numeric(date - min(date))
  )

# impression_putin is ordinal (0-3, almost all mass on 1-3); support_putin,
# criticism_putin, war_mention and post_type are binary 0/1. Treated as
# continuous OLS throughout, consistent with regressions_2608.R.
#
# Same three-way check as Part 1, since these covariates have the same
# slow-moving, trending problem: deviation-from-14-day-mean (already
# detrended), raw level (naive baseline), and raw level with an explicit
# linear time trend added (does the level result survive detrending?).
# Two-way clustering on date & source throughout, for the same reason as
# m_post -- the war covariates only vary by day.

# --- impression_putin ---------------------------------------------------
m_impression_dev <- feols(
  impression_putin ~ occupied_deviation + russian_ler_deviation +
    deaths_russia_deviation + deaths_ukraine_deviation,
  data = tone_data, cluster = ~date + source
)
m_impression_level <- feols(
  impression_putin ~ occupied + russian_ler + deaths_russia + deaths_ukraine,
  data = tone_data, cluster = ~date + source
)
m_impression_trend <- feols(
  impression_putin ~ occupied + russian_ler + deaths_russia + deaths_ukraine + t,
  data = tone_data, cluster = ~date + source
)
etable(m_impression_dev, m_impression_level, m_impression_trend)

# --- support_putin --------------------------------------------------------
m_support_dev <- feols(
  support_putin ~ occupied_deviation + russian_ler_deviation +
    deaths_russia_deviation + deaths_ukraine_deviation,
  data = tone_data, cluster = ~date + source
)
m_support_level <- feols(
  support_putin ~ occupied + russian_ler + deaths_russia + deaths_ukraine,
  data = tone_data, cluster = ~date + source
)
m_support_trend <- feols(
  support_putin ~ occupied + russian_ler + deaths_russia + deaths_ukraine + t,
  data = tone_data, cluster = ~date + source
)
etable(m_support_dev, m_support_level, m_support_trend)

# --- criticism_putin --------------------------------------------------------
m_criticism_dev <- feols(
  criticism_putin ~ occupied_deviation + russian_ler_deviation +
    deaths_russia_deviation + deaths_ukraine_deviation,
  data = tone_data, cluster = ~date + source
)
m_criticism_level <- feols(
  criticism_putin ~ occupied + russian_ler + deaths_russia + deaths_ukraine,
  data = tone_data, cluster = ~date + source
)
m_criticism_trend <- feols(
  criticism_putin ~ occupied + russian_ler + deaths_russia + deaths_ukraine + t,
  data = tone_data, cluster = ~date + source
)
etable(m_criticism_dev, m_criticism_level, m_criticism_trend)

# --- war_mention ------------------------------------------------------------
m_warmention_dev <- feols(
  war_mention ~ occupied_deviation + russian_ler_deviation +
    deaths_russia_deviation + deaths_ukraine_deviation,
  data = tone_data, cluster = ~date + source
)
m_warmention_level <- feols(
  war_mention ~ occupied + russian_ler + deaths_russia + deaths_ukraine,
  data = tone_data, cluster = ~date + source
)
m_warmention_trend <- feols(
  war_mention ~ occupied + russian_ler + deaths_russia + deaths_ukraine + t,
  data = tone_data, cluster = ~date + source
)
etable(m_warmention_dev, m_warmention_level, m_warmention_trend)

# --- post_type ----------------------------------------------------------
m_posttype_dev <- feols(
  post_type ~ occupied_deviation + russian_ler_deviation +
    deaths_russia_deviation + deaths_ukraine_deviation,
  data = tone_data, cluster = ~date + source
)
m_posttype_level <- feols(
  post_type ~ occupied + russian_ler + deaths_russia + deaths_ukraine,
  data = tone_data, cluster = ~date + source
)
m_posttype_trend <- feols(
  post_type ~ occupied + russian_ler + deaths_russia + deaths_ukraine + t,
  data = tone_data, cluster = ~date + source
)
etable(m_posttype_dev, m_posttype_level, m_posttype_trend)
