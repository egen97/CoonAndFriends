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

# ==========================================================================
# Part 3: does any of this differ by channel type? (scope conditions)
# ==========================================================================
# Channel categories are treated as scope conditions, not moderators: some
# outcomes structurally can't occur in some groups (a state channel is not
# going to criticize Putin), so each group gets its own set of models here
# rather than one pooled model with an interaction term.
channel_categories <- readRDS("2_Descriptives/channel_categories.rds")

# --- Part 1, by group: does the mention-rate result hold within each channel type? ---
putin_mention_grouped <- readRDS("Data/putin_mention_population_2022_2025.rds") %>%
  transmute(date = as.Date(date), source, putin_mention) %>%
  left_join(channel_categories, by = "source") %>%
  mutate(category = replace_na(category, "Uncategorized"))

day_panel_t <- day_panel %>% mutate(t = as.numeric(date - min(date)))

day_level_grouped <- putin_mention_grouped %>%
  group_by(category, date) %>%
  summarise(n_posts = n(), n_mentioned = sum(putin_mention, na.rm = TRUE), .groups = "drop") %>%
  mutate(share_mentioned = n_mentioned / n_posts) %>%
  left_join(day_panel_t, by = "date")

groups <- sort(unique(day_level_grouped$category))

m_day_by_group <- setNames(lapply(groups, function(g) {
  feols(
    share_mentioned ~ occupied_deviation + russian_ler_deviation + deaths_russia + deaths_ukraine,
    data = filter(day_level_grouped, category == g), weights = ~n_posts
  )
}), groups)
etable(m_day_by_group)

# PMC/paramilitary-affiliated is the one group where occupied territory
# predicts the mention rate -- check it against raw level and level+trend
# too, the same way Part 1 stress-tested the pooled (null) result.
d_pmc <- filter(day_level_grouped, category == "PMC/paramilitary-affiliated")
m_pmc_dev <- feols(share_mentioned ~ occupied_deviation + russian_ler_deviation + deaths_russia + deaths_ukraine, data = d_pmc, weights = ~n_posts)
m_pmc_lvl <- feols(share_mentioned ~ occupied + russian_ler + deaths_russia + deaths_ukraine, data = d_pmc, weights = ~n_posts)
m_pmc_trd <- feols(share_mentioned ~ occupied + russian_ler + deaths_russia + deaths_ukraine + t, data = d_pmc, weights = ~n_posts)
etable(m_pmc_dev, m_pmc_lvl, m_pmc_trd)

rm(putin_mention_grouped, day_level_grouped); gc()

# --- Part 2, by group: do the tone outcomes and their drivers differ by channel type? ---
tone_data_grouped <- readRDS("Data/complete_data.rds") %>%
  filter(!is.na(impression_putin)) %>%
  left_join(channel_categories, by = "source") %>%
  mutate(
    category = replace_na(category, "Uncategorized"),
    impression_putin = as.numeric(impression_putin),
    support_putin = as.numeric(support_putin),
    criticism_putin = as.numeric(criticism_putin),
    war_mention = as.numeric(war_mention),
    post_type = as.numeric(post_type),
    t = as.numeric(date - min(date))
  )

# Descriptive check first -- the more basic version of the scope-condition
# claim: if criticism barely happens at all in State channels, a regression
# coefficient estimated within that group isn't measuring much either way.
tone_data_grouped %>%
  group_by(category) %>%
  summarise(
    n = n(),
    impression = round(mean(impression_putin), 2),
    support = round(mean(support_putin), 3),
    criticism = round(mean(criticism_putin), 3),
    war_mention = round(mean(war_mention), 3),
    post_type = round(mean(post_type), 3)
  ) %>%
  arrange(desc(n))

tone_outcomes <- c("impression_putin", "support_putin", "criticism_putin", "war_mention", "post_type")
tone_groups <- sort(unique(tone_data_grouped$category))

# Same three-spec battery as Part 2 (deviation / raw level / raw level +
# trend), just looped over outcome x group instead of hand-written -- 75
# models is too many to write out explicitly the way Part 2's 15 were.
# Two-way clustering on date & source throughout, as before. Note PMC (4
# channels) and State/official media (9 channels) have few source clusters,
# so cluster-robust SEs here are less trustworthy for those two groups than
# for the others, and less trustworthy than the day-level model above (which
# doesn't need source clustering at all, since it's already one row per day).
run_spec <- function(outcome, rhs, d) {
  feols(as.formula(paste(outcome, "~", rhs)), data = d, cluster = ~date + source)
}

group_results <- list()
for (g in tone_groups) {
  d <- filter(tone_data_grouped, category == g)
  for (oc in tone_outcomes) {
    m_dev <- run_spec(oc, "occupied_deviation + russian_ler_deviation + deaths_russia_deviation + deaths_ukraine_deviation", d)
    m_lvl <- run_spec(oc, "occupied + russian_ler + deaths_russia + deaths_ukraine", d)
    m_trd <- run_spec(oc, "occupied + russian_ler + deaths_russia + deaths_ukraine + t", d)
    for (spec_name in c("dev", "lvl", "trd")) {
      m <- get(paste0("m_", spec_name))
      ct <- as.data.frame(coeftable(m))
      ct$var <- rownames(ct)
      ct$group <- g; ct$outcome <- oc; ct$spec <- spec_name; ct$n <- nobs(m)
      group_results[[length(group_results) + 1]] <- ct
    }
  }
}

group_results <- bind_rows(group_results) %>%
  as_tibble() %>%
  filter(!var %in% c("(Intercept)", "t")) %>%
  mutate(var = str_remove(var, "_deviation$")) %>%
  select(group, outcome, spec, var, n, Estimate, `Std. Error`, `Pr(>|t|)`)

# Classify each (group, outcome, variable) cell the same way Part 2 did:
# does the effect hold up across all three specs (Robust), only once a trend
# is controlled for (Mixed), only in the naive raw level (Spurious -- the
# pattern Part 1 and 2 both ruled out), only in the already-detrended
# deviation spec (Deviation-only), or nowhere (Null).
group_status <- group_results %>%
  mutate(sig = `Pr(>|t|)` < 0.05, sign = sign(Estimate)) %>%
  select(group, outcome, var, spec, Estimate, sig, sign) %>%
  pivot_wider(names_from = spec, values_from = c(Estimate, sig, sign)) %>%
  mutate(status = case_when(
    sig_dev & sig_lvl & sig_trd & sign_dev == sign_lvl & sign_dev == sign_trd ~ "Robust",
    sig_lvl & sig_trd & !sig_dev ~ "Mixed",
    sig_lvl & !sig_trd ~ "Spurious",
    sig_dev & !sig_lvl & !sig_trd ~ "Deviation-only",
    TRUE ~ "Null"
  ))

group_status %>% filter(status != "Null") %>% arrange(outcome, var, group)

saveRDS(group_results, "2_Descriptives/tone_group_results.rds")
