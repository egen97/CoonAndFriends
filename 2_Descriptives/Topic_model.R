
library(tidyverse)
library(quanteda)
library(tidytext)
library(stm)
library(furrr)
library(purrr)

set.seed(369)

topicmodel_data <- readRDS("./Data/Telegrams/telegrams_cleaned_wartime_pasted_putin.rds") %>%
  mutate(file_id = paste0(source, "_", id)) %>%
  rename(text = message)

topicmodel_dfm <- topicmodel_data %>%
  select(file_id, text) %>%
  unnest_tokens(input = text,
                output = word,
                token = "words") %>%
  group_by(file_id, word) %>%
  count() %>%
  cast_dfm(document = file_id,
           term = word,
           value = n)

topicmodel_dfm$file_id <- topicmodel_data$file_id

## Trim dfm? Not done here because the preprocesing has already been quite comprehensive
topicmodel_dfm_trim <- dfm_trim(topicmodel_dfm,
                                min_docfreq = 0.01,
                                max_docfreq = 0.99,
                                docfreq_type = "prop")


#### SEARCH K ####

plan(multisession) # Plan for parallel processing using several sessions

K <- c(15, 25, 35, 55, 75, 95) # Choose the number of topics for each model

many_models <- tibble(K = K) %>% # Making a tibble with the number of topics as variable
  mutate(topic_model = future_map(K, ~ stm(topicmodel_dfm_trim, # Estimate the topic model using stm and the dfm we made earlier
                                           K = ., # Vary only K -- the dot refers back to the K in the tibble we made
                                           verbose = FALSE), # Stay quiet regarding output
                                  .options = furrr_options(seed = TRUE))) # An option to suppress a warning message

save(many_models, file = "./Data/Topic_model/many_models.rda")

heldout <- make.heldout(topicmodel_dfm_trim) # Making the heldout measure

k_result <- many_models %>% # Using mutate to make new variables
  mutate(exclusivity = map(topic_model, exclusivity),  # Using map to iterate over all topic models in the many_models object and fetch the exclusivity measure
         semantic_coherence = map(topic_model, semanticCoherence, topicmodel_dfm_trim), # Fetching semantic coherence measure
         eval_heldout = map(topic_model, eval.heldout, heldout$missing), # Feting the heldout measure
         residual = map(topic_model, checkResiduals, topicmodel_dfm_trim), # Fething the residuals measure
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound))) # Fetching the number of iterations for each model

k_result %>%
  transmute(K, # Make a new dataframe out of the old one starting with K (number of topics)
            Residuals = map_dbl(residual, "dispersion"), # Adding residual measure
            `Exclusivity` = map_dbl(exclusivity, mean), # Adding exclusivity measure
            `Semantic coherence` = map_dbl(semantic_coherence, mean), # Adding semantic coherence measure
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>% # Adding held-out likelihood measure
  gather(Metric, Value, -K) %>% # Make it into a long dataframe
  ggplot(aes(K, Value, color = Metric)) + # Plot number of topic agains the different metrics (diagnostics)
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) + # Make a line graph
  geom_vline(xintercept = 25, linetype = "dashed") +
  facet_wrap(~Metric, scales = "free_y") + # Make small different plots for each metric
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "The optimal number of topics seems to be around 25") +
  theme_bw()

#### MODELLING ####

# Converting dfm to g in order to retain documents after having ran the data through stm
g <- convert(topicmodel_dfm_trim, to = "stm")

## Dropped empty documents:
# SolovievLive_100023, SolovievLive_104189, SolovievLive_110678, SolovievLive_121153, SolovievLive_135931,
# SolovievLive_139666, SolovievLive_140969, SolovievLive_142130, SolovievLive_143273, SolovievLive_144714,
# SolovievLive_156092, SolovievLive_157599, SolovievLive_167140, SolovievLive_168663, SolovievLive_170288,
# SolovievLive_174810, SolovievLive_178853, SolovievLive_184093, SolovievLive_190656, SolovievLive_192236,
# SolovievLive_195600, SolovievLive_208133, SolovievLive_212554, SolovievLive_213833, SolovievLive_215334,
# SolovievLive_216574, SolovievLive_95285, swodki_160459

# Running the stm model
model_stm <- stm(documents = g$documents,
                 vocab = g$vocab,
                 data = g$meta,
                 seed = 369,
                 K = 25, # Likely good number of topics according to K-test
                 max.em.its = 1000,
                 verbose = TRUE,
                 init.type = "Spectral")

save(model_stm, file = "./Data/Topic_model/stm.rda")
save(g, file = "./Data/Topic_model/g.rda")


#### LABELLING TOPICS ####

topicscore <- as_tibble(model_stm$theta) %>%
  mutate(file_id = g$meta$file_id) %>%
  gather(colnames(.[,1:25]), key = "topic", value = "score")

saveRDS(topicscore, file = "./Data/Topic_model/topicscore.rds")

### Check no. 1: Look at the questions that load highest on each topic

highload <- topicscore %>%
  left_join(topicmodel_data, by = c("file_id")) %>%
  select(source, id, date, topic, score, text) %>%
  group_by(topic) %>%
  top_n(10, score)

# write_excel_csv2(highload, file = "./Data/Topic_model/highload.csv")

### Check no. 2: Look at the words with the highest probability and frex for each topic

topic_label <- labelTopics(model_stm, n = 20)

prob <- unlist(topic_label$prob) %>%
  as_tibble() %>%
  unite("words", V1:V20, sep = " ") %>%
  select(words)

frex <- unlist(topic_label$frex) %>%
  as_tibble() %>%
  unite("words", V1:V20, sep = " ") %>%
  select(words)

prob_frex <- as_tibble(list(topic = topic_label$topicnums,
                            prob = prob$words,
                            frex = frex$words))

# write_excel_csv2(prob_frex, file = "./Data/Topic_model/prob_frex.csv")


