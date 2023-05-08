
library(tidyverse)

telegrams_cleaned <- readRDS("../Data/telegrams_cleaned.rds")

top_reactions <- telegrams_cleaned %>%
  filter(date >= "2022-01-01") %>%
  select(source, reaction) %>%
  unnest(cols = c(reaction)) %>%
  na.omit() %>%
  mutate(count = as.numeric(count)) %>%
  group_by(reaction, source) %>%
  summarise(sum = sum(count, na.rm = T)) %>%
  group_by(source) %>%
  arrange(desc(sum)) %>%
  top_n(5, sum)

top_five_reactions <- top_reactions %>%
  ggplot(aes(fct_reorder(reaction, sum), sum)) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  facet_wrap(~ source, scales = "free") +
  ggtitle("Top five reactions per channel after 1st of January 2022") +
  labs(x = "", y = "") +
  theme_minimal()

top_five_reactions

ggsave(top_five_reactions, file = "./Plots/Top_Five_Reactions_Per_Channel.png",
       width = 12, height = 10, dpi = 150, units = "in", device= "png")

