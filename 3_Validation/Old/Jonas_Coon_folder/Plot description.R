a <- tibble(Posts = c("1-25", "26-50", "51-75", "76-100"),
            GPT1 = rep("Yes", 4),
            GPT2 = rep("Yes", 4),
            GPT3 = rep("Yes", 4),
            Coder_1 = c("Yes", "Yes", "No", "No"),
            Coder_2 = c("Yes", "No", "Yes", "No"),
            Coder_3 = c("Yes", "No", "No", "Yes")
            )
a
rm(a)


library(tidyverse)
library(RColorBrewer)

df <- tibble(Coder = c("GPT 1", "GPT 2", "GPT 3", "Human 1", "Human 2", "Human 3"),
             "1-25" = rep("Yes", 6),
             "26-50" = c(rep("Yes", 4), "No", "No"),
             "51-75" = c(rep("Yes", 3), "No", "Yes", "No"),
             "76-100" = c(rep("Yes", 3), "No", "No", "Yes")) %>% 
  pivot_longer(!Coder, names_to = "Posts", values_to = "Coded")




ggplot(df, aes(y = fct_rev(Posts), x = Coder, fill= fct_rev(Coded))) + 
  geom_tile(color = "black") +
  theme_classic(base_size = 15) +
  scale_fill_brewer(palette ="Dark2") +
  labs(fill="Coded",
        y = "Validation Posts") +
theme(legend.position = "right",
      axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0.2))

  
ggsave("C:/Users/jonassc/Dropbox (UiO)/Apps/Overleaf/ChatGPTResearchNote/Graphics/War_Validation_Posts.pdf", width = 5, height = 3)
