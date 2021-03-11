# TidyTuesday Week 11: Bechdel Test

# Challenge found at https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-03-09/readme.md

# Get the data
raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

# Load libraries
library(tidyverse)

# Data Wrangling
movies_clean <- movies %>%
  filter(rated %in% c("G", "PG", "PG-13", "R")) %>%
  mutate(binary = fct_relevel(binary, "PASS", after = 0)) %>%
  filter(year >= 2000)

# Data Visualization
ggplot(data = movies_clean) +
  geom_bar(aes(x = rated, fill = binary), position = "dodge") +
  scale_fill_manual(values = c("#46BB74", "#DB4A4A")) +
  labs(title = "Every film rating category has more fails of the Bechdel Test\nthan passes",
       subtitle = "Movies 2000-2013",
       x = "Film rating", y = "Number of movies",
       fill = "") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) +
  coord_flip()

# save plot 
ggsave("week11.png", plot= last_plot(), height = 150, width = 200, units = "mm", dpi = 900)

