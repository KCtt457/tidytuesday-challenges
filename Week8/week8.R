# TidyTuesday Week 8: Dubois Challenge
# challenge01: Comparative Increase of White and Colored Population in Georgia

# Challenge found at https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-02-16/readme.md

library(tidyverse)

# Get the data
georgia_pop <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/georgia_pop.csv')

# Data wrangling
georgia_pop_new <- georgia_pop %>%
  pivot_longer(-c(Year), names_to = "Race", values_to = "Percents")

# Data Visualization
ggplot(data = georgia_pop_new) + 
  geom_line(aes(y = Percents, x = Year, linetype = Race)) +
  scale_y_continuous(trans = "reverse", 
                     breaks = seq(0, 100, by = 5),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1790, 1890, by = 10),
                     expand = c(0, 0)) +
  labs(title = "COMPARATIVE INCREASE OF WHITE AND COLORED
       POPULATION OF GEORGIA.", 
       x = "",
       y = "PERCENTS") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#ffe9d7"), 
        panel.grid = element_line(colour = "#FF8686"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.3),
        legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.key.width = unit(10, "mm"),
        plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "#ffe9d7"),
        plot.margin = unit(c(13, 13, 13, 13), "mm"),
        axis.text.x = element_text(size = 6),
        axis.title.x = element_text(size = 6)) +
  coord_flip()

# save plot 
ggsave("week8.png", plot= last_plot(), height = 190, width = 125, units = "mm", dpi = 900)

