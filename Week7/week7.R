# TidyTuesday Week 7: Wealth and Income over time

# Challenge found at https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-02-09/readme.md

# Load data
income_distribution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')

# Data wrangling
income_dis_new <- income_distribution %>%
  mutate(race = case_when((race == "Asian Alone" | 
                             race == "Asian Alone or in Combination") ~ "Asian",
                          (race == "Black Alone" | 
                             race == "Black Alone or in Combination") ~ "Black",
                          (race == "White Alone" | 
                             race == "White Alone, Not Hispanic") ~ "White",
                          (race == "Hispanic (Any Race)") ~ "Hispanic",
                          (race == "All Races") ~ "All Races")) %>%
  filter(race != "All Races") %>%
  filter(year %in% seq(1960, 2025, by = 5)) %>%
  mutate(income_mean = income_mean/1000, 
         number = number/1000000) %>%
  group_by(year, race) %>%
  mutate(income_mean = mean(income_mean))

# Data visualization
income_dis_new %>%
  ggplot(aes(x = year, y = income_mean, colour = race)) +
  geom_line() +
  geom_point(aes(size = number)) +
  scale_x_continuous(limits = c(1968, 2015), 
                     breaks = seq(1965, 2020, by = 5)) +
  scale_y_continuous(limits = c(20, 140), 
                     breaks = seq(20, 140, by = 20)) +
  scale_size_continuous(limits = c(1, 100), 
                        breaks = c(1, 5, 10, 20, 60, 80 ,90)) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "How does income vary by race over the years?",
       subtitle = "Household incomes in America 1970-2015",
       x = "", y = "Mean Income (in thousands USD)", 
       size = "Number of households\n(in millions)", 
       colour = "Race")
