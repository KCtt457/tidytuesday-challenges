# TidyTuesday Week 12: Video Games + Sliced

# Challenge found at https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-03-16/readme.md

library(tidyverse)
library(wordcloud)
library(wordcloud2)
library(stringi)
library(scales)

# Get the data
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

# Data wrangling
# fix character encoding for word cloud
games_clean <- games %>%
  mutate(gamename = stri_trans_general(games$gamename, "ASCII-Latin")) %>%
  mutate(gamename = str_replace(stri_escape_unicode(gamename), "\\\\u\\w+", "")) %>%
  mutate(gamename = str_replace_all(gamename, "<U\\+\\w+>\\d?\\s?", "")) %>%
  mutate(gamename = str_replace_all(gamename, "\\\\", ""))

games2020 <- games_clean[order(-games_clean$peak),] %>%
  filter(year == 2020) %>%
  distinct(gamename, .keep_all = TRUE) %>%
  select(gamename, peak)

top_8_games <- head(games2020, 8) %>%
  mutate(gamename = fct_reorder(as_factor(gamename), peak))

top_8_games_numbered <- games_clean %>%
  filter(gamename %in% top_8_games$gamename, !is.na(avg)) %>%
  select(gamename, year, avg) %>%
  group_by(gamename, year) %>%
  summarise(avg = mean(avg)) %>%
  # number games
  mutate(gamename1 = case_when(gamename == "Counter-Strike: Global Offensive" ~ "1. Counter-Strike: Global Offensive",
                               gamename == "PLAYERUNKNOWN'S BATTLEGROUNDS" ~ "2. PLAYERUNKNOWN'S BATTLEGROUNDS",
                               gamename == "Cyberpunk 2077" ~ "3. Cyberpunk 2077", 
                               gamename == "Dota 2" ~ "4. Dota 2",
                               gamename == "Terraria" ~ "5. Terraria",
                               gamename == "Life is Strange 2" ~ "6. Life is Strange 2",
                               gamename == "Monster Hunter: World" ~ "7. Monster Hunter: World",
                               gamename == "Grand Theft Auto V" ~ "8. Grand Theft Auto V",
                               gamename == "Mount & Blade II: Bannerlord" ~ "9. Mount & Blade II: Bannerlord"))


# Data visualization

# word cloud
set.seed(1234) # for reproducibility 
# save as png
png("wordcloud.png", height = 150, width = 200, units = "mm", res=300)

wordcloud(words = games2020$gamename, 
          scale = c(1, 0.5), 
          freq = games2020$peak, 
          min.freq = 100, max.words=20, 
          random.order=FALSE, rot.per=0.35,
          colors= c("#349beb", "#1ACA33", "#D2B21F", 
                    "pink", "#de8e1f", "yellow", "#c4213f"))

# line chart
top_8_games_numbered %>%
  ggplot(aes(x = year, y = avg, colour = gamename1)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 800000), 
                     breaks = seq(0, 800000, by = 200000), 
                     label = label_number_si()) +
  scale_x_continuous(limits = c(2012, 2020)) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "How player participation has varied over the years",
       subtitle = "Top 8 multiplayer Steam games in 2020*",
       x = "", y = "Monthly average number of players",
       colour = "Game Name",
       caption = "* ordered by highest number of players at the same time") +
  theme_minimal()

# save plot 
ggsave("week12.png", plot= last_plot(), height = 150, width = 200, units = "mm", dpi = 900)
