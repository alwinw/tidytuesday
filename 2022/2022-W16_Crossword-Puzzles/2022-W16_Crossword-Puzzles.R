# TidyTuesday Week 16 Crossword Puzzles and Clues

library(tidyverse)
theme_set(theme_bw())

# Data
big_dave <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/big_dave.csv")
times <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv")

df_cw <- rbind(big_dave, times) %>%
  mutate(
    puzzle_type = str_to_title(gsub("[^a-zA-Z]+$", "", puzzle_name)),
    n_clue = nchar(clue),
    n_answer = nchar(answer),
    phrase = ifelse(grepl(" ", answer), "Answer: Phrase", "Answer: Word")
  ) %>%
  filter(
    puzzle_type != "",
    !is.na(n_clue),
    !is.na(n_answer),
  ) %>%
  add_count(puzzle_type) %>%
  filter(n >= 100) %>%
  select(-n) %>%
  group_by(puzzle_type)


df_cw %>%
  group_by(puzzle_type) %>%
  summarise(n = n()) %>%
  arrange(-n)


df_cw %>%
  ungroup() %>%
  filter(!phrase) %>%
  arrange(-n_answer) %>%
  select(answer)


p <- ggplot(df_cw) +
  geom_point(aes(x = n_clue, y = n_answer))

p <- ggplot(df_cw) +
  geom_boxplot(aes(x = puzzle_type, y = n_answer)) +
  facet_grid(phrase ~ source, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(caption = "#TidyTuesday week 16 | vis: alwinw |  Data from Cryptics.georgeho.org")

# Sort by medians
# Colour by source

print(p)


ggsave("2022-W15_word-frequency.png", p, height = 6, width = 8)
