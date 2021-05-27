library(tidyverse)
options(digits=2)

#importing - source of database is datagraver, results only go up to 2019 (2020 was canceled)
results <- read_csv("eurovision_song_contest_1975_2021.csv", 
                                              col_types = cols(semi_final = col_character(), 
                                              winner = col_character()))

#remove unnecessary columns
results.sub <- select(results, year, semi_final, edition, jury_or_televoting, from_country, to_country, points, winner)

#overall number of 12-point scores for juries up to 2021
results.sub %>%
  filter(jury_or_televoting == "J") %>%
  filter(points == 12) %>%
  filter(year >= 2009) %>%
  filter(semi_final == "f") %>%
  mutate(foo = ifelse(to_country == winner, 1, 0)) %>%
  filter(foo == 1) %>%
  count(from_country) -> juries
View(juries)

#points to 2021 - overall
results.sub %>%
  filter(jury_or_televoting == "J") %>%
  filter(year >= 2009) %>%
  filter(semi_final == "f") %>%
  filter(to_country == winner) %>%
  group_by(from_country) %>% 
  summarise(points_awarded_to_winner = sum(points)) %>%
  mutate(perfection_percent = (points_awarded_to_winner/144)*100) -> point_results
View(point_results)

#all correct votes up to 2021- for debugging and verifying
results.sub %>%
  filter(jury_or_televoting == "J") %>%
  filter(points == 12) %>%
  filter(year >= 2009) %>%
  filter(semi_final == "f") %>%
  mutate(foo = ifelse(to_country == winner, 1, 0)) %>%
  filter(foo == 1) -> all_winners
View(all_winners)





