
# This script produces the descriptive stats that are reportede in the SI
# author: Subhayan Mukerjee

library(rjson)
library(tidyverse)
library(lubridate)
library(lfe)
library(glue)
library(tidymodels)
library(ggrepel)
library(modelsummary)

setwd("path/to/parent/folder/of/scripts/folder")

params <- fromJSON(file = "params/params_60-seasons.json")

all_seasons <- 1:params$n_seasons

season_topic_tbl <- read_csv(glue("auxiliary/optimal-topics-per-season_cv_{params$n_seasons}-seasons.csv"))

media_label_map <- read_csv("auxiliary/media_label_map.csv")
media_details_tbl <- read_csv("auxiliary/media_language.csv")
media_ideology <- read_csv("auxiliary/media_ideo.csv")

media_details <- media_label_map |>
  inner_join(media_details_tbl, by = c("label" = "media")) |>
  select(n, label, short_name)

media_ideology <- media_ideology %>%
  inner_join(media_label_map) %>%
  select(media, label, short_name, ideo)

media_details <- media_details |>
  inner_join(media_ideology) |>
  select(n, label, ideo)

all_s_media_tbl <- NULL
for(s in all_seasons) {
  
  n_topics <- season_topic_tbl %>%
    filter(season == s) %>%
    pull(k)
  
  s_dat <- read_csv(glue('path/to/',
                         '{params$n_seasons}-seasons/modeling/',
                         'season-{s}_{n_topics}-topics-modeling-data.csv'))
  
  s_dat <- s_dat |>
    select(media_id, date)
  
  all_s_media_tbl <- all_s_media_tbl |>
    rbind(s_dat)
}

media_posts_count <- all_s_media_tbl |>
  group_by(media_id) |>
  summarise(n = n()) |>
  inner_join(media_details, by = c("media_id" = "n")) |>
  arrange(ideo)

ggplot(media_posts_count, aes(x = ideo, y = n/1000, label = label)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text_repel(size = 3.5) +  # Automatically moves labels so they don't overlap
  theme_minimal() +
  labs(
    x = "Media Slant",
    y = "Number of Posts (in thoousands)"
  ) +
  # Optional: add a trend line to see the correlation
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 0.5)

window_size <- 1
start_season <- 1
end_season <- params$n_seasons

final_dat <- NULL
for(s in all_seasons[start_season:end_season]) {
  
  n_topics <- season_topic_tbl %>%
    filter(season == s) %>%
    pull(k)
  
  s_dat <- read_csv(glue('path/to/',
                         '{params$n_seasons}-seasons/modeling/',
                         'season-{s}_{n_topics}-topics-modeling-data.csv'), )
  
  s_dat <- s_dat %>%
    mutate(season = s)  %>%
    select(media_id, id, everything()) %>%
    rename(day = date)
  
  # create a new column "window" which can be 1 day, 2 days ... 7 days
  s_dat <- s_dat %>%
    mutate(window = ceiling(day/window_size)) %>%
    select(media_id, id, window, everything())
  
  
  # for each window, for each media outlet,
  # calculate the frequency of posts
  post_freq <- s_dat %>%
    select(media_id, id, window) %>%
    group_by(media_id, window) %>%
    summarize(post_freq = n())
  
  ## for each window, for all outlets,
  # calculate the frequency of posts
  post_freq_all <- s_dat %>%
    select(id, window) %>%
    group_by(window) %>%
    summarize(post_freq = n())
  
  # for each window, for each media outlet,
  # calculate the total frequency of each topic
  topic_freq_tbl <- s_dat %>%
    select(media_id, window, paste0("topic", 1:n_topics)) %>%
    group_by(media_id, window) %>%
    # summarize_at(paste0("topic", 1:n_topics), sum) %>% # sum the probabilities of all topics per outlet per window
    summarize(across(starts_with("topic"), ~ sum(.x, na.rm = TRUE))) %>%
    rename_with(temp <- function(x) { # rename topic1 => topic1_freq, topic2 => topic2_freq
      paste0(x, "_freq")
    }, starts_with("topic"))
  
  all_topic_freq_cols <- paste0("topic", 1:n_topics, "_freq")
  
  topic_freq_long_tbl <- topic_freq_tbl %>%
    pivot_longer(cols = all_of(all_topic_freq_cols), names_to = "topic", values_to = "freq") %>%
    mutate(topic = gsub("topic", "", topic)) %>%
    mutate(topic = gsub("_freq", "", topic)) %>%
    mutate(season = s)
  
  final_dat <- final_dat |>
    rbind(topic_freq_long_tbl)
  
}

politics_topics <- read_csv("auxiliary/topic_politicization_handcode_60.csv")

politics_topics <- politics_topics |>
  rename("political" = 3) |>
  mutate(final_topic = glue("{season}_{topic}"),
         political = ifelse(political > 0, 1, 0)) |> # political = 1 or 2
  select(final_topic, political)

media_pol_prop <- final_dat |>
  group_by(media_id, season, topic) |>
  summarize(total_freq = sum(freq)) |>
  mutate(final_topic = glue("{season}_{topic}")) |>
  ungroup() |>
  left_join(politics_topics, by = "final_topic") |>
  select(media_id, final_topic, political, total_freq) |>
  group_by(media_id, political) |>
  summarize(total_freq = sum(total_freq)) |>
  ungroup() |> 
  pivot_wider(
    names_from = political, 
    values_from = total_freq, 
    names_prefix = "political_"
  ) %>%
  # Calculate the proportion of political (1) out of the total for that media_id
  mutate(
    prop_political = political_1 / (political_0 + political_1)
  ) |>
  select(media_id, prop_political) |>
  inner_join(media_posts_count)

media_pol_prop <- media_pol_prop |>
  select(label, ideo, n, prop_political) |>
  arrange(ideo)

formatted_table <- media_pol_prop %>%
  mutate(
    # Force ideo to exactly 2 decimal places (as character)
    ideo = sprintf("%.2f", ideo),
    prop_political = sprintf("%.2f", prop_political),
    # Keep n as an integer (formatted with no decimals)
    n = as.character(round(n, 0))
  ) %>%
  mutate(
    # Big intervals with commas
    n = format(as.numeric(n), big.mark = ",")
  ) |>
  rename(
    "Media Outlet" = label,
    "Ideology Score" = ideo,
    "Number of Posts" = n,
    "Proportion of Political Posts" = prop_political
  )

datasummary_df(
  formatted_table,
  output = "tables/media_posts_table_new.tex",
  align = "lccc",
  title = "Media Outlet Descriptive Statistics",
  notes = "Note: Ideology scores (ideo) are audience-centric measures.",
  booktabs = TRUE
)


