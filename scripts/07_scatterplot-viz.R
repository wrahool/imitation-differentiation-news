# creates the dotplt for the main results
# author: Subhayan Mukerjee

library(tidyverse)

media_label_map <- read_csv("auxiliary/media_label_map.csv")
media_details_tbl <- read_csv("auxiliary/media_language.csv")
media_ideology <- read_csv("auxiliary/media_ideo.csv")

media_details <- media_label_map |>
  inner_join(media_details_tbl, by = c("label" = "media")) |>
  select(n, label, short_name)

sorted_media_details <- media_label_map |>
  inner_join(media_details_tbl, by = c("label" = "media")) %>%
  inner_join(media_ideology, by = "media") %>%
  select(short_name, n, ideo) %>%
  arrange(ideo)

short_name_ideo_map <- media_label_map |>
  inner_join(media_details_tbl, by = c("label" = "media")) %>%
  inner_join(media_ideology, by = "media") %>%
  select(short_name, n, ideo)

custom_labels <- short_name_ideo_map |>
  filter(short_name %in% c("Vox", "Guardian", "NYT", "WaPo", "PBS", "CNN", "NBC", "ABC", "NYPost", "Washington Examiner", "Fox", "Daily Caller", "Breitbart",  "Rush Limbaugh"))

# number of possible directed pairs of media outlets
n_pairs <- nrow(media_details) * (nrow(media_details) - 1)

res <- read_csv("auxiliary/inter-media-influence.csv")
res_p <- read_csv("auxiliary/inter-media-influence-political.csv")
res_e <- read_csv("auxiliary/inter-media-influence-entertainment.csv")

plot_res <- res %>%
  inner_join(short_name_ideo_map, by = c("lead_media" = "n")) %>%
  select(lead_media, lag_media, short_name, influence, p, ideo) %>%
  rename(lead_media_name = short_name, lead_media_ideo = ideo) %>%
  inner_join(short_name_ideo_map, by = c("lag_media" =  "n")) %>%
  select(lead_media, lag_media, lead_media_name, lead_media_ideo, short_name, influence, p, ideo) %>%
  rename(lag_media_name = short_name, lag_media_ideo = ideo)  %>%
  select(lead_media_ideo, lag_media_ideo, influence, p) %>%
  mutate(influence_sig = ifelse(p < (0.05/n_pairs), influence, 0),
         abs_influence = abs(influence),
         abs_influence_sig = abs(influence_sig)) %>%
  mutate(influence_direction = case_when(
    influence_sig > 0 ~ "positive",
    influence_sig < 0 ~ "negative",
    TRUE ~ "non-significant"
  ))

# build scatterplot

highcolor = "darkorange2"
lowcolor = "deepskyblue3"

scatterplot_withnonsig <- plot_res |>
  ggplot(aes(x = lead_media_ideo, y = lag_media_ideo)) +
  geom_point(
    aes(color = influence_direction, size = abs_influence),
    alpha = 0.6
  ) +
  scale_color_manual(
    values = c("positive" = "darkorange2",
               "negative" = "deepskyblue3",
               "non-significant" = "grey80"),
    name = "Influence Type"
  ) +
  labs(x = "Leading Media Ideology Score",
       y = "Lagging Media Ideology Score") +
  guides(size = "none",
         color = guide_legend(reverse = TRUE)) + # Rotation is now handled above
  
  theme_minimal()

###################################################

scatterplot_withnonsig_customlabels <- plot_res |>
  ggplot(aes(x = lead_media_ideo, y = lag_media_ideo)) +
  geom_point(
    aes(color = influence_direction, size = abs_influence),
    alpha = 0.6
  ) +
  scale_color_manual(
    values = c("positive" = "darkorange2",
               "negative" = "deepskyblue3",
               "non-significant" = "grey80"),
    name = "Influence Type"
  ) +
  
  scale_x_continuous(
    name = "Leading Media Ideology Score", # Primary axis title
    
    sec.axis = sec_axis(
      transform = ~ ., 
      breaks = custom_labels$ideo, 
      labels = custom_labels$short_name,
      guide = guide_axis(angle = 45) 
    )
  ) +
  
  scale_y_continuous(
    name = "Lagging Media Ideology Score", # Primary axis title
    
    sec.axis = sec_axis(
      transform = ~ ., 
      breaks = custom_labels$ideo, 
      labels = custom_labels$short_name,
      guide = guide_axis(angle = 45) 
    )
  ) +
  
  guides(size = "none",
         color = guide_legend(reverse = TRUE)) + # Rotation is now handled above
  
  theme_bw() +
  theme(axis.text.y.right = element_text(color = "grey80"),
        axis.text.x.top = element_text(color = "grey80"))

#######################################################

scatterplot_onlysig <- plot_res |>
  filter(influence_direction != "non-significant") |>
  ggplot(aes(x = lead_media_ideo, y = lag_media_ideo)) +
  geom_point(
    aes(color = influence_direction, size = abs_influence),
    alpha = 0.6 # Adjust alpha for transparency (0.6 is a good starting point)
  ) +
  scale_color_manual(
    values = c("positive" = "darkorange2",
               "negative" = "deepskyblue3"),
    name = "Influence Type" # Set legend title for color
  ) +
  labs(x = "Leading Media Ideology Score",
       y = "Lagging Media Ideology Score") +
  guides(size="none",
         color = guide_legend(reverse = TRUE))  +
  theme_minimal()

########################################################

scatterplot_onlysig_customlabels <- plot_res |>
  filter(influence_direction != "non-significant") |>
  ggplot(aes(x = lead_media_ideo, y = lag_media_ideo)) +
  geom_point(
    aes(color = influence_direction, size = abs_influence),
    alpha = 0.6 # Adjust alpha for transparency (0.6 is a good starting point)
  ) +
  scale_color_manual(
    values = c("positive" = "darkorange2",
               "negative" = "deepskyblue3"),
    name = "Influence Type" # Set legend title for color
  ) +
  
  scale_x_continuous(
    name = "Leading Media Ideology Score", # Primary axis title
    
    sec.axis = sec_axis(
      transform = ~ ., 
      breaks = custom_labels$ideo, 
      labels = custom_labels$short_name,
      guide = guide_axis(angle = 45) 
    )
  ) +
  
  scale_y_continuous(
    name = "Lagging Media Ideology Score", # Primary axis title
    
    sec.axis = sec_axis(
      transform = ~ ., 
      breaks = custom_labels$ideo, 
      labels = custom_labels$short_name,
      guide = guide_axis(angle = 45) 
    )
  ) +
  guides(size="none",
         color = guide_legend(reverse = TRUE))  +
  theme_minimal() +
  theme(axis.text.y.right = element_text(color = "grey80"),
      axis.text.x.top = element_text(color = "grey80"))


ggsave(file="figures/svg/scatterplot_withnonsig.svg",
       plot=scatterplot_withnonsig, width=9, height=8,
       dpi=300)

ggsave(file="figures/pdf/scatterplot_withnonsig.pdf",
       plot=scatterplot_withnonsig, width=9, height=8,
       dpi=300)

ggsave(file="figures/svg/scatterplot_withnonsig_customlabels.svg",
       plot=scatterplot_withnonsig_customlabels, width=9, height=8,
       dpi=300)

ggsave(file="figures/pdf/scatterplot_withnonsig_customlabels.pdf",
       plot=scatterplot_withnonsig_customlabels, width=9, height=8,
       dpi=300)

ggsave(file="figures/svg/scatterplot_onlysig.svg",
       plot=scatterplot_onlysig, width=9, height=8,
       dpi=300)

ggsave(file="figures/pdf/scatterplot_onlysig.pdf",
       plot=scatterplot_onlysig, width=9, height=8,
       dpi=300)

ggsave(file="figures/svg/scatterplot_onlysig_customlabels.svg",
       plot=scatterplot_onlysig_customlabels, width=9, height=8,
       dpi=300)

ggsave(file="figures/pdf/scatterplot_onlysig_customlabels.pdf",
       plot=scatterplot_onlysig_customlabels, width=9, height=8,
       dpi=300)
