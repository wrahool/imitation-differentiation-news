# creates the heatmap for the main results
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

# number of possible directed pairs of media outlets
n_pairs <- nrow(media_details) * (nrow(media_details) - 1)

res <- read_csv("auxiliary/inter-media-influence.csv")
res_p <- read_csv("auxiliary/inter-media-influence-political.csv")
res_e <- read_csv("auxiliary/inter-media-influence-entertainment.csv")

res <- res %>%
  inner_join(media_details, by = c("lead_media" = "n")) %>%
  select(lead_media, lag_media, short_name, influence, p) %>%
  rename(lead_media_name = short_name) %>%
  inner_join(media_details, by = c("lag_media" =  "n")) %>%
  select(lead_media, lag_media, lead_media_name, short_name, influence, p) %>%
  rename(lag_media_name = short_name)  %>%
  select(-lead_media, -lag_media) %>%
  mutate(influence_sig = ifelse(p < (0.05/n_pairs), influence, 0)) %>%
  mutate(influence_direction = case_when(
    influence_sig > 0 ~ "positive",
    influence_sig < 0 ~ "negative",
    TRUE ~ "non-significant"
  ))

# build heatmap (positive/negative)

highcolor = "darkorange2"
lowcolor = "deepskyblue3"

# Extract the custom order vector from your tibble
# This list will be: "Vox", "Guardian", "HuffPost", "NPR", "VICE", ...
custom_media_order <- sorted_media_details$short_name

# Define the custom color palette (same as before)
color_palette <- c("positive" = highcolor,
                   "negative" = lowcolor,
                   "non-significant" = "white")

# Apply the custom order to both axis variables in the 'res' tibble
res_sorted <- res %>%
  # Set the factor levels for the Y-axis (lead media)
  mutate(lead_media_name = factor(lead_media_name, levels = custom_media_order)) %>%
  # Set the factor levels for the X-axis (lag media)
  mutate(lag_media_name = factor(lag_media_name, levels = custom_media_order)) %>%
  # Ensure the influence direction is also a factor (for consistent coloring)
  mutate(influence_direction = factor(influence_direction,
                                      levels = c("positive", "negative", "non-significant")))

# Create a tibble containing all coordinates for the primary diagonal
diagonal_coords <- tibble(
  # Set both row and column names to the custom sorted list
  lead_media_name = custom_media_order,
  lag_media_name = custom_media_order
) %>%
  # Ensure they are factors with the correct levels for ggplot2 to plot them correctly
  mutate(
    lead_media_name = factor(lead_media_name, levels = custom_media_order),
    lag_media_name = factor(lag_media_name, levels = custom_media_order)
  )

discrete_heatmap <- ggplot(res_sorted, aes(x = lag_media_name, y = lead_media_name, fill = influence_direction)) +
  # Plot the tiles
  geom_tile(color = "gray80", linewidth = 0.5) +
  
  # Plot the diagonal layer ON TOP, setting fill manually
  geom_tile(data = diagonal_coords, fill = "gray", color = "gray", linewidth = 0.5) +
  
  # Apply the custom colors
  scale_fill_manual(values = color_palette,
                    name = "Influence",
                    # Optional: drop unused factor levels from the legend
                    drop = FALSE) +
  
  # Aesthetics and Labels
  labs(
    x = "Lagging Media Outlet\n(liberal \u2192 conservative)",
    y = "Leading Media Outlet\n(liberal \u2192 conservative)"
  ) +
  
  # Theme Customization
  theme_minimal() +
  theme(
    # Rotate X-axis labels for readability
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
  )


# build heatmap (gradient)

gradient_heatmap <- ggplot(res_sorted, aes(x = lag_media_name, y = lead_media_name, fill = influence_sig)) +
  #Plot the tiles
  geom_tile(color = "gray80", linewidth = 0.5) +
  
  geom_tile(data = diagonal_coords, fill = "gray", color = "gray", linewidth = 0.5) +
  
  # Apply the custom diverging color scale
  scale_fill_gradient2(
    low = lowcolor,
    mid = "white",
    high = highcolor,
    midpoint = 0, # Center the scale at 0
    name = "Influence \n(Magnitude)"
  ) +
  
  # Aesthetics and Labels (Using the two-line X-axis label)
  labs(
    x = "Lagging Media Outlet\n(liberal \u2192 conservative)",
    y = "Lagging Media Outlet\n(liberal \u2192 conservative)"
  ) +
  
  # ANNOTATIONS REMOVED HERE
  
  # 4. Theme Customization
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    # Ensure the title itself has enough space to render two lines
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    # Reduced margins as we no longer need space for external annotations
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  # No need for clip="off" since we don't have annotations outside the plot area
  coord_cartesian()

ggsave(file="figures/svg/heatmap_discrete.svg",
       plot=discrete_heatmap, width=9, height=8,
       dpi=300)

ggsave(file="figures/pdf/heatmap_discrete.pdf",
       plot=discrete_heatmap, width=9, height=8,
       dpi=300)

ggsave(file="figures/svg/heatmap_gradient.svg",
       plot=gradient_heatmap, width=9, height=8,
       dpi=300)

ggsave(file="figures/pdf/heatmap_gradient.pdf",
       plot=gradient_heatmap, width=9, height=8,
       dpi=300)
