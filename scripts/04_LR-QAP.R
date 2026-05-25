
# This script tests the imitation-differentiation dynamics using LRQAP
# author: Subhayan Mukerjee

library(tidyverse)
library(igraph)
library(ggridges)
library(sna)

set.seed(9)

setwd("path/to/parent/folder/of/scripts/folder")

# main files

res <- read_csv("auxiliary/inter-media-influence.csv")
res_p <- read_csv("auxiliary/inter-media-influence-political.csv")
res_e <- read_csv("auxiliary/inter-media-influence-entertainment.csv")


# auxiliary files

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

# number of possible directed pairs of media outlets
n_pairs <- nrow(media_details) * (nrow(media_details) - 1)

#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------

# All Topics

media_influence_tbl <- res |>
  inner_join(media_details, by = c("lead_media" = "n")) |>
  rename(lead_media_label = label,
         lead_media_ideo = ideo) |>
  inner_join(media_details, by = c("lag_media" = "n")) |>
  rename(lag_media_label = label,
         lag_media_ideo = ideo) |>
  mutate(influence_sig = ifelse(p < (0.05/n_pairs), influence, 0),
         lead_media_party = ifelse(lead_media_ideo < 3, "lib", "cons"),
         lag_media_party = ifelse(lag_media_ideo < 3, "lib", "cons"))

lag1_tbl <- media_influence_tbl |>
  filter(lag == 1) |>
  select(-lag) |>
  mutate(ideo_dist = abs(lead_media_ideo - lag_media_ideo),
         lead_lag_party = paste(lead_media_party, lag_media_party, sep = "_"),
         positive_influence = as.integer(influence_sig > 0),
         negative_influence = as.integer(influence_sig < 0)) |>
  select(lead_media, lag_media, ideo_dist, influence_sig, positive_influence, negative_influence)

# Get all unique media outlets
all_outlets <- sort(unique(c(lag1_tbl$lead_media, lag1_tbl$lag_media)))
n <- length(all_outlets)

# ------------------------------------------------------------------------------
## Effect of ideo_distance on influence_sig after controlling for reciprocity

# Use complete() to ensure every possible pair (29x29) exists in the data
Y_mat <- lag1_tbl %>%
  complete(lead_media = all_outlets, lag_media = all_outlets) %>%
  replace_na(list(influence_sig = 0)) %>%
  # Pivot to a wide format to create the matrix structure
  pivot_wider(names_from = lag_media, values_from = influence_sig, id_cols = lead_media) %>%
  # Ensure the rows follow our master order
  arrange(match(lead_media, all_outlets)) %>%
  select(-lead_media) %>%
  as.matrix()

rownames(Y_mat) <- colnames(Y_mat) <- all_outlets

X_dist <- lag1_tbl %>%
  complete(lead_media = all_outlets, lag_media = all_outlets) %>%
  replace_na(list(ideo_dist = 0)) %>% # Distance to self or missing pairs
  pivot_wider(names_from = lag_media, values_from = ideo_dist, id_cols = lead_media) %>%
  arrange(match(lead_media, all_outlets)) %>%
  select(-lead_media) %>%
  as.matrix()

rownames(X_dist) <- colnames(X_dist) <- all_outlets

X_recip <- t(Y_mat)

# Run the multiple regression QAP
m1_all <- netlm(Y_mat, 
            list(distance = X_dist, reciprocity = X_recip),
            nullhyp = "qap",
            reps = 1000)

summary(m1_all) # not reported in paper

# Even when controlling for the structural tendency of media outlets to influence each other reciprocally, increasing ideological distance significantly drives influence in the other direction ($\beta = -0.0046, p = 0.03$)

# now without controlling for reciprocity

# Run the multiple regression QAP
m2_all <- netlm(Y_mat, 
            list(distance = X_dist),
            nullhyp = "qap",
            reps = 1000)

summary(m2_all)

# Without controlling for the structural tendency of media outlets to influence each other reciprocally, increasing ideological distance significantly drives influence in the other direction ($\beta = -0.01, p = 0.018$)

# ------------------------------------------------------------------------------
# All Topics, Positive Influence Only

# positive influence network

# Create the Binary Positive Outcome Matrix (Y)
Y_pos_binary <- ifelse(Y_mat > 0, 1, 0)

# We use the transpose of the binary matrix to control for mutual positive ties
X_pos_recip_binary <- t(Y_pos_binary)

logit_pos_allA <- netlogit(Y_pos_binary, 
                      list(distance = X_dist, reciprocity = X_pos_recip_binary),
                      nullhyp = "qap",
                      reps = 1000)

summary(logit_pos_allA)

# Effect is negative (-0.31) but not significant (p = 0.218)
# Exp(b) is 0.74
# so one unit increase in distance reduces odds of positive tie by 26%, but not significant at p < 0.05

# without controlling for recip

logit_pos_allB <- netlogit(Y_pos_binary, 
                          list(distance = X_dist),
                          nullhyp = "qap",
                          reps = 1000)

summary(logit_pos_allB)

# Effect is negative (-0.44) but not significant (p = 0.21)
# Exp(b) is 0.64
# so one unit increase in distance reduces odds of positive tie by 36%, but not significant at p < 0.05

#------------------------------------------------------------------
# All Topics, Negative Influence Only

# Create the Binary Positive Outcome Matrix (Y)
Y_neg_binary <- ifelse(Y_mat < 0, 1, 0)

# We use the transpose of the binary matrix to control for mutual positive ties
X_neg_recip_binary <- t(Y_neg_binary)

logit_neg_allA <- netlogit(Y_neg_binary, 
                      list(distance = X_dist, reciprocity = X_neg_recip_binary),
                      nullhyp = "qap",
                      reps = 1000)

summary(logit_neg_allA)

# beta = 0.57, p = 0
# Exp(b) is 1.78
# 78% increase in odds of negative influence for a one unit increase in ideological distance,
# significant at p < 0.01

# without controlling for reciprocity

logit_neg_allB <- netlogit(Y_neg_binary, 
                          list(distance = X_dist),
                          nullhyp = "qap",
                          reps = 1000)

summary(logit_neg_allB)

#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------

# Political Topics Only

media_influence_tbl_p <- res_p |>
  inner_join(media_details, by = c("lead_media" = "n")) |>
  rename(lead_media_label = label,
         lead_media_ideo = ideo) |>
  inner_join(media_details, by = c("lag_media" = "n")) |>
  rename(lag_media_label = label,
         lag_media_ideo = ideo) |>
  mutate(influence_sig = ifelse(p < (0.05/n_pairs), influence, 0),
         lead_media_party = ifelse(lead_media_ideo < 3, "lib", "cons"),
         lag_media_party = ifelse(lag_media_ideo < 3, "lib", "cons"))

lag1_tbl_p <- media_influence_tbl_p |>
  filter(lag == 1) |>
  select(-lag) |>
  mutate(ideo_dist = abs(lead_media_ideo - lag_media_ideo),
         lead_lag_party = paste(lead_media_party, lag_media_party, sep = "_"),
         positive_influence = as.integer(influence_sig > 0),
         negative_influence = as.integer(influence_sig < 0)) |>
  select(lead_media, lag_media, ideo_dist, influence_sig, positive_influence, negative_influence)

# Get all unique media outlets
all_outlets <- sort(unique(c(lag1_tbl_p$lead_media, lag1_tbl_p$lag_media)))
n <- length(all_outlets)

# -----------------------------------------------------------------------------
## Effect of ideo_distance on influence_sig after controlling for reciprocity

# Use complete() to ensure every possible pair (29x29) exists in the data
Y_mat <- lag1_tbl_p %>%
  complete(lead_media = all_outlets, lag_media = all_outlets) %>%
  replace_na(list(influence_sig = 0)) %>%
  # Pivot to a wide format to create the matrix structure
  pivot_wider(names_from = lag_media, values_from = influence_sig, id_cols = lead_media) %>%
  # Ensure the rows follow our master order
  arrange(match(lead_media, all_outlets)) %>%
  select(-lead_media) %>%
  as.matrix()

rownames(Y_mat) <- colnames(Y_mat) <- all_outlets

X_dist <- lag1_tbl_p %>%
  complete(lead_media = all_outlets, lag_media = all_outlets) %>%
  replace_na(list(ideo_dist = 0)) %>% # Distance to self or missing pairs
  pivot_wider(names_from = lag_media, values_from = ideo_dist, id_cols = lead_media) %>%
  arrange(match(lead_media, all_outlets)) %>%
  select(-lead_media) %>%
  as.matrix()

rownames(X_dist) <- colnames(X_dist) <- all_outlets

X_recip <- t(Y_mat)

# Run the multiple regression QAP
m1_pol <- netlm(Y_mat, 
            list(distance = X_dist, reciprocity = X_recip), 
            reps = 1000)

summary(m1_pol)

# Even when controlling for the structural tendency of media outlets to influence each other reciprocally, increasing ideological distance significantly drives influence in the other direction ($\beta = -0.0042, p < 0.05$) in the coverage of political topics

# now without controlling

m2_pol <- netlm(Y_mat, 
                list(distance = X_dist), 
                reps = 1000)

# beta = -.0084, p = 0.01

summary(m2_pol)

# Without controlling for the structural tendency of media outlets to influence each other reciprocally, increasing ideological distance significantly drives influence in the other direction ($\beta = -0.0083, p < 0.05$) in the coverage of political topics

###################################################################
# Political Topics, Positive Influence Only

# positive influence network

# Create the Binary Positive Outcome Matrix (Y)
Y_pos_binary <- ifelse(Y_mat > 0, 1, 0)

# We use the transpose of the binary matrix to control for mutual positive ties
X_pos_recip_binary <- t(Y_pos_binary)

logit_pos_polA <- netlogit(Y_pos_binary, 
                      list(distance = X_dist, reciprocity = X_pos_recip_binary), 
                      nullhyp = "qap",
                      reps = 1000)

summary(logit_pos_polA)

# Effect is negative (-0.31) but not significant (p = 0.31)
# Exp(b) is 0.73
# so one unit increase in distance reduces odds of positive tie by 27%, but not significant at p < 0.05

# without controlling

logit_pos_polB <- netlogit(Y_pos_binary, 
                          list(distance = X_dist), 
                          reps = 1000)

summary(logit_pos_polB)

# Effect is negative (-0.47) but not significant (p = 0.34)
# Exp(b) is 0.62
# so one unit increase in distance reduces odds of positive tie by 38%, but not significant at p < 0.05


#------------------------------------------------------------------
# Political Topics, Negative Influence Only

# Create the Binary Positive Outcome Matrix (Y)
Y_neg_binary <- ifelse(Y_mat < 0, 1, 0)

# We use the transpose of the binary matrix to control for mutual positive ties
X_neg_recip_binary <- t(Y_neg_binary)

logit_neg_polA <- netlogit(Y_neg_binary, 
                      list(distance = X_dist, reciprocity = X_neg_recip_binary), 
                       nullhyp = "qap",
                      reps = 1000)

summary(logit_neg_polA)

# beta = 0.55, p =  0.007
# Exp(b) is 1.74
# 74% increase in odds of negative influence for a one unit increase in ideological distance,
# significant at p < 0.01

# without controlling for reciprocity
logit_neg_polB <- netlogit(Y_neg_binary, 
                          list(distance = X_dist), 
                          nullhyp = "qap",
                          reps = 1000)

summary(logit_neg_polB)

# beta = 0.83, p = 0.003
# exp(b) is 2.29

############################################
# Entertainment Topics Only

media_influence_tbl_e <- res_e |>
  inner_join(media_details, by = c("lead_media" = "n")) |>
  rename(lead_media_label = label,
         lead_media_ideo = ideo) |>
  inner_join(media_details, by = c("lag_media" = "n")) |>
  rename(lag_media_label = label,
         lag_media_ideo = ideo) |>
  mutate(influence_sig = ifelse(p < (0.05/n_pairs), influence, 0),
         lead_media_party = ifelse(lead_media_ideo < 3, "lib", "cons"),
         lag_media_party = ifelse(lag_media_ideo < 3, "lib", "cons"))

lag1_tbl_e <- media_influence_tbl_e |>
  filter(lag == 1) |>
  select(-lag) |>
  mutate(ideo_dist = abs(lead_media_ideo - lag_media_ideo),
         lead_lag_party = paste(lead_media_party, lag_media_party, sep = "_"),
         positive_influence = as.integer(influence_sig > 0),
         negative_influence = as.integer(influence_sig < 0)) |>
  select(lead_media, lag_media, ideo_dist, influence_sig, positive_influence, negative_influence)

# Get all unique media outlets
all_outlets <- sort(unique(c(lag1_tbl_e$lead_media, lag1_tbl_e$lag_media)))
n <- length(all_outlets)

# Use complete() to ensure every possible pair (29x29) exists in the data
Y_mat <- lag1_tbl_e %>%
  complete(lead_media = all_outlets, lag_media = all_outlets) %>%
  replace_na(list(influence_sig = 0)) %>%
  # Pivot to a wide format to create the matrix structure
  pivot_wider(names_from = lag_media, values_from = influence_sig, id_cols = lead_media) %>%
  # Ensure the rows follow our master order
  arrange(match(lead_media, all_outlets)) %>%
  select(-lead_media) %>%
  as.matrix()

rownames(Y_mat) <- colnames(Y_mat) <- all_outlets

X_dist <- lag1_tbl_e %>%
  complete(lead_media = all_outlets, lag_media = all_outlets) %>%
  replace_na(list(ideo_dist = 0)) %>% # Distance to self or missing pairs
  pivot_wider(names_from = lag_media, values_from = ideo_dist, id_cols = lead_media) %>%
  arrange(match(lead_media, all_outlets)) %>%
  select(-lead_media) %>%
  as.matrix()

rownames(X_dist) <- colnames(X_dist) <- all_outlets

X_recip <- t(Y_mat)

# Run the multiple regression QAP
m1_ent <- netlm(Y_mat, 
              list(distance = X_dist, reciprocity = X_recip), 
              reps = 1000)

summary(m1_ent)

# Even when controlling for the structural tendency of media outlets to influence each other reciprocally, increasing ideological distance does not drive influence in the other direction for entertainment topics ($\beta = -0.00075, p = 0.459$)

# without controlling for reciprocity

m2_ent <- netlm(Y_mat, 
              list(distance = X_dist), 
              reps = 1000)

summary(m2_ent)

# Without controlling for the structural tendency of media outlets to influence each other reciprocally, increasing ideological distance does not drive influence in the other direction for entertainment topics ($\beta = -0.0010, p = 0.442$)

###################################################################
# Entertainment Topics, Positive Influence Only

# positive influence network

# Create the Binary Positive Outcome Matrix (Y)
Y_pos_binary <- ifelse(Y_mat > 0, 1, 0)

# We use the transpose of the binary matrix to control for mutual positive ties
X_pos_recip_binary <- t(Y_pos_binary)

logit_pos_entA <- netlogit(Y_pos_binary, 
                          list(distance = X_dist, reciprocity = X_pos_recip_binary), 
                          nullhyp = "qap",
                          reps = 1000)

# Warning messages:
# 1: glm.fit: fitted probabilities numerically 0 or 1 occurred

summary(logit_pos_entA)

# Effect is negative (-0.66) but not significant (p = 0.378)
# Exp(b) is 0.51
# so one unit increase in distance reduces odds of positive tie by 49%, but not significant at p < 0.05

# without controlling for reciprocity
logit_pos_entB <- netlogit(Y_pos_binary, 
                          list(distance = X_dist),
                          nullhyp = "qap",
                          reps = 1000)

summary(logit_pos_entB)

# beta = -0.81, p = 0.389
#  exp(b) = 0.44
# so one unit increase in distance reduces odds of positive tie by 19%, but not significant at p < 0.05

##------------------------------------------------------------------
# Entertainment Topics, Negative Influence Only

# Create the Binary Positive Outcome Matrix (Y)
Y_neg_binary <- ifelse(Y_mat < 0, 1, 0)

# We use the transpose of the binary matrix to control for mutual positive ties
X_neg_recip_binary <- t(Y_neg_binary)

logit_neg_entA <- netlogit(Y_neg_binary, 
                          list(distance = X_dist, reciprocity = X_neg_recip_binary),
                          nullhyp = "qap",
                          reps = 1000)

# Warning messages:
#   1: glm.fit: fitted probabilities numerically 0 or 1 occurred

summary(logit_neg_entA)

# beta = -0.22, p =  0.87
# Exp(b) is 0.79

# without controlling for reciprocity

logit_neg_entB <- netlogit(Y_neg_binary, 
                          list(distance = X_dist),
                          nullhyp = "qap",
                          reps = 1000)

# Warning messages:
#   1: glm.fit: fitted probabilities numerically 0 or 1 occurred

summary(logit_neg_entB)
# beta = -0.22, p  = 0.862
#  exp(b) is 0.8

#------------------------------------------------------------------------------------------------------------

# models with reciprocity control are labeled "A", models without reciprocity control are labeled "B"

# Combine all QAP correlation A results into a list
qap_cor_models <- qap_cor_models_A <- list("All" = m1_all,
                       "Political" =  m1_pol,
                       "Entertainment" = m1_ent)

# Combine all positive influence LRQAP A models into a list
lrqap_pos_models <- lrqap_pos_models_A <- list("All" = logit_pos_allA,
                         "Political" = logit_pos_polA,
                         "Entertainment" = logit_pos_entA)

# Combine all negative influence LRQAP A models into a list
lrqap_neg_models <- lrqap_neg_models_A <- list("All" = logit_neg_allA,
                         "Political" = logit_neg_polA,
                         "Entertainment" = logit_neg_entA)

save(qap_cor_models,
     lrqap_pos_models,
     lrqap_neg_models,
     file = "results/new_qap_results-60seasons-A.RData")


# Combine all QAP correlation B results into a list
qap_cor_models <- qap_cor_models_B <- list("All" = m2_all,
                       "Political" =  m2_pol,
                       "Entertainment" = m2_ent)

# Combine all positive influence LRQAP B models into a list
lrqap_pos_models <- lrqap_pos_models_B <- list("All" = logit_pos_allB,
                         "Political" = logit_pos_polB,
                         "Entertainment" = logit_pos_entB)

# Combine all negative influence LRQAP B models into a list
lrqap_neg_models <-  lrqap_neg_models_B <- list("All" = logit_neg_allB,
                         "Political" = logit_neg_polB,
                         "Entertainment" = logit_neg_entB)

save(qap_cor_models,
     lrqap_pos_models,
     lrqap_neg_models,
     file = "results/new_qap_results-60seasons-B.RData")



#------------------------------------------------------------------------------------------------------------

lag1_tbl <- lag1_tbl |>
  mutate(pid_quintile = ntile(ideo_dist, 5)) |>
  mutate(topic = "All")

lag1_tbl_p <- lag1_tbl_p |>
  mutate(pid_quintile = ntile(ideo_dist, 5)) |>
  mutate(topic = "Political")

lag1_tbl_e <- lag1_tbl_e |>
  mutate(pid_quintile = ntile(ideo_dist, 5)) |>
  mutate(topic = "Entertainment")

# Now bind rows the three dataframes

lag1_tbl_all <- bind_rows(lag1_tbl, lag1_tbl_p, lag1_tbl_e)

# visualisations
# violin plot

violin_plot_all <- lag1_tbl_all |>
  filter(topic %in% c("All")) |>
  ggplot(aes(x = influence, y = as.factor(pid_quintile), fill = topic)) +
  geom_violin() +
  geom_boxplot(aes(color = topic),
               width=0.1, outlier.shape = NA, fill = "white", 
               position=position_dodge(0.9),
               show.legend = F) +
  scale_fill_manual(values = c("#fdc086")) +
  scale_color_manual(values = c("#000000", "#000000")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_bw() +
  labs(x = "Influence", y = "Quintiles of\nIdeological Distance", fill = "Topic") +
  lims(x = c(-0.2, 0.3)) +
  scale_y_discrete(
    labels = c("1\n(Ideologically\nClosest Pairs)",
               2,3,4, 
               "5\n(Ideologically\nFarthest Pairs)"))

violin_plot_pol_ent <- lag1_tbl_all |>
  filter(topic %in% c("Political", "Entertainment")) |>
  ggplot(aes(x = influence, y = as.factor(pid_quintile), fill = topic)) +
  geom_violin(position = position_dodge(0.5)) +
  geom_boxplot(aes(color = topic),
               width=0.1, outlier.shape = NA, fill = "white", 
               position=position_dodge(0.5),
               show.legend = F) +
  scale_fill_manual(values = c("#7fc97f", "#beaed4")) +
  scale_color_manual(values = c("#000000", "#000000")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_bw() +
  labs(x = "Influence", y = "Quintiles of\nIdeological Distance", fill = "Topic") +
  lims(x = c(-0.2, 0.3)) +
  scale_y_discrete(
    labels = c("1\n(Ideologically\nClosest Pairs)",
               2,3,4, 
               "5\n(Ideologically\nFarthest Pairs)")) +
  guides(fill = guide_legend(reverse = TRUE))

violin_plot_all_pol_ent <- cowplot::plot_grid(violin_plot_all, violin_plot_pol_ent,
                                              labels = LETTERS[1:2], ncol = 1,
                                              rel_heights = c(0.8, 1.5),
                                              align = "v",
                                              axis = "lr")

ggsave(file="figures/svg/violinplot-1.svg",
       plot=violin_plot_all_pol_ent, width=6, height=8,
       dpi=300)

ggsave(file="figures/pdf/violinplot-1.pdf",
       plot=violin_plot_all_pol_ent, width=6, height=8,
       dpi=300)

violin_plot_pol_ent_lib_cons <- lag1_tbl_all |>
  filter(topic %in% c("Political", "Entertainment")) |>
  mutate(lead_lag_party = factor(lead_lag_party, 
                                 levels = c("cons_lib", "lib_cons", 
                                            "cons_cons", "lib_lib"))) |>
  ggplot(aes(x = influence, y = lead_lag_party, fill = topic)) +
  geom_violin(position=position_dodge(0.6)) +
  geom_boxplot(aes(color = topic),
               width=0.1, outlier.shape = NA, fill = "white", 
               position=position_dodge(0.6),
               show.legend = F) +
  scale_fill_manual(values = c("#7fc97f", "#beaed4")) +
  scale_color_manual(values = c("#000000", "#000000")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_bw() +
  labs(x = "Influence", y = "Partisanship of Outlet Pairs", fill = "Topic") +
  scale_y_discrete(
    labels = c("Right-leaning outlets' \non left-leaning outlets",
               "Left-leaning outlets' \non right-leaning outlets",
               "Right-leaning outlets' \non right-leaning outlets",
               "Left-leaning outlets' \non left-leaning outlets")) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave(file="figures/svg/violinplot-2.svg",
       plot=violin_plot_pol_ent_lib_cons,
       width=6, height=5,
       units = "in",
       dpi = 300)

ggsave(file="figures/pdf/violinplot-2.pdf",
       plot=violin_plot_pol_ent_lib_cons,
       width=6, height=5,
       units = "in",
       dpi = 300)
