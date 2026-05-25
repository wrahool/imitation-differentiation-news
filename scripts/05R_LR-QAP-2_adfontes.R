
# This script tests the liberal-conservative asymmetry, robustness with adfontes scores
# author: Subhayan Mukerjee


library(tidyverse)
library(igraph)
library(ggridges)
library(sna)
library(broom)

set.seed(9)

setwd("path/to/parent/folder/of/scripts/folder")


# main files

res <- read_csv("auxiliary/inter-media-influence_10seasons.csv")
res_p <- read_csv("auxiliary/inter-media-influence-political_10seasons.csv")
res_e <- read_csv("auxiliary/inter-media-influence-entertainment_10seasons.csv")

# auxiliary files

media_label_map <- read_csv("auxiliary/media_label_map.csv")
media_details_tbl <- read_csv("auxiliary/media_language.csv")
media_ideology <- read_csv("auxiliary/media_ideo2.csv")

media_details <- media_label_map |>
  inner_join(media_details_tbl, by = c("label" = "media")) |>
  select(n, label, short_name)

media_ideology <- media_ideology %>%
  inner_join(media_label_map, by = c("media" = "label")) %>%
  select(media, short_name, ideo)

media_details <- media_details |>
  inner_join(media_ideology, by = c("label" = "media")) |>
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
  mutate(lead_lag_party = paste(lead_media_party, lag_media_party, sep = "_"),
         positive_influence = as.integer(influence_sig > 0),
         negative_influence = as.integer(influence_sig < 0)) |>
  select(lead_media, lag_media, lead_lag_party, influence_sig, positive_influence, negative_influence)

# Get every unique media outlet ID from your data
all_outlets <- sort(unique(c(lag1_tbl$lead_media, lag1_tbl$lag_media)))

# Verify you have exactly 29
length(all_outlets)

Y_mat <- lag1_tbl %>%
  # Force the data to include all 841 possible combinations (29x29)
  complete(lead_media = all_outlets, lag_media = all_outlets) %>%
  
  # Fill missing influence values with 0
  mutate(influence_sig = replace_na(influence_sig, 0)) %>%
  
  # Pivot into a wide format (Rows = Lead, Cols = Lag)
  pivot_wider(
    names_from = lag_media, 
    values_from = influence_sig, 
    id_cols = lead_media
  ) %>%
  
  # Ensure the rows are in the same alphabetical order as the columns
  arrange(match(lead_media, all_outlets)) %>%
  
  # Remove the ID column so only the numbers remain
  select(-lead_media) %>%
  
  # Convert to a formal R matrix
  as.matrix()

# Add names for final verification
rownames(Y_mat) <- colnames(Y_mat) <- all_outlets

# Create the binary matrix
# 1 = Significant Positive Influence, 0 = Negative or No Influence
Y_pos_binary <- ifelse(Y_mat < 0, 1, 0)

# Helper function to keep it clean
make_categorical_matrix <- function(target_category) {
  lag1_tbl %>%
    complete(lead_media = all_outlets, lag_media = all_outlets) %>%
    mutate(present = ifelse(lead_lag_party == target_category, 1, 0)) %>%
    mutate(present = replace_na(present, 0)) %>%
    pivot_wider(names_from = lag_media, values_from = present, id_cols = lead_media) %>%
    arrange(match(lead_media, all_outlets)) %>%
    select(-lead_media) %>%
    as.matrix()
}

X_LR <- make_categorical_matrix("lib_cons")
X_RL <- make_categorical_matrix("cons_lib")
X_RR <- make_categorical_matrix("cons_cons")
X_LL <- make_categorical_matrix("lib_lib")

# do left outlets imitate themselves more than they imitate the right?

model_pos_asym1A <- netlogit(Y_pos_binary, 
                             list(LL = X_LL, LR = X_LR, RR = X_RR, recip = t(Y_pos_binary)), 
                             reps = 1000)

summary(model_pos_asym1A)

# baseline is L outlets imitating the R
# x1 (LL): L outlets imitating themselves more than they imitate R outlets

# without reciprocity
model_pos_asym1B <- netlogit(Y_pos_binary, 
                             list(LL = X_LL, LR = X_LR, RR = X_RR), 
                             reps = 1000)

summary(model_pos_asym1B)

# x1 (LL): L outlets imitate L outlets than they do imitate R outlets

# do right outlets imitate themselves more than they imitate the left? 
model_pos_asym2A <- netlogit(Y_pos_binary, 
                             list(RR = X_RR, RL = X_RL, LL = X_LL, recip = t(Y_pos_binary)), 
                             reps = 1000)

summary(model_pos_asym2A)

# baseline is R imitating L
# x1 (RR): R outlets do not imitate R any more than they imitate L

# without reciprocity
model_pos_asym2B <- netlogit(Y_pos_binary, 
                             list(RR = X_RR, RL = X_RL, LL = X_LL), 
                             reps = 1000)

summary(model_pos_asym2B)

# x1 (RR): R outlets do not imitate R any more than they imitate L

# ------------------------------------------------


# Political Topics

media_influence_tbl <- res_p |>
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
  mutate(lead_lag_party = paste(lead_media_party, lag_media_party, sep = "_"),
         positive_influence = as.integer(influence_sig > 0),
         negative_influence = as.integer(influence_sig < 0)) |>
  select(lead_media, lag_media, lead_lag_party, influence_sig, positive_influence, negative_influence)

# Get every unique media outlet ID from your data
all_outlets <- sort(unique(c(lag1_tbl$lead_media, lag1_tbl$lag_media)))

# Verify you have exactly 29
length(all_outlets)

Y_mat_pol <- lag1_tbl %>%
  # Force the data to include all 841 possible combinations (29x29)
  complete(lead_media = all_outlets, lag_media = all_outlets) %>%
  
  # Fill missing influence values with 0
  mutate(influence_sig = replace_na(influence_sig, 0)) %>%
  
  # Pivot into a wide format (Rows = Lead, Cols = Lag)
  pivot_wider(
    names_from = lag_media, 
    values_from = influence_sig, 
    id_cols = lead_media
  ) %>%
  
  # Ensure the rows are in the same alphabetical order as the columns
  arrange(match(lead_media, all_outlets)) %>%
  
  # Remove the ID column so only the numbers remain
  select(-lead_media) %>%
  
  # Convert to a formal R matrix
  as.matrix()

# Add names for final verification
rownames(Y_mat_pol) <- colnames(Y_mat_pol) <- all_outlets

# Create the binary matrix
# 1 = Significant Positive Influence, 0 = Negative or No Influence
Y_pos_binary_pol <- ifelse(Y_mat_pol > 0, 1, 0)

# Helper function to keep it clean
make_categorical_matrix <- function(target_category) {
  lag1_tbl %>%
    complete(lead_media = all_outlets, lag_media = all_outlets) %>%
    mutate(present = ifelse(lead_lag_party == target_category, 1, 0)) %>%
    mutate(present = replace_na(present, 0)) %>%
    pivot_wider(names_from = lag_media, values_from = present, id_cols = lead_media) %>%
    arrange(match(lead_media, all_outlets)) %>%
    select(-lead_media) %>%
    as.matrix()
}

X_LR <- make_categorical_matrix("lib_cons")
X_RL <- make_categorical_matrix("cons_lib")
X_RR <- make_categorical_matrix("cons_cons")
X_LL <- make_categorical_matrix("lib_lib")

# do left outlets imitate themselves more than than they imitate the R?

model_pos_asym_pol1A <- netlogit(Y_pos_binary_pol, 
                                 list(LL = X_LL, LR = X_LR, RR = X_RR, recip = t(Y_pos_binary_pol)), 
                                 reps = 1000)

summary(model_pos_asym_pol1A)

# baseline is L outlets imitating R
# x1 (LL): L outlets imitate L outlets more than they imitate R outlets

# without reciprocity
model_pos_asym_pol1B <- netlogit(Y_pos_binary_pol, 
                                 list(LL = X_LL, LR = X_LR, RR = X_RR), 
                                 reps = 1000)

summary(model_pos_asym_pol1B)

# x2 (RL): L outlets do not imitate L outlets more than they imitate R outlets

# do right outlets imitate the right than they do the left? 
model_pos_asym_pol2A <- netlogit(Y_pos_binary_pol, 
                                 list(RR = X_RR, RL = X_RL, LL = X_LL, recip = t(Y_pos_binary_pol)), 
                                 reps = 1000)

summary(model_pos_asym_pol2A)

# baseline is R outlets imitating L
# x1 (RR): R outlets do not differentiate from L outlets any more than they do from R outlets

# without reciprocity
model_pos_asym_pol2B <- netlogit(Y_pos_binary_pol, 
                                 list(RR = X_RR, RL = X_RL, LL = X_LL), 
                                 reps = 1000)

summary(model_pos_asym_pol2B)

# x1 (RR): R outlets do not imitate R outlets any more than they do L outlets

# -----------------------------------------------------------

# Entertainment Topics

media_influence_tbl <- res_e |>
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
  mutate(lead_lag_party = paste(lead_media_party, lag_media_party, sep = "_"),
         positive_influence = as.integer(influence_sig > 0),
         negative_influence = as.integer(influence_sig < 0)) |>
  select(lead_media, lag_media, lead_lag_party, influence_sig, positive_influence, negative_influence)

# Get every unique media outlet ID from your data
all_outlets <- sort(unique(c(lag1_tbl$lead_media, lag1_tbl$lag_media)))

# Verify you have exactly 29
length(all_outlets)

Y_mat_ent <- lag1_tbl %>%
  # Force the data to include all 841 possible combinations (29x29)
  complete(lead_media = all_outlets, lag_media = all_outlets) %>%
  
  # Fill missing influence values with 0
  mutate(influence_sig = replace_na(influence_sig, 0)) %>%
  
  # Pivot into a wide format (Rows = Lead, Cols = Lag)
  pivot_wider(
    names_from = lag_media, 
    values_from = influence_sig, 
    id_cols = lead_media
  ) %>%
  
  # Ensure the rows are in the same alphabetical order as the columns
  arrange(match(lead_media, all_outlets)) %>%
  
  # Remove the ID column so only the numbers remain
  select(-lead_media) %>%
  
  # Convert to a formal R matrix
  as.matrix()

# Add names for final verification
rownames(Y_mat_ent) <- colnames(Y_mat_ent) <- all_outlets

# Create the binary matrix
# 1 = Significant Positive Influence, 0 = Negative or No Influence
Y_pos_binary_ent <- ifelse(Y_mat_ent > 0, 1, 0)

# Helper function to keep it clean
make_categorical_matrix <- function(target_category) {
  lag1_tbl %>%
    complete(lead_media = all_outlets, lag_media = all_outlets) %>%
    mutate(present = ifelse(lead_lag_party == target_category, 1, 0)) %>%
    mutate(present = replace_na(present, 0)) %>%
    pivot_wider(names_from = lag_media, values_from = present, id_cols = lead_media) %>%
    arrange(match(lead_media, all_outlets)) %>%
    select(-lead_media) %>%
    as.matrix()
}

X_LR <- make_categorical_matrix("lib_cons")
X_RL <- make_categorical_matrix("cons_lib")
X_RR <- make_categorical_matrix("cons_cons")
X_LL <- make_categorical_matrix("lib_lib")

# do left outlets imitate the L, more than the R

model_pos_asym_ent1A <- netlogit(Y_pos_binary_ent, 
                                 list(LR = X_LL, RL = X_LR, RR = X_RR, recip = t(Y_pos_binary_ent)), 
                                 reps = 1000)

summary(model_pos_asym_ent1A)

# no significant effects

# without reciprocity
model_pos_asym_ent1B <- netlogit(Y_pos_binary_ent, 
                                 list(LR = X_LL, RL = X_LR, RR = X_RR), 
                                 reps = 1000)

summary(model_pos_asym_ent1B)

# nothing is significant

# do right outlets imitate themselves more than they do the left? 
model_pos_asym_ent2A <- netlogit(Y_pos_binary_ent, 
                                 list(RR = X_RR, RL = X_RL, LL = X_LL, recip = t(Y_pos_binary_ent)), 
                                 reps = 1000)

summary(model_pos_asym_ent2A)

# baseline is R imitating the L
# nothing is significant

# without reciprocity
model_pos_asym_ent2B <- netlogit(Y_pos_binary_ent, 
                                 list(RR = X_RR, RL = X_RL, LL = X_LL, recip = t(Y_pos_binary_ent)), 
                                 reps = 1000)

summary(model_pos_asym_ent2B)

# nothing is significant

# Combine all QAP correlation results into a list
qap_asym_pos_adfontes <- list("RL baseline All A" = model_pos_asym1A,
                               "RL baseline All B" = model_pos_asym1B,
                               "LR baseline All A" =  model_pos_asym2A,
                               "LR baseline All B" =  model_pos_asym2B,
                               
                               "RL baseline Political A" = model_pos_asym_pol1A,
                               "RL baseline Political B" = model_pos_asym_pol1B,
                               "LR baseline Political A" = model_pos_asym_pol2A,
                               "LR baseline Political B" = model_pos_asym_pol2B,
                               
                               "RL baseline Entertainment A" = model_pos_asym_ent1A,
                               "RL baseline Entertainment B" = model_pos_asym_ent1B,
                               "LR baseline Entertainment A" = model_pos_asym_ent2A,
                               "LR baseline Entertainment B" = model_pos_asym_ent2B)

save(qap_asym_pos_adfontes,
     file = "results/new_qap_pos_asymmetry_results-adfontes.RData")
