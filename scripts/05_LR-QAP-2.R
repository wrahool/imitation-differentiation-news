
# This script tests the liberal-conservative asymmetry in the effect of negative influence
# produces Table 2
# author: Subhayan Mukerjee


library(tidyverse)
library(igraph)
library(ggridges)
library(sna)
library(broom)

set.seed(9)

setwd("C:/Users/Subhayan/Work/intermedia-influence/")


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
# 1 = Significant Negative Influence, 0 = Positive or No Influence
Y_neg_binary <- ifelse(Y_mat < 0, 1, 0)

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

# do left outlets differentiate more from right than they do form the left?

model_neg_asym1A <- netlogit(Y_neg_binary, 
                             list(LR = X_LR, RL = X_RL, RR = X_RR, recip = t(Y_neg_binary)),
                             nullhyp = "qap",
                             reps = 1000)

summary(model_neg_asym1A)

# baseline is L outlets differentiating from L
# x2 (RL): L outlets differentiate from R outlets more than they do from L outlets
# estimate = 1.46, 4.3 times more, p ~ 0

# without reciprocity
model_neg_asym1B <- netlogit(Y_neg_binary, 
                             list(LR = X_LR, RL = X_RL, RR = X_RR),
                             nullhyp = "qap",
                             reps = 1000)

summary(model_neg_asym1B)

# baseline is L outlets differentiating from L
# x2 (RL): L outlets differentiate from R outlets more than they do from L outlets
# estimate = 1.46, 4.3 times more, p ~ 0

# x2 (RL): L outlets differentiate more from R outlets than they do from L outlets

# do right outlets differentiate more from left than they do form the right? 
model_neg_asym2A <- netlogit(Y_neg_binary, 
                             list(LR = X_LR, RL = X_RL, LL = X_LL, recip = t(Y_neg_binary)), 
                             nullhyp = "qap",
                             reps = 1000)

summary(model_neg_asym2A)

# baseline is R outlets differentiating from R
# x1 (LR): R outlets do not differentiate from L outlets any more than they do from R outlets. Estimate = 1.18, 3.2 times more, but p =  0.139

# without reciprocity
model_neg_asym2B <- netlogit(Y_neg_binary, 
                             list(LR = X_LR, RL = X_RL, LL = X_LL), 
                             nullhyp = "qap",
                             reps = 1000)

summary(model_neg_asym2B)

# baseline is R outlets differentiating from R
# x1 (LR): R outlets do differentiate from L outlets significantly more than they do (but only if you donot control for reciprocity) from R outlets. Estimate = 2.56, 12.9 times more, p = 0.026

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
# 1 = Significant Negative Influence, 0 = Positive or No Influence
Y_neg_binary_pol <- ifelse(Y_mat_pol < 0, 1, 0)

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

# do left outlets differentiate more from right than they do form the left?

model_neg_asym_pol1A <- netlogit(Y_neg_binary_pol, 
                                 list(LR = X_LR, RL = X_RL, RR = X_RR, recip = t(Y_neg_binary_pol)), 
                                 nullhyp = "qap",
                                 reps = 1000)

summary(model_neg_asym_pol1A)

# baseline is L outlets differentiating from L
# x2 (RL): L outlets differentiate from R outlets more than they do from L outlets
# estimate is 1.48, 4.4  times,  p ~ 0.001

# without reciprocity
model_neg_asym_pol1B <- netlogit(Y_neg_binary_pol, 
                                 list(LR = X_LR, RL = X_RL, RR = X_RR),
                                 nullhyp = "qap",
                                 reps = 1000)

summary(model_neg_asym_pol1B)

# x2 (RL): L outlets differentiate more from R outlets than they do from L outlets
# baseline is L outlets differentiating from L
# estimate is 1.55, 4.7 times,  p ~ 0

# do right outlets differentiate more from left than they do form the right? 
model_neg_asym_pol2A <- netlogit(Y_neg_binary_pol, 
                                 list(LR = X_LR, RL = X_RL, LL = X_LL, recip = t(Y_neg_binary_pol)),
                                 nullhyp = "qap",
                                 reps = 1000)

summary(model_neg_asym_pol2A)

# baseline is R outlets differentiating from R
# x1 (LR): R outlets do not differentiate from L outlets any more than they do from R outlets
# estimate = 0.9, 2.46 times, but p is 0.213

# without reciprocity
model_neg_asym_pol2B <- netlogit(Y_neg_binary_pol, 
                                 list(LR = X_LR, RL = X_RL, LL = X_LL), 
                                 nullhyp = "qap",
                                 reps = 1000)

summary(model_neg_asym_pol2B)

# x1 (LR): R outlets do not differentiate from L outlets any more than they do from R outlets
# estimate = 2.07, 7.945 times, but p is 0.051

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
# 1 = Significant Negative Influence, 0 = Positive or No Influence
Y_neg_binary_ent <- ifelse(Y_mat_ent < 0, 1, 0)

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

# do left outlets differentiate more from right than they do form the left?

model_neg_asym_ent1A <- netlogit(Y_neg_binary_ent, 
                                 list(LR = X_LR, RL = X_RL, RR = X_RR, recip = t(Y_neg_binary_ent)), 
                                 nullhyp = "qap",
                                 reps = 1000)

summary(model_neg_asym_ent1A)

# baseline is L outlets differentiating from L
# look at x2 (RL)
# no significant effects
#  estimate = -16.11, 1e-7 times, but p is 0.234

# without reciprocity
model_neg_asym_ent1B <- netlogit(Y_neg_binary_ent, 
                                 list(LR = X_LR, RL = X_RL, RR = X_RR),
                                 nullhyp = "qap",
                                 reps = 1000)

summary(model_neg_asym_ent1B)

# nothing is significant
# look at x2 (RL)
# estimate = -16.12,  9.9e-8 times, p =  0.247

# do right outlets differentiate more from left than they do form the right? 
model_neg_asym_ent2A <- netlogit(Y_neg_binary_ent, 
                                 list(LR = X_LR, RL = X_RL, LL = X_LL, recip = t(Y_neg_binary_ent)), 
                                 nullhyp = "qap",
                                 reps = 1000)

summary(model_neg_asym_ent2A)

# baseline is R outlets differentiating from R
# look at x1 (LR)
# nothing is significant
# estimate =  17.24, 3e+7 times, p = 0.688

# without reciprocity
model_neg_asym_ent2B <- netlogit(Y_neg_binary_ent, 
                                 list(LR = X_LR, RL = X_RL, LL = X_LL),
                                 nullhyp = "qap",
                                 reps = 1000)

summary(model_neg_asym_ent2B)

# baseline is R outlets  differentiating from R
# look at x1 (LR)
# nothing is significant
# estimate = 17.2, 3.06e+7, p = 0.710

# Combine all QAP correlation results into a list
# A models include reciprocity control
qap_asym_neg_60seasons <- list("LL_baseline_All" = model_neg_asym1A,
                               "RR_baseline_All" =  model_neg_asym2A,
                               
                               "LL_baseline_Political" = model_neg_asym_pol1A,
                               "RR_baseline_Political" = model_neg_asym_pol2A,
                               
                               "LL_baseline_Entertainment" = model_neg_asym_ent1A,
                               "RR_baseline_Entertainment" = model_neg_asym_ent2A)

save(qap_asym_neg_60seasons,
     file = "results/new_qap_neg_asymmetry_results-A.RData")

# Combine all QAP correlation results into a list
# B models exclude reciprocity control
qap_asym_neg_60seasons <- list("LL_baseline_All" = model_neg_asym1B,
                               "RR_baseline_All" =  model_neg_asym2B,
                               
                               "LL_baseline_Political" = model_neg_asym_pol1B,
                               "RR_baseline_Political" = model_neg_asym_pol2B,
                               
                               "LL_baseline_Entertainment" = model_neg_asym_ent1B,
                               "RR_baseline_Entertainment" = model_neg_asym_ent2B)

save(qap_asym_neg_60seasons,
     file = "results/new_qap_neg_asymmetry_results-B.RData")
