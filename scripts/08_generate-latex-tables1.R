library(tidyverse)
library(glue)
library(sna)

generate_latex_table1 <- function(modelsfilepath, tablename) {
  
  load(modelsfilepath)

  model_list <- list(
    pos_all = lrqap_pos_models$All,
    pos_pol = lrqap_pos_models$Political,
    pos_ent = lrqap_pos_models$Entertainment,
    
    neg_all = lrqap_neg_models$All,
    neg_pol = lrqap_neg_models$Political,
    neg_ent = lrqap_neg_models$Entertainment
  )
  
  # Custom Extraction Function
  
  # This function pulls all required metrics based on specified object paths.
  get_netlogit_stats <- function(model) {
    
    # --- COEFFS and P-VALUES ---
    
    # Main Coefficient (I_ij) - m$coefficients[2]
    i_ij_est <- round(model$coefficients[2], 2)
    
    # Reciprocity cofficient (y_ji) - m$coefficients[3]
    y_ji_est <- round(model$coefficients[3], 2)
    
    # Intercept Coefficient - m$coefficients[1]
    intercept_est <- round(model$coefficients[1], 2)
    
    # One-sided P-value for the main coefficient - m$pgreqabs[2]
    i_ij_pvalue <- model$pgreqabs[2] 
    
    # One-sided P-value for the reciprocity coefficient - m$pgreqabs[3]
    y_ji_pvalue <- model$pgreqabs[3]
    
    # One-sided P-value for the intercept - m$pgreqabs[1]
    intercept_pvalue <- model$pgreqabs[1]
    
    # --- GOF STATS ---
    
    # AIC - m$aic
    aic <- round(model$aic, 2)
    
    # BIC - m$bic
    bic <- round(model$bic, 2)
    
    # Pseudo-R^2 Calculation (McFadden's R-squared)
    
    # Null Deviance (Dn)
    null_deviance <- model$null.deviance
    
    # Residual Deviance (Dr)
    residual_deviance <- model$deviance 
    
    # Calculate Pseudo-R^2 = (Dn - Dr) / Dn
    pseudo_r2 <- round((null_deviance - residual_deviance) / null_deviance, 2)
    
    return(list(
      i_ij_est = i_ij_est,
      i_ij_pvalue = i_ij_pvalue,
      y_ji_est = y_ji_est,
      y_ji_pvalue = y_ji_pvalue,
      intercept_est = intercept_est,
      intercept_pvalue = intercept_pvalue,
      pseudo_r2 = pseudo_r2,
      aic = aic,
      bic = bic
    ))
  }
  
  # Processing and Formatting
  
  # Function to add significance stars based on ONE-SIDED p-value
  add_stars <- function(est, p_val) {
    stars <- ""
    # Use abs() to ensure positive coefficients don't drop the minus sign if negative
    est_str <- as.character(est) 
    
    if (p_val < 0.001) {
      stars <- "***"
    } else if (p_val < 0.01) {
      stars <- "**"
    } else if (p_val < 0.05) {
      stars <- "*"
    } else if (p_val < 0.1) {
      stars <- "+"
    }
    return(paste0(est_str, stars))
  }
  
  # Extract and format data for all models
  results <- lapply(model_list, get_netlogit_stats)
  
  # Initialize vectors to hold the formatted row data
  I_ij_row <- c()
  y_ji_row <- c()
  Intercept_row <- c()
  PseudoR2_row <- c()
  AIC_row <- c()
  BIC_row <- c()
  
  # Populate the rows
  for (res in results) {
    I_ij_row <- c(I_ij_row, add_stars(res$i_ij_est, res$i_ij_pvalue))
    y_ji_row <- c(y_ji_row, add_stars(res$y_ji_est, res$y_ji_pvalue))
    Intercept_row <- c(Intercept_row, add_stars(res$intercept_est, res$intercept_pvalue))
    PseudoR2_row <- c(PseudoR2_row, res$pseudo_r2)
    AIC_row <- c(AIC_row, res$aic)
    BIC_row <- c(BIC_row, res$bic)
  }
  
  # Assemble the Final LaTeX Code
  
  latex_code <- paste0(
    "\\begin{table}[h!]\n",
    "\\centering\n",
    "\\renewcommand{\\arraystretch}{1.8}\n",
    "\\begin{tabularx}{\\textwidth}{l X X X X X X}\n",
    "\\toprule\n",
    "\\toprule\n",
    "& \\multicolumn{3}{c}{\\textbf{Positive Influence}} & \\multicolumn{3}{c}{\\textbf{Negative Influence}} \\\\\n",
    "& \\multicolumn{3}{c}{DV: $\\text{logit}(P(\\mathbf{Y}_{ij} = 1))$} & \\multicolumn{3}{c}{DV: $\\text{logit}(P(\\mathbf{Y}^\\prime_{ij} = 1))$} \\\\\n",
    "\\cmidrule{2-7}\n",
    "& \\scriptsize All & \\scriptsize Political & \\scriptsize Entertainment & \\scriptsize All & \\scriptsize Political & \\scriptsize Entertainment \\\\\n",
    "\\midrule\n",
    "\\midrule\n",
    "$\\mathbf{I_{i,j}}$ (ideo. dist.) & ", paste(I_ij_row, collapse = " & "), " \\\\\n",
    "$\\mathbf{Y^\\prime_{j,i}}$ (reciprocity) & ", paste(y_ji_row, collapse = " & "), " \\\\\n",
    "(intercept) & ", paste(Intercept_row, collapse = " & "), " \\\\\n",
    "\\midrule\n",
    "Pseudo-$R^2$ & ", paste(PseudoR2_row, collapse = " & "), " \\\\\n",
    "AIC & ", paste(AIC_row, collapse = " & "), " \\\\\n",
    "BIC & ", paste(BIC_row, collapse = " & "), " \\\\\n",
    "\\bottomrule\n",
    "\\bottomrule\n",
    "\\end{tabularx}\n",
    "\\begin{minipage}{\\textwidth}\n",
    "\\vspace{0.3cm}\n",
    "+ p < 0.1 , * p < 0.05, ** p < 0.01, *** p < 0.001\n",
    "\\vspace{0.5cm}\n",
    "\\end{minipage}\n",
    "\\caption{LRQAP model results for Positive Influence and Negative Influence. \\\\\n",
    "Note: All LRQAP used 1000 simulations}\n",
    "\\label{table:tab1}\n",
    "\\end{table}"
  )
  
  # Print the final LaTeX code
  writeLines(latex_code, tablename)
  
}

generate_latex_table1("results/new_qap_results-60seasons-A.RData", "tables/table1.tex")

