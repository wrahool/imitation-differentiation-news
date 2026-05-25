library(sna)

generate_latex_table2 <- function(modelsfilepath, tablename) {

  # 1. Load Data
  load(modelsfilepath)
  
  # 2. Robust Significance Function
  add_stars <- function(est, p_val) {
    if (length(est) == 0 || is.null(est) || is.na(est)) {
      return("-")
    }
    
    stars <- ""
    p_val <- as.numeric(unlist(p_val))[1]
    est_val <- as.numeric(unlist(est))[1]
    
    if (is.na(p_val)) return(sprintf("%.2f", est_val))
    
    if (p_val < 0.001) {
      stars <- "***"
    } else if (p_val < 0.01) {
      stars <- "**"
    } else if (p_val < 0.05) {
      stars <- "*"
    } else if (p_val < 0.1) {
      stars <- "+"
    }
    return(paste0(sprintf("%.2f", est_val), stars))
  }
  
  # 3. Organize the Model List
  model_list <- list(
    LL_baseline_All = qap_asym_neg_60seasons$LL_baseline_All,
    LL_baseline_Pol = qap_asym_neg_60seasons$LL_baseline_Political,
    LL_baseline_Ent = qap_asym_neg_60seasons$LL_baseline_Entertainment,
    RR_baseline_All = qap_asym_neg_60seasons$RR_baseline_All,
    RR_baseline_Pol = qap_asym_neg_60seasons$RR_baseline_Political,
    RR_baseline_Ent = qap_asym_neg_60seasons$RR_baseline_Entertainment
  )
  
  # 4. Extract Data
  extracted_data <- lapply(names(model_list), function(name) {
    m <- model_list[[name]]
    is_ll_model <- grepl("LL_baseline", name)
    
    safe_aic <- as.numeric(unlist(m$aic))[1]
    safe_bic <- as.numeric(unlist(m$bic))[1] # Added BIC since it's in your image
    null_dev <- as.numeric(unlist(m$null.deviance))[1]
    res_dev  <- as.numeric(unlist(m$deviance))[1]
    
    pseudo_r2_val <- if(!is.na(null_dev) && !is.na(res_dev)) {
      (null_dev - res_dev) / null_dev
    } else { NA }
    
    list(
      intercept = add_stars(m$coefficients[1], m$pgreqabs[1]),
      LR        = add_stars(m$coefficients[2], m$pgreqabs[2]),
      RL        = add_stars(m$coefficients[3], m$pgreqabs[3]),
      RR        = if(is_ll_model) add_stars(m$coefficients[4], m$pgreqabs[4]) else "\\tiny(baseline)",
      LL        = if(!is_ll_model) add_stars(m$coefficients[4], m$pgreqabs[4]) else "\\tiny(baseline)",
      recip     = add_stars(m$coefficients[5], m$pgreqabs[5]),
      pseudo_r2 = if(!is.na(pseudo_r2_val)) sprintf("%.2f", pseudo_r2_val) else "-",
      aic       = if(!is.na(safe_aic)) format(round(safe_aic, 0), big.mark=",") else "-",
      bic       = if(!is.na(safe_bic)) format(round(safe_bic, 0), big.mark=",") else "-"
    )
  })
  
  # 5. Build the LaTeX Table string
  row_definitions <- list(
    c("$\\mathbf{X}_{LL}$", "LL"),
    c("$\\mathbf{X}_{RR}$", "RR"),
    c("$\\mathbf{X}_{LR}$", "LR"),
    c("$\\mathbf{X}_{RL}$", "RL"),
    c("$\\mathbf{Y^\\prime}_{j,i}$ (reciprocity)", "recip"),
    c("(intercept)", "intercept"),
    c("Pseudo $R^2$", "pseudo_r2"),
    c("AIC", "aic"),
    c("BIC", "bic")
  )
  
  # Using >{\centering\arraybackslash}X forces the columns to spread and center the text
  latex_code <- paste0(
    "\\begin{table}[h!]\n",
    "\\centering\n",
    "\\small\n",
    "\\renewcommand{\\arraystretch}{1.8}\n",
    "\\begin{tabularx}{\\textwidth}{l *{6}{>{\\centering\\arraybackslash}X}}\n", 
    "\\hline\\hline\n",
    "& \\multicolumn{6}{c}{\\textbf{Negative Influence}} \\\\\n",
    " & \\multicolumn{6}{c}{DV: $\\text{logit}(P(\\mathbf{Y}^\\prime_{ij} = 1))$} \\\\\n",
    "\\cmidrule{2-7}\n",
    " & All & Pol & Ent & All & Pol & Ent \\\\\n",
    "\\hline\\hline\n"
  )
  
  for (row in row_definitions) {
    label <- row[1]
    key   <- row[2]
    
    if (label == "Pseudo $R^2$") {
      latex_code <- paste0(latex_code, "\\hline\n")
    }
    
    values <- sapply(extracted_data, function(x) x[[key]])
    line <- paste0(label, " & ", paste(values, collapse = " & "), " \\\\\n")
    latex_code <- paste0(latex_code, line)
  }
  
  latex_code <- paste0(
    latex_code,
    "\\hline\\hline\n",
    "\\end{tabularx}\n",
    "\\begin{minipage}{\\textwidth}\n",
    "\\vspace{0.3cm}\n",
    "+ p < 0.1 , * p < 0.05, ** p < 0.01, *** p < 0.001\n",
    "\\vspace{0.5cm}\n",
    "\\end{minipage}\n",
    "\\caption{LRQAP models showing partisan asymmetry in differentiation\\\\\n",
    "Note: All LRQAP used 1000 simulations}\n",
    "\\end{table}"
  )
  
  # 6. Output
  writeLines(latex_code, tablename)
}

generate_latex_table2("results/new_qap_neg_asymmetry_results-A.RData", "tables/table2.tex")