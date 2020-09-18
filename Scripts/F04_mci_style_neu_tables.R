#/* Run this first piece of code only if you want to create a PDF report for GitHub/OSF
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::pdf_document(),
                  output_dir = "Scripts/Output",
                  knit_root_dir = getwd()) #*/
#' ---
#' author: Aristei et al.
#' date: 2020
#' classoption: landscape
#' ---
#+ results="asis"

## MCI_STYLE_NEU TABLES SCRIPT ##

# Creates a table for the output of our four linear mixed-effects models. The upper half of the table includes ANOVA-
# style type III tests (F-tests), the bottom half contains planned follow-up contrasts. For the F-tests, F-values,
# degrees of freedom, and p-values are  printed, whereas for the contrasts, regression estimates, 95% confidence
# intervals, and p-values are printed.

# Load packages
library(huxtable)    # Version 5.0.0

# Load output from mixed models
load("EEG/export/stats.RData")

# Extract a table for the F tests for each model (columns: F value (df), p-value)
anovas <- lapply(tests, function(x){
  coefs <- data.frame(paste0(format(round(x$`F value`, 2), trim = TRUE, nsmall = 2),
                             "<br/>(", x$NumDF, ", ", format(round(x$DenDF, 1), trim = TRUE, nsmall = 1), ")"),
                      format(round(x$`Pr(>F)`, 3), nsmall = 3),
                      fix.empty.names = FALSE)
  coefs[,2] <- substr(coefs[,2], 1, 5)
  coefs[coefs[,2] == "0.000", 2] <- "< .001"
  return(coefs)})

# Bind all the F-tests to one data frame
anovas <- do.call(cbind, anovas)
anovas <- rbind(c("**_F_** (**_df_**)", "**_p_**"), anovas)

# Extract a table for the planned contrasts for each model (columns: estimate [CI], p-value)
conts <- lapply(means.nested, function(x){
  x <- as.data.frame(x)
  coefs <- data.frame(paste0(format(round(x$estimate, 2), trim = TRUE, nsmall = 2),
                             "<br/>[", format(round(x$lower.CL, 2), trim = TRUE, nsmall = 2), ", ",
                             format(round(x$upper.CL, 2), trim = TRUE, nsmall = 2), "]"),
                      format(round(x$p.value, 3), nsmall = 3),
                      fix.empty.names = FALSE)
  coefs[,2] <- substr(coefs[,2], 1, 5)
  coefs[coefs[,2] == "0.000", 2] <- "< .001"
  return(coefs)})

# Bind all the planned contrasts to one data frame
conts <- do.call(cbind, conts)
conts <- rbind(c("**Est. [95% CI]**", "**_p_**"), conts)

# Bind both data frames (F-tests and contrasts) below one another
tab <- rbind(anovas, conts)

# Add model names (dependent variables) as the first row
tab <- rbind(c("Rating 1", "", "Rating 2", "", "Verb-Related N400", "", "Picture-Related N400", ""), tab)

# Add a stub column
tab <- cbind(c("", "**Model output**", "Semantics", "Style", "Semantics × style",
               "**Planned contrasts**", "Vio. - int.<br/>(normal)", "MCI - int.<br/>(normal)",
               "Vio. - int.<br/>(fairytale)", "MCI - int.<br/>(fairytale)"), tab)

# Remove old column names
names(tab) <- NULL

# Create a huxtable and output as markdown
huxt <- huxtable(tab, add_colnames = FALSE)
print_md(huxt, max_width = Inf)

# Export as a word file (after some re-formatting)
tab_word <- data.frame(lapply(tab, function(x){gsub("<br/>", "\n", x)}))
tab_word <- data.frame(lapply(tab_word, function(x){gsub("\\*|\\_", "", x)}))
huxt_word <- huxtable(tab_word, add_colnames = FALSE)
quick_docx(huxt_word, file = "EEG/tables/lmm_table.docx", open = FALSE)

#+

# System specs and package versions
sessionInfo()

