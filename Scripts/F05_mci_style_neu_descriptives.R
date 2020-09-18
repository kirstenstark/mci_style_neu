#/* Run this first piece of code only if you want to create a html report for GitHub/OSF
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = prettydoc::html_pretty(),
                  output_dir = "Scripts/Output",
                  knit_root_dir = getwd()) #*/

## MCI_STYLE_NEU DESCRIPTIVES SCRIPT ##

# Gives descriptive information about the number of (a) rejected ERP epochs and (b) errors or unrealistically short
# reaction times per participant. For the latter, an ANOVA also checks whether the number of errors differs between
# experimental conditions.

# Load packages
library(Rmisc)        # version 1.5
library(tidyverse)    # Version 1.3.0
library(magrittr)     # Version 1.5

# Load preprocessed data
a1 <- readRDS("EEG/export/a1.RDS")

# Add a column for artifact-rejected ERPs
a1 %<>% mutate(rejected = is.na(N400_verb) | is.na(N400_pict))

# Check number of rejected ERPs participant
mean(table(a1$rejected, a1$participant)[2,])      # Mean
mean(table(a1$rejected, a1$participant)[2,])/300  # Percent
median(table(a1$rejected, a1$participant)[2,])    # Median
min(table(a1$rejected, a1$participant)[2,])       # Min
max(table(a1$rejected, a1$participant)[2,])       # Max

# Remove rejected ERPs from the data
a1 %<>% filter(!rejected)

# Check number of errors per participant
mean(table(a1$error, a1$participant)[2,])      # Mean
mean(table(a1$error, a1$participant)[2,])/300  # Percent
median(table(a1$error, a1$participant)[2,])    # Median
min(table(a1$error, a1$participant)[2,])       # Min
max(table(a1$error, a1$participant)[2,])       # Max

# Check if error rates differ between conditions
accuracy <- summarySEwithin(a1, measurevar = "error", withinvars = c("semantics", "style"),
                            betweenvars = "participant", idvar = "participant", na.rm = TRUE)
summary(aov(error ~ semantics*style + Error(participant/(semantics*style)), data = accuracy))

# System specs and package versions
sessionInfo()

