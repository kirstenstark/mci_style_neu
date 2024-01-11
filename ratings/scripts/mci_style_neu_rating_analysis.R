#/* Run this first piece of code only if you want to create a PDF report for GitHub/OSF
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::pdf_document(),
                  output_dir = here::here("ratings", "output"),
                  knit_root_dir = getwd()) #*/
#' ---
#' author: ""
#' classoption: "landscape"
#' ---
#' 

## RATING DATA ANALYSES ##
# Script reads-in context story rating data, provides descriptive data and 
# computes statistical analyses. 
# For quality control, 32 participants rated the context stories for 
# "fairytaleness" on a 5-point Likert style. We expected fairy-tale
# context stories to receive higher ratings than the unmarked versions.

## Setup ## ------------------------------------------------
# Encoding
Sys.setlocale("LC_ALL", "de_DE.UTF-8")

# Load packages
library(MASS)         # Version 7.3-51.6
library(lme4)         # Version 1.1-23
library(lmerTest)     # Version 3.1-2
library(afex)         # Version 0.27-2
library(emmeans)      # Version 1.4.8
library(tidyverse)    # Version 1.3.0

## Read-in raw files ## -------------------------------------
# Read in file names 
files_full <- list.files(list.dirs(list.dirs(
  path=here::here("ratings", "raw", "rating_data"), recursive=T), 
  recursive=F), full.names=T)

# Bind all files into one
df <- data.frame()
for(i in 1:length(files_full)){
  temp <- read.csv(files_full[i])
  df <- rbind(df, temp)
}

## Language check ## -----------------------------------------
# Check whether all participants indicated German as their mother tongue
df %>% group_by(language) %>% count()

# Check whether 50% of the participants saw version A and B
df %>% group_by(rating_version) %>% count()

## Quality check ## ------------------------------------------
# Make sure none of the participants had zero variance in the ratings
df %>% group_by(subject) %>% 
  summarise(mean = mean(rating), 
            sd = sd(rating), 
            n = sum(!is.na(rating))) %>% 
  filter(sd == 0)

## Participant descriptives ## ------------------------------
# gender
df %>% filter(presentation_order==1) %>% 
  group_by(gender) %>% 
  count()
# age
df %>% filter(presentation_order==1) %>% 
  group_by(age) %>% 
  summarise(
    mean=mean(age),
    sd=sd(age),
    min=min(age),
    max=max(age))
# language
(df %>% filter(presentation_order==1) %>% 
  group_by(language) %>% 
  count() -> x)
(no_of_German_speakers <- x$n[x$language=="Deutsch"] +
  x$n[x$language=="unter anderem Deutsch"])

## Clean data frame and export # -------------------------------
# replace jatos ID by subject numbers
df$subject <- as.numeric(factor(df$subject, levels=unique(df$subject)))
# select only relevant columns
df <- df %>% select(subject, presentation_order, rating_version, 
                    presentation_order, context_no, version, rating) %>% 
  arrange(subject, presentation_order)
# save as csv
write.csv(df, here::here("ratings", "raw", "cleaned", "rating_results.csv"),
          col.names = F)

## --------------------------------------------------------------
## DESCRIPTIVES OF RATING DATA
# Descriptives: All context stories
df <- read.csv2(here::here("ratings", "raw", "cleaned", "rating_results.csv"), 
                sep=",")
df %>% group_by(version) %>% 
  summarise(mean=mean(rating),
            sd=sd(rating),
            min=min(rating),
            max=max(rating))

# Descriptives: Separately for all context stories
df %>% group_by(context_no, version) %>% 
  summarise(mean=mean(rating),
            sd=sd(rating),
            min=min(rating),
            max=max(rating))

## MIXED MODEL ANALYSES
# Preparation
df$style <- factor(df$version, levels=c("unmarked", "fairytale"))
t(contrasts.style <- t(cbind(c("unmarked" = -1, "fairytale" = 1))))
contrasts(df$style) <- ginv(contrasts.style)
df$rating <- scale(df$rating, center=T, scale=F)
df$subject <- factor(df$subject)
df$context_no <- factor(df$context_no)
control_params <- lmerControl(calc.derivs = FALSE, 
                              optimizer = "bobyqa", 
                              optCtrl = list(maxfun = 2e5))

# LMM for fairytaleness-rating
mod_rating <- lmer_alt(
  rating ~ style + (style||subject) + (style||context_no),
  data = df, control = control_params)
#summary(mod_rating)
anova(mod_rating)
