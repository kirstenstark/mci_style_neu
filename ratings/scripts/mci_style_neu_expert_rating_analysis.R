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

## EXPERT RATING DATA ANALYSES ##
# Script reads-in context story rating data, provides descriptive data and 
# computes statistical analyses. 
# To evaluate the stimulus material, four export rated 50 % of the stories 
# each regarding their For quality control, "fairytaleness" on a 5-point 
# Likert style. We expected fairy-tale context stories to receive higher 
# ratings than the unmarked versions. Stories with outlier ratings were
# slightly adapted for the main study to incraese discriminability.

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
# Read in rating data file
d <- read.csv2(here::here("ratings", "raw", "expert_ratings", 
                          "expert_ratings_updated.csv"))
# Add item numbers
## Read in raw file 
story_items <- read.csv2(here::here("ratings", "raw", "stimuli", "stimuli.csv"))
## add version and context number
d$context_version <- NA
d$context_number <- NA
for(i in 1:nrow(d)) {
  if(d$Story[i] %in% story_items$fairytale) {
    d$context_version[i] <- "fairytale"
    d$context_number[i] <- 
      story_items$context_no[story_items$fairytale==d$Story[i]]
  } else if (d$Story[i] %in% story_items$unmarked) {
    d$context_version[i] <- "unmarked"
    d$context_number[i] <- 
      story_items$context_no[story_items$unmarked==d$Story[i]]
  } else {
    print("Error: Non-matching story")
  }
}
# table(is.na(d$context_version)); table(is.na(d$context_number))
# table(d$context_number); table(d$context_version)

# Convert to long df
df<- d[rep(row.names(d), each = 4), ] %>% 
  select(context_version, context_number)
df$rating <- rep(seq(1:4), times=100)
row.names(df) <- NULL
df$rating <- NA
d <- d %>% arrange(context_number, context_version)
df <- df %>% arrange(context_number, context_version)
for(i in 1:nrow(d)){
  if(!is.na(d$def.not[i])){r <- rep(1, times=d$def.not[i])} else {r <- c()}
  if(!is.na(d$prob.no[i])){
    r <- c(r,rep(2, times=d$prob.no[i]))}
  if(!is.na(d$undecided[i])){
    r <- c(r,rep(3, times=d$undecided[i]))}
  if(!is.na(d$prob.yes[i])){
    r <- c(r,rep(4, times=d$prob.yes[i]))}
  if(!is.na(d$def.yes[i])){
    r <- c(r,rep(5, times=d$def.yes[i]))}
  
  df$rating[df$context_number==d$context_number[i] & 
              df$context_version==d$context_version[i]] <- r
}


## --------------------------------------------------------------
## DESCRIPTIVES OF RATING DATA
# Descriptives: All context stories
df %>% group_by(context_version) %>% 
  summarise(mean=mean(rating),
            sd=sd(rating),
            min=min(rating),
            max=max(rating))

# Descriptives: Separately for all context stories
df %>% group_by(context_number, context_version) %>% 
  summarise(mean=mean(rating),
            sd=sd(rating),
            min=min(rating),
            max=max(rating))

## MIXED MODEL ANALYSES
# Preparation
df$style <- factor(df$context_version, levels=c("unmarked", "fairytale"))
t(contrasts.style <- t(cbind(c("unmarked" = -1, "fairytale" = 1))))
contrasts(df$style) <- ginv(contrasts.style)
df$rating <- scale(df$rating, center=T, scale=F)
df$context_number <- factor(df$context_number)
control_params <- lmerControl(calc.derivs = FALSE, 
                              optimizer = "bobyqa", 
                              optCtrl = list(maxfun = 2e5))

# LMM for fairytaleness-rating
mod_rating <- lmer_alt(
  rating ~ style + (style||context_number),
  data = df, control = control_params)
#summary(mod_rating)
anova(mod_rating)
