#/* Run this first piece of code only if you want to create a PDF report for GitHub/OSF
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::pdf_document(),
                  output_dir = "output",
                  knit_root_dir = getwd()) #*/
#' ---
#' author: ""
#' classoption: "landscape"
#' ---

## MCI_STYLE_NEU ADDITIONAL ANALYSES ##

# This script contains additional control analyses in addition to the planned 
# mixed models to control for semantic contexts instead of item as the random level.
# Both models are compared using a Likelihood Ratio Test. 

## SETUP ## ------------------------------------------------------------------------------------------------------------

# Load packages
library(MASS)         # Version 7.3-51.6
library(lme4)         # Version 1.1-23
library(lmerTest)     # Version 3.1-2
library(afex)         # Version 0.27-2
library(emmeans)      # Version 1.4.8
library(tidyverse)    # Version 1.3.0
library(magrittr)     # Version 1.5

# Load preprocessed data
a1 <- readRDS("EEG/export/a1.RDS")

# Remove trials with errors or invalid RTs/ERPs
a1 %<>% filter(!error) %>% na.omit()

# Center behavioral ratings (valence and arousal) around 0
a1 %<>% mutate(rating_1 = Rating1Resp - 2, rating_2 = Rating2Resp - 2)

# Define simple contrast coding for context narrative style (normal - fairytale)
#     HO(Intercept): (mu1+mu2)/2 = 0 <-> mu1+mu2 = 0
#     H0(Slope): -mu1 + mu2 = 0
#     with mu1 = mean of the normal style and mu2 = mean of the fairytale style
t(contrasts.style <- t(cbind(c("nor" = -1, "ftl" = 1))))
contrasts(a1$style) <- ginv(contrasts.style)

# Define simple contrast coding for semantics (violation - intuitive, mci - intuitive)
#     H0(Intercept): (mu1+mu2+mu3)/3 = 0 <-> mu1+mu2+mu3 = 0
#     H0(Slope1): -1*mu1 +1*mu2 + 0*mu3 = 0
#     H0(Slope2): -1*mu1 +0*mu2 + 1*mu3 = 0
#     with mu1 = mean of intuitive concepts, mu2 = mean of violations, mu3 = mean of MCIs
t(contrasts.semantics <- t(cbind(c("int" = -1, "vio" = 1, "mci" = 0),
                                 c("int" = -1, "vio" = 0, "mci" = 1))))
contrasts(a1$semantics) <- ginv(contrasts.semantics)

## LINEAR MIXED-EFFECTS MODELS ## --------------------------------------------------------------------------------------

# Specifiy settings for optimization in lmer
control_params <- lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# LMM for verb-related N400
mod_N400_verb <- lmer_alt(N400_verb ~ semantics*style + (semantics*style||participant) + (semantics*style||item),
                          data = a1, control = control_params)
mod_N400_verb2 <- lmer_alt(N400_verb ~ semantics*style + (semantics*style||participant) + (semantics*style||item)+
                            (semantics*style||kontext_nr) ,
                          data = a1, control = control_params)
anova(mod_N400_verb, mod_N400_verb2)

# LMM for picture-related N400
mod_N400_pict <- lmer_alt(N400_pict ~ semantics*style + (semantics*style||participant) + (semantics*style||item),
                          data = a1, control = control_params)
mod_N400_pict2 <- lmer_alt(N400_pict ~ semantics*style + (semantics*style||participant) + (semantics*style||item) +
                             (semantics*style||kontext_nr) ,
                          data = a1, control = control_params)
anova(mod_N400_pict, mod_N400_pict2)

# Create a list of all four models
models <- list("N400_VERB" = mod_N400_verb, 
               "N400_VERB2" = mod_N400_verb2, "N400_PICT" = mod_N400_pict,
               "N400_PICT2" = mod_N400_pict2)

# F-tests (type III tests)
(tests <- map(models, anova))

## PLANNED FOLLOW-UP CONTRASTS ## --------------------------------------------------------------------------------------

# Allow emmeans to compute Satterthwaites p-values
emm_options(lmer.df = "Satterthwaite", lmerTest.limit = Inf)

# Follow-up contrasts for the main effect of semantics
(means.semantics <- map(models,function(x){
  emmeans(x, trt.vs.ctrl ~ semantics, infer = TRUE, adjust = "bonferroni")$contrasts
  }))

# Follow-up contrasts for the main effect of context style
(means.style <- map(models, function(x){
  emmeans(x, trt.vs.ctrl ~ style, infer = TRUE, adjust = "bonferroni")$contrasts
  }))

# Follow-up contrasts for semantics within each context style
(means.nested <- map(models, function(x){
  emmeans(x, trt.vs.ctrl ~ semantics|style, infer = TRUE, adjust = "bonferroni")$contrasts
  }))

# Backup results
save(models, tests, means.semantics, means.style, means.nested, file = "EEG/export/stats.RData")

# System specs and package versions
sessionInfo()

