#/* Run this first piece of code only if you want to create a PDF report for GitHub/OSF
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::pdf_document(),
                  output_dir = "output",
                  knit_root_dir = getwd()) #*/
#' ---
#' author: Aristei et al.
#' date: 2020
#' classoption: landscape
#' ---

## MCI_STYLE_NEU MIXED MODELS SCRIPT ##

# Computes linear mixed-effects regression models with simple contrast coding for the fixed effects of semantics and
# narrative context (the context being emotionally neutral). Thus, in each model, the estimate of the intercept is the
# grand mean, while the estimates of the slopes contrast "treatment" levels to their respective reference levels
# (semantics: violation - intuitive, mci - intuitive; narrative context style: fairytale - normal). The maximal random
# effects structure is used with all by-participant and by-item random slopes and random intercepts. Correlations
# between random effects are removed if the model fails to converge with two different numerical optimizers. Planned
# follow-up contrasts are computed for the main effects and the effects of semantics separately within each type of
# narrative context style.

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

# LMM for rating 1
mod_valence <- lmer_alt(rating_1 ~ semantics*style + (semantics*style||participant) + (semantics*style||item),
                        data = a1, control = control_params)

# LMM for rating 2
mod_arousal <- lmer_alt(rating_2 ~ semantics*style + (semantics*style||participant) + (semantics*style||item),
                        data = a1, control = control_params)

# LMM for verb-related N400
mod_N400_verb <- lmer_alt(N400_verb ~ semantics*style + (semantics*style||participant) + (semantics*style||item),
                          data = a1, control = control_params)

# LMM for picture-related N400
mod_N400_pict <- lmer_alt(N400_pict ~ semantics*style + (semantics*style||participant) + (semantics*style||item),
                          data = a1, control = control_params)

# Create a list of all four models
models <- list("RATING_1" = mod_valence, "RATING_2" = mod_arousal,
               "N400_VERB" = mod_N400_verb, "N400_PICT" = mod_N400_pict)

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

