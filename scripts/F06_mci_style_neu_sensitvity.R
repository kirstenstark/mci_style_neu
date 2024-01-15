#/* Run this first piece of code only if you want to create a PDF report for GitHub/OSF
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::pdf_document(),
                  output_dir = "output",
                  knit_root_dir = getwd()) #*/
#' ---
#' author: ""
#' classoption: "landscape"
#' ---

## MCI_STYLE_NEU SENSITIVITY ANALYSIS SCRIPT ##

# Runs a sensitivity analysis for the (nested) effect of interest ("stylenor:semantics2")
# for the verb and picture models. For this sensitivity analysis, the effect size is changed
# linearly from 0.1 to 1.0 µV, and the model is re-fitted many times tom compute the power
# at each effect size (defined as the average number of simulated models for which the
# effect of interest is significant). This is used to determine the smallest effect size
# at which (given the current participant and item sample sizes) the effect can be detected
# with sufficient statistical power (e.g., 80 or 90%).

## SETUP ## ------------------------------------------------------------------------------------------------------------

# Load packages
library(MASS)         # Version 7.3-51.6
library(lme4)         # Version 1.1-23
library(lmerTest)     # Version 3.1-2
library(afex)         # Version 0.27-2
library(emmeans)      # Version 1.4.8
library(tidyverse)    # Version 1.3.0
library(magrittr)     # Version 1.5
library(simr)         # Version 1.0.7
library(furrr)        # Version 0.3.1
library(glue)         # Version 1.4.1
library(testit)       # Version ???
library(tictoc)       # Version ???

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

# Specify settings for optimization in lmer
control_params <- lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# LMM for verb-related N400
mod_N400_verb <- lmer_alt(N400_verb ~ style/semantics + (semantics*style||participant) + (semantics*style||item),
                          data = a1, control = control_params)

# LMM for picture-related N400
mod_N400_pict <- lmer_alt(N400_pict ~ style/semantics + (semantics*style||participant) + (semantics*style||item),
                          data = a1, control = control_params)

# Create a list of all four models
models <-  list("N400_VERB" = mod_N400_verb, "N400_PICT" = mod_N400_pict)

# Settings for sensitivity simulation
effect_name <- "stylenor:semantics2"
effect_sizes <- seq(0.1, 0.2, 0.1)  # seq(0.1, 1.0, 0.1)
alpha <- 0.05
n_sim <- 10  # 1000
n_cores <- 8

# Set up parallel processing
plan(multisession, workers = n_cores)

# Loop over models (verb/pict)
message("Launching sensitivity simulations")
tic()
power <- map_dfr(models, function(model) {
  
  # Extract data from the original model
  new_data <- model@frame
  dep_var <- all.vars(formula(model))[1]
  
  # Loop over effect sizes of interest
  future_map_dfr(effect_sizes, function(effect_size,
                                        model_ = model,
                                        control_params_ = control_params) {
    
    # Print progress (only gets printed once the worker is done with all effect sizes)
    message(glue("Finished simulating effect size {effect_size} for model {dep_var}."))
    
    # Generate many simulations (response vectors) with the new effect size
    fixef(model_)[effect_name] <- effect_size
    resp_sims <- simulate(model_, nsim = n_sim)
    
    # Loop over simulations
    map_dfr(resp_sims, function(resp_sim) {
      
      # Re-fit the model
      new_data[[dep_var]] <- resp_sim
      is_not_converged <- has_warning(new_model <- update(model_,
                                                          data = new_data,
                                                          control = control_params_))
      
      # Extract model outputs
      estimate <- round(fixef(new_model)[effect_name], 4)
      p_value <- round(summary(new_model)$coefficients[effect_name, "Pr(>|t|)"], 6)
      is_significant <- p_value < alpha
      is_singular <- any(grepl("singular", new_model@optinfo$conv$lme4$messages))
      
      # Return model outputs
      data.frame(dep_var,
                 effect_name,
                 effect_size,
                 estimate,
                 p_value,
                 is_significant,
                 is_singular,
                 is_not_converged)
    })
  },
  .options=furrr_options(seed = 42))
})
toc()

# Stop parallel processing
plan(sequential)

# Summarize across simulations
conf_level <- 1 - alpha
conf_method <- "logit"
power_summary <- power %>%
  group_by(dep_var, effect_name, effect_size) %>%
  summarize(estimate_mean = mean(estimate),
            n_sim = n(),
            n_significant = sum(is_significant),
            perc_singular = mean(is_singular),
            perc_not_converged = mean(is_not_converged),
            .groups = "drop")

# Compute average power incl. binomial confidence interval
power_confint <- with(power_summary, 
                      binom::binom.confint(n_significant, n_sim, conf_level, conf_method)) %>%
  select(-c(method, x, n)) %>%
  rename(power_mean = mean, power_lower = lower, power_upper = upper)

# Combine and round
power_summary <- power_summary %>%
  cbind(power_confint) %>%
  mutate(across(.cols = c(estimate_mean, power_mean, power_lower, power_upper,
                          perc_singular, perc_not_converged),
                .fns = ~ round(.x, 4)))
print(power_summary)

# Save to files
write_csv(power, "EEG/tables/power.csv")
write_csv(power_summary, "EEG/tables/power_summary.csv")
