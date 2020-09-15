#/* Run this first piece of code only if you want to create a markdown report for GitHub/OSF
rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,
                  output_format = rmarkdown::md_document(),
                  output_dir = "Scripts/Output",
                  knit_root_dir = getwd()) #*/

### MCI_STYLE_NEU PREPROCESSING SCRIPT ###

# Reads behavioral log files for all participants and binds them together. Performs EEG preprocessing including
# re-referencing, ocular artifact correction, filtering, epoching, baseline correction, and automatic artifact
# rejection, for both verb- and picture-related potentials. Computes single-trial mean ERP amplitudes for the N400
# component and exports by-participant averaged waveforms for plotting.

## SETUP ## ------------------------------------------------------------------------------------------------------------

# Load packages
library(naturalsort)  # Version 0.1.3
library(tidyverse)    # Version 1.3.0
library(magrittr)     # Version 1.5
library(eeguana)      # Version 0.1.4.9000

# Make sure we have enough (virtual) RAM available
memory.limit(size = 64000)

## BEHAVIORAL DATA ## --------------------------------------------------------------------------------------------------

# List behavioral log files
filenames.rt <- list.files("RT", pattern = ".txt", full.names = TRUE) %>% naturalsort()

# Read behavioral data into one data frame (if umlaute can't be read, try: Sys.setlocale("LC_ALL", "C"))
a1 <- map(filenames.rt, read.delim2) %>% bind_rows()

# Remove filler trials and empty lines
a1 %<>% filter(SatzBed != "filler") %>% na.omit()

# Factorize columns for semantics and context (fixed effects) and participants and items (random variables)
a1 %<>% mutate(semantics = factor(SatzBed, levels = c("correct", "sev", "mci"), labels = c("int", "vio", "mci")),
               style = factor(Version, levels = c(1, 2), labels = c("nor", "ftl")),
               participant = factor(VP_nr, levels = as.character(1:length(unique(VP_nr)))),
               item = factor(Verb))

# Add a column which checks if participants made an error or if the RT was unrealistically short (< 200 ms)
a1 %<>% mutate(error = Errors == 99 | BildRT < 200)

## EEG DATA ## ---------------------------------------------------------------------------------------------------------

# List EEG header files and BESA matrices (for ocular correction)
filenames_eeg <- list.files("EEG/raw", pattern = ".vhdr", full.names = TRUE)
filenames_besa <- list.files("EEG/cali", pattern = ".matrix", full.names = TRUE)

# Preprocessing
eeg <- map2(filenames_eeg, filenames_besa, function(vhdr_filename, besa_filename){
  message(paste("## PREPROCESSING", vhdr_filename, "WITH", besa_filename, "(VERB-RELATED)"))
  dat <- read_vhdr(vhdr_filename)
  message("## FIXING CHANNEL SETUP...")
  eog <- dat$.signal$Auge_u
  dat$.signal$Auge_u <- 0
  dat %<>% rename(A2 = Mastoid, A1 = Auge_u)
  message("## RE-REFERENCING...")
  channames <- dat %>% channel_names(.)
  dat %<>% eeg_rereference(ref = channames)
  message("## OCCULAR CORRECTION...")
  besa <- as.matrix(read.delim(besa_filename, row.names = 1))
  tmp <- t(dat$.signal %>% select(all_of(channames)))
  tmp <- besa %*% tmp # This is the actual OC; lines above and below are just transforming the signal table
  tmp <- split(tmp, row(tmp))
  tmp <- map(tmp, channel_dbl)
  dat$.signal[,channames] <- tmp[1:length(channames)]
  dat$.signal$IO1 <- eog
  message("## FILTERING...")
  dat %<>% eeg_filt_band_pass(freq = c(0.1, 30))
  message("## EPOCHING...")
  dat %<>% eeg_segment(.description %in% c("S211", "S212", "S213", "S221", "S222", "S223",
                                           "S181", "S182", "S183", "S191", "S192", "S193"),
                       lim = c(-200, 998), unit = "ms")
  message("## BASELINE CORRECTION...")
  dat %<>% eeg_baseline()
  message("## ARTIFACT REJECTION...")
  dat %<>%
    eeg_artif_amplitude(-IO1, threshold = c(-200, 200)) %>%
    eeg_artif_minmax(-IO1, threshold = 50, window = 2, unit = "ms") %>%
    eeg_artif_minmax(-IO1, threshold = 200, window = 200, unit = "ms") %>%
    eeg_events_to_NA(.type == "artifact", all_chs = TRUE)
  message("## DONE\n")
  return(dat)
})

# Bind data from all participants together
eeg %<>% do.call(what = bind)

# Extract experimental factors from EEG triggers and factorize
eeg %<>%
  mutate(type = case_when(description %in% c("S211", "S212", "S213", "S221", "S222", "S223") ~ "Verb-related",
                          description %in% c("S181", "S182", "S183", "S191", "S192", "S193") ~ "Picture-related"),
         semantics = case_when(description %in% c("S211", "S221", "S181", "S191") ~ "int",
                               description %in% c("S212", "S222", "S182", "S192") ~ "vio",
                               description %in% c("S213", "S223", "S183", "S193") ~ "mci"),
         style = case_when(description %in% c("S211", "S212", "S213", "S181", "S182", "S183") ~ "nor",
                           description %in% c("S221", "S222", "S223", "S191", "S192", "S193") ~ "ftl")) %>%
  mutate(type = factor (type, levels = c("Verb-related", "Picture-related")),
         semantics = factor(semantics, levels = c("int", "vio", "mci")),
         style = factor(style, levels = c("nor", "ftl")))

# Compute mean amplitude across electrodes in the N400 ROI
eeg %<>% mutate(ROI_verb = chs_mean(C1, C2, C3, C4, Cz, CP1, CP2, CP3, CP4, CPz, P3, P4),
                ROI_pict = chs_mean(Fz, Cz))

# Average single trial ERPs in the ROI across the relevant time window (and bind to behavioral data)
a1 <- eeg %>%
  filter(type == "Verb-related", between(as_time(.sample), 0.300, 0.500)) %>%
  group_by(.id) %>%
  summarise(erps = mean(ROI_verb)) %>%
  pull(erps) %>%
  as.numeric() %>%
  bind_cols(a1, N400_verb = .)
a1 <- eeg %>%
  filter(type == "Picture-related", between(as_time(.sample), 0.150, 0.350)) %>%
  group_by(.id) %>%
  summarise(erps = mean(ROI_pict)) %>%
  pull(erps) %>%
  as.numeric() %>%
  bind_cols(a1, N400_pict = .)

# Export behavioral data and ERPs for mixed models
saveRDS(a1, file = "EEG/export/a1.RDS")

## PREPARE FOR PLOTTING ## ---------------------------------------------------------------------------------------------

# Compute and export averaged waveforms for plotting
eeg %>%
  mutate(participant = rep(a1$participant, each = 2),
         error = rep(a1$error, each = 2)) %>%
  filter(!error) %>%
  group_by(.sample, type, semantics, style, participant) %>%
  summarize_at(channel_names(.), mean, na.rm = TRUE) %>%
  saveRDS("EEG/export/avgs.RDS")

# System specs and package versions
sessionInfo()

