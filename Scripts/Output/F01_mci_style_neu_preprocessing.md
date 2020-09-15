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

    ## [1] 64000

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

    ## ## PREPROCESSING EEG/raw/Vp0001.vhdr WITH EEG/cali/Vp0001.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0001.vhdr...

    ## # Data from EEG/raw/Vp0001.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 4.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 8

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 4

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0002.vhdr WITH EEG/cali/Vp0002.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0002.vhdr...

    ## # Data from EEG/raw/Vp0002.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 4.1 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 4

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 6

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0003.vhdr WITH EEG/cali/Vp0003.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0003.vhdr...

    ## # Data from EEG/raw/Vp0003.eeg was read.

    ## # Data from 5 segment(s) and 64 channels was loaded.

    ## # Object size in memory 4.3 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 161

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 206

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0004.vhdr WITH EEG/cali/Vp0004.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0004.vhdr...

    ## # Data from EEG/raw/Vp0004.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 4.1 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0005.vhdr WITH EEG/cali/Vp0005.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0005.vhdr...

    ## # Data from EEG/raw/Vp0005.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 4.2 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0006.vhdr WITH EEG/cali/Vp0006.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0006.vhdr...

    ## # Data from EEG/raw/Vp0006.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.8 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 2

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 5

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0007.vhdr WITH EEG/cali/Vp0007.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0007.vhdr...

    ## # Data from EEG/raw/Vp0007.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 4 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0008.vhdr WITH EEG/cali/Vp0008.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0008.vhdr...

    ## # Data from EEG/raw/Vp0008.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 4.2 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 1

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0009.vhdr WITH EEG/cali/Vp0009.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0009.vhdr...

    ## # Data from EEG/raw/Vp0009.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 4 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 1

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 2

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0010.vhdr WITH EEG/cali/Vp0010.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0010.vhdr...

    ## # Data from EEG/raw/Vp0010.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 1.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 280 segments found.

    ## # Object size in memory 84.7 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 11

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 22

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0010b.vhdr WITH EEG/cali/Vp0010b.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0010b.vhdr...

    ## # Data from EEG/raw/Vp0010b.eeg was read.

    ## # Data from 1 segment(s) and 64 channels was loaded.

    ## # Object size in memory 2.1 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 320 segments found.

    ## # Object size in memory 96.8 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 13

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 34

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0011.vhdr WITH EEG/cali/Vp0011.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0011.vhdr...

    ## # Data from EEG/raw/Vp0011.eeg was read.

    ## # Data from 2 segment(s) and 64 channels was loaded.

    ## # Object size in memory 4.2 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0012.vhdr WITH EEG/cali/Vp0012.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0012.vhdr...

    ## # Data from EEG/raw/Vp0012.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 4.2 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0013.vhdr WITH EEG/cali/Vp0013.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0013.vhdr...

    ## # Data from EEG/raw/Vp0013.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 4.5 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0014.vhdr WITH EEG/cali/Vp0014.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0014.vhdr...

    ## # Data from EEG/raw/Vp0014.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.8 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 7

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 6

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0015.vhdr WITH EEG/cali/Vp0015.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0015.vhdr...

    ## # Data from EEG/raw/Vp0015.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.7 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0016.vhdr WITH EEG/cali/Vp0016.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0016.vhdr...

    ## # Data from EEG/raw/Vp0016.eeg was read.

    ## # Data from 4 segment(s) and 64 channels was loaded.

    ## # Object size in memory 4 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 3

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 5

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0017.vhdr WITH EEG/cali/Vp0017.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0017.vhdr...

    ## # Data from EEG/raw/Vp0017.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 4.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0
    ## # Number of intervals with artifacts: 0

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0018.vhdr WITH EEG/cali/Vp0018.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0018.vhdr...

    ## # Data from EEG/raw/Vp0018.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 4.3 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 8

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 16

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0019.vhdr WITH EEG/cali/Vp0019.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0019.vhdr...

    ## # Data from EEG/raw/Vp0019.eeg was read.

    ## # Data from 5 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 25

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 89

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0020.vhdr WITH EEG/cali/Vp0020.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0020.vhdr...

    ## # Data from EEG/raw/Vp0020.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 2

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 4

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0021.vhdr WITH EEG/cali/Vp0021.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0021.vhdr...

    ## # Data from EEG/raw/Vp0021.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 14

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 12

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0022.vhdr WITH EEG/cali/Vp0022.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0022.vhdr...

    ## # Data from EEG/raw/Vp0022.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 4 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 34

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 20

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0023.vhdr WITH EEG/cali/Vp0023.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0023.vhdr...

    ## # Data from EEG/raw/Vp0023.eeg was read.

    ## # Data from 3 segment(s) and 64 channels was loaded.

    ## # Object size in memory 3.9 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 2

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 2

    ## ## DONE

    ## ## PREPROCESSING EEG/raw/Vp0024.vhdr WITH EEG/cali/Vp0024.matrix (VERB-RELATED)

    ## Reading file EEG/raw/Vp0024.vhdr...

    ## # Data from EEG/raw/Vp0024.eeg was read.

    ## # Data from 5 segment(s) and 64 channels was loaded.

    ## # Object size in memory 4 Gb

    ## ## FIXING CHANNEL SETUP...

    ## ## RE-REFERENCING...

    ## ## OCCULAR CORRECTION...

    ## ## FILTERING...

    ## Width of the transition band at the low cut-off frequency is 0.1 Hz

    ## Width of the transition band at the high cut-off frequency is 7.5 Hz

    ## Setting up band-pass filter from 0.1 - 30 Hz

    ## ## EPOCHING...

    ## # Total of 600 segments found.

    ## # Object size in memory 181.4 Mb after segmentation.

    ## ## BASELINE CORRECTION...

    ## ## ARTIFACT REJECTION...

    ## # Number of intervals with artifacts: 51

    ## # Number of intervals with artifacts: 0

    ## # Number of intervals with artifacts: 57

    ## ## DONE

    # Bind data from all participants together
    eeg %<>% do.call(what = bind)

    ## # Object size in memory 4.3 Gb

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

    ## R version 4.0.2 (2020-06-22)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 18362)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                   
    ## [5] LC_TIME=German_Germany.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices datasets  utils     methods   base     
    ## 
    ## other attached packages:
    ##  [1] eeguana_0.1.4.9000 magrittr_1.5       forcats_0.5.0      stringr_1.4.0      dplyr_1.0.0        purrr_0.3.4        readr_1.3.1       
    ##  [8] tidyr_1.1.0        tibble_3.0.3       ggplot2_3.3.2      tidyverse_1.3.0    naturalsort_0.1.3 
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.5        lubridate_1.7.9   assertthat_0.2.1  digest_0.6.25     R6_2.4.1          cellranger_1.1.0  backports_1.1.8   signal_0.7-6     
    ##  [9] reprex_0.3.0      evaluate_0.14     httr_1.4.2        highr_0.8         pillar_1.4.6      rlang_0.4.7       readxl_1.3.1      rstudioapi_0.11  
    ## [17] data.table_1.13.0 blob_1.2.1        rmarkdown_2.3     munsell_0.5.0     tinytex_0.25      broom_0.7.0       compiler_4.0.2    modelr_0.1.8     
    ## [25] xfun_0.16         pkgconfig_2.0.3   htmltools_0.5.0   tidyselect_1.1.0  RcppRoll_0.3.0    fansi_0.4.1       crayon_1.3.4      dbplyr_1.4.4     
    ## [33] withr_2.2.0       MASS_7.3-51.6     grid_4.0.2        jsonlite_1.7.0    gtable_0.3.0      lifecycle_0.2.0   DBI_1.1.0         scales_1.1.1     
    ## [41] cli_2.0.2         stringi_1.4.6     renv_0.12.0       fs_1.5.0          ini_0.3.1         xml2_1.3.2        ellipsis_0.3.1    generics_0.0.2   
    ## [49] vctrs_0.3.2       tools_4.0.2       glue_1.4.1        hms_0.5.3         yaml_2.2.1        colorspace_1.4-1  rvest_0.3.6       knitr_1.29       
    ## [57] haven_2.3.1
