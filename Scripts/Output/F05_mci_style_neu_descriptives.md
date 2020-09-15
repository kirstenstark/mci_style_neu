    ### MCI_STYLE_NEU DESCRIPTIVES SCRIPT ###

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

    ## [1] 10.29167

    mean(table(a1$rejected, a1$participant)[2,])/300  # Percent

    ## [1] 0.03430556

    median(table(a1$rejected, a1$participant)[2,])    # Median

    ## [1] 3

    min(table(a1$rejected, a1$participant)[2,])       # Min

    ## [1] 0

    max(table(a1$rejected, a1$participant)[2,])       # Max

    ## [1] 116

    # Remove rejected ERPs from the data
    a1 %<>% filter(!rejected)

    # Check number of errors per participant
    mean(table(a1$error, a1$participant)[2,])      # Mean

    ## [1] 1.5

    mean(table(a1$error, a1$participant)[2,])/300  # Percent

    ## [1] 0.005

    median(table(a1$error, a1$participant)[2,])    # Median

    ## [1] 1

    min(table(a1$error, a1$participant)[2,])       # Min

    ## [1] 0

    max(table(a1$error, a1$participant)[2,])       # Max

    ## [1] 8

    # Check if error rates differ between conditions
    accuracy <- summarySEwithin(a1, measurevar = "error", withinvars = c("semantics", "style"),
                                betweenvars = "participant", idvar = "participant", na.rm = TRUE)
    summary(aov(error ~ semantics*style + Error(participant/(semantics*style)), data = accuracy))

    ## 
    ## Error: participant
    ##           Df    Sum Sq   Mean Sq F value Pr(>F)
    ## Residuals 23 3.735e-07 1.624e-08               
    ## 
    ## Error: participant:semantics
    ##           Df   Sum Sq   Mean Sq F value Pr(>F)
    ## semantics  2 0.000396 0.0001978   1.849  0.169
    ## Residuals 46 0.004921 0.0001070               
    ## 
    ## Error: participant:style
    ##           Df   Sum Sq   Mean Sq F value Pr(>F)
    ## style      1 0.000008 8.030e-06   0.063  0.805
    ## Residuals 23 0.002955 1.285e-04               
    ## 
    ## Error: participant:semantics:style
    ##                 Df   Sum Sq   Mean Sq F value Pr(>F)
    ## semantics:style  2 0.000111 5.533e-05   0.643   0.53
    ## Residuals       46 0.003958 8.605e-05

    # System specs and package versions
    sessionInfo()

    ## R version 4.0.2 (2020-06-22)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Catalina 10.15.6
    ## 
    ## Matrix products: default
    ## BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices datasets  utils     methods   base     
    ## 
    ## other attached packages:
    ##  [1] magrittr_1.5    forcats_0.5.0   stringr_1.4.0   dplyr_1.0.0     purrr_0.3.4     readr_1.3.1     tidyr_1.1.0     tibble_3.0.3    ggplot2_3.3.2  
    ## [10] tidyverse_1.3.0 Rmisc_1.5       plyr_1.8.6      lattice_0.20-41
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.1.0 xfun_0.16        haven_2.3.1      colorspace_1.4-1 vctrs_0.3.2      generics_0.0.2   htmltools_0.5.0  yaml_2.2.1       blob_1.2.1      
    ## [10] rlang_0.4.7      pillar_1.4.6     glue_1.4.1       withr_2.2.0      DBI_1.1.0        dbplyr_1.4.4     modelr_0.1.8     readxl_1.3.1     lifecycle_0.2.0 
    ## [19] munsell_0.5.0    gtable_0.3.0     cellranger_1.1.0 rvest_0.3.6      evaluate_0.14    knitr_1.29       fansi_0.4.1      highr_0.8        broom_0.7.0     
    ## [28] Rcpp_1.0.5       renv_0.12.0      scales_1.1.1     backports_1.1.8  jsonlite_1.7.0   fs_1.5.0         hms_0.5.3        digest_0.6.25    stringi_1.4.6   
    ## [37] grid_4.0.2       cli_2.0.2        tools_4.0.2      crayon_1.3.4     pkgconfig_2.0.3  ellipsis_0.3.1   xml2_1.3.2       reprex_0.3.0     lubridate_1.7.9 
    ## [46] assertthat_0.2.1 rmarkdown_2.3    httr_1.4.2       rstudioapi_0.11  R6_2.4.1         compiler_4.0.2
