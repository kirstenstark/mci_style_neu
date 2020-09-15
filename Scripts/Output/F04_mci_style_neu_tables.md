    ### MCI_STYLE_NEU TABLES SCRIPT ###

    # Creates a table for the output of our four linear mixed-effects models. The upper half of the table includes ANOVA-
    # style type III tests (F-tests), the bottom half contains planned follow-up contrasts. For the F-tests, F-values,
    # degrees of freedom, and p-values are  printed, whereas for the contrasts, regression estimates, 95% confidence
    # intervals, and p-values are printed.

    # Load packages
    library(huxtable)    # Version 5.0.0

    # Load output from mixed models
    load("EEG/export/stats.RData")

    ## Registered S3 methods overwritten by 'car':
    ##   method                          from
    ##   influence.merMod                lme4
    ##   cooks.distance.influence.merMod lme4
    ##   dfbeta.influence.merMod         lme4
    ##   dfbetas.influence.merMod        lme4

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

<table style="width:100%;">
<colgroup>
<col style="width: 18%" />
<col style="width: 14%" />
<col style="width: 5%" />
<col style="width: 15%" />
<col style="width: 5%" />
<col style="width: 15%" />
<col style="width: 5%" />
<col style="width: 15%" />
<col style="width: 5%" />
</colgroup>
<thead>
<tr class="header">
<th></th>
<th style="text-align: left;">Rating 1</th>
<th></th>
<th style="text-align: left;">Rating 2</th>
<th></th>
<th style="text-align: left;">Verb-Related N400</th>
<th></th>
<th style="text-align: left;">Picture-Related N400</th>
<th></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><strong>Model output</strong></td>
<td style="text-align: left;"><strong><em>F</em></strong> (<strong><em>df</em></strong>)</td>
<td><strong><em>p</em></strong></td>
<td style="text-align: left;"><strong><em>F</em></strong> (<strong><em>df</em></strong>)</td>
<td><strong><em>p</em></strong></td>
<td style="text-align: left;"><strong><em>F</em></strong> (<strong><em>df</em></strong>)</td>
<td><strong><em>p</em></strong></td>
<td style="text-align: left;"><strong><em>F</em></strong> (<strong><em>df</em></strong>)</td>
<td><strong><em>p</em></strong></td>
</tr>
<tr class="even">
<td>Semantics</td>
<td style="text-align: left;">0.06<br/>(2, 90.1)</td>
<td>0.945</td>
<td style="text-align: left;">0.05<br/>(2, 78.9)</td>
<td>0.949</td>
<td style="text-align: left;">2.15<br/>(2, 167.0)</td>
<td>0.120</td>
<td style="text-align: left;">1.89<br/>(2, 75.8)</td>
<td>0.158</td>
</tr>
<tr class="odd">
<td>Style</td>
<td style="text-align: left;">1.79<br/>(1, 22.2)</td>
<td>0.195</td>
<td style="text-align: left;">5.85<br/>(1, 23.1)</td>
<td>0.024</td>
<td style="text-align: left;">1.06<br/>(1, 49.8)</td>
<td>0.308</td>
<td style="text-align: left;">6.92<br/>(1, 6692.5)</td>
<td>0.009</td>
</tr>
<tr class="even">
<td>Semantics × style</td>
<td style="text-align: left;">0.22<br/>(2, 161.9)</td>
<td>0.800</td>
<td style="text-align: left;">0.22<br/>(2, 69.6)</td>
<td>0.804</td>
<td style="text-align: left;">1.56<br/>(2, 58.8)</td>
<td>0.218</td>
<td style="text-align: left;">4.78<br/>(2, 82.2)</td>
<td>0.011</td>
</tr>
<tr class="odd">
<td><strong>Planned contrasts</strong></td>
<td style="text-align: left;"><strong>Est. [95% CI]</strong></td>
<td><strong><em>p</em></strong></td>
<td style="text-align: left;"><strong>Est. [95% CI]</strong></td>
<td><strong><em>p</em></strong></td>
<td style="text-align: left;"><strong>Est. [95% CI]</strong></td>
<td><strong><em>p</em></strong></td>
<td style="text-align: left;"><strong>Est. [95% CI]</strong></td>
<td><strong><em>p</em></strong></td>
</tr>
<tr class="even">
<td>Vio. - int.<br/>(normal)</td>
<td style="text-align: left;">0.00<br/>[-0.05, 0.06]</td>
<td>1.000</td>
<td style="text-align: left;">0.01<br/>[-0.05, 0.07]</td>
<td>1.000</td>
<td style="text-align: left;">-0.02<br/>[-0.35, 0.30]</td>
<td>1.000</td>
<td style="text-align: left;">-0.22<br/>[-0.77, 0.33]</td>
<td>0.720</td>
</tr>
<tr class="odd">
<td>MCI - int.<br/>(normal)</td>
<td style="text-align: left;">0.00<br/>[-0.05, 0.06]</td>
<td>1.000</td>
<td style="text-align: left;">0.01<br/>[-0.06, 0.07]</td>
<td>1.000</td>
<td style="text-align: left;">-0.38<br/>[-0.74, -0.03]</td>
<td>0.033</td>
<td style="text-align: left;">-0.75<br/>[-1.27, -0.23]</td>
<td>0.003</td>
</tr>
<tr class="even">
<td>Vio. - int.<br/>(fairytale)</td>
<td style="text-align: left;">0.01<br/>[-0.04, 0.07]</td>
<td>1.000</td>
<td style="text-align: left;">0.00<br/>[-0.06, 0.06]</td>
<td>1.000</td>
<td style="text-align: left;">0.06<br/>[-0.26, 0.39]</td>
<td>1.000</td>
<td style="text-align: left;">0.27<br/>[-0.28, 0.82]</td>
<td>0.540</td>
</tr>
<tr class="odd">
<td>MCI - int.<br/>(fairytale)</td>
<td style="text-align: left;">0.00<br/>[-0.06, 0.05]</td>
<td>1.000</td>
<td style="text-align: left;">-0.01<br/>[-0.08, 0.05]</td>
<td>1.000</td>
<td style="text-align: left;">0.02<br/>[-0.34, 0.37]</td>
<td>1.000</td>
<td style="text-align: left;">0.05<br/>[-0.48, 0.57]</td>
<td>1.000</td>
</tr>
</tbody>
</table>

    # Export as a word file (after some re-formatting)
    tab_word <- data.frame(lapply(tab, function(x){gsub("<br/>", "\n", x)}))
    tab_word <- data.frame(lapply(tab_word, function(x){gsub("\\*|\\_", "", x)}))
    huxt_word <- huxtable(tab_word, add_colnames = FALSE)
    quick_docx(huxt_word, file = "EEG/tables/lmm_table.docx", open = FALSE)

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
    ## [1] huxtable_5.0.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.5          mvtnorm_1.1-1       lattice_0.20-41     assertthat_0.2.1    digest_0.6.25       R6_2.4.1            cellranger_1.1.0   
    ##  [8] plyr_1.8.6          evaluate_0.14       ggplot2_3.3.2       highr_0.8           pillar_1.4.6        gdtools_0.2.2       rlang_0.4.7        
    ## [15] uuid_0.1-4          curl_4.3            readxl_1.3.1        rstudioapi_0.11     minqa_1.2.4         data.table_1.13.0   car_3.0-8          
    ## [22] nloptr_1.2.2.2      Matrix_1.2-18       flextable_0.5.11    rmarkdown_2.3       splines_4.0.2       lme4_1.1-23         statmod_1.4.34     
    ## [29] stringr_1.4.0       foreign_0.8-80      afex_0.27-2         munsell_0.5.0       compiler_4.0.2      numDeriv_2016.8-1.1 xfun_0.16          
    ## [36] systemfonts_0.3.1   base64enc_0.1-3     pkgconfig_2.0.3     lmerTest_3.1-2      htmltools_0.5.0     tidyselect_1.1.0    tibble_3.0.3       
    ## [43] rio_0.5.16          crayon_1.3.4        dplyr_1.0.0         commonmark_1.7      MASS_7.3-51.6       grid_4.0.2          nlme_3.1-148       
    ## [50] xtable_1.8-4        gtable_0.3.0        lifecycle_0.2.0     magrittr_1.5        scales_1.1.1        zip_2.1.1           estimability_1.3   
    ## [57] stringi_1.4.6       carData_3.0-4       renv_0.12.0         reshape2_1.4.4      xml2_1.3.2          ellipsis_0.3.1      generics_0.0.2     
    ## [64] vctrs_0.3.2         boot_1.3-25         openxlsx_4.1.5      tools_4.0.2         forcats_0.5.0       glue_1.4.1          officer_0.3.14     
    ## [71] purrr_0.3.4         hms_0.5.3           emmeans_1.4.8       abind_1.4-5         parallel_4.0.2      yaml_2.2.1          colorspace_1.4-1   
    ## [78] cpp11_0.2.1         knitr_1.29          haven_2.3.1
