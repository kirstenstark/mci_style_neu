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

## ARRAY GENERATION FOR STORY RATINGS ##
# Script generates arrays for context story ratings that can be read into 
# jsPsych files. The rating itself is programmed in jsPsych (de Leeuw et al., 
# 2023) and hosted on the Jatos server (Lange et al., 2015)

## Setup ## ------------------------------------------------

library(tidyverse)
sessionInfo()
Sys.setlocale("LC_ALL", "de_DE.UTF-8")

## Read in stimulus list as data frame ## ------------------

df <- read.csv2(here::here(
  "ratings", "raw", "stimuli", "stimuli.csv"), 
  sep = ";", encoding = "UTF8") %>% filter(!is.na(context_no))
length(unique(df$context_no))
colnames(df)

## Generate two data frames ## -----------------------------
# A) fairy tale context for even and unmarked context for uneven numbers
# B) unmarked context for even and fairy tale context for uneven numbers
df$context_no <- as.numeric(as.character(df$context_no))

even <- df %>% filter(context_no %% 2 == 0)
uneven <- df %>% filter(context_no %% 2 == 1)

versiona <- rbind(even %>% rename(story=fairytale) %>% 
                    mutate(style="fairytale") %>% 
                    select(context_no, style, story), 
                  uneven %>% rename(story=unmarked) %>% 
                    mutate(style="unmarked") %>% 
                    select(context_no, style, story)) %>% 
  arrange(context_no)
versionb <- rbind(uneven %>% rename(story=fairytale) %>% 
                    mutate(style="fairytale") %>% 
                    select(context_no, style, story), 
                  even %>% rename(story=unmarked) %>% 
                    mutate(style="unmarked") %>% 
                    select(context_no, style, story)) %>% 
  arrange(context_no)

## Create vector as needed in jspsych and save as .txt ## ------------
# Each line is written in "[],\n", each column is separated by ",", 
# and characters are embedded in ' '. The first element has an 
# additional "[", and the last element "]]" instead of "],\n".
colnames(versiona)
versiona <- versiona %>% 
  mutate(context_no = paste0("[", context_no, ",", sep=""),
         style = paste0("'", style, "',", sep=""),
         story = paste0("'", story, "'],\n"))
versiona[1,1] <- paste0("[", versiona[1,1])
versiona[nrow(versiona), ncol(versiona)] <- 
  paste0(stringr::str_sub(versiona[nrow(versiona), ncol(versiona)], end=-3), 
         "]", sep="")
versiona_final <- apply(versiona, 1, 
                        function(row) paste(row, collapse = ""))
versiona_final <- paste(versiona_final, collapse = "")
writeLines(versiona_final, here::here(
  "ratings", "raw", "stimuli", "versiona.txt"))

colnames(versionb)
versionb <- versionb %>% 
  mutate(context_no = paste0("[", context_no, ",", sep=""),
         style = paste0("'", style, "',", sep=""),
         story = paste0("'", story, "'],\n"))
versionb[1,1] <- paste0("[", versionb[1,1])
versionb[nrow(versionb), ncol(versionb)] <- 
  paste0(stringr::str_sub(versionb[nrow(versionb), ncol(versionb)], end=-3), 
         "]", sep="")
versionb_final <- apply(versionb, 1, 
                        function(row) paste(row, collapse = ""))
versionb_final <- paste(versionb_final, collapse = "")
writeLines(versionb_final, here::here(
  "ratings", "raw", "stimuli", "versionb.txt"))
