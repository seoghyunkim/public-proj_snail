#' ---
#' title: Snail movement plot
#' author: Bribiesca J., Terui, A. & Kim, S.
#' output:
#'   html_document:
#'     theme: paper
#'     toc: true
#'     toc_float: true
#'     number_sections: TRUE
#' ---


#' # Load package -----
  rm(list = ls())
  library(tidyverse)
  library(ggpubr)

  
  
#' # Data -----
  
#'- Mark data (Occasion1)
  df_snail_occ1 <- read_csv("./data_raw/proj_snail_tag.csv")
  
  df_snail_occ1 <- df_snail_occ1 %>%
    select(Occasion, Date, Section = Transect, Tag_ID)
  
#'- Recapture data (Occasion2)  
  df_snail_occ2 <- read_csv("./data_raw/proj_snail_recap.csv")
  df_snail_occ2$Occasion <- "2" # add occasion

  df_snail_occ2 <- df_snail_occ2 %>%
   select(Occasion, Date, Section, Tag_ID) %>%
   drop_na(Section) # remove snail recaptured out of the study site

#'- combine two data
  df_snail <- rbind(df_snail_occ1, df_snail_occ2)

  
  
#' # Movement distance -----

#'- Occasion as character for plot
  df_snail$Occasion2 <- ifelse(df_snail$Occasion == 1, "Occ1", "Occ2")

  
#'- Section of capture and recapture
  rcp_sec <- filter(df_snail, !is.na(Tag_ID)) %>% 
    distinct(Occasion2, Tag_ID, .keep_all = TRUE) %>%
    select(Occasion2, Tag_ID, Section) %>%
    spread(Occasion2, Section)
  
#'- Calculate movement 
  snail_move <- rcp_sec %>%
    mutate(Mv12 = Occ2 - Occ1) %>%
    select(Tag_ID, Mv12) %>%
    gather(Interv, Dist, Mv12, factor_key = TRUE) %>%
    mutate(Dist = Dist * 5) %>% # section = 5 m
    na.omit()

# Check summary 
  summary(snail_move$Dist)
  summary(abs(snail_move$Dist))
  
#'- Movement plot
  snail_move_dist <- gghistogram(snail_move, x = "Dist", fill = "lightgrey", binwidth = 5) +
    scale_y_continuous("Frequency",  breaks = seq(0, 8, 2)) +
    scale_x_continuous("Distance (m)",  breaks = seq(-80, 80, 5)) +
    geom_vline(xintercept = 3.3, linetype="dashed", color = "red", size=1.0) + # value 3.3 is mean movement
    theme_bw() +
    theme(text=element_text(face="bold", size=8),  
        legend.position = "none",                     # remove legend panel
        panel.border=element_rect(colour='black'),
        panel.grid.major=element_line(colour=NA),
        panel.grid.minor=element_line(colour=NA),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)))  # remove grid
    theme(panel.background = element_rect(fill = "white")) 

  snail_move_dist


