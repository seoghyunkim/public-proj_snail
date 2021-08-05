
# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)

# data --------------------------------------------------------------------

df_tag <- read_csv("data_raw/proj_snail_tag.csv") %>% 
        select(-Comments,
               -Shell_width_unit) %>% 
        mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

df_habitat <- read_csv("data_raw/proj_snail_habitat.csv") %>% 
        select(-Comments,
               -Width_unit) %>% 
        mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

df_recap <- read_csv("data_raw/proj_snail_recap.csv") %>% 
        mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
               Occasion = 2)


# formatting --------------------------------------------------------------

## merge tag data and habitat
df0 <- df_tag %>% 
        left_join(df_habitat,
                  by = c("Transect", "Quadrat", "Date")) %>% 
        bind_rows(df_recap) %>% 
        drop_na(Tag_ID)

## tag id for recaptured individuals
recap_id <- df0 %>% 
        group_by(Tag_ID) %>% 
        summarize(n = n()) %>% 
        filter(n > 1) %>%
        pull(Tag_ID)

## merge quadrat and visual surveys
df_tag_habitat <- df0 %>% 
        filter(Tag_ID %in% recap_id) %>%
        mutate(coarseness = 1 * SL/25 +
                            2 * SD/25 +
                            3 * GV/25 +
                            4 * PE/25 +
                            5 * CO/25 +
                            6 * BO/25 +
                            7 * BR/25) %>% 
        group_by(Occasion, Tag_ID) %>% 
        slice(which.max(Date)) # select most recent observation in each occasion


# plot --------------------------------------------------------------------

df_tag_habitat %>% 
        ggplot() +
        geom_point(aes(y = coarseness,
                       x = Tag_ID))
