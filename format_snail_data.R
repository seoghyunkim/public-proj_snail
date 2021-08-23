
# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)


# data --------------------------------------------------------------------

df_habitat <- read_csv("data_raw/proj_snail_habitat.csv")
df_tag <- read_csv("data_raw/proj_snail_tag.csv")


# data manipulation -------------------------------------------------------

## summarize tagged individual data into count data
## substrates are divided by 25 because we assessed substrates for 25 grids

df_count <- df_tag %>% 
 group_by(Occasion, Transect, Quadrat) %>% 
 summarize(Date = unique(Date),
           Count = n())

df_h <- df_habitat %>% 
 select(Occasion = Ocassion,
        Transect,
        Quadrat,
        Width,
        Depth,
        Velocity,
        BR:SL,
        Time) %>% 
 mutate(per_BR = BR/25,
        per_BO = BO/25,
        per_CO = CO/25,
        per_PE = PE/25,
        per_GV = GV/25,
        per_SD = SD/25,
        per_SL = SL/25,
        coarseness = 1 * per_SL +
                     2 * per_SD +
                     3 * per_GV +
                     4 * per_PE +
                     5 * per_CO +
                     6 * per_BO +
                     7 * per_BR)


# combine data frames -----------------------------------------------------

df_snail <- df_h %>% 
 left_join(df_count, by = c("Occasion","Transect", "Quadrat")) %>% 
 mutate(Count = ifelse(is.na(Count),
                       0,
                       Count))

#write.csv(df_snail, file = "data_format/proj_snail_count_env_20210823.csv")
