
# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(patchwork)
source("format_snail_data.R")


# figure ------------------------------------------------------------------

g1 <- df_snail %>% 
 ggplot() +
 geom_point(mapping = aes(x = Velocity,
                          y = Count))

g2 <- df_snail %>% 
 ggplot() +
 geom_point(mapping = aes(x = coarseness,
                          y = Count))

g3 <- df_snail %>% 
 ggplot() +
 geom_point(mapping = aes(x = Depth,
                          y = Count))

g1 | g2 | g3
