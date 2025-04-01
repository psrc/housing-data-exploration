library(psrccensus)
library(tidyverse)

# BINCOME
# NP Number persons associate with housing record
# BLD == 1,2,3
# 01 .Mobile home or trailer
# 02 .One-family house detached
# 03 .One-family house attached
# HHT household/family type
# 4.Nonfamily household: Male householder: Living alone
# 6.Nonfamily household: Female householder: Living alone

x <- get_psrc_pums(span = 5,                     # Denoting ACS 5-year estimates; 1-year also available
                   dyear = 2023,                 # Last data year of span
                   level = "h",                  
                   vars = c("BINCOME", "NP", "BLD", "HHT"))      

View(x$variables)

x_s <- psrc_pums_count(x, stat_var = "NP", group_vars = c("BINCOME", "BLD", "NP", "HHT"))

df <- x_s |> 
  filter(HHT %in% c(4, 6) & BLD %in% c(1:3) & NP == 1)
