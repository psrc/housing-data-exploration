library(psrccensus)
library(tidyverse)

x <- get_psrc_pums(span = 5,                     
                   dyear = 2023,                 
                   level = "h",                  
                   vars = c("BINCOME", "NP", "BLD", "TEN", "HHT", "BIN_AGE", "SEX", "VEH"))

# Under $50,000

df_filter <- x |> 
  filter(NP == 1 & 
           TEN %in% c("Rented", "Occupied without payment of rent")
         ) |> 
  mutate(bincome_w_50_under = factor(case_when(BINCOME %in% c("Under $25,000", "$25,000-$49,999") ~ "Under $50,000",
                                             .default = BINCOME)))

df <- psrc_pums_count(df_filter,
                      stat_var = "NP",
                      group_vars = c("bincome_w_50_under"))

write.csv(df, "data/single-renter-by-income-agg.csv")

# Under $50k & age group ----
# Under $50k & gender ----
# Under $50k & 0 vehicle hhs ----

