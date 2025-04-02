library(psrccensus)
library(tidyverse)

x <- get_psrc_pums(span = 5,                     
                   dyear = 2023,                 
                   level = "h",                  
                   vars = c("BINCOME", "NP", "BLD", "TEN", "HHT", "BIN_AGE", "AGEP", "SEX", "VEH"))

# Under $50,000

df_filter <- x |> 
  filter(NP == 1 & 
           TEN %in% c("Rented", "Occupied without payment of rent")
         ) |> 
  mutate(bincome_w_50_under = factor(case_when(BINCOME %in% c("Under $25,000", "$25,000-$49,999") ~ "Under $50,000",
                                             .default = BINCOME), 
                                     levels = c("Under $50,000", "$75,000-$99,999", "$50,000-$74,999", "$100,000 or more")))

df <- psrc_pums_count(df_filter,
                      stat_var = "NP",
                      group_vars = c("bincome_w_50_under"))

write.csv(df, "data/single-renter-by-income-agg.csv", row.names = FALSE)

# Under $50k & age group ----


df_filter <- x |> 
  filter(NP == 1 & 
           TEN %in% c("Rented", "Occupied without payment of rent") &
           BINCOME %in% c("Under $25,000", "$25,000-$49,999")
  ) |> 
  mutate(bin_age_broad = factor(case_when(AGEP %in% 0:19 ~ "Under 20 years",
                                          AGEP %in% 20:29 ~ "20 to 29 years",
                                          AGEP %in% 30:39 ~ "30 to 39 years",
                                          AGEP %in% 40:49 ~ "40 to 49 years",
                                          AGEP >= 50 ~ "50 years and over"), 
                                levels = c("Under 20 years", "20 to 29 years", "30 to 39 years", "40 to 49 years", "50 years and over")))

df <- psrc_pums_count(df_filter,
                      stat_var = "NP",
                      group_vars = c("bin_age_broad"))

write.csv(df, "data/single-renter-under-50k-by-age.csv", row.names = FALSE)

# Under $50k & gender ----

df_filter <- x |> 
  filter(NP == 1 & 
           TEN %in% c("Rented", "Occupied without payment of rent") &
           BINCOME %in% c("Under $25,000", "$25,000-$49,999")
  )

df <- psrc_pums_count(df_filter,
                      stat_var = "NP",
                      group_vars = c("SEX"))

write.csv(df, "data/single-renter-under-50k-by-sex.csv", row.names = FALSE)

# Under $50k & 0 vehicle hhs ----

labels <- as.character(unique(x$variables$VEH))

df_filter <- x |> 
  filter(NP == 1 & 
           TEN %in% c("Rented", "Occupied without payment of rent") &
           BINCOME %in% c("Under $25,000", "$25,000-$49,999")
  ) |> 
  mutate(vehicles = factor(VEH, levels = c("No vehicles", labels[grepl("\\d+", labels)])))

df <- psrc_pums_count(df_filter,
                      stat_var = "NP",
                      group_vars = c("vehicles"))

write.csv(df, "data/single-renter-under-50k-by-veh.csv", row.names = FALSE)
