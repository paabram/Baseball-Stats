library(baseballr)
library(dplyr)
library(ggplot2)

# get last year's ops leaders
leaders <- fg_batter_leaders(startseason = "2025", endseason = "2025")
leaders <- leaders %>% 
  select(PlayerName, Age, G:PA, HR, OBP:OPS) %>%
  filter(PA > 400) %>% 
  arrange(-OPS)

# peek
head(leaders[ , c("PlayerName", "OPS")])

# plot distribution of OPS
ggplot()