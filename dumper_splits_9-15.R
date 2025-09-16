library(baseballr)
library(dplyr)

# get Cal's FG lookup
cal <- playerid_lookup("Raleigh", "Cal")$fangraphs_id

# pull game-by-game batting records
cal_batting <- fg_batter_game_logs(cal, 2025) %>% select(Date, Pos, AB:CS)

# group by position and calculate statistics
cal_batting %>% 
  filter(Pos == "C" | Pos == "DH") %>% 
  group_by(Pos) %>% 
  summarise(
    "BA" = sum(H) / sum(AB),
    "OBP" = (sum(H) + sum(BB) + sum(HBP)) / (sum(AB) + sum(BB) + sum(HBP) + sum(SF)),
    "SLG" = (sum(`1B`) + 2*sum(`2B`) + 3*sum(`3B`) + 4*sum(HR)) / sum(AB),
    "OPS" = OBP + SLG
  )

# I have made a meaningful insight into baseball data!