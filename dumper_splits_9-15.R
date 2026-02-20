library(baseballr)
library(dplyr)
library(ggplot2)

# get Cal's FG lookup
cal <- playerid_lookup("Raleigh", "Cal")$fangraphs_id

# pull game-by-game batting records
cal_batting <- fg_batter_game_logs(cal, 2025) %>% 
  select(Date, Pos, AB:CS) %>% 
  filter(Pos == "C" | Pos == "DH")

# group by position and calculate statistics
cal_batting %>% 
  group_by(Pos) %>% 
  summarise(
    "BA" = sum(H) / sum(AB),
    "OBP" = (sum(H) + sum(BB) + sum(HBP)) / (sum(AB) + sum(BB) + sum(HBP) + sum(SF)),
    "SLG" = (sum(`1B`) + 2*sum(`2B`) + 3*sum(`3B`) + 4*sum(HR)) / sum(AB),
    "OPS" = OBP + SLG
  )

# I have made a meaningful insight into baseball data!
# in what specific ways is he better?

cal_summary <- cal_batting %>%
  filter(Pos == "C" | Pos == "DH") %>% 
  group_by(Pos) %>%
  summarise(
    Hits = mean(H, na.rm = TRUE),
    Walks = mean(BB, na.rm = TRUE),
    Strikeouts = mean(SO, na.rm = TRUE)
  )

# Step 2: reshape data to long format for ggplot
cal_long <- cal_summary %>%
  pivot_longer(cols = -Pos, names_to = "Stat", values_to = "Value")

# Step 3: plot
ggplot(cal_long, aes(x = Stat, y = Value, fill = Pos)) +
  geom_col(position = "dodge") +
  labs(title = "Batting Stats by Position",
       x = "Stat",
       y = "Total") +
  theme_minimal()
