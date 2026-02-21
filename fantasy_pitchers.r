library(baseballr)
library(dplyr)
library(ggplot2)

# get last year's fip leaders
pleaders <- fg_pitcher_leaders(startseason = "2025", endseason = "2025")
pleaders <- pleaders %>% 
  select(PlayerName, Age, IP, GS, SV, SO, FIP, WHIP) %>%
  filter(IP > 50) %>% 
  arrange(FIP)
head(pleaders[ , c("PlayerName", "FIP")])

rp <- pleaders %>% 
  filter(IP < 90, GS == 0) %>% 
  mutate(rank = row_number(FIP))
sp <- pleaders %>% 
  filter(IP > 150) %>% 
  mutate(rank = row_number(FIP))

# sp dist
ggplot(data = sp) + 
  geom_point(aes(rank, FIP))
# flattens after top 10
# whip chart
ggplot(data = sp) + 
  geom_point(aes(reorder(rank, WHIP), WHIP))
# exponential below 1.2

# rp dist
ggplot(data = rp) + 
  geom_point(aes(rank, FIP))
# top 2 exceed, top 10 somewhat
# more than 3.5 fip probably not great
ggplot(data = rp) + 
  geom_point(aes(reorder(rank, WHIP), WHIP))
# not much spread here

# consistency check
past23 <- fg_pitcher_leaders(startseason = "2023", endseason = "2023") %>% 
  select(PlayerName, Age, FIP, WHIP)
past24 <- fg_pitcher_leaders(startseason = "2024", endseason = "2024") %>% 
  select(PlayerName, Age, FIP, WHIP)
past25 <- fg_pitcher_leaders(startseason = "2025", endseason = "2025") %>% 
  select(PlayerName, Age, FIP, WHIP)
past3yrs <- rbind(past23, rbind(past24, past25))

past3_consistent <- past3yrs %>% 
  group_by(PlayerName) %>%
  summarise(FIPpp = mean(FIP), devFIP = sd(FIP), WHIPpp = mean(WHIP), devWHIP = sd(WHIP))

sp_con <- sp %>% merge(past3_consistent) %>% 
  mutate(floorFIP = FIPpp + devFIP, floorWHIP = WHIPpp + devWHIP) %>% 
  filter(IP > 150) %>%
  arrange(FIP)
print(sp_con %>% select(PlayerName, FIP, floorFIP, WHIP, floorWHIP), n = 40)

rp_con <- rp %>% merge(past3_consistent) %>% 
  mutate(floorFIP = FIPpp + devFIP, floorWHIP = WHIPpp + devWHIP) %>% 
  filter(IP < 90, GS == 0) %>%
  arrange(FIP)
print(rp_con %>% select(PlayerName, FIP, floorFIP, WHIP, floorWHIP), n = 40)
