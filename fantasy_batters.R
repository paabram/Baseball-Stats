library(baseballr)
library(dplyr)
library(ggplot2)

# get last year's ops leaders
leaders <- fg_batter_leaders(startseason = "2025", endseason = "2025")
leaders <- leaders %>% 
  select(PlayerName, playerid, Age, G:PA, HR, OBP:OPS) %>%
  filter(PA > 400) %>% 
  arrange(-OPS) %>% 
  mutate(rank = row_number(-OPS))

# peek
head(leaders[ , c("rank", "PlayerName", "OPS")])

# plot distribution of OPS
top125 <- leaders[1:125, ] %>% arrange(-OPS)
medOPS <- median(top125$OPS)

ggplot(data = top125) + 
  geom_point(aes(rank, OPS)) +
  geom_hline(yintercept = medOPS) +
  labs(title = "OPS Distribution") +
  geom_abline(slope = (0.75 - 0.85)/75, intercept = 0.8825) # rough line for inflection point

# you're gonna want to get lots of the top 20 guys
# who are they?
top125[1:20, ]

# who has been consistent in the past 3 years?
past23 <- fg_batter_leaders(startseason = "2023", endseason = "2023") %>% 
  filter(PA > 400) %>% 
  select(PlayerName, playerid, Age, wRC_plus)
past24 <- fg_batter_leaders(startseason = "2024", endseason = "2024") %>% 
  filter(PA > 400) %>% 
  select(PlayerName, playerid, Age, wRC_plus)
past25 <- fg_batter_leaders(startseason = "2025", endseason = "2025") %>% 
  filter(PA > 400) %>% 
  select(PlayerName, playerid, Age, wRC_plus)

past3yrs <- rbind(past23, rbind(past24, past25))
# keep only top 125 from last year
top_past3 <- past3yrs %>% filter(playerid %in% top125$playerid)
# drop single records and calculate sd of wrc+
past3_consistent <- top_past3 %>% 
  group_by(PlayerName) %>% 
  filter(n() != 1) %>%
  summarise(wRCpp = mean(wRC_plus), dev = sd(wRC_plus))
# how consistent can you be?
past3_consistent %>% arrange(dev)
ggplot(data = past3_consistent) + 
  geom_point(aes(row_number(dev), dev)) +
  geom_abline(slope = 5/25, intercept = 1)
# more than ~17 deviation in wRC+ YoY probably bad

# how does this bode for last year's best?
top125 <- top125 %>% 
  merge(past3_consistent) %>% 
  arrange(-OPS)
top125[1:20, ] %>% select(PlayerName, OPS, dev)
# passes sanity check, but note that for guys like Judge a deviation of 24 points means 175 vs 200 wrc+
# so really we want to ignore guys whose yoy deviation could swing them out of the top
top125 <- top125 %>% mutate(floor = wRCpp - dev)
top125[1:20, ] %>% select(PlayerName, OPS, floor, dev) # now this feels right

# plot floor distribution to see how greedy we can be
ggplot(data = top125) + 
  geom_point(aes(row_number(-floor), floor)) +
  geom_abline(slope = (100 - 113)/(62.5 - 25), intercept = 125)
# first 10 guys exceed
# sharp fall off below ~95 wrc+
# get out!
top_consistent <- top125 %>% 
  filter(floor > 95) %>% 
  arrange(-floor)
# no old heads
top_consistent <- top_consistent %>% filter(Age < 34)
head(top_consistent %>% select(PlayerName, OPS, floor, Age), 20)

# these are blue chip, but w only 75 records will need to supplement w up and comers
rookies <- top_past3 %>% 
  group_by(PlayerName) %>% 
  filter(n() == 1) %>% 
  summarise(wRCpp = mean(wRC_plus), age = max(Age)) %>% 
  arrange(-wRCpp)
rookies
# some injury-prone non rookies as well but use discretion

# todo: positional breakdown
