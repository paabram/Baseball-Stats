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
top150 <- leaders[1:150, ] %>% arrange(-OPS)
medOPS <- median(top150$OPS)

ggplot(data = top150) + 
  geom_point(aes(rank, OPS)) +
  geom_hline(yintercept = medOPS) +
  labs(title = "OPS Distribution") +
  geom_abline(slope = (0.75 - 0.85)/75, intercept = 0.8825) # rough line for inflection point

# you're gonna want to get lots of the top 20 guys
# who are they?
top150[1:20, ]

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
top_past3 <- past3yrs %>% filter(playerid %in% top150$playerid)
# drop single records and calculate sd of wrc+
past3_consistent <- top_past3 %>% 
  group_by(PlayerName) %>% 
  filter(n() != 1) %>%
  summarise(wRCpp = mean(wRC_plus), dev = sd(wRC_plus))
# how consistent can you be?
past3_consistent %>% arrange(dev)
ggplot(data = past3_consistent) + 
  geom_point(aes(row_number(dev), dev)) +
  geom_abline(slope = (17-5)/(87.5-25), intercept = 0)
# more than ~17 deviation in wRC+ YoY probably bad

# how does this bode for last year's best?
top150 <- top150 %>% 
  merge(past3_consistent) %>% 
  arrange(-OPS)
top150[1:20, ] %>% select(PlayerName, OPS, dev)
# passes sanity check, but note that for guys like Judge a deviation of 24 points means 175 vs 200 wrc+
# so really we want to ignore guys whose yoy deviation could swing them out of the top
top150 <- top150 %>% mutate(floor = wRCpp - dev)
top150[1:20, ] %>% select(PlayerName, OPS, floor, dev) # now this feels right

# plot floor distribution to see how greedy we can be
ggplot(data = top150) + 
  geom_point(aes(row_number(-floor), floor)) +
  geom_abline(slope = (100 - 113)/(62.5 - 25), intercept = 125)
# first 10 guys exceed
# sharp fall off below ~90 floor
# get out!
top_consistent <- top150 %>% 
  filter(floor > 90) %>% 
  arrange(-floor)
# no old heads
top_consistent <- top_consistent %>% filter(Age < 34)

# TOP 15 BY FLOOR - exponential decrease through first 10
head(top_consistent %>% select(PlayerName, OPS, floor, Age), 20)
# TOP 15 BY OPS
head(top_consistent %>% arrange(-OPS) %>% select(PlayerName, OPS, floor, dev, Age), 20)

# these are blue chip, but w only 75 records will need to supplement w up and comers
rookies <- top_past3 %>% 
  group_by(PlayerName) %>% 
  filter(n() == 1) %>% 
  summarise(wRCpp = mean(wRC_plus), age = max(Age)) %>% 
  arrange(-wRCpp)
rookies
# some injury-prone non rookies as well but use discretion

# todo: positional breakdown

# from QuickLahman project
playerPos <- function(pID, list = FALSE, years = c(2025, 2025)) {
  if (length(pID) == 0 || is.na(pID))
    return(NA_character_)
  
  # if only one year is passed in, we still want a range of two years, so concat to itself
  if (length(years) == 1)
    years <- c(years, years)
  
  # get the table for the given player in the given years
  filtered <- Lahman::Fielding %>% dplyr::filter(playerID == pID, yearID >= years[1] & yearID <= years[2])
  # aggregate and count games
  positions <- filtered %>% dplyr::group_by(POS) %>% dplyr::summarise(totalG = sum(G))
  # get the table for outfield positions with similar aggregation
  filteredOF <- Lahman::FieldingOFsplit %>% dplyr::filter(playerID == pID, yearID >= years[1] & yearID <= years[2])
  positionsOF <- filteredOF %>% dplyr::group_by(POS) %>% dplyr::summarise(totalG = sum(G))
  
  # combine these two tables, removing the generic OF indicator
  posTable <- dplyr::union(positions, positionsOF) %>% dplyr::filter(POS != "OF") %>% dplyr::arrange(-totalG)
  
  # handle errors
  if (nrow(posTable) == 0)
    message("No results; ensure you have the right playerID (use getPlayerID()) and what years he played.")
  
  # if not list, return the position with the highest total games
  if (!list)
    return(posTable$POS[1])
  # otherwise return the whole table
  return(posTable)
}

getPlayerID <- function(name) {
  first_last <- unlist(strsplit(name, " "))
  
  # guard against bad formatting
  if (length(first_last) < 2)
    return(NA_character_)
  
  first <- stringr::str_to_lower(first_last[1])
  last  <- stringr::str_to_lower(first_last[2])
  
  matches <- Lahman::People[
    stringr::str_to_lower(Lahman::People$nameFirst) == first &
      stringr::str_to_lower(Lahman::People$nameLast)  == last,
    "playerID"
  ]
  
  # No match
  if (length(matches) == 0)
    return(NA_character_)
  
  # Multiple matches → choose first deterministically
  return(matches[1])
}

primary_pos <- Lahman::Fielding %>%
  group_by(playerID, POS) %>%
  summarise(totalG = sum(G), .groups = "drop") %>%
  arrange(playerID, desc(totalG)) %>%
  distinct(playerID, .keep_all = TRUE) %>%
  select(playerID, POS)

top150pos <- top150 %>%
  mutate(playerID = map_chr(PlayerName, getPlayerID)) %>%
  left_join(primary_pos, by = "playerID")


# manually adding nas
extras <- top150pos %>% 
  filter(is.na(POS)) %>% 
  arrange(-OPS)
names <- c(extras$PlayerName, "Shohei Ohtani", "Bobby Witt Jr.")
truepos <- c("C", "OF", "1B", "3B", "OF", "1B", "1B", "3B", "SS", "OF", "OF", "OF", "SS", "OF", "SS", "OF", "SS", "SS", "1B", "1B", "OF", "OF", "SS")
napos <- data.frame(PlayerName = names, POS = truepos)
top150pos <- rows_update(top150pos, napos, by = "PlayerName")

# median OPS of positions
ggplot(data = top150pos) + 
  geom_bar(aes(reorder(POS, OPS, FUN = median), OPS), stat = "summary", fun = "median") +
  coord_cartesian(ylim = c(0.7, 0.85))
# suggests order to prioritize: median 1B is far better than median C, etc
# 1b, 3b, ss, of, c, 2b

# plots for each position's OPS distribution
plot_data <- top150pos %>%
  filter(POS %in% c("C","1B","2B","3B","SS","OF")) %>%
  group_by(POS) %>%
  arrange(desc(OPS), .by_group = TRUE) %>%
  mutate(rank_pos = row_number()) %>%
  ungroup()
ggplot(plot_data, aes(x = rank_pos, y = OPS)) +
  geom_point() +
  facet_wrap(~ POS, ncol = 3)
# suggests a point for each position past which there's little point to rush
# 1b: top 6, top 10 surely
# 2b: really just 1, soft top 6 maybe
# 3b: top 8, surely top 10
# c: top 3, hopefully top 5
# of: top 2 really, top 5, cant go wrong top 11
# ss: top 8, maybe 11

b1 <- top150pos %>% 
  filter(POS == "1B") %>% 
  arrange(-OPS)
b1 %>% select(PlayerName, Age, OPS, wRCpp, dev, floor)

b3 <- top150pos %>% 
  filter(POS == "3B") %>% 
  arrange(-OPS)
b3 %>% select(PlayerName, Age, OPS, wRCpp, dev, floor)

c <- top150pos %>% 
  filter(POS == "C") %>% 
  arrange(-OPS)
c %>% select(PlayerName, Age, OPS, wRCpp, dev, floor)

b2 <- top150pos %>% 
  filter(POS == "2B") %>% 
  arrange(-OPS)
b2 %>% select(PlayerName, Age, OPS, wRCpp, dev, floor)

ss <- top150pos %>% 
  filter(POS == "SS") %>% 
  arrange(-OPS)
ss %>% select(PlayerName, Age, OPS, wRCpp, dev, floor)

of <- top150pos %>% 
  filter(POS == "OF") %>% 
  arrange(-OPS)
of %>% select(PlayerName, Age, OPS, wRCpp, dev, floor)