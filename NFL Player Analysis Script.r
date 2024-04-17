
library(dplyr)
library(purrr)

## Function to filter out player who played before 1980 and players who no single year data (aka only have one row of data) from the dataset, also filters to only focus on career stats (Year == 'TOTAL'). 
allstatsfilter <- function(playersdataframe){
  # Identify player_ids with any year less than 1980
  players_to_remove <- playersdataframe %>%
    filter(Year < 1980) %>%
    pull(Player_Id) %>%
    unique()

  # Remove these player_ids from the dataset
  playersdataframe <- playersdataframe %>%
    filter(!Player_Id %in% players_to_remove)

  # Find players with more atleast one year of play so atleast two rows of data, a year and total career. Remove those who only have one row of data.
  # Get unique player_ids
  player_ids <- unique(playersdataframe$Player_Id)
  # Count the number of rows for each player_id
  player_counts_ids <- table(playersdataframe$Player_Id)
  # Get the player_ids of players with more than one row  
  multi_row_players <- names(player_counts_ids[player_counts_ids > 1])
  # Filter to include only these players
  playersdataframe <- subset(playersdataframe, Player_Id %in% multi_row_players)

  # Subset to only Career Stats
  playersdataframe <- subset(playersdataframe, Year == 'TOTAL')

  return(playersdataframe)
}
## function to fix the data for the defensive stats
process_defensive_data <- function(PlayerDefensivedata) {

  PlayerDefensivedata <- allstatsfilter(PlayerDefensivedata)
# Fixing columns and making new empty but correct dimension data frame

  PlayerDefensivedataFixed <- data.frame(
    Player_Id = rep("", nrow(PlayerDefensivedata)),
    Games_Played = rep(0, nrow(PlayerDefensivedata)),
    Solo_Tackles = rep(0, nrow(PlayerDefensivedata)),
    Assisted_Tackles = rep(0, nrow(PlayerDefensivedata)),
    Sacks = rep(0, nrow(PlayerDefensivedata)),
    Safeties = rep(0, nrow(PlayerDefensivedata)),
    Passes_Defended = rep(0, nrow(PlayerDefensivedata)),
    Interceptions = rep(0, nrow(PlayerDefensivedata)),
    TDs = rep(0, nrow(PlayerDefensivedata)),
    INT_Yards = rep(0, nrow(PlayerDefensivedata)),
    AverageYrdperINT = rep(0, nrow(PlayerDefensivedata)),
    Tot_CareerHighINTReturn = rep(0, nrow(PlayerDefensivedata)),
    stringsAsFactors = FALSE
  )

  ## Filling in new Dataframe
  # Assign Player_Id column
  PlayerDefensivedataFixed$Player_Id <- as.character(PlayerDefensivedata$Player_Id)
  ## Year not included since it is already filtered to only include 'TOTAL'

  #removed Team since not needed
  PlayerDefensivedataFixed$Games_Played <- as.integer(PlayerDefensivedata$Games_Played)

  ## Removed Total tackles as it is included within Solo Tackles and Assisted Tackles. 

  PlayerDefensivedataFixed$Solo_Tackles <- as.integer(PlayerDefensivedata$Solo_Tackles)

  PlayerDefensivedataFixed$Assisted_Tackles <- as.integer(PlayerDefensivedata$Assisted_Tackles)

  PlayerDefensivedataFixed$Sacks <- as.integer(PlayerDefensivedata$Sacks)   

  PlayerDefensivedataFixed$Safeties <- as.integer(PlayerDefensivedata$Sack_Yards)

  PlayerDefensivedataFixed$Passes_Defended <- as.integer(PlayerDefensivedata$Safties)

  PlayerDefensivedataFixed$Interceptions <- as.integer(PlayerDefensivedata$Passes_Deflected)

  PlayerDefensivedataFixed$def_TDs <- as.integer(PlayerDefensivedata$INTs)

  PlayerDefensivedataFixed$INT_Yards <- as.integer(PlayerDefensivedata$TDs)

  PlayerDefensivedataFixed$AverageYrdperINT <- as.integer(PlayerDefensivedata$INT_Yards)

  PlayerDefensivedataFixed$Tot_CareerHighINTReturn <- as.integer(PlayerDefensivedata$Average)

  return(PlayerDefensivedataFixed)
}
## function to fix the data for the fumbles stats
process_fumbles_data <- function(Playerfumblesdata) {

  Playerfumblesdata <- allstatsfilter(Playerfumblesdata)

#This data is accurate the way it is, just need to remove the team column and year column since it is already filtered to only include 'TOTAL'

PlayerFumblesFixed <- data.frame(
  Player_Id = rep("", nrow(Playerfumblesdata)),
  Games_Played = rep(0, nrow(Playerfumblesdata)),
  Fumbles = rep(0, nrow(Playerfumblesdata)),
  Fumbles_Lost = rep(0, nrow(Playerfumblesdata)),
  Forced_Fumbles = rep(0, nrow(Playerfumblesdata)),
  Own_Recovery = rep(0, nrow(Playerfumblesdata)),
  Opposing_Recovery = rep(0, nrow(Playerfumblesdata)),
  Fumbles_TD = rep(0, nrow(Playerfumblesdata)),
  stringsAsFactors = FALSE
)
## Filling in new Dataframe
  PlayerFumblesFixed$Player_Id <- as.character(Playerfumblesdata$Player_Id)

  PlayerFumblesFixed$Games_Played <- as.integer(Playerfumblesdata$Games_Played)

  PlayerFumblesFixed$Fumbles <- as.integer(Playerfumblesdata$Fumbles)

  PlayerFumblesFixed$Fumbles_Lost <- as.integer(Playerfumblesdata$Fumbles_Lost)

  PlayerFumblesFixed$Forced_Fumbles <- as.integer(Playerfumblesdata$Forced_Fumbles)

  PlayerFumblesFixed$Own_Recovery <- as.integer(Playerfumblesdata$Own_Recovery)

  PlayerFumblesFixed$Opposing_Recovery <- as.integer(Playerfumblesdata$Opposing_Recovery)

  PlayerFumblesFixed$Fumbles_TD <- as.integer(Playerfumblesdata$TDs) # nolint

  return(PlayerFumblesFixed)
}

process_basicStats_data <- function(PlayerbasicStatsdata){
  PlayerbasicstatFixed <- subset(PlayerbasicStatsdata, !apply(PlayerbasicStatsdata == "--", 1, any))

  # Split the string on "-", convert to numeric, multiply and add
  PlayerbasicstatFixed$Height <- sapply(strsplit(PlayerbasicstatFixed$Height, "-"), function(x) {
    as.numeric(x[1]) * 12 + as.numeric(x[2])
  })

   # Check if the Hall_Of_Fame column exists, and add it if it doesn't
  if (!"Hall_Of_Fame" %in% names(PlayerbasicstatFixed)) {
    PlayerbasicstatFixed <- PlayerbasicstatFixed %>%
      mutate(Hall_Of_Fame = "Not Eligible")
  }

  # Only include the specified columns in the returned dataframe
  PlayerbasicstatFixed <- PlayerbasicstatFixed %>%
    select(Player_Id, Position, Height, Weight, College, Hall_Of_Fame)

  # Change the classes of the specified columns
  PlayerbasicstatFixed <- PlayerbasicstatFixed %>%
    mutate_at(vars(Player_Id, College), as.character) %>%
    mutate_at(vars(Position, Hall_Of_Fame), as.factor) %>%
    mutate_at(vars(Height, Weight), as.integer)

  return(PlayerbasicstatFixed)
}

process_passing_data <- function(PlayerPassingdata){
  PlayerPassingdata <- allstatsfilter(PlayerPassingdata)

  PlayerPassingFixed <- data.frame(
    Player_Id = rep("", nrow(PlayerPassingdata)),
    Games_Played = rep(0, nrow(PlayerPassingdata)),
    Passing_Attempts = rep(0, nrow(PlayerPassingdata)),
    Passing_Completions = rep(0, nrow(PlayerPassingdata)),
    Completion_Percentage = rep(0, nrow(PlayerPassingdata)),
    Passing_Yards_Gained = rep(0, nrow(PlayerPassingdata)),
    Yrds_gained_per_Attempt = rep(0, nrow(PlayerPassingdata)),
    Sum_cr_long_passes = rep(0, nrow(PlayerPassingdata)),
    Passing_TDs = rep(0, nrow(PlayerPassingdata)),
    Interceptions_thrown = rep(0, nrow(PlayerPassingdata)),
    First_downs_thrown = rep(0, nrow(PlayerPassingdata)),
    First_downs_per_attempt_percentage = rep(0, nrow(PlayerPassingdata)),
    Been_Sacked = rep(0, nrow(PlayerPassingdata)),
    Sack_Yards = rep(0, nrow(PlayerPassingdata)),
    Sack_Percentage = rep(0, nrow(PlayerPassingdata)),
    Passer_Rating = rep(0, nrow(PlayerPassingdata)),
    stringsAsFactors = FALSE
  )

  PlayerPassingFixed$Player_Id <- as.character(PlayerPassingdata$Player_Id)

  PlayerPassingFixed$Games_Played <- as.integer(PlayerPassingdata$Games_Played)

  PlayerPassingFixed$Passing_Attempts <- as.integer(PlayerPassingdata$Attempts)

  PlayerPassingFixed$Passing_Completions <- as.integer(PlayerPassingdata$Completions)

  PlayerPassingFixed$Completion_Percentage <- as.numeric(PlayerPassingdata$Completion_Percentage)

  PlayerPassingFixed$Passing_Yards_Gained <- as.integer(PlayerPassingdata$Yards)

  PlayerPassingFixed$Yrds_gained_per_Attempt <- as.numeric(PlayerPassingdata$Average)

  PlayerPassingFixed$Sum_cr_long_passes <- as.integer(PlayerPassingdata$Long)

  PlayerPassingFixed$Passing_TDs <- as.integer(PlayerPassingdata$TDs)

  PlayerPassingFixed$Interceptions_thrown <- as.integer(PlayerPassingdata$INTs)

  PlayerPassingFixed$First_downs_thrown <- as.integer(PlayerPassingdata$First_Downs)

  PlayerPassingFixed$First_downs_per_attempt_percentage <- as.numeric(PlayerPassingdata$First_Down_Percentage)

  PlayerPassingFixed$Been_Sacked <- as.integer(PlayerPassingdata$Passes_Over_Forty_Yards)

  PlayerPassingFixed$Sack_Yards <- as.integer(PlayerPassingdata$Sacks)

  PlayerPassingFixed$Sack_Percentage <- with(PlayerPassingdata, (as.numeric(PlayerPassingdata$Passes_Over_Forty_Yards) / (as.numeric(PlayerPassingdata$Attempts) + as.numeric(PlayerPassingdata$Passes_Over_Forty_Yards)))*100) # nolint

  PlayerPassingFixed$Passer_Rating <- as.numeric(PlayerPassingdata$Sack_Yards) # nolint


  return(PlayerPassingFixed)
}

process_receiving_data <- function(PlayerReceivingdata){
  PlayerReceivingdata <- allstatsfilter(PlayerReceivingdata)

  PlayerReceivingFixed <- data.frame(
    Player_Id = rep("", nrow(PlayerReceivingdata)),
    Games_Played = rep(0, nrow(PlayerReceivingdata)),
    Receptions = rep(0, nrow(PlayerReceivingdata)),
    Receiving_Yards = rep(0, nrow(PlayerReceivingdata)),
    Yards_per_Reception = rep(0, nrow(PlayerReceivingdata)),
    Sum_cr_long_receptions = rep(0, nrow(PlayerReceivingdata)),
    Receiving_TDs = rep(0, nrow(PlayerReceivingdata)),
    Receptions_per_Game = rep(0, nrow(PlayerReceivingdata)),
    Receiving_Yards_per_Game = rep(0, nrow(PlayerReceivingdata)),
    First_down_receptions = rep(0, nrow(PlayerReceivingdata)),
    First_down_receptions_percentage = rep(0, nrow(PlayerReceivingdata)),
    TDs_per_Reception = rep(0, nrow(PlayerReceivingdata)),
    stringsAsFactors = FALSE
  )

  PlayerReceivingFixed$Player_Id <- as.character(PlayerReceivingdata$Player_Id)

  PlayerReceivingFixed$Games_Played <- as.integer(PlayerReceivingdata$Games_Played)

  PlayerReceivingFixed$Receptions <- as.integer(PlayerReceivingdata$Receptions)

  PlayerReceivingFixed$Receiving_Yards <- as.integer(PlayerReceivingdata$Yards)

  PlayerReceivingFixed$Yards_per_Reception <- as.numeric(PlayerReceivingdata$Average)

  PlayerReceivingFixed$Sum_cr_long_receptions <- as.integer(PlayerReceivingdata$Long)

  PlayerReceivingFixed$Receiving_TDs <- as.integer(PlayerReceivingdata$TDs)

  PlayerReceivingFixed$Receptions_per_Game <-as.numeric(with(PlayerReceivingdata, as.numeric(PlayerReceivingdata$Receptions) / as.numeric(PlayerReceivingdata$Games_Played)))

  PlayerReceivingFixed$Receiving_Yards_per_Game <- as.numeric(with(PlayerReceivingdata, as.numeric(PlayerReceivingdata$Yards) / as.numeric(PlayerReceivingdata$Games_Played)))

  PlayerReceivingFixed$First_down_receptions <- as.integer(PlayerReceivingdata$First_Downs)

  PlayerReceivingFixed$First_down_receptions_percentage <- as.numeric(PlayerReceivingdata$First_Down_Percentage)

  PlayerReceivingFixed$TDs_per_Reception <- as.numeric(with(PlayerReceivingdata, as.numeric(PlayerReceivingdata$TDs) / as.numeric(PlayerReceivingdata$Receptions)))

  return(PlayerReceivingFixed)
}

process_rushing_data <- function(PlayerRushingdata){
  PlayerRushingdata <- allstatsfilter(PlayerRushingdata)

  PlayerRushingFixed <- data.frame(
    Player_Id = rep("", nrow(PlayerRushingdata)),
    Games_Played = rep(0, nrow(PlayerRushingdata)),
    Rushing_Attempts = rep(0, nrow(PlayerRushingdata)),
    Rushing_Yards = rep(0, nrow(PlayerRushingdata)),
    Rushing_Yards_per_Attempt = rep(0, nrow(PlayerRushingdata)),
    Sum_cr_long_rush_yds = rep(0, nrow(PlayerRushingdata)),
    Rushing_TDs = rep(0, nrow(PlayerRushingdata)),
    Rushing_Yards_per_Game = rep(0, nrow(PlayerRushingdata)),
    Rushing_Attempts_per_Game = rep(0, nrow(PlayerRushingdata)),
    First_down_rushes = rep(0, nrow(PlayerRushingdata)),
    First_down_rushes_percentage = rep(0, nrow(PlayerRushingdata)),
    TDs_per_Rush_Attempt = rep(0, nrow(PlayerRushingdata)),
    Rush_TD_per_Game = rep(0, nrow(PlayerRushingdata)),
    stringsAsFactors = FALSE
  )

  PlayerRushingFixed$Player_Id <- as.character(PlayerRushingdata$Player_Id)

  PlayerRushingFixed$Games_Played <- as.integer(PlayerRushingdata$Games_Played)

  PlayerRushingFixed$Rushing_Attempts <- as.integer(PlayerRushingdata$Attempts)

  PlayerRushingFixed$Rushing_Yards <- as.integer(PlayerRushingdata$Yards)

  PlayerRushingFixed$Rushing_Yards_per_Attempt <- as.numeric(PlayerRushingdata$Average)

  PlayerRushingFixed$Sum_cr_long_rush_yds <- as.integer(PlayerRushingdata$Long)

  PlayerRushingFixed$Rushing_TDs <- as.integer(PlayerRushingdata$TDs)

  PlayerRushingFixed$Rushing_Yards_per_Game <- as.numeric(with(PlayerRushingdata, as.numeric(PlayerRushingdata$Yards) / as.numeric(PlayerRushingdata$Games_Played)))

  PlayerRushingFixed$Rushing_Attempts_per_Game <- as.numeric(with(PlayerRushingdata, as.numeric(PlayerRushingdata$Attempts) / as.numeric(PlayerRushingdata$Games_Played)))

  PlayerRushingFixed$First_down_rushes <- as.integer(PlayerRushingdata$First_Downs)

  PlayerRushingFixed$First_down_rushes_percentage <- as.numeric(PlayerRushingdata$First_Down_Percentage)

  PlayerRushingFixed$TDs_per_Rush_Attempt <- as.numeric(with(PlayerRushingdata, as.numeric(PlayerRushingdata$TDs) / as.numeric(PlayerRushingdata$Attempts)))

  PlayerRushingFixed$Rush_TD_per_Game <- as.numeric(with(PlayerRushingdata, as.numeric(PlayerRushingdata$TDs) / as.numeric(PlayerRushingdata$Games_Played)))

  return(PlayerRushingFixed)
}

merge_Active_Retired_Player_Data <- function(retiredPlayerData, activePlayerData) {
  unique_ids <- unique(c(retiredPlayerData$Player_Id, activePlayerData$Player_Id))

  # Iterate over the unique Player_Id
  merged_data <- map_dfr(unique_ids, function(id) {
    retired_data <- retiredPlayerData %>% filter(Player_Id == id)
    active_data <- activePlayerData %>% filter(Player_Id == id)

    if(nrow(retired_data) > 0 && nrow(active_data) > 0) {
      if(!all(retired_data == active_data)) {
        stop(paste("Error: Different values for Player_Id", id))
      }
      return(retired_data)
    } else if(nrow(retired_data) > 0) {
      return(retired_data)
    } else {
      return(active_data)
    }
  })

  return(merged_data)
}
# Data Load
## Retired Player Data
retiredFumblesdataframe <- read.csv("E:\\R_Code\\NFL Data\\RetiredPlayer_Fumbles_Stats.csv")
retiredPlayerDefensivedataframe <- read.csv("E:\\R_Code\\NFL Data\\RetiredPlayer_Defense_Stats.csv")
retiredbasicstat <- read.csv("E:\\R_Code\\NFL Data\\Retired_Player_Basic_Stats (1).csv") 
retiredPassingdataframe <- read.csv("E:\\R_Code\\NFL Data\\RetiredPlayer_Passing_Stats.csv")
retiredReceivingdataframe <- read.csv("E:\\R_Code\\NFL Data\\RetiredPlayer_Receiving_Stats.csv")
retiredRushingdataframe <- read.csv("E:\\R_Code\\NFL Data\\RetiredPlayer_Rushing_Stats.csv")

## Active Player Data
activedefensivedataframe <- read.csv("E:\\R_Code\\NFL Data\\ActivePlayer_Defense_Stats.csv")
activeFumblesdataframe <- read.csv("E:\\R_Code\\NFL Data\\ActivePlayer_Fumbles_Stats.csv")
activebasicstat <- read.csv("E:\\R_Code\\NFL Data\\Active_Player_Basic_Stats.csv")
activePassingdataframe <- read.csv("E:\\R_Code\\NFL Data\\ActivePlayer_Passing_Stats.csv")
activeReceivingdataframe <- read.csv("E:\\R_Code\\NFL Data\\ActivePlayer_Receiving_Stats.csv")
activeRushingdataframe <- read.csv("E:\\R_Code\\NFL Data\\ActivePlayer_Rushing_Stats.csv")

# Data Cleaning
### Function Call

#Defensive Data
retiredPlayerDefensivedataFixed <- process_defensive_data(PlayerDefensivedata = retiredPlayerDefensivedataframe)
activePlayerDefensivedataFixed <- process_defensive_data(PlayerDefensivedata = activedefensivedataframe)

#Fumbles Data
retiredFumblesPlayerFixed <- process_fumbles_data(Playerfumblesdata = retiredFumblesdataframe)
activeFumblesPlayerFixed <- process_fumbles_data(Playerfumblesdata = activeFumblesdataframe)

#Passing Data
retiredPassingdataFixed <- process_passing_data(PlayerPassingdata = retiredPassingdataframe)
activePassingdataFixed <- process_passing_data(PlayerPassingdata = activePassingdataframe)

#Receiving Data
retiredReceivingdataFixed <- process_receiving_data(PlayerReceivingdata = retiredReceivingdataframe)
activeReceivingdataFixed <- process_receiving_data(PlayerReceivingdata = activeReceivingdataframe)

#Rushing Data
retiredRushingdataFixed <- process_rushing_data(PlayerRushingdata = retiredRushingdataframe)
activeRushingdataFixed <- process_rushing_data(PlayerRushingdata = activeRushingdataframe)

#Basic Stats Data
retiredbasicstatFixed <- process_basicStats_data(PlayerbasicStatsdata = retiredbasicstat)
activebasicstatFixed <- process_basicStats_data(PlayerbasicStatsdata = activebasicstat)
activebasicstatFixed$Hall_Of_Fame <- rep("Not Eligible", nrow(activebasicstatFixed)) # nolint

## Data merge

# Defensive Player Merge
DefensivePlayerData <- merge_Active_Retired_Player_Data(retiredPlayerData = retiredPlayerDefensivedataFixed, activePlayerData = activePlayerDefensivedataFixed)

# Fumbles Player Merge
FumblesPlayerData <- merge_Active_Retired_Player_Data(retiredPlayerData = retiredFumblesPlayerFixed, activePlayerData = activeFumblesPlayerFixed)

# Passing Player Merge
PassingPlayerData <- merge_Active_Retired_Player_Data(retiredPlayerData = retiredPassingdataFixed, activePlayerData = activePassingdataFixed)

# Receiving Player Merge
ReceivingPlayerData <- merge_Active_Retired_Player_Data(retiredPlayerData = retiredReceivingdataFixed, activePlayerData = activeReceivingdataFixed)

# Rushing Player Merge
RushingPlayerData <- merge_Active_Retired_Player_Data(retiredPlayerData = retiredRushingdataFixed, activePlayerData = activeRushingdataFixed)

# Basic Stats Player Merge
BasicStatsPlayerData <- merge_Active_Retired_Player_Data(retiredPlayerData = retiredbasicstatFixed, activePlayerData = activebasicstatFixed)












# kicking data seems to have been scraped incorrectly. Many players have 0s for all stats execpt for FG and FG_Attempted or all empty when they should not be empty. 
  # Need to remove players who have all 0s or all empty stats. Ex: jamal-anderson..
  # Most of these players are most likely not kickers but have another main position and have kicking stats from a few plays in a few games in their careers. 
  # Maybe not look at special teams players for this analysis?
  # I would need to compare against basic stat data to see if these players are kickers or not. 
    # if ID from kicking is not a K in basic stats then remove from kicking data.
  # however even with this some of the kicker data is not accurate even for the kicker position players.
  # Exclude for now but add later on my own time for experimentation beyond the scope of this project analysis. 
