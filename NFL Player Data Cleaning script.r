
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)

## Function to filter out player who played before 1980 and players who no single year data (aka only have one row of data) from the dataset, also filters to only focus on career stats (Year == 'TOTAL'). 
allstatsfilter <- function(playersdataframe){
  # replace total with 9999 in Year column
  playersdataframe$Year[playersdataframe$Year == "TOTAL"] <- 9999

  # Convert Year to integer since all whole numbers
  playersdataframe$Year <- as.integer(playersdataframe$Year)


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
  playersdataframe <- subset(playersdataframe, Year == 9999)

  playersdataframe$Year[playersdataframe$Year == 9999] <- "Total Career"

  return(playersdataframe)
}
## function to fix the data for the defensive stats
process_defensive_data <- function(PlayerDefensivedataframe) {

  PlayerDefensivedata <- allstatsfilter(PlayerDefensivedataframe)
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
    def_TDs = rep(0, nrow(PlayerDefensivedata)),
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
process_fumbles_data <- function(Playerfumblesdataframe) {

  Playerfumblesdata <- allstatsfilter(Playerfumblesdataframe)

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

  PlayerFumblesFixed$Fumbles_TD <- as.integer(Playerfumblesdata$TDs) 

  return(PlayerFumblesFixed)
}

process_basicStats_data <- function(PlayerbasicStatsdata){
  # Replace missing values in Height and Weight columns
  PlayerbasicStatsdata <- PlayerbasicStatsdata %>%
    mutate(Height = ifelse(Height == "--", "0-0", Height),
           Weight = ifelse(Weight %in% c("0", "--", "-1"), 0, Weight))

  class(PlayerbasicStatsdata$Height)
  class(PlayerbasicStatsdata$Weight)
  # Split the string on "-", convert to numeric, multiply and add
  PlayerbasicStatsdata$Height <- sapply(strsplit(as.character(PlayerbasicStatsdata$Height), "-"), function(x) {
    as.numeric(x[1]) * 12 + as.numeric(x[2])
  })
  
  # Convert Weight to numeric
  PlayerbasicStatsdata$Weight <- as.numeric(PlayerbasicStatsdata$Weight)

   # Check if the Hall_Of_Fame column exists, and add it if it doesn't
  if (!"Hall_Of_Fame" %in% names(PlayerbasicStatsdata)) {
    PlayerbasicStatsdata <- PlayerbasicStatsdata %>%
      mutate(Hall_Of_Fame = "False")
  }

  # Only include the specified columns in the returned dataframe
  PlayerbasicStatsdata <- PlayerbasicStatsdata %>%
    select(Player_Id, Position, Height, Weight, College, Hall_Of_Fame)

  # Change the classes of the specified columns
  PlayerbasicstatFixed <- PlayerbasicStatsdata %>%
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
    Sacked_per_game = rep(0, nrow(PlayerPassingdata)),
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

  PlayerPassingFixed$Sacked_per_game <- with(PlayerPassingdata, (as.numeric(PlayerPassingdata$Passes_Over_Forty_Yards) / as.numeric(PlayerPassingdata$Games_Played))) # nolint

  PlayerPassingFixed$Passer_Rating <- as.numeric(PlayerPassingdata$Sack_Yards) 

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
  #print(unique_ids)
  #print(length(unique_ids))
  # Iterate over the unique Player_Id
  merged_data <- map_dfr(unique_ids, function(id) {
    retired_data <- retiredPlayerData %>% filter(Player_Id == id)
    active_data <- activePlayerData %>% filter(Player_Id == id)

    if(nrow(retired_data) > 0 && nrow(active_data) > 0) {
      if(!all(retired_data == active_data)) {
        if(!all(retired_data$Position == active_data$Position)) {
          print(paste("Different Position values for Player_Id", id))
          correct_position <- readline(prompt="Enter the correct Position: ")
          retired_data$Position <- correct_position
          active_data$Position <- correct_position
        } else {
          stop(paste("Error: Different values for Player_Id", id))
        }
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
Check_Values_in_basic_dataframe <- function(dataframe) {
# Iterate over the rows of filtered_BasicStatsPlayerData
  for(i in 1:nrow(dataframe)) {
    # If Height or Weight is 0, ask for user input
    while(dataframe$Height[i] == 0 || dataframe$Weight[i] == 0) {
      print(paste("Incorrect Height or Weight for Player_Id", dataframe$Player_Id[i]))
      correct_height <- as.numeric(readline(prompt="Enter the correct Height: "))
      correct_weight <- as.numeric(readline(prompt="Enter the correct Weight: "))
      dataframe$Height[i] <- correct_height
      dataframe$Weight[i] <- correct_weight
    }
  }
  return(dataframe)
}

merge_dataframes <- function(positional_dataframes) {
  # Use Reduce to iteratively merge all dataframes
  merged_data_positional <- Reduce(function(x, y) merge(x, y, by = c("Player_Id", "Games_Played"), all = TRUE), positional_dataframes)

  # Replace NAs with 0
  merged_data_positional[is.na(merged_data_positional)] <- 0

  return(merged_data_positional)
}

merge_with_basicStats <- function(basicStatsData, otherdataframe) {
  # Merge the basic stats dataframe with the other dataframe
  merged_data <- merge(basicStatsData, otherdataframe, by = c("Player_Id"), all = TRUE)

  return(merged_data)
}

write_df_to_csv <- function(dataframe){
  dataframe_name <- deparse(substitute(dataframe))
  file_path1 <- file.path("E:\\R_Code\\NFL Data\\FixedDataCSVfiles\\", paste0(dataframe_name, ".csv"))
  file_path2 <- file.path("E:\\GitHub Rep\\Retired-NFL-Analysis\\Retired-NFL-Analysis\\", paste0(dataframe_name, ".csv"))

  write.csv(dataframe, file = file_path1, row.names = FALSE)
  write.csv(dataframe, file = file_path2, row.names = FALSE)
}

# Data Load
## Retired Player Data
retiredPlayerDefensivedataframe <- read.csv("E:\\R_Code\\NFL Data\\RetiredPlayer_Defense_Stats.csv")
colnames(retiredPlayerDefensivedataframe)
retiredFumblesdataframe <- read.csv("E:\\R_Code\\NFL Data\\RetiredPlayer_Fumbles_Stats.csv")
colnames(retiredFumblesdataframe)
retiredPassingdataframe <- read.csv("E:\\R_Code\\NFL Data\\RetiredPlayer_Passing_Stats.csv")
colnames(retiredPassingdataframe)
retiredReceivingdataframe <- read.csv("E:\\R_Code\\NFL Data\\RetiredPlayer_Receiving_Stats.csv")
colnames(retiredReceivingdataframe)
retiredRushingdataframe <- read.csv("E:\\R_Code\\NFL Data\\RetiredPlayer_Rushing_Stats.csv")
colnames(retiredRushingdataframe)
retiredbasicstat <- read.csv("E:\\R_Code\\NFL Data\\Retired_Player_Basic_Stats (1).csv") 
colnames(retiredbasicstat)

## Active Player Data
activedefensivedataframe <- read.csv("E:\\R_Code\\NFL Data\\ActivePlayer_Defense_Stats.csv")
colnames(activedefensivedataframe)
activeFumblesdataframe <- read.csv("E:\\R_Code\\NFL Data\\ActivePlayer_Fumbles_Stats.csv")
colnames(activeFumblesdataframe)
activePassingdataframe <- read.csv("E:\\R_Code\\NFL Data\\ActivePlayer_Passing_Stats.csv")
colnames(activePassingdataframe)
activeReceivingdataframe <- read.csv("E:\\R_Code\\NFL Data\\ActivePlayer_Receiving_Stats.csv")  
colnames(activeReceivingdataframe)
activeRushingdataframe <- read.csv("E:\\R_Code\\NFL Data\\ActivePlayer_Rushing_Stats.csv")
colnames(activeRushingdataframe)
activebasicstat <- read.csv("E:\\R_Code\\NFL Data\\Active_Player_Basic_Stats.csv") 
colnames(activebasicstat)

# Data Cleaning
### Function Call

#Defensive Data
retiredPlayerDefensivedataFixed <- process_defensive_data(PlayerDefensivedataframe = retiredPlayerDefensivedataframe)
activePlayerDefensivedataFixed <- process_defensive_data(PlayerDefensivedataframe = activedefensivedataframe)

totaldefRows <- nrow(retiredPlayerDefensivedataFixed) + nrow(activePlayerDefensivedataFixed)
ids_in_both_def <- intersect(retiredPlayerDefensivedataFixed$Player_Id, activePlayerDefensivedataFixed$Player_Id)


#Fumbles Data
retiredFumblesPlayerFixed <- process_fumbles_data(Playerfumblesdata = retiredFumblesdataframe)
activeFumblesPlayerFixed <- process_fumbles_data(Playerfumblesdata = activeFumblesdataframe)

totalfumblesRows <- nrow(retiredFumblesPlayerFixed) + nrow(activeFumblesPlayerFixed)
ids_in_both_fumbles <- intersect(retiredFumblesPlayerFixed$Player_Id, activeFumblesPlayerFixed$Player_Id)

#Passing Data
retiredPassingdataFixed <- process_passing_data(PlayerPassingdata = retiredPassingdataframe)
activePassingdataFixed <- process_passing_data(PlayerPassingdata = activePassingdataframe)

totalpassingRows <- nrow(retiredPassingdataFixed) + nrow(activePassingdataFixed)
ids_in_both_passing <- intersect(retiredPassingdataFixed$Player_Id, activePassingdataFixed$Player_Id)

#Receiving Data
retiredReceivingdataFixed <- process_receiving_data(PlayerReceivingdata = retiredReceivingdataframe)
activeReceivingdataFixed <- process_receiving_data(PlayerReceivingdata = activeReceivingdataframe)

totalreceivingRows <- nrow(retiredReceivingdataFixed) + nrow(activeReceivingdataFixed)
ids_in_both_receiving <- intersect(retiredReceivingdataFixed$Player_Id, activeReceivingdataFixed$Player_Id)

#Rushing Data
retiredRushingdataFixed <- process_rushing_data(PlayerRushingdata = retiredRushingdataframe)
activeRushingdataFixed <- process_rushing_data(PlayerRushingdata = activeRushingdataframe)

totalrushingRows <- nrow(retiredRushingdataFixed) + nrow(activeRushingdataFixed)
ids_in_both_rushing <- intersect(retiredRushingdataFixed$Player_Id, activeRushingdataFixed$Player_Id)

#Basic Stats Data
retiredbasicstatFixed <- process_basicStats_data(PlayerbasicStatsdata = retiredbasicstat)
activebasicstatFixed <- process_basicStats_data(PlayerbasicStatsdata = activebasicstat)
activebasicstatFixed$Hall_Of_Fame <- rep("False", nrow(activebasicstatFixed)) 

totalbasicstatRows <- nrow(retiredbasicstatFixed) + nrow(activebasicstatFixed)
ids_in_both_basicstat <- intersect(retiredbasicstatFixed$Player_Id, activebasicstatFixed$Player_Id)


## Data merge
### NOTE: There are some players who might be missing height, weight, or position. This will need to be entered manually for those players what are included in the other dataframes.


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


# unique_ids_in_position_data <- unique(c(DefensivePlayerData$Player_Id, FumblesPlayerData$Player_Id, PassingPlayerData$Player_Id, ReceivingPlayerData$Player_Id, RushingPlayerData$Player_Id))



######### combine all positional dataframes into one dataframe to be able to merge into basics stats dataframe.
# List of dataframes
position_dataframes <- list(DefensivePlayerData, FumblesPlayerData, PassingPlayerData, ReceivingPlayerData, RushingPlayerData)

merged_positional_data <- merge_dataframes(positional_dataframes = position_dataframes)
#View(merged_postional_data)

merged_ids_not_in_basicstats <- setdiff(merged_positional_data$Player_Id, BasicStatsPlayerData$Player_Id)
print(length(merged_ids_not_in_basicstats)) #0 so all players in positional dataframes are in basic stats dataframe

# Filter BasicStatsPlayerData
filtered_BasicStatsPlayerData <- BasicStatsPlayerData %>%
  filter(Player_Id %in% merged_positional_data$Player_Id)

filtered_BasicStatsPlayerData <- Check_Values_in_basic_dataframe(filtered_BasicStatsPlayerData)

# Merge BasicStatsPlayerData with merged_positional_data
merged_data_all <- merge_with_basicStats(basicStatsData = filtered_BasicStatsPlayerData, otherdataframe = merged_positional_data)
View(merged_data_all)

write_df_to_csv(merged_data_all)

##########################################################################
### The data has been cleaned and merged into one dataframe

##########################################################################
## merge all retired players into one dataframe

retiredPlayersData_list <- list(retiredPlayerDefensivedataFixed, retiredFumblesPlayerFixed, retiredPassingdataFixed, retiredReceivingdataFixed, retiredRushingdataFixed)

retiredPlayersData <- merge_dataframes(positional_dataframes = retiredPlayersData_list)

#check if player_ids from retired not in basic stats
retired_ids_not_in_basicstats <- setdiff(retiredPlayersData$Player_Id, BasicStatsPlayerData$Player_Id)
print(length(retired_ids_not_in_basicstats)) #0 so all players in retired dataframes are in basic stats dataframe

# Filter basic stats to the retired players using massive merged basic stats dataframe for completeness

filtered_retiredBasicStatsPlayerData <- BasicStatsPlayerData %>%
  filter(Player_Id %in% retiredPlayersData$Player_Id)

filtered_filled_retiredBasicStatsPlayerData <- Check_Values_in_basic_dataframe(filtered_retiredBasicStatsPlayerData)

merged_retired_data_all <- merge_with_basicStats(basicStatsData = filtered_filled_retiredBasicStatsPlayerData, otherdataframe = retiredPlayersData)
View(merged_retired_data_all)
write_df_to_csv(merged_retired_data_all)
######################################################################
## merge all active players into one dataframe

activePlayersData_list <- list(activePlayerDefensivedataFixed, activeFumblesPlayerFixed, activePassingdataFixed, activeReceivingdataFixed, activeRushingdataFixed)

activePlayersData <- merge_dataframes(positional_dataframes = activePlayersData_list)

#check if player_ids from active not in basic stats
active_ids_not_in_basicstats <- setdiff(activePlayersData$Player_Id, BasicStatsPlayerData$Player_Id)
print(length(active_ids_not_in_basicstats)) #0 so all players in active dataframes are in basic stats dataframe

# Filter basic stats to the active players using massive merged basic stats dataframe for completeness
filter_activeBasicStatsPlayerData <- BasicStatsPlayerData %>%
  filter(Player_Id %in% activePlayersData$Player_Id)

filtered_filled_activeBasicStatsPlayerData <- Check_Values_in_basic_dataframe(filter_activeBasicStatsPlayerData)

merged_active_data_all <- merge_with_basicStats(basicStatsData = filtered_filled_activeBasicStatsPlayerData, otherdataframe = activePlayersData)

View(merged_active_data_all)
write_df_to_csv(merged_active_data_all)


# kicking data seems to have been scraped incorrectly. Many players have 0s for all stats execpt for FG and FG_Attempted or all empty when they should not be empty. 
  # Need to remove players who have all 0s or all empty stats. Ex: jamal-anderson..
  # Most of these players are most likely not kickers but have another main position and have kicking stats from a few plays in a few games in their careers. 
  # Maybe not look at special teams players for this analysis?
  # I would need to compare against basic stat data to see if these players are kickers or not. 
    # if ID from kicking is not a K in basic stats then remove from kicking data.
  # however even with this some of the kicker data is not accurate even for the kicker position players.
  # Exclude for now but add later on my own time for experimentation beyond the scope of this project analysis. 












