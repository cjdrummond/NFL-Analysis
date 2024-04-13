retiredDefensivedata <- read.csv("E:\\R_Code\\NFL Data\\RetiredPlayer_Defense_Stats.csv")
attach(data)


retiredbasicstat <- read.csv("E:\\R_Code\\NFL Data\\Retired_Player_Basic_Stats (1).csv")

retiredbasicstat <- subset(retiredbasicstat, !apply(retiredbasicstat == "--", 1, any))
View(retiredbasicstat)

# Filter to include only players who have played after 1980
retiredDefensivedata <- subset(retiredDefensivedata, Year >= 1980)

# Get unique player_ids
player_ids_def <- unique(retiredDefensivedata$Player_Id)


# Count the number of rows for each player_id
def_player_counts <- table(retiredDefensivedata$Player_Id)

# Get the player_ids of players with more than one row
multi_row_def_players <- names(def_player_counts[def_player_counts > 1])

# Filter data2 to include only these players
retiredDefensivedata <- subset(retiredDefensivedata, Player_Id %in% multi_row_def_players)

# View the data
View(retiredDefensivedata)

# Subset to only Career Stats
retiredDefensivedata <- subset(retiredDefensivedata, Year == 'TOTAL')

# View the data
View(retiredDefensivedata)


# Fixing columns 

RetiredDefPlayerFixed <- data.frame(
  Player_Id = rep("", nrow(retiredDefensivedata)),
  Games_Played = rep(0, nrow(retiredDefensivedata)),
  Solo_Tackles = rep(0, nrow(retiredDefensivedata)),
  Assisted_Tackles = rep(0, nrow(retiredDefensivedata)),
  Sacks = rep(0, nrow(retiredDefensivedata)),
  Safeties = rep(0, nrow(retiredDefensivedata)),
  Passes_Defended = rep(0, nrow(retiredDefensivedata)),
  Interceptions = rep(0, nrow(retiredDefensivedata)),
  TDs = rep(0, nrow(retiredDefensivedata)),
  INT_Yards = rep(0, nrow(retiredDefensivedata)),
  AverageYrdperINT = rep(0, nrow(retiredDefensivedata)),
  Tot_CareerHighINTReturn = rep(0, nrow(retiredDefensivedata)),
  stringsAsFactors = FALSE
)



str(RetiredDefPlayerFixed)

# Assign Player_Id column
RetiredDefPlayerFixed$Player_Id <- as.character(retiredDefensivedata$Player_Id)
RetiredDefPlayerFixed$Player_Id <- as.character(retiredDefensivedata$Player_Id)
## Year not included since it is already filtered to only include 'TOTAL'

#removed Team since not needed
RetiredDefPlayerFixed$Games_Played <- as.integer(retiredDefensivedata$Games_Played)

## Removed Total tackles as it is included within Solo Tackles and Assisted Tackles. 

RetiredDefPlayerFixed$Solo_Tackles <- as.integer(retiredDefensivedata$Solo_Tackles)

RetiredDefPlayerFixed$Assisted_Tackles <- as.integer(retiredDefensivedata$Assisted_Tackles)

RetiredDefPlayerFixed$Sacks <- as.integer(retiredDefensivedata$Sacks)   

RetiredDefPlayerFixed$Safeties <- as.integer(retiredDefensivedata$Sack_Yards)

RetiredDefPlayerFixed$Passes_Defended <- as.integer(retiredDefensivedata$Safties)

RetiredDefPlayerFixed$Interceptions <- as.integer(retiredDefensivedata$Passes_Deflected)

RetiredDefPlayerFixed$TDs <- as.integer(retiredDefensivedata$INTs)

RetiredDefPlayerFixed$INT_Yards <- as.integer(retiredDefensivedata$TDs)

RetiredDefPlayerFixed$AverageYrdperINT <- as.integer(retiredDefensivedata$INT_Yards)

RetiredDefPlayerFixed$Tot_CareerHighINTReturn <- as.integer(retiredDefensivedata$Average)

View(RetiredDefPlayerFixed)

## Exporting the data to a csv file
# Export RetiredDefPlayerFixed to a CSV file
write.csv(RetiredDefPlayerFixed, "E:\\R_Code\\NFL Data\\RetiredDefPlayerFixed.csv", row.names = FALSE)

# Export retiredbasicstat to a CSV file
write.csv(retiredbasicstat, "E:\\R_Code\\NFL Data\\retiredbasicstat.csv", row.names = FALSE)