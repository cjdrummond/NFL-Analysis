# Retired-NFL-Analysis
Analysis on retired NFL player data from 1980 - 2020. Goal is to create a model that can predict a players height based on their career statistics along with other factors

## Data Cleaning
### Basic Statistics Data
Starting with the basic stat dataset, there are a few rows with missing values written as "--" those are removed
Then the dataframe was exported to a csv file to be used with data merging later. 
### Defensive Statistics Data 
This file contains defensive players statistics for each year of their career as well as their total career numbers. There are a few changes in data collection that happened around 1960-1999. To limit some of this inaccuacry and to focus more on current metrics the dataset was limited down to those players that played during and after 1980. There were some players who had career stats but had no year data with it. To look at the most accurate level of NFL players I wanted to focus on players that had played atleast 1 full year (They could have not played in a game but were on a roster for a year.) I also wanted to limit the rows to just the career totals as the information from the yearly data is empassed within the career totals. 

There was an issue in the data that the variable Sack_yards was not scraped correctly and messed up the columns following. To fix this a new dataset was created so we could correctly assign the data to its variable and also drop some of the variables that were no longer of use from the old dataset. This also allowed for more clear column names as some of them weren't as clear as to what they were reporting. 

Then the fixed dataset was exported to a csv file to merge with the basic statistics dataset later. 

### NOTES:
In passing data, Columns are again messed up. 
Games_Played = Games_Played
Attempts = Attempts
Completions = Completions
Completions_Percentage = Completion_Percentage
Yards (yards gained) = Yards (Yards gained)
Average = Average (Yards gained per pass attempt)
Long = Sum of career longest passes
TDs  = TDs
INTs = INTs
First_Downs = First_Downs
First_Down_Percentage  = First_Down_Percentage (first downs/attempts)

Passes_Over_Twenty_Yards = unknown. Does not match consistently against any information I can find. The scraping code also doesnt provide any insight to what this number is either. Given that I cannot verify the validity of this variabale and considering that the following variable is incorrectly identified, I am removing this variable from the analysis

Passes_Over_Forty_Yards = to remove

Sacks = Passes_Over_forty_yards value

Sack_Yards = Sacks value

Passer_rating = Sack_Yards value

Games_Played is repeated in many datasets so when merging I will need to have a check for if it is already included in the merged dataset then dont add it again


find fumbles per possession by (fumbles / (rush attempt + receptions) within each player_id

also processing active players with functions



