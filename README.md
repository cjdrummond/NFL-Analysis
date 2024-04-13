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

