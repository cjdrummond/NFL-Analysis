Untitled
================

## Introduction

    American football is a sport loved by many. It is a sport that is played by two teams of eleven players on a rectangular field with goalposts at each end. The object of the game is to score points by carrying the ball across the opponent's goal line or by kicking the ball through the opponent's goalposts. The team with the most points at the end of the game wins. Many of the players in the National Football League (NFL) are considered to be some of the best athletes in the world. They are strong, fast, and agile, and they have to be in order to compete at the highest level. In this project, we will analyze data of NFL players to see if we can identify some of the key factors that contribute to a player's success in the league. The data was obtained from Kaggle and was collected by Trevor Youngquist. The link to the dataset can be found [here](https://www.kaggle.com/trevoryoungquist/nfl-players-career-data). The dataset contains basic and career statistics for players from the 1960s to 2020. The data initial is separated into twenty different csv files, each containing data for a different position. For this project we will ignore the special teams positions and their data which lowers the csv files from twenty to twelve. 

    This data needed heavy cleaning and merging to able to analyze it. Of the twelve csv files there are six for active players and six for retired players. For the retired players the csv files are Defensive Stats, Fumbles Stats, Passing Stats, Receiving Stats, Rushing Stats, and Basic Stats. For the active players the csv files are Defensive Stats, Fumbles Stats, Passing Stats, Receiving Stats, Rushing Stats, and Basic Stats. 

    The csv files all had the same columns except for the active and retired basic stats csv files. 
    The column names for the defensive csv files are "Player_Id", "Year", "Team", "Games_Played", "Tackles", "Solo_Tackles", "Assisted_Tackles", "Sacks", "Sack_Yards", "Safties", "Passes_Deflected", "INTs", "TDs", "INT_Yards", "Average", "Long". 
    The Fumbles data has columns "Player_Id", "Year", "Team", "Games_Played", "Fumbles", "Fumbles_Lost", "Forced_Fumbles", "Own_Recovery", "Opposing_Recovery", "TDs"
    The Passing data has columns, "Player_Id", "Year", "Team", "Games_Played", "Attempts"                 "Completions", "Completion_Percentage", "Yards", "Average","Long", "TDs", "INTs", "First_Downs", "First_Down_Percentage", "Passes_Over_Twenty_Yards", "Passes_Over_Forty_Yards", "Sacks", "Sack_Yards", "Passer_Rating"
    The Receiving data has columns,"Player_Id", "Year", "Team", "Games_Played", "Receptions", "Yard", "Average", "Long", "TDs", "First_Downs", "First_Down_Percentage", "Receptions_Over_Twenty_Yards", "Receptions_Over_Forty_Yards" 
    The Rushing data has columns, "Player_Id", "Year", "Team", "Games_Played", "Attempts"                 "Yards", "Average", "Long", "TDs", "First_Downs", "First_Down_Percentage", "Rushes_Over_Twenty_Yards", "Rushes_Over_Forty_Yards", "Fumbles"
    The retired basic stats data has columns, "Player_Id", "Full_Name", "Position", "Height", "Weight", "College", "Hall_Of_Fame"
    The active basic stats data has columns, "Player_Id", "Full_Name", "Position", "Number", "Current_Team", "Height", "Weight", "Experience", "Age", "College"

    Since the data was conviently separated into active players and retired players I chose to go clean the data by positional data for both active and retired then merge those into large postional dataframes then merge those all together to create one massive dataframe. However to accomplish this, the data needed to heavy cleaning which is described below.

### Data Cleaning

    The libraries needed for data cleaning are dplyr, purrr, tidyr, and tibble packages. The first step was to load the data. When I looked at the each of the dataframes there seemed to be a lot of issues. All of the dataframes had different columns except for one column that allowed for merging, Player_Id and varied in the number of rows in each dataframe indicating that some Player_Id's were missing in some dataframes. While some dataframes had matching column names like INTs and Fumbles, it was inconistent on if these values were implying the same thing. Upon investigating further I also found that alot of the issues came from players before 1980. Some data collection methods were different back then and some of the performance metrics were not even collected until after 1960. I decided to only use data from the players who played from 1980 and beyond. The years the players played are not shown in the basic stats dataframes so I focused on the other dataframes for this. I also wanted to focus on players total career data instead of their individual year data. Because of this, I also needed to remove players that only had a career row which means that they played less than 1 year. Since I wanted to apply this process to multiple dataframes I created a function called allstatsfilter that takes a dataframe and does the following; Replaces TOTAL in the Year column to be 9999 and then converts Year to an integer class since all of them are whole numbers. Then it finds and removes any Player_Ids who have any year less than 1980. It then finds the Player_Ids who have atleast 2 rows of data meaning they have a year and total career of data, and removes those who only have one row of data. Then it subsets the dataframe to be only the rows where Year == 9999 and then changes 9999 back to "Total Career". The function then returns this new filtered dataframe. The function allstatsfilter is called within another function I created which will be mentioned shortly. When I was looking at the filtered dataframes more closely I saw that there were some errors when the data was scraped initially that I needed to address. To fix these issues and also help with renaming the columns to help with merging my data later on I created functions for each of the positional grouping dataframes. I will not describe each function in detail but essentially each of the functions takes in a dataframe and then calls the allstatsfilter function to create the filtered dataframe. Then creates an empty dataframe that has the correct dimesions and column names. It then fills in that new dataframe with the values from the filtered dataframe. This way it also assigns the correct class to the new columns as well. The function then returns the new fixed dataframe. This is done for all of the dataframes including the basic stats dataframes. 

    Then the dataframes are then merged using Player_Id and then are written to a csv file to preserve these changes for the analysis. This was a very brief explanation of this data cleaning and processing as that was not the main purpose of this project. The full detailed process for the data cleaning and processing can be found on my Github in the NFL Player Data Cleaning script. 

    One important thing to note about this data is the large amount of missing values for some of the players. This makes analysis much more difficult however it also makes sense for the data. The positions of the players determines a lot of what data is going to be collected on them. For example a quarterback is not going to have any data for Sacks due to the definition of what a Sack is. Many of these performance metrics that I will be analyzing are grouped by offensive and defensive positions. Since all of my variables except for a select few have some amount of missing data, a regression model is not beneficial with my available data. Instead I plan to use factorial ANOVA and ANCOVA to see the effect of a player's College geographical region, hall of fame status and height and weight (combined into BMI) has on their performance metrics. For both the factorial ANOVA and ANCOVA I analyzed 3 times. Once for the offense using the mainly offensive metrics. Another for the defense using the mainly defensive metrics, and a final one using the full ungrouped data with all of the performance metrics. 

## Literature Reviews

<https://journals.lww.com/nsca-jscr/fulltext/2013/04000/Changes_in_the_Athletic_Profile_of_Elite_College.1.aspx>

<https://www.proquest.com/openview/904419161ef9fda6686871710ee31b83/1?pq-origsite=gscholar&cbl=18750&diss=y>

<https://www.actionnetwork.com/ncaaf/what-state-produces-the-most-nfl-players>

<https://www.espn.com/nfl/story/_/id/18598468/what-artificial-intelligence-says-hall-fame-chances-kurt-warner-ladainian-tomlinson-terrell-davis>

<https://link.springer.com/article/10.2165/00007256-200131110-00001>

<https://journals.lww.com/nsca-jscr/fulltext/2020/03000/a_comparison_of_the_national_football_league_s.20.aspx>

<https://surface.syr.edu/honors_capstone/792/>

## Method

To start my analysis I chose to focus on players who had played at least
2 games played. I then looked at the correlation between height and
weight for each of the players since I had a suspicion that these
variables would be correlated. Height and weight had a correlation of
`0.7081832` which confirmed my suspicions so I chose to add a new
variable, BMI to combine height and weight for analysis. Then with that
I went and viewed a full correlation breakdown. I used a function I
created that outputs a upper correlation plot as well as prints out
pairs that have a high correlation.

![](NFL-Report_files/figure-gfm/Full%20Correlation%20breakdown-1.png)<!-- -->

    ## Solo_Tackles and Assisted_Tackles have a correlation of 0.8831814 
    ## Solo_Tackles and Passes_Defended have a correlation of 0.7689866 
    ## Interceptions and INT_Yards have a correlation of 0.9099564 
    ## def_TDs and INT_Yards have a correlation of 0.7631004 
    ## Interceptions and Tot_CareerHighINTReturn have a correlation of 0.8904848 
    ## def_TDs and Tot_CareerHighINTReturn have a correlation of 0.7422354 
    ## INT_Yards and Tot_CareerHighINTReturn have a correlation of 0.9641279 
    ## Fumbles and Fumbles_Lost have a correlation of 0.9645013 
    ## Fumbles and Own_Recovery have a correlation of 0.9191412 
    ## Fumbles_Lost and Own_Recovery have a correlation of 0.848438 
    ## def_TDs and Passing_Attempts have a correlation of 0.9312661 
    ## Fumbles and Passing_Attempts have a correlation of 0.868412 
    ## Fumbles_Lost and Passing_Attempts have a correlation of 0.8073945 
    ## Own_Recovery and Passing_Attempts have a correlation of 0.8005756 
    ## Fumbles and Passing_Completions have a correlation of 0.8715021 
    ## Fumbles_Lost and Passing_Completions have a correlation of 0.82009 
    ## Own_Recovery and Passing_Completions have a correlation of 0.8069362 
    ## Passing_Attempts and Passing_Completions have a correlation of 0.9978167 
    ## Fumbles and Passing_Yards_Gained have a correlation of 0.8684514 
    ## Fumbles_Lost and Passing_Yards_Gained have a correlation of 0.8167976 
    ## Own_Recovery and Passing_Yards_Gained have a correlation of 0.8022151 
    ## Passing_Attempts and Passing_Yards_Gained have a correlation of 0.9979062 
    ## Passing_Completions and Passing_Yards_Gained have a correlation of 0.9988973 
    ## Fumbles and Sum_cr_long_passes have a correlation of 0.8384727 
    ## Fumbles_Lost and Sum_cr_long_passes have a correlation of 0.77684 
    ## Own_Recovery and Sum_cr_long_passes have a correlation of 0.7691093 
    ## Passing_Attempts and Sum_cr_long_passes have a correlation of 0.947112 
    ## Passing_Completions and Sum_cr_long_passes have a correlation of 0.9354452 
    ## Passing_Yards_Gained and Sum_cr_long_passes have a correlation of 0.9378553 
    ## Interceptions and Passing_TDs have a correlation of -1 
    ## Fumbles and Passing_TDs have a correlation of 0.8467077 
    ## Fumbles_Lost and Passing_TDs have a correlation of 0.7993516 
    ## Own_Recovery and Passing_TDs have a correlation of 0.789834 
    ## Passing_Attempts and Passing_TDs have a correlation of 0.9828258 
    ## Passing_Completions and Passing_TDs have a correlation of 0.9892758 
    ## Passing_Yards_Gained and Passing_TDs have a correlation of 0.9910151 
    ## Sum_cr_long_passes and Passing_TDs have a correlation of 0.9065698 
    ## Fumbles and Interceptions_thrown have a correlation of 0.8679618 
    ## Fumbles_Lost and Interceptions_thrown have a correlation of 0.8211938 
    ## Own_Recovery and Interceptions_thrown have a correlation of 0.7939956 
    ## Passing_Attempts and Interceptions_thrown have a correlation of 0.9651149 
    ## Passing_Completions and Interceptions_thrown have a correlation of 0.9489339 
    ## Passing_Yards_Gained and Interceptions_thrown have a correlation of 0.9540301 
    ## Sum_cr_long_passes and Interceptions_thrown have a correlation of 0.9421799 
    ## Passing_TDs and Interceptions_thrown have a correlation of 0.9224274 
    ## Fumbles and First_downs_thrown have a correlation of 0.8864984 
    ## Fumbles_Lost and First_downs_thrown have a correlation of 0.8475305 
    ## Own_Recovery and First_downs_thrown have a correlation of 0.8279248 
    ## Passing_Attempts and First_downs_thrown have a correlation of 0.9643779 
    ## Passing_Completions and First_downs_thrown have a correlation of 0.9737488 
    ## Passing_Yards_Gained and First_downs_thrown have a correlation of 0.9679485 
    ## Sum_cr_long_passes and First_downs_thrown have a correlation of 0.8878438 
    ## Passing_TDs and First_downs_thrown have a correlation of 0.962451 
    ## Interceptions_thrown and First_downs_thrown have a correlation of 0.8864291 
    ## Completion_Percentage and First_downs_per_attempt_percentage have a correlation of 0.833172 
    ## Yrds_gained_per_Attempt and First_downs_per_attempt_percentage have a correlation of 0.7218182 
    ## Fumbles and Been_Sacked have a correlation of 0.8780714 
    ## Fumbles_Lost and Been_Sacked have a correlation of 0.8319553 
    ## Own_Recovery and Been_Sacked have a correlation of 0.8100796 
    ## Passing_Attempts and Been_Sacked have a correlation of 0.9461408 
    ## Passing_Completions and Been_Sacked have a correlation of 0.9342282 
    ## Passing_Yards_Gained and Been_Sacked have a correlation of 0.9373409 
    ## Sum_cr_long_passes and Been_Sacked have a correlation of 0.9365159 
    ## Passing_TDs and Been_Sacked have a correlation of 0.9022453 
    ## Interceptions_thrown and Been_Sacked have a correlation of 0.9203122 
    ## First_downs_thrown and Been_Sacked have a correlation of 0.8819686 
    ## Passes_Defended and Sack_Yards have a correlation of 0.9191582 
    ## AverageYrdperINT and Sack_Yards have a correlation of -1 
    ## Fumbles and Sack_Yards have a correlation of 0.8611133 
    ## Fumbles_Lost and Sack_Yards have a correlation of 0.818269 
    ## Own_Recovery and Sack_Yards have a correlation of 0.7859039 
    ## Passing_Attempts and Sack_Yards have a correlation of 0.9405773 
    ## Passing_Completions and Sack_Yards have a correlation of 0.9270709 
    ## Passing_Yards_Gained and Sack_Yards have a correlation of 0.9321255 
    ## Sum_cr_long_passes and Sack_Yards have a correlation of 0.9297615 
    ## Passing_TDs and Sack_Yards have a correlation of 0.8975301 
    ## Interceptions_thrown and Sack_Yards have a correlation of 0.9226076 
    ## First_downs_thrown and Sack_Yards have a correlation of 0.8637313 
    ## Been_Sacked and Sack_Yards have a correlation of 0.9934898 
    ## Interceptions and Sacked_per_game have a correlation of -1 
    ## INT_Yards and Sacked_per_game have a correlation of -1 
    ## Tot_CareerHighINTReturn and Sacked_per_game have a correlation of -1 
    ## First_downs_per_attempt_percentage and Passer_Rating have a correlation of 0.779224 
    ## Receptions and Receiving_Yards have a correlation of 0.9763763 
    ## Receptions and Sum_cr_long_receptions have a correlation of 0.9232466 
    ## Receiving_Yards and Sum_cr_long_receptions have a correlation of 0.927574 
    ## Receptions and Receiving_TDs have a correlation of 0.9072474 
    ## Receiving_Yards and Receiving_TDs have a correlation of 0.9498617 
    ## Sum_cr_long_receptions and Receiving_TDs have a correlation of 0.8641397 
    ## Sacked_per_game and Receptions_per_Game have a correlation of -0.7062682 
    ## Receptions and Receptions_per_Game have a correlation of 0.7856012 
    ## Receiving_Yards and Receptions_per_Game have a correlation of 0.7659396 
    ## Sum_cr_long_receptions and Receptions_per_Game have a correlation of 0.7309987 
    ## Receptions and Receiving_Yards_per_Game have a correlation of 0.7619192 
    ## Receiving_Yards and Receiving_Yards_per_Game have a correlation of 0.7951733 
    ## Sum_cr_long_receptions and Receiving_Yards_per_Game have a correlation of 0.754185 
    ## Receiving_TDs and Receiving_Yards_per_Game have a correlation of 0.7305 
    ## Receptions_per_Game and Receiving_Yards_per_Game have a correlation of 0.9495188 
    ## Receptions and First_down_receptions have a correlation of 0.944204 
    ## Receiving_Yards and First_down_receptions have a correlation of 0.9567219 
    ## Sum_cr_long_receptions and First_down_receptions have a correlation of 0.8677464 
    ## Receiving_TDs and First_down_receptions have a correlation of 0.9135949 
    ## Receptions_per_Game and First_down_receptions have a correlation of 0.7701865 
    ## Receiving_Yards_per_Game and First_down_receptions have a correlation of 0.7960868 
    ## Forced_Fumbles and TDs_per_Reception have a correlation of 0.741522 
    ## Sacked_per_game and TDs_per_Reception have a correlation of 0.7653447 
    ## Rushing_Attempts and Rushing_Yards have a correlation of 0.9941193 
    ## Rushing_Attempts and Sum_cr_long_rush_yds have a correlation of 0.8796913 
    ## Rushing_Yards and Sum_cr_long_rush_yds have a correlation of 0.8892801 
    ## Rushing_Attempts and Rushing_TDs have a correlation of 0.9420781 
    ## Rushing_Yards and Rushing_TDs have a correlation of 0.9450549 
    ## Sum_cr_long_rush_yds and Rushing_TDs have a correlation of 0.8370862 
    ## Rushing_Attempts and Rushing_Yards_per_Game have a correlation of 0.7886155 
    ## Rushing_Yards and Rushing_Yards_per_Game have a correlation of 0.7938822 
    ## Sum_cr_long_rush_yds and Rushing_Yards_per_Game have a correlation of 0.7001313 
    ## Rushing_TDs and Rushing_Yards_per_Game have a correlation of 0.7222067 
    ## Rushing_Attempts and Rushing_Attempts_per_Game have a correlation of 0.7847854 
    ## Rushing_Yards and Rushing_Attempts_per_Game have a correlation of 0.777076 
    ## Rushing_TDs and Rushing_Attempts_per_Game have a correlation of 0.7084183 
    ## Rushing_Yards_per_Game and Rushing_Attempts_per_Game have a correlation of 0.9841729 
    ## Rushing_Attempts and First_down_rushes have a correlation of 0.9394355 
    ## Rushing_Yards and First_down_rushes have a correlation of 0.9423879 
    ## Sum_cr_long_rush_yds and First_down_rushes have a correlation of 0.8529021 
    ## Rushing_TDs and First_down_rushes have a correlation of 0.9059212 
    ## Rushing_Yards_per_Game and First_down_rushes have a correlation of 0.7785345 
    ## Rushing_Attempts_per_Game and First_down_rushes have a correlation of 0.7683133 
    ## First_down_rushes_percentage and TDs_per_Rush_Attempt have a correlation of 0.7378462 
    ## def_TDs and Rush_TD_per_Game have a correlation of -0.9405623 
    ## Rushing_Yards_per_Game and Rush_TD_per_Game have a correlation of 0.8390271 
    ## Rushing_Attempts_per_Game and Rush_TD_per_Game have a correlation of 0.8365489

  
  

## Results

You can also embed plots, for example:

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

## Discussion and Conclusion
