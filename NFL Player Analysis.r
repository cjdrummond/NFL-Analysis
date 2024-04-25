library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4)
library(car)
library(corrplot)
library(lsr)
library(multcomp)
library(rlang)

Data_Load_and_Simple_Fixing <- function(dataframe){
    dataframe$Position <- as.factor(dataframe$Position)
    dataframe$College <- as.factor(dataframe$College)
    dataframe$Hall_Of_Fame <- as.factor(dataframe$Hall_Of_Fame)
    dataframe$Player_Id <- as.factor(dataframe$Player_Id)

    # filter out players with less than 2 games played
    dataframe <- dataframe %>%
        filter(Games_Played >= 2)
    
    # Drop players in Special Teams positions
    dataframe <- dataframe %>%
    filter(Position != "K" & Position != "P" & Position != "LS")
    # Remove unused levels
    dataframe$Position <- droplevels(dataframe$Position)

    # add column to dataframe to indicate if player is offensive or defensive
    offensive_positions <- c("RB", "WR", "OG", "TE", "QB", "C", "FB", "G", "T", "OT", "HB", "OL")
    defensive_positions <- c("CB", "LB", "DE", "DT", "ILB", "DB", "OLB", "FS", "SS", "NT", "MLB","SAF")

    dataframe$is_offense <- ifelse(dataframe$Position %in% offensive_positions, "True", "False")
    dataframe$is_offense <- as.factor(dataframe$is_offense)

    #colnames to change 0 to NA
    # List of column names
    cols_to_change <- c("Solo_Tackles", "Assisted_Tackles", "Sacks", 
    "Safeties", "Passes_Defended", "Interceptions", "def_TDs", "INT_Yards", 
    "AverageYrdperINT", "Tot_CareerHighINTReturn", "Fumbles", "Fumbles_Lost", 
    "Forced_Fumbles", "Own_Recovery", "Opposing_Recovery", "Fumbles_TD", 
    "Passing_Attempts", "Passing_Completions", "Completion_Percentage", 
    "Passing_Yards_Gained", "Yrds_gained_per_Attempt", "Sum_cr_long_passes", 
    "Passing_TDs", "Interceptions_thrown", "First_downs_thrown", 
    "First_downs_per_attempt_percentage", "Been_Sacked", "Sack_Yards", 
    "Sacked_per_game", "Passer_Rating", "Receptions", "Receiving_Yards", 
    "Yards_per_Reception", "Sum_cr_long_receptions", "Receiving_TDs", 
    "Receptions_per_Game", "Receiving_Yards_per_Game", "First_down_receptions", 
    "First_down_receptions_percentage", "TDs_per_Reception", "Rushing_Attempts", 
    "Rushing_Yards", "Rushing_Yards_per_Attempt", "Sum_cr_long_rush_yds", 
    "Rushing_TDs", "Rushing_Yards_per_Game", "Rushing_Attempts_per_Game", 
    "First_down_rushes", "First_down_rushes_percentage", "TDs_per_Rush_Attempt", 
    "Rush_TD_per_Game")

    # Convert all the columns to numeric
    for (col in cols_to_change) {
        dataframe[[col]] <- as.numeric(as.character(dataframe[[col]]))
    }

    # Replace 0's with NA in the columns
    for (col in cols_to_change) {
        dataframe[[col]][dataframe[[col]] == 0 & !is.na(dataframe[[col]])] <- NA
    }

    ## adding BMI Column to dataframe
    dataframe$BMI <- dataframe$Weight / (dataframe$Height * dataframe$Height) * 703

    return(dataframe)
}

# Create new variables

New_Variable_and_DF_Creation <- function(dataframe){
    # Create new variables
    dataframe$Total_Tackles <- dataframe$Solo_Tackles + dataframe$Assisted_Tackles
    dataframe$Fumble_recovery_kept_pct <- (((dataframe$Fumbles - dataframe$Fumbles_Lost) / dataframe$Fumbles) *100)
    dataframe$Passing_TD_per_Game <- dataframe$Passing_TDs / dataframe$Games_Played
    dataframe$Interceptions_Thrown_per_passing_attempt <- dataframe$Interceptions_thrown / dataframe$Passing_Attempts

    # Create new dataframe with specified variables
    new_df <- dataframe[, c("Player_Id", "Position", "is_offense", "College", "Hall_Of_Fame", "BMI", "Games_Played", "Total_Tackles", "Sacks", "Safeties", "Passes_Defended", "def_TDs", "AverageYrdperINT", "Fumble_recovery_kept_pct", "Forced_Fumbles", "Completion_Percentage", "Yrds_gained_per_Attempt", "Passing_TD_per_Game", "Interceptions_Thrown_per_passing_attempt", "First_downs_per_attempt_percentage", "Been_Sacked", "Passer_Rating", "Yards_per_Reception", "First_down_receptions_percentage", "Receiving_TDs", "Rushing_Yards_per_Attempt", "First_down_rushes", "TDs_per_Rush_Attempt")]

    return(new_df)
}

Correlation_Matrix_and_Plot <- function(dataframe, columns_for_correlation){
    # Compute the correlation matrix
    cor_matrix <- cor(dataframe[columns_for_correlation], use = "pairwise.complete.obs")

    # Create the correlation plot
    corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 0.6)
    high_cor_pairs <- which(abs(cor_matrix) > 0.7 & cor_matrix != 1, arr.ind = TRUE)

    if (nrow(high_cor_pairs) == 0) {
    cat("There are no correlations more extreme than 0.7.\n")
    } else {
        # Print pairs
        for (i in 1:nrow(high_cor_pairs)) {
            row <- high_cor_pairs[i, "row"]
            col <- high_cor_pairs[i, "col"]
            if (row < col) {
                cat(columns_for_correlation[row], "and", columns_for_correlation[col], "have a correlation of", cor_matrix[row, col], "\n")
            }
        }
    }
}

Factorial_ANOVA <- function(dataframe, cols_for_fact_anova){
    for (metric in cols_for_fact_anova) {
        formula <- as.formula(paste(metric, "~ Hall_Of_Fame*Region"))
        factorial_anova <- aov(formula, data = dataframe)
        cat("\nFactorial ANOVA for", metric, "accounting for interactions between HOF status and College Region:\n")
        print(summary(factorial_anova))

        # Create a new dataframe excluding rows with NA in the current metric, is_offense, and Hall_Of_Fame
        dataframe_no_na <- dataframe[!is.na(dataframe[[metric]]) & !is.na(dataframe$Region) & !is.na(dataframe$Hall_Of_Fame), ]
        
        # Create a new graphics device interaction plot
        png(filename = paste0(metric, "_interaction_plot.png"))
        interaction.plot(x.factor = dataframe_no_na$Region, trace.factor = dataframe_no_na$Hall_Of_Fame, response = dataframe_no_na[[metric]], type = "b", fun = mean, fixed = TRUE, leg.bty = "o", xlab = "is Offense Position", ylab = metric, main = paste(metric, "by College Region and Hall of Fame Status"))
        dev.off()
        # Perform Tukey's HSD test

        tukey_results <- TukeyHSD(factorial_anova)
        # Convert the results to dataframes
        tukey_df_Hall_Of_Fame <- data.frame(tukey_results$Hall_Of_Fame)
        tukey_df_Region <- data.frame(tukey_results$Region)
        tukey_df_Hall_Of_Fame_Region <- data.frame(tukey_results$`Hall_Of_Fame:Region`)
        
        # Filter the dataframes to only include rows where the adjusted p-value is less than 0.05
        significant_results_Hall_Of_Fame <- tukey_df_Hall_Of_Fame[tukey_df_Hall_Of_Fame$p.adj < 0.05 & !is.na(tukey_df_Hall_Of_Fame$p.adj), ]
        significant_results_Region <- tukey_df_Region[tukey_df_Region$p.adj < 0.05 & !is.na(tukey_df_Region$p.adj), ]
        significant_results_Hall_Of_Fame_Region <- tukey_df_Hall_Of_Fame_Region[tukey_df_Hall_Of_Fame_Region$p.adj < 0.05 & !is.na(tukey_df_Hall_Of_Fame_Region$p.adj), ]
        
        # Print the significant results
        cat("\nSignificant Tukey's HSD results for Hall_Of_Fame against", metric, ":\n")
        print(significant_results_Hall_Of_Fame)
        cat("\nSignificant Tukey's HSD results for Region against", metric, ":\n")
        print(significant_results_Region)
        cat("\nSignificant Tukey's HSD results for Hall_Of_Fame:Region against", metric, ":\n")
        print(significant_results_Hall_Of_Fame_Region)

        # Perform Levene's test
        cat("\nLevene's test for", metric, ":\n")
        print(leveneTest(formula, data = dataframe))

        # effect size
        cat("\nEta squared for", metric, ":\n")
        print(etaSquared(factorial_anova))

        # Perform Kolmogorov-Smirnov test since data is too large for Shapiro-Wilk test 
        aov_residuals <- residuals(factorial_anova)
        #print(shapiro.test(aov_residuals))

        print(ks.test(factorial_anova$residuals, "pnorm", mean = mean(factorial_anova$residuals), sd = sd(factorial_anova$residuals)))
        cat("\n--------------------------------------------------\n")
    }
}

ANCOVA_Region_HOF_BMI <- function(dataframe, cols_for_ANCOVA){
    for (metric in cols_for_ANCOVA) {
        formula <- as.formula(paste(metric, "~ Hall_Of_Fame*Region + BMI"))
        ancova <- aov(formula, data = dataframe)
        cat("\nANCOVA for", metric, "accounting for BMI across different College Regions:\n")
        print(summary(ancova))

        dataframe_no_na <- dataframe[!is.na(dataframe[[metric]]) & !is.na(dataframe$Region) & !is.na(dataframe$Hall_Of_Fame) & !is.na(dataframe$BMI), ]

        cat("\nLevene's test for", metric, "~ Region:\n")
        print(leveneTest(dataframe_no_na[[metric]] ~ dataframe_no_na$Region, data = dataframe_no_na))
        
        cat("\nLevene's test for", metric, "~ Hall_Of_Fame:\n")
        print(leveneTest(dataframe_no_na[[metric]] ~ Hall_Of_Fame, data = dataframe_no_na))

        cat("\nLevene's test for", metric, "~ Hall_Of_Fame:Region:\n")
        print(leveneTest(dataframe_no_na[[metric]] ~ Hall_Of_Fame:Region, data = dataframe_no_na))

        postHocs <- glht(ancova, linfct = mcp(Region = "Tukey", Hall_Of_Fame = "Tukey"))
        cat("\nTukey's HSD post-hoc test for", metric, "compared with Region and Hall of Fame:\n")
        print(summary(postHocs))
        cat("\n--------------------------------------------------\n")
    }
}
Log_transform_and_plot <- function(df_all, df_retired, column_name) {
    # Create copies of the input dataframes
    df_all_copy <- df_all
    df_retired_copy <- df_retired
    
    # Apply log transformation to positive values and add it as a new column
    df_all_copy[[paste0(column_name, "_trans")]] <- ifelse(df_all_copy[[column_name]] > 0, log(df_all_copy[[column_name]]), NA)
    df_retired_copy[[paste0(column_name, "_trans")]] <- ifelse(df_retired_copy[[column_name]] > 0, log(df_retired_copy[[column_name]]), NA)
    
    # Omit NA values for plotting
    df_all_copy_plot <- na.omit(df_all_copy[, c(column_name, "is_offense", paste0(column_name, "_trans"))])
    df_retired_copy_plot <- na.omit(df_retired_copy[, c(column_name, "is_offense", paste0(column_name, "_trans"))])
    
    print(min(df_all_copy_plot[[column_name]]))
    # Create the histogram of original data
    p1_OG <- ggplot(df_all_copy_plot, aes(x = !!sym(column_name), fill = is_offense)) +
        geom_histogram(binwidth = 10, color = "black") +
        labs(x = column_name, y = "Count", title = paste0("Histogram of ", column_name, " for all Players")) +
        scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
    
    # Create the histogram of transformed data
    p2_tran <- ggplot(df_all_copy_plot, aes(x = !!sym(paste0(column_name, "_trans")), fill = is_offense)) +
        geom_histogram(binwidth = 0.5, color = "black") +
        labs(x = paste0("Log Transformed ", column_name), y = "Count", title = paste0("Histogram of Log Transformed ", column_name, " for all Players")) +
        scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
    
    # Return the updated dataframes and the plots
    list(df_all = df_all_copy, df_retired = df_retired_copy, p1_OG = p1_OG, p2_tran = p2_tran)
}

Inverse_transform_and_plot <- function(df_all, df_retired, column_name) {
    # Create copies of the input dataframes
  df_all_copy <- df_all
  df_retired_copy <- df_retired
  
  # Apply reciprocal transformation to non-zero values and add it as a new column
  df_all_copy[[paste0(column_name, "_trans")]] <- ifelse(df_all_copy[[column_name]] != 0, 1/df_all_copy[[column_name]], NA)
  df_retired_copy[[paste0(column_name, "_trans")]] <- ifelse(df_retired_copy[[column_name]] != 0, 1/df_retired_copy[[column_name]], NA)
  
  # Omit NA values for plotting
  df_all_copy_plot <- na.omit(df_all_copy[, c(column_name, "is_offense", paste0(column_name, "_trans"))])
  df_retired_copy_plot <- na.omit(df_retired_copy[, c(column_name, "is_offense", paste0(column_name, "_trans"))])
  
  # Create the histogram of original data
  p1_OG <- ggplot(df_all_copy_plot, aes(x = !!sym(column_name), fill = is_offense)) +
    geom_histogram(binwidth = 10, color = "black") +
    labs(x = column_name, y = "Count", title = paste0("Histogram of ", column_name, " for all Players")) +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
  
  # Create the histogram of transformed data
  p2_tran <- ggplot(df_all_copy_plot, aes(x = !!sym(paste0(column_name, "_trans")), fill = is_offense)) +
    geom_histogram(binwidth = 0.1, color = "black") +  # Adjust binwidth
    labs(x = paste0("Reciprocal Transformed ", column_name), y = "Count", title = paste0("Histogram of Reciprocal Transformed ", column_name, " for all Players")) +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
  
  # Return the updated dataframes and the plots
  list(df_all = df_all_copy, df_retired = df_retired_copy, p1_OG = p1_OG, p2_tran = p2_tran)
}
Sqrt_transform_and_plot <- function(df_all, df_retired, column_name) {
    # Create copies of the input dataframes
    df_all_copy <- df_all
    df_retired_copy <- df_retired
    
    # Apply square root transformation to non-negative values and add it as a new column
    df_all_copy[[paste0(column_name, "_trans")]] <- ifelse(df_all_copy[[column_name]] >= 0, sqrt(df_all_copy[[column_name]]), NA)
    df_retired_copy[[paste0(column_name, "_trans")]] <- ifelse(df_retired_copy[[column_name]] >= 0, sqrt(df_retired_copy[[column_name]]), NA)
    
    # Omit NA values for plotting
    df_all_copy_plot <- na.omit(df_all_copy[, c(column_name, "is_offense", paste0(column_name, "_trans"))])
    df_retired_copy_plot <- na.omit(df_retired_copy[, c(column_name, "is_offense", paste0(column_name, "_trans"))])
    
    # Create the histogram of original data
    p1_OG <- ggplot(df_all_copy_plot, aes(x = !!sym(column_name), fill = is_offense)) +
        geom_histogram(binwidth = 10, color = "black") +
        labs(x = column_name, y = "Count", title = paste0("Histogram of ", column_name, " for all Players")) +
        scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
    
    # Create the histogram of transformed data
    p2_tran <- ggplot(df_all_copy_plot, aes(x = !!sym(paste0(column_name, "_trans")), fill = is_offense)) +
        geom_histogram(binwidth = 0.1, color = "black") +  # Adjust binwidth
        labs(x = paste0("Square Root Transformed ", column_name), y = "Count", title = paste0("Histogram of Square Root Transformed ", column_name, " for all Players")) +
        scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
    
    # Return the updated dataframes and the plots
    list(df_all = df_all_copy, df_retired = df_retired_copy, p1_OG = p1_OG, p2_tran = p2_tran)
}
CubeRoot_transform_and_plot <- function(df_all, df_retired, column_name) {
    # Create copies of the input dataframes
    df_all_copy <- df_all
    df_retired_copy <- df_retired
    
    # Apply cube root transformation to all values and add it as a new column
    df_all_copy[[paste0(column_name, "_trans")]] <- df_all_copy[[column_name]]^(1/3)
    df_retired_copy[[paste0(column_name, "_trans")]] <- df_retired_copy[[column_name]]^(1/3)
    
    # Omit NA values for plotting
    df_all_copy_plot <- na.omit(df_all_copy[, c(column_name, "is_offense", paste0(column_name, "_trans"))])
    df_retired_copy_plot <- na.omit(df_retired_copy[, c(column_name, "is_offense", paste0(column_name, "_trans"))])
    
    # Create the histogram of original data
    p1_OG <- ggplot(df_all_copy_plot, aes(x = !!sym(column_name), fill = is_offense)) +
        geom_histogram(binwidth = 10, color = "black") +
        labs(x = column_name, y = "Count", title = paste0("Histogram of ", column_name, " for all Players")) +
        scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
    
    # Create the histogram of transformed data
    p2_tran <- ggplot(df_all_copy_plot, aes(x = !!sym(paste0(column_name, "_trans")), fill = is_offense)) +
        geom_histogram(binwidth = 0.1, color = "black") +  # Adjust binwidth
        labs(x = paste0("Cube Root Transformed ", column_name), y = "Count", title = paste0("Histogram of Cube Root Transformed ", column_name, " for all Players")) +
        scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
    
    # Return the updated dataframes and the plots
    list(df_all = df_all_copy, df_retired = df_retired_copy, p1_OG = p1_OG, p2_tran = p2_tran)
}
player_data <- read.csv("E:\\GitHub Rep\\Retired-NFL-Analysis\\Retired-NFL-Analysis\\merged_data_all.csv")
player_retired <- read.csv("E:\\GitHub Rep\\Retired-NFL-Analysis\\Retired-NFL-Analysis\\merged_retired_data_all.csv")
college_df <- read.csv("E:\\GitHub Rep\\Retired-NFL-Analysis\\Retired-NFL-Analysis\\college_regions.csv")

player_data_filtered <- Data_Load_and_Simple_Fixing(player_data)
player_retired_filtered <- Data_Load_and_Simple_Fixing(player_retired)
#player_data_filtered$BMI[1]




cor(player_data_filtered$Height, player_data_filtered$Weight, use = "complete.obs")
### height and weight are highly correlated so I will make a new variable BMI to combine the two. I am chosing not to remove the variables since there is some informational benefit to keeping them separate for further research questions.
View(player_data_filtered)

## correlation matrix
cols_for_corr <- c("Solo_Tackles", "Assisted_Tackles", "Sacks", 
"Safeties", "Passes_Defended", "Interceptions", "def_TDs", "INT_Yards", 
"AverageYrdperINT", "Tot_CareerHighINTReturn", "Fumbles", "Fumbles_Lost", 
"Forced_Fumbles", "Own_Recovery", "Opposing_Recovery", "Fumbles_TD", 
"Passing_Attempts", "Passing_Completions", "Completion_Percentage", 
"Passing_Yards_Gained", "Yrds_gained_per_Attempt", "Sum_cr_long_passes", 
"Passing_TDs", "Interceptions_thrown", "First_downs_thrown", 
"First_downs_per_attempt_percentage", "Been_Sacked", "Sack_Yards", 
"Sacked_per_game", "Passer_Rating", "Receptions", "Receiving_Yards", 
"Yards_per_Reception", "Sum_cr_long_receptions", "Receiving_TDs", 
"Receptions_per_Game", "Receiving_Yards_per_Game", "First_down_receptions", 
"First_down_receptions_percentage", "TDs_per_Reception", "Rushing_Attempts", 
"Rushing_Yards", "Rushing_Yards_per_Attempt", "Sum_cr_long_rush_yds", 
"Rushing_TDs", "Rushing_Yards_per_Game", "Rushing_Attempts_per_Game", 
"First_down_rushes", "First_down_rushes_percentage", "TDs_per_Rush_Attempt", 
"Rush_TD_per_Game", "BMI", "Games_Played")

Correlation_Matrix_and_Plot(player_data_filtered, cols_for_corr)

#### There are quite a few pairs with high correlation that need to be addressed. I alot of these make conceptual sense such as the attempts_per_game and total_yards_per_game as you need to attempt in order to get yards. I will not remove these variables but will keep them in mind for further analysis. A lot of the correlations and issues come from the data that is able to be collected by position. For example a QB is not going to have any Sack data as they are the players being sacked. To possibly address this issue I will split the data into offensive and defensive positions and see if the correlations are still present.
# some correlations however dont make sense such as the correlation between passing_yards_gainedand Interceptions_thrown. Since this is almost a mutually exclusive event. If a pass is completed and yards are gained then an interception was not thrown. This requires further variable manipulation to address.


### variable manipulation ###
new_df_all <- New_Variable_and_DF_Creation(player_data_filtered)
new_df_retired <- New_Variable_and_DF_Creation(player_retired_filtered)

new_df_all_regions <- merge(new_df_all, college_df, by = "College", all.x = TRUE)
# Reorder the columns
new_df_all_regions <- new_df_all_regions %>%
    dplyr::select(Player_Id, Hall_Of_Fame, College, Region, Position, is_offense, BMI, Games_Played, everything())

new_df_all_regions$Region <- as.factor(new_df_all_regions$Region)
#colnames(new_df_all_regions)
#dim(new_df_all_regions)

new_df_retired_regions <- merge(new_df_retired, college_df, by = "College", all.x = TRUE)
# Reorder the columns  
new_df_retired_regions <- new_df_retired_regions %>%
    dplyr::select(Player_Id, Hall_Of_Fame, College, Region, Position, is_offense, BMI, Games_Played, everything())
new_df_retired_regions$Region <- as.factor(new_df_retired_regions$Region)
#View(new_df_retired_regions)
## now re-examine correlations with adjusted variables ##

cols_for_corr_new <- c("BMI", "Total_Tackles", "Sacks", "Safeties", "Passes_Defended", "def_TDs", "AverageYrdperINT", "Fumble_recovery_kept_pct", "Forced_Fumbles", "Completion_Percentage", "Yrds_gained_per_Attempt", "Passing_TD_per_Game", "Interceptions_Thrown_per_passing_attempt", "First_downs_per_attempt_percentage", "Been_Sacked", "Passer_Rating", "Yards_per_Reception", "First_down_receptions_percentage", "Receiving_TDs", "Rushing_Yards_per_Attempt", "First_down_rushes", "TDs_per_Rush_Attempt")

# Compute the correlation matrix
Correlation_Matrix_and_Plot(new_df_all_regions, cols_for_corr_new)

### Some correlations are still present but now make atleast some conceptual sense. For example the perfect negative correlation between Sacks and Passing_TD_per_Game makes sense in the definition of a sack. Where a QB is tackled before getting a pass off. The correlation between Interceptions_Thrown_per_passing_attempt and Passer_Rating is heavily tied together as the better the QB the fewer interceptions they will throw in their career making them a better QB and similarly the more first_downs they will throw. 



## Histograms for new_df_all_regions and transformations if needed

##### TRANSFORMATIONS ####
#### BMI ####

BMI_all <- na.omit(new_df_all_regions[, c("BMI", "is_offense")])

# Filter out rows with missing or zero values
BMI_all <- BMI_all[BMI_all$BMI > 0, ]

# Create the histogram
ggplot(BMI_all, aes(x = BMI, fill = is_offense)) +
    geom_histogram(binwidth = 10, color = "black") +
    labs(x = "BMI", y = "Count", title = "Histogram of BMI all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
### fariy normal distribution so no transformation needed

BMI_retired <- na.omit(new_df_retired_regions[, c("BMI", "is_offense")])

ggplot(BMI_retired, aes(x = BMI, fill = is_offense)) +
    geom_histogram(binwidth = 10, color = "black") +
    labs(x = "BMI", y = "Count", title = "Histogram of BMI for Retired Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## fairly normal distribution so no transformation needed

dim(new_df_all_regions)
dim(new_df_retired_regions)

#### Total Tackles ####
total_tackles_all <- na.omit(new_df_all_regions[, c("Total_Tackles", "is_offense")])

# Filter out rows with missing or zero values
total_tackles_all <- total_tackles_all[total_tackles_all$Total_Tackles > 0, ]

# Create the histogram
ggplot(total_tackles_all, aes(x = Total_Tackles, fill = is_offense)) +
    geom_histogram(binwidth = 10, color = "black") +
    labs(x = "Total Tackles", y = "Count", title = "Histogram of Total Tackles for all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## data is heavily right skewed so a transformation is needed
#testing log transformation
Total_Tackles_results <- Log_transform_and_plot(new_df_all_regions, new_df_retired_regions, "Total_Tackles")

## Right skewed so log transformation transformation needed
Total_Tackles_results$p2_tran
## data much more normal after transformation

## dataframe updated with new column for log transformed total tackles
new_df_all_regions_1t <- Total_Tackles_results$df_all
dim(new_df_all_regions_1t)

new_df_retired_regions_1t <- Total_Tackles_results$df_retired
dim(new_df_retired_regions_1t)
#### Sacks ####
sacks_all <- na.omit(new_df_all_regions_1t[, c("Sacks", "is_offense")])
sacks_retired <- na.omit(new_df_retired_regions_1t[, c("Sacks", "is_offense")])
# Filter out rows with missing or zero values
sacks_all <- sacks_all[sacks_all$Sacks > 0, ]
sacks_retired <- sacks_retired[sacks_retired$Sacks > 0, ]
# Create the histogram
ggplot(sacks_all, aes(x = Sacks, fill = is_offense)) +
    geom_histogram(binwidth = 10, color = "black") +
    labs(x = "Sacks", y = "Count", title = "Histogram of Sacks for all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## data is heavily right skewed so a transformation is needed
#testing log transformation
sacks_log_results <- Log_transform_and_plot(new_df_all_regions_1t, new_df_retired_regions_1t, "Sacks")
sacks_log_results$p2_tran

##still is right skewed so will try reciprocal transformation
sack_inv_results <- Inverse_transform_and_plot(new_df_all_regions_1t, new_df_retired_regions, "Sacks")
sack_inv_results$p2_tran
## more broken but is less skewed than the log transformation so will keep this transformation
## update dataframe with new column
new_df_all_regions_2t <- sack_inv_results$df_all
dim(new_df_all_regions_2t)
new_df_retired_regions_2t <- sack_inv_results$df_retired
dim(new_df_retired_regions_2t)



#### Safeties ####
safeties_all <- na.omit(new_df_all_regions_2t[, c("Safeties", "is_offense")])
safeties_retired <- na.omit(new_df_retired_regions_2t[, c("Safeties", "is_offense")])

# Filter out rows with missing or zero values
safeties_all <- safeties_all[safeties_all$Safeties > 0, ]
safeties_retired <- safeties_retired[safeties_retired$Safeties > 0, ]
# Create the histogram
ggplot(safeties_all, aes(x = Safeties, fill = is_offense)) +
    geom_histogram(binwidth = 0.1, color = "black") +
    labs(x = "Safeties", y = "Count", title = "Histogram of Safeties all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale

## right skewed so log transformation 
safeties_log_results <- Log_transform_and_plot(new_df_all_regions_2t, new_df_retired_regions_2t, "Safeties")
safeties_log_results$p2_tran

## still right skewed so reciprocal transformation
safeties_inv_results <- Inverse_transform_and_plot(new_df_all_regions_2t, new_df_retired_regions_2t, "Safeties")    
safeties_inv_results$p2_tran
## now is left skewed. Looking at the dimensions of safeties_all there is only 345 observations that are valid for this variable. Will be removing from analysis since it is a rare event. 

#### Passes Defended ####
passes_defended_all <- na.omit(new_df_all_regions[, c("Passes_Defended", "is_offense")])
passes_defended_retired <- na.omit(new_df_retired_regions[, c("Passes_Defended", "is_offense")])

# Filter out rows with missing or zero values
passes_defended_all <- passes_defended_all[passes_defended_all$Passes_Defended > 0, ]
passes_defended_retired <- passes_defended_retired[passes_defended_retired$Passes_Defended > 0, ]

# Create the histogram
ggplot(passes_defended_all, aes(x = Passes_Defended, fill = is_offense)) +
    geom_histogram(binwidth = 10, color = "black") +
    labs(x = "Passes Defended", y = "Count", title = "Histogram of Passes Defended all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## right skewed so log transformation to start

passes_defended_log_results <- Log_transform_and_plot(new_df_all_regions_2t, new_df_retired_regions_2t, "Passes_Defended")

passes_defended_log_results$p2_tran
## still right skewed so reciprocal transformation
passes_defended_inv_results <- Inverse_transform_and_plot(new_df_all_regions_2t, new_df_retired_regions_2t, "Passes_Defended")
passes_defended_inv_results$p2_tran
## log transformation is better so will keep this transformation
#update dataframe
new_df_all_regions_3t <- passes_defended_log_results$df_all
new_df_retired_regions_3t <- passes_defended_log_results$df_retired

#### def_TDs ####

def_TDs_all <- na.omit(new_df_all_regions[, c("def_TDs", "is_offense")])
def_TDs_retired <- na.omit(new_df_retired_regions[, c("def_TDs", "is_offense")])

# Filter out rows with missing or zero values
def_TDs_all <- def_TDs_all[def_TDs_all$def_TDs > 0, ]
def_TDs_retired <- def_TDs_retired[def_TDs_retired$def_TDs > 0, ]

# Create the histogram
ggplot(def_TDs_all, aes(x = def_TDs, fill = is_offense)) +
    geom_histogram(binwidth = 0.1, color = "black") +
    labs(x = "def_TDs", y = "Count", title = "Histogram of def_TDs all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale

## right skewed so log transformation
def_TDs_log_results <- Log_transform_and_plot(new_df_all_regions_3t, new_df_retired_regions_3t, "def_TDs")
def_TDs_log_results$p2_tran
## still right skewed so reciprocal transformation
def_TDs_inv_results <- Inverse_transform_and_plot(new_df_all_regions_3t, new_df_retired_regions_3t, "def_TDs")
def_TDs_inv_results$p2_tran

## now is left skewed so will try square root transformation
def_TDs_sqrt_results <- Sqrt_transform_and_plot(new_df_all_regions_3t, new_df_retired_regions_3t, "def_TDs")
def_TDs_sqrt_results$p2_tran
## still is right skewed so will keep log transformation as it appeared cleaner
new_df_all_regions_4t <- def_TDs_log_results$df_all
new_df_retired_regions_4t <- def_TDs_log_results$df_retired

#### AverageYrdperINT ####
average_yrd_per_int_all <- na.omit(new_df_all_regions_4t[, c("AverageYrdperINT", "is_offense")])
average_yrd_per_int_retired <- na.omit(new_df_retired_regions_4t[, c("AverageYrdperINT", "is_offense")])

average_yrd_per_int_all <- average_yrd_per_int_all[average_yrd_per_int_all$AverageYrdperINT > 0, ]
average_yrd_per_int_retired <- average_yrd_per_int_retired[average_yrd_per_int_retired$AverageYrdperINT > 0, ]
View(average_yrd_per_int_all)
# Create the histogram
ggplot(average_yrd_per_int_all, aes(x = AverageYrdperINT, fill = is_offense)) +
    geom_histogram(binwidth = 10, color = "black") +
    labs(x = "AverageYrdperINT", y = "Count", title = "Histogram of AverageYrdperINT all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## right skewed so log transformation

average_yrd_per_int_log_results <- Log_transform_and_plot(new_df_all_regions_4t, new_df_retired_regions_4t, "AverageYrdperINT")
## gives warnings but shouldnt so will ignore
average_yrd_per_int_log_results$p2_tran
## data is fairly normal after transformation
new_df_all_regions_5t <- average_yrd_per_int_log_results$df_all
new_df_retired_regions_5t <- average_yrd_per_int_log_results$df_retired

#### Fumble_recovery_kept_pct ####
fumble_recovery_kept_pct_all <- na.omit(new_df_all_regions_5t[, c("Fumble_recovery_kept_pct", "is_offense")])
fumble_recovery_kept_pct_retired <- na.omit(new_df_retired_regions_5t[, c("Fumble_recovery_kept_pct", "is_offense")])

fumble_recovery_kept_pct_all <- fumble_recovery_kept_pct_all[fumble_recovery_kept_pct_all$Fumble_recovery_kept_pct > 0, ]
fumble_recovery_kept_pct_retired <- fumble_recovery_kept_pct_retired[fumble_recovery_kept_pct_retired$Fumble_recovery_kept_pct > 0, ]

# Create the histogram
ggplot(fumble_recovery_kept_pct_all, aes(x = Fumble_recovery_kept_pct, fill = is_offense)) +
    geom_histogram(binwidth = 10, color = "black") +
    labs(x = "Fumble_recovery_kept_pct", y = "Count", title = "Histogram of Fumble_recovery_kept_pct all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## appears to be fairly normal so no transformation needed

#### Forced_Fumbles ####
forced_fumbles_all <- na.omit(new_df_all_regions_5t[, c("Forced_Fumbles", "is_offense")])
forced_fumbles_retired <- na.omit(new_df_retired_regions_5t[, c("Forced_Fumbles", "is_offense")])

forced_fumbles_all <- forced_fumbles_all[forced_fumbles_all$Forced_Fumbles > 0, ]
forced_fumbles_retired <- forced_fumbles_retired[forced_fumbles_retired$Forced_Fumbles > 0, ]

# Create the histogram
ggplot(forced_fumbles_all, aes(x = Forced_Fumbles, fill = is_offense)) +
    geom_histogram(binwidth = 5, color = "black") +
    labs(x = "Forced_Fumbles", y = "Count", title = "Histogram of Forced_Fumbles all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## right skewed so log transformation

forced_fumbles_log_results <- Log_transform_and_plot(new_df_all_regions_5t, new_df_retired_regions_5t, "Forced_Fumbles")
forced_fumbles_log_results$p2_tran
### still right skewed so reciprocal transformation
forced_fumbles_inv_results <- Inverse_transform_and_plot(new_df_all_regions_5t, new_df_retired_regions_5t, "Forced_Fumbles")
forced_fumbles_inv_results$p2_tran
## weird looking so will try square root transformation
forced_fumbles_sqrt_results <- Sqrt_transform_and_plot(new_df_all_regions_5t, new_df_retired_regions_5t, "Forced_Fumbles")
forced_fumbles_sqrt_results$p2_tran

## trying cube root transformation
forced_fumbles_cubert_results <- CubeRoot_transform_and_plot(new_df_all_regions_5t, new_df_retired_regions_5t, "Forced_Fumbles")
forced_fumbles_cubert_results$p2_tran

## best transformation was the log transformation
new_df_all_regions_6t <- forced_fumbles_log_results$df_all
new_df_retired_regions_6t <- forced_fumbles_log_results$df_retired

#### Completion_Percentage ####
completion_percentage_all <- na.omit(new_df_all_regions_6t[, c("Completion_Percentage", "is_offense")])
completion_percentage_retired <- na.omit(new_df_retired_regions_6t[, c("Completion_Percentage", "is_offense")])

completion_percentage_all <- completion_percentage_all[completion_percentage_all$Completion_Percentage > 0, ]
completion_percentage_retired <- completion_percentage_retired[completion_percentage_retired$Completion_Percentage > 0, ]

# Create the histogram
ggplot(completion_percentage_all, aes(x = Completion_Percentage, fill = is_offense)) +
    geom_histogram(binwidth = 5, color = "black") +
    labs(x = "Completion_Percentage", y = "Count", title = "Histogram of Completion_Percentage all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## fairly normal so no transformation needed

#### Yrds_gained_per_Attempt ####
yrds_gained_per_attempt_all <- na.omit(new_df_all_regions_6t[, c("Yrds_gained_per_Attempt", "is_offense")])
yrds_gained_per_attempt_retired <- na.omit(new_df_retired_regions_6t[, c("Yrds_gained_per_Attempt", "is_offense")])

yrds_gained_per_attempt_all <- yrds_gained_per_attempt_all[yrds_gained_per_attempt_all$Yrds_gained_per_Attempt > 0, ]
yrds_gained_per_attempt_retired <- yrds_gained_per_attempt_retired[yrds_gained_per_attempt_retired$Yrds_gained_per_Attempt > 0, ]

# Create the histogram
ggplot(yrds_gained_per_attempt_all, aes(x = Yrds_gained_per_Attempt, fill = is_offense)) +
    geom_histogram(binwidth = 5, color = "black") +
    labs(x = "Yrds_gained_per_Attempt", y = "Count", title = "Histogram of Yrds_gained_per_Attempt all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## right skewed so log transformation

yrds_gained_per_attempt_log_results <- Log_transform_and_plot(new_df_all_regions_6t, new_df_retired_regions_6t, "Yrds_gained_per_Attempt")
yrds_gained_per_attempt_log_results$p2_tran
## warnings given but shouldnt and function still works be so warnings will ignored. Data becomes more normal after transformation
new_df_all_regions_7t <- yrds_gained_per_attempt_log_results$df_all
new_df_retired_regions_7t <- yrds_gained_per_attempt_log_results$df_retired

#### Passing_TD_per_Game ####
passing_td_per_game_all <- na.omit(new_df_all_regions_7t[, c("Passing_TD_per_Game", "is_offense")])
passing_td_per_game_retired <- na.omit(new_df_retired_regions_7t[, c("Passing_TD_per_Game", "is_offense")])

passing_td_per_game_all <- passing_td_per_game_all[passing_td_per_game_all$Passing_TD_per_Game > 0, ]
passing_td_per_game_retired <- passing_td_per_game_retired[passing_td_per_game_retired$Passing_TD_per_Game > 0, ]

# Create the histogram
ggplot(passing_td_per_game_all, aes(x = Passing_TD_per_Game, fill = is_offense)) +
    geom_histogram(binwidth = 0.1, color = "black") +
    labs(x = "Passing_TD_per_Game", y = "Count", title = "Histogram of Passing_TD_per_Game all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## right skewed so log transformation

passing_td_per_game_log_results <- Log_transform_and_plot(new_df_all_regions_7t, new_df_retired_regions_7t, "Passing_TD_per_Game")
passing_td_per_game_log_results$p2_tran
## becomes bimodal so will try reciprocal transformation

passing_td_per_game_inv_results <- Inverse_transform_and_plot(new_df_all_regions_7t, new_df_retired_regions_7t, "Passing_TD_per_Game")
passing_td_per_game_inv_results$p2_tran
## heavily right skewed so will try square root transformation

passing_td_per_game_sqrt_results <- Sqrt_transform_and_plot(new_df_all_regions_7t, new_df_retired_regions_7t, "Passing_TD_per_Game")
passing_td_per_game_sqrt_results$p2_tran
## fairly normal but will try cube root to see if it is better

passing_td_per_game_cubert_results <- CubeRoot_transform_and_plot(new_df_all_regions_7t, new_df_retired_regions_7t, "Passing_TD_per_Game")
passing_td_per_game_cubert_results$p2_tran

## cube root transformation is best
new_df_all_regions_8t <- passing_td_per_game_cubert_results$df_all
new_df_retired_regions_8t <- passing_td_per_game_cubert_results$df_retired

#### Interceptions_Thrown_per_passing_attempt ####
interceptions_thrown_per_passing_attempt_all <- na.omit(new_df_all_regions_8t[, c("Interceptions_Thrown_per_passing_attempt", "is_offense")])
interceptions_thrown_per_passing_attempt_retired <- na.omit(new_df_retired_regions_8t[, c("Interceptions_Thrown_per_passing_attempt", "is_offense")])

interceptions_thrown_per_passing_attempt_all <- interceptions_thrown_per_passing_attempt_all[interceptions_thrown_per_passing_attempt_all$Interceptions_Thrown_per_passing_attempt > 0, ]
interceptions_thrown_per_passing_attempt_retired <- interceptions_thrown_per_passing_attempt_retired[interceptions_thrown_per_passing_attempt_retired$Interceptions_Thrown_per_passing_attempt > 0, ]

# Create the histogram
ggplot(interceptions_thrown_per_passing_attempt_all, aes(x = Interceptions_Thrown_per_passing_attempt, fill = is_offense)) +
    geom_histogram(binwidth = 0.1, color = "black") +
    labs(x = "Interceptions_Thrown_per_passing_attempt", y = "Count", title = "Histogram of Interceptions_Thrown_per_passing_attempt all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## right skewed so log transformation

interceptions_thrown_per_passing_attempt_log_results <- Log_transform_and_plot(new_df_all_regions_8t, new_df_retired_regions_8t, "Interceptions_Thrown_per_passing_attempt")
interceptions_thrown_per_passing_attempt_log_results$p2_tran
## fairly normal after transformation

new_df_all_regions_9t <- interceptions_thrown_per_passing_attempt_log_results$df_all
new_df_retired_regions_9t <- interceptions_thrown_per_passing_attempt_log_results$df_retired

#### First_downs_per_attempt_percentage ####
first_downs_per_attempt_percentage_all <- na.omit(new_df_all_regions_9t[, c("First_downs_per_attempt_percentage", "is_offense")])
first_downs_per_attempt_percentage_retired <- na.omit(new_df_retired_regions_9t[, c("First_downs_per_attempt_percentage", "is_offense")])

first_downs_per_attempt_percentage_all <- first_downs_per_attempt_percentage_all[first_downs_per_attempt_percentage_all$First_downs_per_attempt_percentage > 0, ]
first_downs_per_attempt_percentage_retired <- first_downs_per_attempt_percentage_retired[first_downs_per_attempt_percentage_retired$First_downs_per_attempt_percentage > 0, ]

# Create the histogram
ggplot(first_downs_per_attempt_percentage_all, aes(x = First_downs_per_attempt_percentage, fill = is_offense)) +
    geom_histogram(binwidth = 10, color = "black") +
    labs(x = "First_downs_per_attempt_percentage", y = "Count", title = "Histogram of First_downs_per_attempt_percentage all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## roughly normal so no transformation needed

#### Been_Sacked ####
been_sacked_all <- na.omit(new_df_all_regions_9t[, c("Been_Sacked", "is_offense")])
been_sacked_retired <- na.omit(new_df_retired_regions_9t[, c("Been_Sacked", "is_offense")])

been_sacked_all <- been_sacked_all[been_sacked_all$Been_Sacked > 0, ]
been_sacked_retired <- been_sacked_retired[been_sacked_retired$Been_Sacked > 0, ]

# Create the histogram
ggplot(been_sacked_all, aes(x = Been_Sacked, fill = is_offense)) +
    geom_histogram(binwidth = 10, color = "black") +
    labs(x = "Been_Sacked", y = "Count", title = "Histogram of Been_Sacked all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## right skewed so log transformation

been_sacked_log_results <- Log_transform_and_plot(new_df_all_regions_9t, new_df_retired_regions_9t, "Been_Sacked")
been_sacked_log_results$p2_tran
## kind of normal but will try reciprocal transformation

been_sacked_inv_results <- Inverse_transform_and_plot(new_df_all_regions_9t, new_df_retired_regions_9t, "Been_Sacked")
been_sacked_inv_results$p2_tran
# no
## try square root transformation
been_sacked_sqrt_results <- Sqrt_transform_and_plot(new_df_all_regions_9t, new_df_retired_regions_9t, "Been_Sacked")
been_sacked_sqrt_results$p2_tran
## still right skewed so will try cube root transformation
been_sacked_cubert_results <- CubeRoot_transform_and_plot(new_df_all_regions_9t, new_df_retired_regions_9t, "Been_Sacked")
been_sacked_cubert_results$p2_tran
## still right skewed so will keep log transformation

new_df_all_regions_10t <- been_sacked_log_results$df_all
new_df_retired_regions_10t <- been_sacked_log_results$df_retired

#### Passer_Rating ####
passer_rating_all <- na.omit(new_df_all_regions_10t[, c("Passer_Rating", "is_offense")])
passer_rating_retired <- na.omit(new_df_retired_regions_10t[, c("Passer_Rating", "is_offense")])

passer_rating_all <- passer_rating_all[passer_rating_all$Passer_Rating > 0, ]
passer_rating_retired <- passer_rating_retired[passer_rating_retired$Passer_Rating > 0, ]

# Create the histogram
ggplot(passer_rating_all, aes(x = Passer_Rating, fill = is_offense)) +
    geom_histogram(binwidth = 10, color = "black") +
    labs(x = "Passer_Rating", y = "Count", title = "Histogram of Passer_Rating all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## right skewed so log transformation

passer_rating_log_results <- Log_transform_and_plot(new_df_all_regions_10t, new_df_retired_regions_10t, "Passer_Rating")
passer_rating_log_results$p2_tran
## warnings given but shouldnt and function still works be so warnings will ignored. Data becomes more normal after transformation

new_df_all_regions_11t <- passer_rating_log_results$df_all
new_df_retired_regions_11t <- passer_rating_log_results$df_retired

#### Yards_per_Reception ####
yards_per_reception_all <- na.omit(new_df_all_regions_11t[, c("Yards_per_Reception", "is_offense")])
yards_per_reception_retired <- na.omit(new_df_retired_regions_11t[, c("Yards_per_Reception", "is_offense")])

yards_per_reception_all <- yards_per_reception_all[yards_per_reception_all$Yards_per_Reception > 0, ]
yards_per_reception_retired <- yards_per_reception_retired[yards_per_reception_retired$Yards_per_Reception > 0, ]

# Create the histogram
ggplot(yards_per_reception_all, aes(x = Yards_per_Reception, fill = is_offense)) +
    geom_histogram(binwidth = 10, color = "black") +
    labs(x = "Yards_per_Reception", y = "Count", title = "Histogram of Yards_per_Reception all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## right skewed so log transformation

yards_per_reception_log_results <- Log_transform_and_plot(new_df_all_regions_11t, new_df_retired_regions_11t, "Yards_per_Reception")
yards_per_reception_log_results$p2_tran
## warnings given but shouldnt and function still works be so warnings will ignored. Data becomes more normal after transformation

new_df_all_regions_12t <- yards_per_reception_log_results$df_all
new_df_retired_regions_12t <- yards_per_reception_log_results$df_retired

#### First_down_receptions_percentage ####

first_down_receptions_percentage_all <- na.omit(new_df_all_regions_12t[, c("First_down_receptions_percentage", "is_offense")])
first_down_receptions_percentage_retired <- na.omit(new_df_retired_regions_12t[, c("First_down_receptions_percentage", "is_offense")])

first_down_receptions_percentage_all <- first_down_receptions_percentage_all[first_down_receptions_percentage_all$First_down_receptions_percentage > 0, ]
first_down_receptions_percentage_retired <- first_down_receptions_percentage_retired[first_down_receptions_percentage_retired$First_down_receptions_percentage > 0, ]

# Create the histogram
ggplot(first_down_receptions_percentage_all, aes(x = First_down_receptions_percentage, fill = is_offense)) +
    geom_histogram(binwidth = 10, color = "black") +
    labs(x = "First_down_receptions_percentage", y = "Count", title = "Histogram of First_down_receptions_percentage all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## fairly normal so no transformation needed

#### Receiving_TDs ####
receiving_tds_all <- na.omit(new_df_all_regions_12t[, c("Receiving_TDs", "is_offense")])
receiving_tds_retired <- na.omit(new_df_retired_regions_12t[, c("Receiving_TDs", "is_offense")])

receiving_tds_all <- receiving_tds_all[receiving_tds_all$Receiving_TDs > 0, ]
receiving_tds_retired <- receiving_tds_retired[receiving_tds_retired$Receiving_TDs > 0, ]

# Create the histogram
ggplot(receiving_tds_all, aes(x = Receiving_TDs, fill = is_offense)) +
    geom_histogram(binwidth = 10, color = "black") +
    labs(x = "Receiving_TDs", y = "Count", title = "Histogram of Receiving_TDs all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## right skewed so log transformation

receiving_tds_log_results <- Log_transform_and_plot(new_df_all_regions_12t, new_df_retired_regions_12t, "Receiving_TDs")
receiving_tds_log_results$p2_tran
# still right skewed so reciprocal transformation

receiving_tds_inv_results <- Inverse_transform_and_plot(new_df_all_regions_12t, new_df_retired_regions_12t, "Receiving_TDs")
receiving_tds_inv_results$p2_tran

## still right skewed so will try square root transformation
receiving_tds_sqrt_results <- Sqrt_transform_and_plot(new_df_all_regions_12t, new_df_retired_regions_12t, "Receiving_TDs")
receiving_tds_sqrt_results$p2_tran

## still right skewed so will try cube root transformation
receiving_tds_cubert_results <- CubeRoot_transform_and_plot(new_df_all_regions_12t, new_df_retired_regions_12t, "Receiving_TDs")
receiving_tds_cubert_results$p2_tran
## still right skewed so will keep log transformation since least skewed

new_df_all_regions_13t <- receiving_tds_log_results$df_all
new_df_retired_regions_13t <- receiving_tds_log_results$df_retired

#### Rushing_Yards_per_Attempt ####
rushing_yards_per_attempt_all <- na.omit(new_df_all_regions_13t[, c("Rushing_Yards_per_Attempt", "is_offense")])
rushing_yards_per_attempt_retired <- na.omit(new_df_retired_regions_13t[, c("Rushing_Yards_per_Attempt", "is_offense")])

rushing_yards_per_attempt_all <- rushing_yards_per_attempt_all[rushing_yards_per_attempt_all$Rushing_Yards_per_Attempt > 0, ]
rushing_yards_per_attempt_retired <- rushing_yards_per_attempt_retired[rushing_yards_per_attempt_retired$Rushing_Yards_per_Attempt > 0, ]

# Create the histogram
ggplot(rushing_yards_per_attempt_all, aes(x = Rushing_Yards_per_Attempt, fill = is_offense)) +
    geom_histogram(binwidth = 5, color = "black") +
    labs(x = "Rushing_Yards_per_Attempt", y = "Count", title = "Histogram of Rushing_Yards_per_Attempt all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## right skewed so log transformation

rushing_yards_per_attempt_log_results <- Log_transform_and_plot(new_df_all_regions_13t, new_df_retired_regions_13t, "Rushing_Yards_per_Attempt")
rushing_yards_per_attempt_log_results$p2_tran
## warnings given but shouldnt and function still works be so warnings will ignored. Data becomes more normal after transformation

new_df_all_regions_14t <- rushing_yards_per_attempt_log_results$df_all
new_df_retired_regions_14t <- rushing_yards_per_attempt_log_results$df_retired

#### First_down_rushes ####
first_down_rushes_all <- na.omit(new_df_all_regions_14t[, c("First_down_rushes", "is_offense")])
first_down_rushes_retired <- na.omit(new_df_retired_regions_14t[, c("First_down_rushes", "is_offense")])

first_down_rushes_all <- first_down_rushes_all[first_down_rushes_all$First_down_rushes > 0, ]
first_down_rushes_retired <- first_down_rushes_retired[first_down_rushes_retired$First_down_rushes > 0, ]

# Create the histogram
ggplot(first_down_rushes_all, aes(x = First_down_rushes, fill = is_offense)) +
    geom_histogram(binwidth = 10, color = "black") +
    labs(x = "First_down_rushes", y = "Count", title = "Histogram of First_down_rushes all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale

## right skewed so log transformation

first_down_rushes_log_results <- Log_transform_and_plot(new_df_all_regions_14t, new_df_retired_regions_14t, "First_down_rushes")
first_down_rushes_log_results$p2_tran
##try reciprocal transformation

first_down_rushes_inv_results <- Inverse_transform_and_plot(new_df_all_regions_14t, new_df_retired_regions_14t, "First_down_rushes")
first_down_rushes_inv_results$p2_tran

## still right skewed so will try square root transformation
first_down_rushes_sqrt_results <- Sqrt_transform_and_plot(new_df_all_regions_14t, new_df_retired_regions_14t, "First_down_rushes")
first_down_rushes_sqrt_results$p2_tran
## still right skewed so will try cube root transformation

first_down_rushes_cubert_results <- CubeRoot_transform_and_plot(new_df_all_regions_14t, new_df_retired_regions_14t, "First_down_rushes")
first_down_rushes_cubert_results$p2_tran

##best transformation was the log transformation
new_df_all_regions_15t <- first_down_rushes_log_results$df_all
new_df_retired_regions_15t <- first_down_rushes_log_results$df_retired

#### TDs_per_Rush_Attempt ####
tds_per_rush_attempt_all <- na.omit(new_df_all_regions_15t[, c("TDs_per_Rush_Attempt", "is_offense")])
tds_per_rush_attempt_retired <- na.omit(new_df_retired_regions_15t[, c("TDs_per_Rush_Attempt", "is_offense")])

tds_per_rush_attempt_all <- tds_per_rush_attempt_all[tds_per_rush_attempt_all$TDs_per_Rush_Attempt > 0, ]
tds_per_rush_attempt_retired <- tds_per_rush_attempt_retired[tds_per_rush_attempt_retired$TDs_per_Rush_Attempt > 0, ]

# Create the histogram
ggplot(tds_per_rush_attempt_all, aes(x = TDs_per_Rush_Attempt, fill = is_offense)) +
    geom_histogram(binwidth = 0.1, color = "black") +
    labs(x = "TDs_per_Rush_Attempt", y = "Count", title = "Histogram of TDs_per_Rush_Attempt all Players") +
    scale_fill_manual(values = c("blue", "red"))  # Use manual color scale
## right skewed so log transformation

tds_per_rush_attempt_log_results <- Log_transform_and_plot(new_df_all_regions_15t, new_df_retired_regions_15t, "TDs_per_Rush_Attempt")
tds_per_rush_attempt_log_results$p2_tran
## normal after transformation

new_df_all_regions_16t <- tds_per_rush_attempt_log_results$df_all
new_df_retired_regions_16t <- tds_per_rush_attempt_log_results$df_retired
####### End of transformations #######


## now that all variables have been transformed I will re-examine correlations
col_for_correl <- c("BMI", "Total_Tackles_trans", "Sacks_trans", "Passes_Defended_trans", "def_TDs_trans", "AverageYrdperINT_trans", "Fumble_recovery_kept_pct", "Forced_Fumbles_trans", "Completion_Percentage", "Yrds_gained_per_Attempt_trans", "Passing_TD_per_Game_trans", "Interceptions_Thrown_per_passing_attempt_trans", "First_downs_per_attempt_percentage", "Been_Sacked_trans", "Passer_Rating_trans", "Yards_per_Reception_trans", "First_down_receptions_percentage", "Receiving_TDs_trans", "Rushing_Yards_per_Attempt_trans", "First_down_rushes_trans", "TDs_per_Rush_Attempt_trans")

Correlation_Matrix_and_Plot(new_df_all_regions_16t, col_for_correl)
## I still have some high correlations but they are not as high as before. I will keep these variables for now and see how the models perform.

#could make sense to split the data into offensive and defensive positions to see if the correlations are still present within the groups of positions


## test correlation within offensive and defensive positions
# Filter for offensive players
offensive_players_all <- new_df_all_regions_16t[new_df_all_regions_16t$is_offense == "True", ]

cols_for_corr_off <- c("BMI", "Total_Tackles_trans", "Fumble_recovery_kept_pct", "Completion_Percentage", "Yrds_gained_per_Attempt_trans", "Passing_TD_per_Game_trans", "Interceptions_Thrown_per_passing_attempt_trans", "First_downs_per_attempt_percentage", "Been_Sacked_trans", "Passer_Rating_trans", "Yards_per_Reception_trans", "First_down_receptions_percentage", "Receiving_TDs_trans", "Rushing_Yards_per_Attempt_trans", "First_down_rushes_trans", "TDs_per_Rush_Attempt_trans")
Correlation_Matrix_and_Plot(offensive_players_all, cols_for_corr_off)

defensive_players_all <- new_df_all_regions_16t[new_df_all_regions_16t$is_offense == "False", ]

cols_for_corr_def <- c("BMI", "Total_Tackles_trans", "Sacks_trans", "Passes_Defended_trans", "def_TDs_trans", "AverageYrdperINT_trans", "Fumble_recovery_kept_pct", "Forced_Fumbles_trans")
Correlation_Matrix_and_Plot(defensive_players_all, cols_for_corr_def)

# looking at the correlations of just the retired players
offensive_players_retired <- new_df_retired_regions_16t[new_df_retired_regions_16t$is_offense == "True", ]
defensive_players_retired <- new_df_retired_regions_16t[new_df_retired_regions_16t$is_offense == "False", ]

View(new_df_retired_regions_16t)
Correlation_Matrix_and_Plot(offensive_players_retired, cols_for_corr_off)
Correlation_Matrix_and_Plot(defensive_players_retired, cols_for_corr_def)


# Perform factorial ANOVA for each performance metric based on Position and hall of fame status of the retired players. I am choosing to look at only the retired players since the collected data is as recent as 2020. To be eligible for hall of fame a player must be retired for 5 years. This will allow the data to be more applicible to current hall of fame status as the players that have retired in 2019 are eligible for the 2024 hall of fame class.



cols_for_fact_anova <- c("BMI", "Total_Tackles_trans", "Sacks_trans", "Passes_Defended_trans", "def_TDs_trans", "AverageYrdperINT_trans", "Fumble_recovery_kept_pct", "Forced_Fumbles_trans", "Completion_Percentage", "Yrds_gained_per_Attempt_trans", "Passing_TD_per_Game_trans", "Interceptions_Thrown_per_passing_attempt_trans", "First_downs_per_attempt_percentage", "Been_Sacked_trans", "Passer_Rating_trans", "Yards_per_Reception_trans", "First_down_receptions_percentage", "Receiving_TDs_trans", "Rushing_Yards_per_Attempt_trans", "First_down_rushes_trans", "TDs_per_Rush_Attempt_trans")


Factorial_ANOVA(offensive_players_retired, cols_for_corr_off)
Factorial_ANOVA(defensive_players_retired, cols_for_corr_def)
Factorial_ANOVA(new_df_retired_regions, cols_for_fact_anova)

cols_for_ancova <- c("Total_Tackles", "Sacks", "Safeties", "Passes_Defended", "def_TDs", "AverageYrdperINT", "Fumble_recovery_kept_pct", "Forced_Fumbles", "Completion_Percentage", "Yrds_gained_per_Attempt", "Passing_TD_per_Game", "Interceptions_Thrown_per_passing_attempt", "First_downs_per_attempt_percentage", "Been_Sacked", "Passer_Rating", "Yards_per_Reception", "First_down_receptions_percentage", "Receiving_TDs", "Rushing_Yards_per_Attempt", "First_down_rushes", "TDs_per_Rush_Attempt")

## perform ANCOVA for each performance metric by position grouped into offense and defense
ANCOVA_Region_HOF_BMI(offensive_players_retired, cols_for_corr_off)
ANCOVA_Region_HOF_BMI(defensive_players_retired, cols_for_corr_def)ANCOVA_Region_HOF_BMI(new_df_retired_regions, cols_for_ancova)