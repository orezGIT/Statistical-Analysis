#calling different libraries


library(tidyverse)
library(qqplotr) 
library(corrplot)
library(rcompanion)
library(ggplot2)
library(car)
library(RVAideMemoire)
library(readxl)



#load energy-efficiency data
energy_df <- read_excel("Energy Efficiency Data.xlsx")

#inspect the dataset
tail(energy_df)
head(energy_df)
names(energy_df)
str(energy_df)
summary(energy_df)

#rename column names containing space 
colnames(energy_df) <- ifelse(grepl(" ", colnames(energy_df)),
                              gsub(" ", "_", colnames(energy_df)),
                              colnames(energy_df))

names(energy_df)


#EXPLORATORY DATA ANALYSIS

# create a long format dataset from the energy_df
energy_long <- pivot_longer(energy_df,
                            cols = everything(), 
                            names_to = "attribute", 
                            values_to = "value")

#ploting a histogram for all attributes
ggplot(energy_long, aes(x = value)) + 
  geom_histogram(bins = 30, fill = "skyblue", color = "black") + 
  facet_wrap(~attribute, scales = "free") + 
  labs(title = "Histogram of all Attributes", x = "Values", y = "Frequency")



#checking if the data is normally distributed 

# Loop through each column and generate Q-Q plots
for (col_name in names(energy_df)) {
  # Create the plot for the current column
  variables <- ggplot(energy_df, mapping = aes(sample = .data[[col_name]])) + 
    stat_qq_point(size = 2, color = "red") + 
    stat_qq_line(color = "black") + 
    xlab("Theoretical Quantiles") + 
    ylab("Sample Quantiles") + 
    ggtitle(paste("QQ plot for design attrributes:", col_name)) +
    theme_minimal()
  
  print(variables)  # Display each plot
  readline(prompt = "Press [Enter] to see the next plot...")
  
}



# create a Function to apply Shapiro-Wilk test and return results
normality_test <- function(data) {
  sapply(names(data), function(col_name) {
    col_data <- data[[col_name]] # extract the column values
    shapiro_result <- shapiro.test(col_data)
    c(W_statistic = shapiro_result$statistic, p_value = shapiro_result$p.value) 
  })
}


# Apply the function to for normality on the original data
shapiro_summary <- normality_test(energy_df)

# Log transformation and normality check
log_transform <- energy_df %>% mutate(across(everything(), log))
shapiro_summary_log <- normality_test(log_transform)

# Square root transformation and normality check
sqrt_transform <- energy_df %>% mutate(across(everything(), sqrt))
shapiro_summary_sqrt <- normality_test(sqrt_transform)


# Combine all results into one summary
all_shapiro_results <- list(
  original_data = shapiro_summary,
  log_transformed = shapiro_summary_log,
  sqrt_transformed = shapiro_summary_sqrt
)

all_shapiro_results


# Adding small constant to Glazing Area and Glazing Area Distribution to NaN value
energy_df_log <- energy_df %>%
  mutate(across(c(`Glazing_Area`, `Glazing_Area_Distribution`), ~ . + 0.0001))

# Apply log transformation to the modified dataset
log_transform <- energy_df_log %>% mutate(across(everything(), log))

# Check the results of the log transformation
shapiro_summary_log <- normality_test(log_transform)

shapiro_summary_log



#Correlation Analysis
colnames(energy_df)

# plot a pair plot to visualise heating load against the independent variable
pairs(energy_df[, c(9,1,2,3,4,5, 6,7,8)], lower.panel = NULL, pch = 19, cex = 0.7)

# plot a pair plot to visualise cooling load against the independent variable
pairs(energy_df[, c(10,1,2,3,4,5,6,7,8)], lower.panel = NULL, pch = 19, cex = 0.7) 


#using spearman correlation for non normality assumption
spearman_cor <- cor(energy_df, method = "spearman")
spearman_cor

par(pin = c(10, 10))  # Adjust width and height in inches

#visualising the graph 
corrplot(spearman_cor, 
         method = "number",
         order = "hclust", 
         type = "upper")




#random forest 

# Load required libraries
install.packages("randomForest")
library(randomForest)


# for heating load 
# Split data into train and test sets (80% / 20%)
set.seed(42)  
train_index <- sample(1:nrow(energy_df), 0.8 * nrow(energy_df))
train_data <- energy_df[train_index, ]
test_data <- energy_df[-train_index, ]

# Fit Random Forest model
#heating load
rf_model <- randomForest(Heating_Load ~ Relative_Compactness + Surface_Area + 
                           Wall_Area + Roof_Area + Overall_Height + Orientation,
                         data = train_data, 
                         importance = TRUE, 
                         ntree = 500)

# model summary
rf_model

# Make predictions on test set
predictions <- predict(rf_model, test_data)

# Calculate RMSE
rmse <- sqrt(mean((predictions - test_data$Heating_Load)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))

importance(rf_model)



#for cooling load
# Fit the Random Forest model with Cooling_Load as the target
rf_model_cooling <- randomForest(formula = Cooling_Load ~ Relative_Compactness + Surface_Area + 
                                   Wall_Area + Roof_Area + Overall_Height + Orientation,
                                 data = train_data, 
                                 importance = TRUE, 
                                 ntree = 500)
# Print the model results
print(rf_model_cooling)

# Make predictions on test set
predictions_cooling <- predict(rf_model_cooling, test_data)

# Calculate RMSE for Cooling_Load
rmse_cooling <- sqrt(mean((predictions_cooling - test_data$Cooling_Load)^2))
print(paste("Root Mean Squared Error (RMSE) for Cooling_Load:", rmse_cooling))

# Feature importance
importance(rf_model_cooling)




#hypothesis testing 

#create a new dataframe  
energy_df2 <- energy_df[, c("Orientation", "Heating_Load", "Cooling_Load")]


#convert orientation variable to factor
energy_df2$Orientation <- as.factor(energy_df2$Orientation)

str(energy_df2)

#checking uniqe values 
unique(energy_df2$Orientation)


#renaming the values of the two columns 
levels(energy_df2$Orientation) <- c("N", "E", "S", "W")


energy_df2


# Run Hypotheses test for One-Way ANOVA on heating load using Kruskal-Wallis Test

#Null Hypothesis (H0):
#There is no significant difference in heating load across the different glazing area distributions

#Alternative Hypothesis (Ha):
#There is a significant difference in heating load across the different glazing area distribution categories.

heat_anova_orient <-  kruskal.test(`Heating_Load` ~ Orientation, data = energy_df2)
heat_anova_orient

# Run Hypotheses test for One-Way ANOVA on cooling load using Kruskal-Wallis Test
cool_anova_orient <- kruskal.test(`Cooling_Load` ~ Orientation, data = energy_df2)
cool_anova_orient



# Perform Wilcoxon Rank Sum Test on heating load and cooling load

#compare heating load between two orientation groups "N" & "E"

#Null Hypothesis (H0):
#The distribution of Heating Load for North (N) is equal to the distribution of Heating Load for East (E)

#Alternative Hypothesis (HA):
#The distribution of Heating Load for North (N) is not equal to the distribution of Heating Load for East (E)

north_data <- subset(energy_df2, Orientation == "N")$Heating_Load

east_data <- subset(energy_df2, Orientation == "E")$Heating_Load


#run Wilcoxon Rank Sum Test on north_data and east_data 
wilcox.test(north_data, east_data)



#compare cooling load between two orientation groups "S" & "W"

#Null Hypothesis (H0):
#The distribution of Cooling Load for south (s) is equal to the distribution of Heating Load for west (w)

#Alternative Hypothesis (HA):
#The distribution of cooling Load for south (s) is not equal to the distribution of Heating Load for west (w)


south_data <- subset(energy_df2, Orientation == "S")$Cooling_Load

west_data <- subset(energy_df2, Orientation == "W")$Cooling_Load


#run Wilcoxon Rank Sum Test on west_data and south_data 
wilcox.test(south_data, west_data)

