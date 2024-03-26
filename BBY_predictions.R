#' A2 Assignment
#' Purpose: Predictive Modeling BBY 
#' Giovanni Ghedini
#' Feb 01, 2024

library(rpart)
library(rpart.plot)
library(caret)
library(zoo)
library(leaflet)
library(caret)
library(ggplot2)
library(dplyr)
library(tseries)
library(rugarch)
library(plotly)
library(MLmetrics)
library(vtreat)
library(plyr)
library(Metrics)
library(randomForest)
library(stringr)
library(leaflet)
library(mapproj)

options(scixpen=999)

# WD
setwd("~/Desktop/Dual Degree/R/hult_class/Personal_folder/A2_Assignment")


## FUNCTIONS
# 1 if YES and 0 if BLANK or UNKNOWN
convert_to_binary <- function(x) {
  ifelse(x == "Yes", 1, ifelse(x == "", 0, NA))
}

# Function that extract number
extract_number <- function(column_name) {
  as.numeric(gsub("\\D", "", column_name))
}

rmse_fun <- function(actual, predictions){
  # Calculate residuals
  residuals <- actual$yHat - predictions
  # Calculate squared residuals
  squared_residuals <- residuals^2
  # Calculate mean squared error
  mse <- mean(squared_residuals, na.rm = TRUE)
  # Calculate root mean squared error
  rmse_linear_train <- sqrt(mse)
}


# ALL TRAINING DATASET
allTrainingFiles <- list.files(path = '~/Desktop/Dual Degree/R/hult_class/Personal_folder/A2_Assignment/Data',
                               pattern = 'training',
                               full.names = T)

# Load the files and arrange them with a left join
allTrainingDF <- lapply(allTrainingFiles, read.csv)
allTrainingDF <- join_all(allTrainingDF, by='tmpID', type='left')

# ALL TESTING DATASET
allTestingFiles <- list.files(path = '~/Desktop/Dual Degree/R/hult_class/Personal_folder/A2_Assignment/Data',
                               pattern = 'testing',
                               full.names = T)

# Load the files and arrange them with a left join
allTestingDF <- lapply(allTestingFiles, read.csv)
allTestingDF <- join_all(allTestingDF, by='tmpID', type='left')

# ALL PROSEPCTS DATASET
allProspectsFiles <- list.files(path = '~/Desktop/Dual Degree/R/hult_class/Personal_folder/A2_Assignment/Data',
                               pattern = 'prospects',
                               full.names = T)

# Load the files and arrange them with a left join
allProspectsDF <- lapply(allProspectsFiles, read.csv)
allProspectsDF <- join_all(allProspectsDF, by='tmpID', type='left')



### FEATURE ENGINEERING for our 3 main dataset

### Create 1 column with the total number of DONATIONS made by each household
donations <- c("DonatesEnvironmentCauseInHome", "DonatesToCharityInHome", "DonatestoAnimalWelfare", "DonatestoArtsandCulture",
               "DonatestoChildrensCauses", "DonatestoHealthcare", "DonatestoInternationalAidCauses", "DonatestoVeteransCauses",
               "DonatestoHealthcare1", "DonatestoInternationalAidCauses1", "DonatestoWildlifePreservation",
               "DonatestoLocalCommunity", "DonatestoConservativeCauses", "DonatestoLiberalCauses")


## TRAINING DF
# Apply conversion to each selected column (donations)
allTrainingDF[donations] <- lapply(allTrainingDF[donations], convert_to_binary)

# Create new column (Total Donations)
allTrainingDF$tot_donations <- rowSums(allTrainingDF[,donations],na.rm = TRUE)
allTrainingDF$tot_donations

# i have 3954 empty values --> 73.64% of values are valid
sum(allTrainingDF$tot_donations == 0) 
sum(allTrainingDF$tot_donations != 0)/nrow(allTrainingDF)


## TESTING DF
# Apply conversion to each selected column (donations)
allTestingDF[donations] <- lapply(allTestingDF[donations], convert_to_binary)

# Create new column (Total Donations)
allTestingDF$tot_donations <- rowSums(allTestingDF[,donations],na.rm = TRUE)
allTestingDF$tot_donations

# i have 1326 empty values --> 73.48% of values are valid
sum(allTestingDF$tot_donations == 0) 
sum(allTestingDF$tot_donations != 0)/nrow(allTestingDF)


## PROSPECTS DF
# Apply conversion to each selected column (donations)
allProspectsDF[donations] <- lapply(allProspectsDF[donations], convert_to_binary)

# Create new column (Total Donations)
allProspectsDF$tot_donations <- rowSums(allProspectsDF[,donations],na.rm = TRUE)
allProspectsDF$tot_donations

# i have 1609 empty values --> 73.15% of values are valid
sum(allProspectsDF$tot_donations == 0) 
sum(allProspectsDF$tot_donations != 0)/nrow(allProspectsDF)


### Add 1 COLUMN with total PETS
pets_variables <- c("HorseOwner", "CatOwner", "DogOwner", "OtherPetOwner")

## TRAINING DF
# Apply conversion to selected columns (pet)
allTrainingDF[pets_variables] <- lapply(allTrainingDF[pets_variables], convert_to_binary)
# Create new column with total pets
allTrainingDF$tot_pets <- rowSums(allTrainingDF[,pets_variables], na.rm = TRUE)
# Check how many missing --> 35.24% of the data are valid (64.76% is missing)
sum(allTrainingDF$tot_pets != 0)/nrow(allTrainingDF)

## TESTING DF
# Apply conversion to selected columns (pet)
allTestingDF[pets_variables] <- lapply(allTestingDF[pets_variables], convert_to_binary)
# Create new column with total pets
allTestingDF$tot_pets <- rowSums(allTestingDF[,pets_variables], na.rm = TRUE)
# Check how many missing --> 35.32% of the data are valid 
sum(allTestingDF$tot_pets != 0)/nrow(allTestingDF)

## PROSPECTS DF
# Apply conversion to selected columns (pet)
allProspectsDF[pets_variables] <- lapply(allProspectsDF[pets_variables], convert_to_binary)
# Create new column with total pets
allProspectsDF$tot_pets <- rowSums(allProspectsDF[,pets_variables], na.rm = TRUE)
# Check how many missing --> 36.45% of the data are valid 
sum(allProspectsDF$tot_pets != 0)/nrow(allProspectsDF)


## Add 1 Column with number of subscriptions to MAGAZINES made

magazines <- c("FamilyMagazineInHome", "FemaleOrientedMagazineInHome", "ReligiousMagazineInHome",
               "GardeningMagazineInHome", "CulinaryInterestMagazineInHome", "HealthFitnessMagazineInHome",
               "DoItYourselfMagazineInHome", "FinancialMagazineInHome", "InterestinCurrentAffairsPoliticsInHousehold")


## TRAINING DF
# Apply the function extract_number to the specified columns
allTrainingDF[magazines] <- lapply(allTrainingDF[magazines], extract_number)
allTrainingDF$magazines_sub <- rowSums(allTrainingDF[,magazines], na.rm = TRUE)
# Check how many missing --> 41.3% of the data are valid
sum(allTrainingDF$magazines_sub != 0)/nrow(allTrainingDF)

## TESTING DF
# Apply the function extract_number to the specified columns
allTestingDF[magazines] <- lapply(allTestingDF[magazines], extract_number)
allTestingDF$magazines_sub <- rowSums(allTestingDF[,magazines], na.rm = TRUE)
# Check how many missing --> 40.86% of the data are valid
sum(allTestingDF$magazines_sub != 0)/nrow(allTestingDF)

## PROSPECTS DF
# Apply the function extract_number to the specified columns
allProspectsDF[magazines] <- lapply(allProspectsDF[magazines], extract_number)
allProspectsDF$magazines_sub <- rowSums(allProspectsDF[,magazines], na.rm = TRUE)
# Check how many missing --> 41.09% of the data are valid (58.97% is missing)
sum(allProspectsDF$magazines_sub != 0)/nrow(allProspectsDF)

## REPLACE ALL NaN WITH 0
allTrainingDF <- mutate_all(allTrainingDF, ~ifelse(is.na(.), 0, .))
allTestingDF <- mutate_all(allTestingDF, ~ifelse(is.na(.), 0, .))
allProspectsDF <- mutate_all(allProspectsDF, ~ifelse(is.na(.), 0, .))

## sampling the TRAINING dataset into TRAIN (80%) and VALIDATION (20%)
training_idx <- sample(1:nrow(allTrainingDF), size=0.8*nrow(allTrainingDF))
allTraining_train <- allTrainingDF[training_idx,]
allTraining_valid <- allTrainingDF[-training_idx,]



### BUILDING a PLAN ReligiousContributorInHome; PoliticalContributerInHome
informartiveFeatures <- c('ResidenceHHGenderDescription', 'PresenceOfChildrenCode',
                          'HomeOwnerRenter', 'MedianEducationYears', 'NetWorth','Investor', 'BusinessOwner', 'OccupationIndustry',
                          'tot_pets', 'HomeOffice', 'UpscaleBuyerInHome','BuyerofAntiquesinHousehold', 'BuyerofArtinHousehold', 'ComputerOwnerInHome',
                          'tot_donations','Gender', 'Age','state',
                          'HomePurchasePrice', 'LandValue', 'storeVisitFrequency','PropertyType', 'EstHomeValue', 'magazines_sub',
                          'supportsAffordableCareAct')

targetName <- 'yHat'
plan <- designTreatmentsN(dframe      = allTraining_train, 
                          varlist     = informartiveFeatures,
                          outcomename = targetName)
# Apply the plan to all sections of the data
treatedTrain <- prepare(plan, allTraining_train)
treatedValidation <- prepare(plan, allTraining_valid)
treatedTest <- prepare(plan, allTestingDF )
treatedProspects <- prepare(plan, allProspectsDF)



#### MODEL

### LINEAR MODEL
# -- TRAIN
fit_lm <- lm(yHat~., treatedTrain)
summary(fit_lm)                     # Adjusted R-squared:  0.4095

#----- Parsimony 
# First get the variable and p-values
pVals <- data.frame(varNames = names(na.omit(coef(fit_lm))),
                    pValues = summary(fit_lm)$coefficients[,4])
# Determine which variable names to keep 
keeps <- subset(pVals$varNames, pVals$pValues<0.05)
# Remove unwanted columns
treatedTrainParsimony <- treatedTrain[,names(treatedTrain) %in% keeps]
# Append the y-variable
treatedTrainParsimony$yHat <- treatedTrain$yHat
#-----

# Refit linear model - Training
fit_lm_2 <- lm(yHat ~ ., treatedTrainParsimony)
summary(fit_lm_2)                  # Adjusted R-squared:  0.4117 -- 7 variables considered


## DECISION TREE
# -- TRAIN
set.seed(1234)
fit_tree <- rpart(as.factor(yHat) ~.,
                        data = treatedTrain)

## RANDOM FOREST
# -- TRAIN
rf_model <- randomForest(yHat ~ ., data = treatedTrain, ntree = 1000, mtry = 6)
summary(rf_model)


#### PREDICTION

### LINEAR MODEL
# -- TRAIN
linearTrainPredictions <- predict(fit_lm_2, treatedTrainParsimony)
summary(linearTrainPredictions)

# -- VALID
linearValidPredictions <- predict(fit_lm_2, treatedValidation)

# -- TEST
linearTestPredictions <- predict(fit_lm_2, treatedTest)
summary(linearTestPredictions)

# -- PROSPECTS
linearProspPredictions <- predict(fit_lm_2, treatedProspects)
summary(linearProspPredictions)


### DECISION TREE
# -- TRAIN
treeTrainPredictions <- predict(fit_tree,treatedTrain)

# -- VALID
treeValidPredictions <- predict(fit_tree, treatedValidation)
head(treeValidPredictions, 10)


### RANDOM FOREST
# -- TRAIN
rfTrainPerdictions <- predict(rf_model, treatedTrain)
head(rfTrainPerdictions)

# -- VALID
rfValidPredictions <- predict(rf_model, treatedValidation)
head(rfValidPredictions)



#### RMSE

### Linear Model
# -- TRAIN
rmse_lm_2_train <- rmse_fun(treatedTrainParsimony, linearTrainPredictions)
rmse_lm_2_train ## 76.01
# -- VALID
rmse_lm_valid_2 <- rmse_fun(treatedValidation, linearValidPredictions)
rmse_lm_valid_2 ## 93.79
# -- TEST
rmse_lm_2_test <- rmse_fun(treatedTest, linearTestPredictions)
rmse_lm_2_test ## 92.73


### Decision Tree
# -- TRAIN
rmse_tree_train <- rmse_fun(treatedTrain, treeTrainPredictions)
rmse_tree_train # RMSE 314.9595
# -- VALID
rmse_tree_valid <- rmse_fun(treatedValidation, treeValidPredictions)
rmse_tree_valid # RMSE 317.4062

### RANDOM FOREST
# -- TRAIN
rmse_rf_train <- rmse_fun(treatedTrain, rfTrainPerdictions)
rmse_rf_train ## 51.062
# -- VALID
rmse_rf_valid <- rmse_fun(treatedValidation, rfValidPredictions)
rmse_rf_valid ## 93.36



# TABLES WITH PREDICTIONS (if needed)
linear_prediction_table <- data.frame(
  Actual = treatedTrain$yHat,
  Predicted = linearTrainPredictions
)
# TREE -- Validation -- Get the final predicted and actuals
validClass <- data.frame(class = colnames(treeValidPredictions)[max.col(treeValidPredictions)],
                         actual = treatedValidation$yHat)
head(validClass,10)
# Confusion Matrix
confMat_valid <- table(validClass$class,validClass$actual)
confMat_valid



## FINAL TABLE Prospects -- Linear Model (Parsimony)
ProspectsPred_lm <- data.frame(tmpID = allProspectsDF$tmpID, predicted = linearProspPredictions)
write.csv(ProspectsPred_lm, "~/Desktop/Dual Degree/R/hult_class/Personal_folder/A2_Assignment/Submission/ProspectsPred_lm.csv", row.names = FALSE)


### EDA Prospects
full_data_predictions <- cbind(allProspectsDF[informartiveFeatures], linearProspPredictions)
head(full_data_predictions)

summary(ProspectsPred_lm)


full_data_predictions$state <- as.factor(full_data_predictions$state)
# Spending per STATE
spending_per_state <- full_data_predictions %>%
  group_by(state) %>%
  summarize(Tot_spending = sum(linearProspPredictions),
            avg_spending = mean(linearProspPredictions),
            std_dev = sd(linearProspPredictions))

per_state <- aggregate(linearProspPredictions ~ state, data = full_data_predictions, sum)
# Get top states based on total spending
top_states <- per_state[order(per_state$linearProspPredictions, decreasing = TRUE), ]

# top 10 states
top_states <- top_states[order(top_states$linearProspPredictions, decreasing = TRUE), ][1:10, ]

# Plot
# Create a bar plot using ggplot2
ggplot(top_states, aes(x = reorder(state, -linearProspPredictions), y = linearProspPredictions / 1000)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = sprintf("%.2f", linearProspPredictions / 1000)), vjust = -0.5) +
  labs(title = "Top 10 States by Spending Predictions",
       x = "State",
       y = "$ thousands") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()) 


# Create AGE bins
full_data_predictions$Age <- as.numeric(as.character(full_data_predictions$Age))
full_data_predictions_filtered <- full_data_predictions[!is.na(full_data_predictions$Age) & full_data_predictions$Age >= 20 & full_data_predictions$Age <= 80 & full_data_predictions$linearProspPredictions >= 0, ]

# Create a grouped bar chart
ggplot(full_data_predictions_filtered, aes(x = age_group, y = linearProspPredictions, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "white", alpha = 0.7) +
  labs(title = "Linear Prosp Predictions by Gender and Age Group",
       x = "Age Group",
       y = "Linear Prosp Predictions") +
  theme_minimal() +
  theme(legend.position = "top") 

## AGE DISTRIUTION
# line chart
ggplot(full_data_predictions_filtered, aes(x = Age, fill = Gender)) +
  geom_density(alpha = 0.5, position = "identity") +
  labs(title = "Distribution of Ages by Gender",
       x = "Age",
       y = "Density") +
  theme_minimal()

# histogram
ggplot(full_data_predictions_filtered, aes(x = Age, fill = Gender)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
  labs(title = "Distribution of Ages by Gender",
       x = "Age",
       y = "Frequency") +
  theme_minimal()



# Spending by property type
sum_spending_per_type <- full_data_predictions_filtered %>%
  group_by(PropertyType) %>%
  summarise(total_spending = sum(linearProspPredictions))

per_type <- aggregate(linearProspPredictions ~ PropertyType, data = full_data_predictions, sum)
# Sort the data frame 
per_type <- per_type[order(per_type$linearProspPredictions, decreasing = TRUE), ]

ggplot(per_type, aes(x = factor(PropertyType, levels = PropertyType), y = linearProspPredictions / 1000)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = sprintf("%.1f", linearProspPredictions / 1000)), vjust = -0.5, position = position_dodge(width = 0.9)) +  # Add labels in thousands
  labs(title = "Total Spending by PropertyType",
       x = "PropertyType",
       y = "Total Spending (in thousands)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none", legend.title = element_blank())





