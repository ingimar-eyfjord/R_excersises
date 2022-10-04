# Exercises ('Collection, cleaning & transformation w/ R, part 1')
#------------------------------------------------------------------------------------------
#
# For this set of exercises we will be using the well-known Titanic data set This data set 
# contains information the Titanic's passenger manifest as well as whether each passenger
# survived the disaster or not. This data set was original part of a Kaggle data science
# competition on learning to predict someone's chances of surviving the Titanic's maiden
# voyage based on their characteristics.
#
# The original questionnaire consisted of two parts, of which part 2 was optional to complete. 
# Part one contained approx. 38 questions (depending on how you count) and was completed by all 800+
# participants. Part two contained an additional 56 questions and was completed by 646 participants. 
#
# You can find the original data set here:
#    https://www.kaggle.com/c/titanic/overview
#
# I have uploaded the data set to Moodle, so you can download it there.
#
# There are seven exercises belonging to this data set. Good luck! :)

#------------------------------------------------------------------------------------------
# 1. Read in the Titanic data from file. How many passengers and how many variables 
#    does it contain?
library(tidyverse)
library(randomForest)
library(dplyr)
library(caret)
library(readr)

data <- read_csv("./titanic.csv")

nr <- nrow(data)
nh <- ncol(data)

#------------------------------------------------------------------------------------------
# 2. What is the highest ticket fare any passenger paid to sail aboard the Titanic?

max_fari <- max(data$Fare)

#------------------------------------------------------------------------------------------
# 3. What percentage of passengers survived the Titanic disaster?
#
# Hint: In addition to looking at the dimensions of a tibble or simply counting by hand, 
# you can also count how many rows a tibble contains by using the count() function.

percentage <- mean(data$Survived) * 100

#------------------------------------------------------------------------------------------
# 4. A common phrase during evacuations is "Woman and children first!" Was this true on the
#    Titanic? Which gender had the best odds of survival?

# Hint: While we haven't learned to group rows together yet by gender, I would recommend you
# filter the data by gender, one gender at a time. For each separate gender, do what you did
# for question 3.

survival_table <- data %>%
    group_by(Sex, Survived) %>%
    summarise(number = n()) %>%
    mutate(freq = (number / sum(number) * 100))

perc_female <- filter(survival_table, Sex == "female" & Survived == 1)$freq
perc_male <- filter(survival_table, Sex == "male" & Survived == 1)$freq

#------------------------------------------------------------------------------------------
# 5. Did richer passengers survive at a higher rate?
#
# Money can buy you eveyrthing they say. Is that true for survival odds too? How much
# money did the surviving passengers pay for their ticket on average, compared to those 
# who did not survive the trip?
#
# Hint: We will run into two issues here. First, to calculate the mean of a variable, we 
# can use a dedicated function for this, so we need to figure out what that is called. 
# Second, we cannot use this to calculate the mean of a tibble, so we need to get out a 
# vector of values instead. We use the pull() function for this.

first_class <- filter(data, TicketClass == 1)
survival_table_first_class <- first_class %>%
    group_by(Sex, Survived) %>%
    summarise(number = n()) %>%
    mutate(freq = (number / sum(number) * 100))

perc_female_first_class <- filter(survival_table_first_class, Sex == "female" & Survived == 1)$freq
perc_male_first_class <- filter(survival_table_first_class, Sex == "male" & Survived == 1)$freq


#------------------------------------------------------------------------------------------
# 6. Were the people who survived younger on average?
#
# Hint: We cannot simply repeat our recipe from the previous question here or we will run 
# into a problem. Read the error message carefully to try to fix it.




#------------------------------------------------------------------------------------------
# 7. Passengers could embark in one of three different locations: Cherbourg, France (C), 
#    Queenstown, Ireland (Q), or Southampton, UK (S). Did the passengers who boarded in France
#    spend more money on their ticket on average than passengers from the other ports of 
#    embarkation combined?
#
# Hint: Please not that we are comparing C vs. Q + S here. Not C vs. Q vs. S!





# # Example model
# set.seed(123) # for reproducibility
# survived_equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
# survived_formula <- as.formula(survived_equation)
# titanic_model <- randomForest(formula = survived_formula, data = data, ntree = 500, mtry = 3, nodesize = .01 * nrow(data))

# # Building Features
# features_equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

# # Make a Predition
# Survived <- predict(titanic_model, newdata = data)
# table(survived)

# # Binding the Passenger ID
# PassengerId <- data$PassengerId

# output_df <- as.data.frame(PassengerId)
# output_df$Survived <- Survived