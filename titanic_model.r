library(randomForest)
library(dplyr)
library(caret)
library(readr)



set.seed(101)

data <- read.csv("titanic.csv", stringsAsFactors = FALSE, header = TRUE)



train_data <- sample_frac(data, 0.7)
sid <- as.numeric(rownames(train_data)) # because rownames() returns character
test_data <- data[-sid, ]

nrow(train_data)
sum(is.na(train_data))
sum(is.na(train_data$Age))

table(train_data$Age)

nrow(test_data)
sum(is.na(test_data))
sum(is.na(test_data$Age))

# Filling missing values for Age
median(train_data$Age, na.rm = TRUE)
median(test_data$Age, na.rm = TRUE)
train_data$Age <- ifelse(is.na(train_data$Age), 28, train_data$Age)
test_data$Age <- ifelse(is.na(test_data$Age), 27, test_data$Age)

subset(test_data, is.na(test_data$Fare))
thrd_cl_fr <- subset(test_data, c(test_data$TicketClass == 3, test_data$PortOfEmbarkation == "S"))
m_fare <- round(median(thrd_cl_fr$Fare, na.rm = TRUE), 2)
m_fare
test_data$Fare <- ifelse(is.na(test_data$Fare), m_fare, test_data$Fare)

# Identify and fill missing Embark values
table(train_data$PortOfEmbarkation)
table(test_data$PortOfEmbarkation)

m_PortOfEmbarkation <- subset(train_data, train_data$PortOfEmbarkation == "")
m_PortOfEmbarkation

train_data[train_data$PortOfEmbarkation == "", "PortOfEmbarkation"] <- "S"

# Looking at the Categories of each column
str(test_data)
str(train_data)

# Changing certain colums to factors
train_data$TicketClass <- as.factor(train_data$TicketClass)
test_data$TicketClass <- as.factor(test_data$TicketClass)
train_data$Sex <- as.factor(train_data$Sex)
test_data$Sex <- as.factor(test_data$Sex)
train_data$PortOfEmbarkation <- as.factor(train_data$PortOfEmbarkation)
test_data$PortOfEmbarkation <- as.factor(test_data$PortOfEmbarkation)

# Making sure Survived is a facotr to do and Orgnaizational analysis and not a Regression Analysis.
train_data$Survived <- as.factor(train_data$Survived)

# Creating a Oclass ordinal Category in a new column
train_data <- mutate(train_data, Oclass = TicketClass)
train_data$Oclass <- gsub("1", "first", train_data$Oclass)
train_data$Oclass <- gsub("2", "second", train_data$Oclass)
train_data$Oclass <- gsub("3", "third", train_data$Oclass)
test_data <- mutate(test_data, Oclass = TicketClass)
test_data$Oclass <- gsub("1", "first", test_data$Oclass)
test_data$Oclass <- gsub("2", "second", test_data$Oclass)
test_data$Oclass <- gsub("3", "third", test_data$Oclass)

train_data$Oclass <- factor(train_data$Oclass, order = TRUE, levels = c("first", "second", "third"))
test_data$Oclass <- factor(test_data$Oclass, order = TRUE, levels = c("first", "second", "third"))

str(test_data)
str(train_data)

women <- subset(train_data, train_data$Sex == "female")
count(women)
n_women <- count(women)

sur_women <- subset(women, women$Survived == 1)
count(sur_women)
n_sur_women <- count(sur_women)

n_sur_women / n_women

men <- subset(train_data, train_data$Sex == "male")
count(men)
n_men <- count(men)

sur_men <- subset(men, men$Survived == 1)
count(sur_men)
n_sur_men <- count(sur_men)

n_sur_men / n_men

# Example model
set.seed(123) # for reproducibility
survived_equation <- "Survived ~ TicketClass + Sex + Age + SiblingsSpouses + ParentsChildren + Fare + PortOfEmbarkation"
survived_formula <- as.formula(survived_equation)
titanic_model <- randomForest(formula = survived_formula, data = train_data, ntree = 500, mtry = 3, nodesize = .01 * nrow(train_data))

# Building Features
features_equation <- "TicketClass + Sex + Age + SiblingsSpouses + ParentsChildren + Fare + PortOfEmbarkation"

# Make a Predition
Survived <- predict(titanic_model, newdata = test_data)
table(survived)

# Binding the Passenger ID
PassengerId <- test_data$PassengerId

output_df <- as.data.frame(PassengerId)
output_df$Survived <- Survived
