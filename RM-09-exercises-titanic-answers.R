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

# Answer: 891 passengers and 12 variables

library(tidyverse)
titanic <- read_csv("titanic.csv")

no_of_rows <- nrow(titanic)
no_of_columns <- ncol(titanic)





#------------------------------------------------------------------------------------------
# 2. What is the highest ticket fare any passenger paid to sail aboard the Titanic?

# Answer: 512 GBP (I presume)

titanic %>% 
  # The ticket prices are contained in the Fare column, which is the only one we need to
  # solve this problem, so let's first select that column.
  select(Fare) %>% 
  
  # To figure out what the highest fare is, we need to sort this column in descending 
  # order by ticket price.
  arrange(desc(Fare)) %>% 
  
  # We can either simply look up the ticket price from the top of this table or we can 
  # use the head() function to select the top row for the answer.
  head(1)





#------------------------------------------------------------------------------------------
# 3. What percentage of passengers survived the Titanic disaster?
#
# Hint: In addition to looking at the dimensions of a tibble or simply counting by hand, 
# you can also count how many rows a tibble contains by using the count() function.

# Answer: 342 passengers or 38.4%

# How many passengers do we have in total? We can use the nrow() function for this.
# Since we will be using this number in a calculation, let's save it to a variable.
total_count <- nrow(titanic)

# Whether someone survived or not, is in the 'Survived' column. If there is a '1' in there,
# that passenger survived. If it's a '0', then they perished. We can use the filter() 
# function to keep only the passengers who survived. After that, we count how many rows
# there are in that filtered tibble, and save it to a variable.
survived_count <- filter(titanic, Survived == 1) %>% nrow()

# The percentage of survivors is found by dividing the survived count by the total count 
# and multiplying it by 100.
survived_count / total_count * 100





#------------------------------------------------------------------------------------------
# 4. A common phrase during evacuations is "Woman and children first!" Was this true on the
#    Titanic? Which gender had the best odds of survival?
#
# Hint: While we haven't learned to group rows together yet by gender, you can inspect the 
# results to visually identify the most frequent countries. You could then
# filter by the top countries one by one to count which one is the most frequent one.

# Answer: 74.2% of women survived, but only 19.0% of men, so women had the best survival odds.

# How many female passengers are there? We can find out by using filter() and only keeping
# those that have the value 'female' in the 'Sex' column.
women_total <- filter(titanic, Sex == "female") %>% nrow()

# How many female passengers survived? These are two conditions that both need to be true.
# We can combine conditions using the AND operator ('&').
women_survived <- filter(titanic, Sex == "female" & Survived == 1) %>% nrow()

# What is the percentage?
women_survived / women_total * 100


# For men it's the same, but with a different gender value.
men_total <- filter(titanic, Sex == "male") %>% count()
men_survived <- filter(titanic, Sex == "male" & Survived == 1) %>% count()
men_survived / men_total * 100





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

# Answer: Surviving passengers paid 48.4 on average, while the others paid 22.1, so money
#         does seem to matter.

titanic %>% 
  # Keep only the survivors.
  filter(Survived == 1) %>% 
  
  # For this subset of the data, we now need only the fare, so select this column.
  select(Fare) %>% 
  
  # Here, we pull the contents of the filtered and selected tibble into a vector. We need 
  # to do this, because the mean() function only works on vectors, not tibbles.
  pull() %>%       
  
  # Calculate the mean of this vector of fare values.
  mean()

# Do the same thing for the passengers who perished.
titanic %>% 
  filter(Survived == 0) %>% 
  select(Fare) %>% 
  pull() %>% 
  mean()





#------------------------------------------------------------------------------------------
# 6. Were the people who survived younger on average?
#
# Hint: We cannot simply repeat our recipe from the previous question here or we will run 
# into a problem. Read the error message carefully to try to fix it.

# Answer: Surviving passengers were 28.3 years old on average, while passengers who died
#         were 30.6 on average. The answer is therefore yes.

# A first attempt to solve this could look like this:
filter(titanic, Survived == 1) %>% 
  select(Age) %>% 
  pull() %>% 
  mean()       # This will result in a mean of 'NA', because the variable contains NA values


# The way around the NA issue is like this:
filter(titanic, Survived == 1) %>% 
  select(Age) %>% 
  pull() %>% 
  mean(na.rm = TRUE)     # By telling the function to ignore these NA values, we can get the true mean

filter(titanic, Survived == 0) %>% 
  select(Age) %>% 
  pull() %>% 
  mean(na.rm = TRUE)





#------------------------------------------------------------------------------------------
# 7. Passengers could embark in one of three different locations: Cherbourg, France (C), 
#    Queenstown, Ireland (Q), or Southampton, UK (S). Did the passengers who boarded in France
#    spend more money on their ticket on average than passengers from the other ports of 
#    embarkation combined?
#
# Hint: Please not that we are comparing C vs. Q + S here. Not C vs. Q vs. S!

# Answer: Yes, the average French fare was 60.0 while the average for the other passengers 
#         was 25.6.

filter(titanic, PortOfEmbarkation == "C") %>% 
  select(Fare) %>% 
  pull() %>%
  mean()

# For the other ports, we can either extend our filter conditions...
filter(titanic, PortOfEmbarkation == "Q" | PortOfEmbarkation == "S") %>% 
  select(Fare) %>% 
  pull() %>%
  mean()
  
# ...or we can do the opposite of Cherbourg...
filter(titanic, PortOfEmbarkation != "C") %>% 
  select(Fare) %>% 
  pull() %>%
  mean()

# ...or we can define a vector of the relevant values...
other_ports <- c("Q", "S")
filter(titanic, PortOfEmbarkation %in% other_ports) %>% 
  select(Fare) %>% 
  pull() %>%
  mean()
