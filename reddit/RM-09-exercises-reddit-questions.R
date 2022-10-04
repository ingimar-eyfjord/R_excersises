library(tidyverse)
library(dplyr)
# Exercises ('Collection, cleaning & transformation w/ R, part 1')
#------------------------------------------------------------------------------------------
#

# For this set of exercises we will be using the results of a survey on travel behavior posted to the 
# Reddit /r/travel/ subreddit. This subreddit (obviously) focuses on travel and the goal of 
# the survey was to collect and analyze information about the travel behavior of its members.
#
# The original questionnaire consisted of two parts, of which part 2 was optional to complete. 
# Part one contained approx. 38 questions (depending on how you count) and was completed by all 800+
# participants. Part two contained an additional 56 questions and was completed by 646 participants. 
#
# You can find the original survey questions here:
#    https://docs.google.com/forms/d/e/1FAIpQLSfy0jyTicyMYk2xAzzR1OH264EsQRnZ6LcF_A5S_aEHdRESGQ/viewform
#
# You can find the original data here:
#    https://goo.gl/Dev1pk
#
# I have filtered out the least interesting questions, so we are left with a smaller survey data set. 
# You can find this filtered data set on Moodle or download it here:
#    http://toinebogers.com/content/teaching/dvir2020/reddit-travel-survey.csv
#
# There are seven exercises belonging to this data set. Good luck! :)

data <- read_csv("reddit-travel-survey.csv")


#------------------------------------------------------------------------------------------
# 1. Read in the travel survey data from file. How many participants and how many variables 
#    does it contain?

nr_rows <- nrow(data)
nr_head <- spec(data)

#------------------------------------------------------------------------------------------
# 2. How many participants live in a country different than their nationality?

# Hint: In addition to looking at the dimensions of a tibble or simply counting by hand, 
# you can also count how many rows a tibble contains by using the count() function.

participants_not_living_own_country <- filter(data, country_of_nationality != country_of_residence)
nr_participants_not_living_own_country <- nrow(participants_not_living_own_country)

#------------------------------------------------------------------------------------------
# 3. Which nationality most frequently lives abroad?
#
# Hint: While we haven't learned to group rows together yet by country of nationality, you 
# can inspect the results to visually identify the most frequent countries. You could then
# filter by the top countries one by one to count which one is the most frequent one.

most_frequent_table <- participants_not_living_own_country %>%
    group_by(country_of_nationality) %>%
    summarise(number = n()) %>%
    arrange(desc(number))

most_frequent_country <- head(most_frequent_table, 1)

#------------------------------------------------------------------------------------------
# 4. What is the most common relationship status among the Scandinavian countries? Let's 
#    define Scandinavia as containing Denmark, Sweden, Norway, Iceland, and Finland.

scandinavia <- c("Norway", "Iceland", "Sweeden", "Finland", "Denmark")

relationship_status_table_common <- data %>%
    group_by(country_of_nationality,country_of_residence, relationship_status) %>%
    summarise(number = n()) %>%
    arrange(desc(number)) %>%
    filter(country_of_nationality %in% scandinavia | country_of_residence %in% scandinavia) %>%
    group_by(relationship_status) %>%
    summarise(sum(number))

#------------------------------------------------------------------------------------------
# 5. A common stereotype is that many Americans do not own a valid passport (or any at all), 
#    whereas, for instance, most Scandinavians do. Which participant group has the highest 
#    percentage of valid passports: the US or the Scandinavian countries?


not <- c("No", "Yes - expired")
yes <- c('Yes - valid')

percentage_valid_passport <- data %>%
    group_by(country_of_nationality, passport) %>%
    summarise(number = n()) %>%
    arrange(desc(number)) %>%
    filter(country_of_nationality %in% scandinavia | country_of_nationality %in% "United States") 


num_us_valid <- percentage_valid_passport %>%
    filter(country_of_nationality == "United States" & passport %in% yes)

num_us_valid <- sum(num_us_valid$number)

num_us_invalid <- percentage_valid_passport %>%
    filter(country_of_nationality == "United States" & passport %in% not)

num_us_invalid <- sum(num_us_invalid$number)

num_scand_valid <- percentage_valid_passport %>%
    filter(country_of_nationality %in%  scandinavia & passport %in% yes)

num_scand_valid <- sum(num_scand_valid$number)

num_scand_invalid <- percentage_valid_passport %>%
    filter(country_of_nationality %in% scandinavia & passport %in% not)

num_scand_invalid <- sum(num_scand_invalid$number)

percentage_valid_scandinavia <- num_scand_invalid / num_scand_valid * 100

percentage_valid_us <- num_us_valid / num_us_invalid * 100

#------------------------------------------------------------------------------------------
# 6. Which participant from the Benelux travels most often per year? 
#
# The Benelux countries consist of Belgium, Luxembourg and the Netherlands. Find 
# out which participant from these three countries has the highest number of 
# travel days per year. How many days do they travel per year and what country 
# are they from?



#------------------------------------------------------------------------------------------
# 7. Find out which participant has the highest percentage of business days traveled. In case 
#    of ties, find the participant with the highest number of business days traveled in total.
#    Also include their age, gender and nationality.
#
# Hint: Be careful, some people have filled out the survey incorrectly and have listed more 
# business travel days than total travel days, so you need to filter these out.


