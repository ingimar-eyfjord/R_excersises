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




#------------------------------------------------------------------------------------------
# 1. Read in the travel survey data from file. How many participants and how many variables 
#    does it contain?
library(tidyverse)
travel <- read_csv("reddit-travel-survey.csv")
travel

# Answer: 858 participants and 42 variables





#------------------------------------------------------------------------------------------
# 2. How many participants live in a country different than their nationality?
#
# Hint: In addition to looking at the dimensions of a tibble or simply counting by hand, 
# you can also count how many rows a tibble contains by using the count() function.

# Answer: 140 participants
filter(travel, country_of_nationality != country_of_residence) %>% count()





#------------------------------------------------------------------------------------------
# 3. Which nationality most frequently lives abroad?
#
# Hint: While we haven't learned to group rows together yet by country of nationality, you 
# can inspect the results to visually identify the most frequent countries. You could then
# filter by the top countries one by one to count which one is the most frequent one.

# Answer: United States at 43.
filter(travel, country_of_nationality != country_of_residence) %>% 
  arrange(country_of_nationality) %>% 
  select(country_of_nationality) %>% View()

# UK: 12 cases
filter(travel, country_of_nationality != country_of_residence) %>% 
  arrange(country_of_nationality) %>% 
  select(country_of_nationality) %>% filter(country_of_nationality == "United Kingdom") %>% 
  count()

# Germany: 4 cases
filter(travel, country_of_nationality != country_of_residence) %>% 
  arrange(country_of_nationality) %>% 
  select(country_of_nationality) %>% filter(country_of_nationality == "Germany") %>% 
  count()

# US: 43 cases
filter(travel, country_of_nationality != country_of_residence) %>% 
  arrange(country_of_nationality) %>% 
  select(country_of_nationality) %>% filter(country_of_nationality == "United States") %>% 
  count()





#------------------------------------------------------------------------------------------
# 4. What is the most common relationship status among the Scandinavian countries? Let's 
#    define Scandinavia as containing Denmark, Sweden, Norway, Iceland, and Finland.

# Answer: Most people are single (n=17)
scandinavian_countries <- c("Denmark", "Sweden", "Norway", "Iceland", "Finland")
filter(travel, country_of_nationality %in% scandinavian_countries) %>% 
  select(country_of_nationality, relationship_status) %>% 
  arrange(relationship_status) %>% View()

# Single: 17 cases
filter(travel, country_of_nationality %in% scandinavian_countries) %>% 
  select(country_of_nationality, relationship_status) %>% 
  arrange(relationship_status) %>% 
  filter(relationship_status == "Single") %>% 
  count()

# Long-term partnership: 15 cases
filter(travel, country_of_nationality %in% scandinavian_countries) %>% 
  select(country_of_nationality, relationship_status) %>% 
  arrange(relationship_status) %>% 
  filter(relationship_status == "Long-term partnership") %>% 
  count()





#------------------------------------------------------------------------------------------
# 5. A common stereotype is that many Americans do not own a valid passport (or any at all), 
#    whereas, for instance, most Scandinavians do. Which participant group has the highest 
#    percentage of valid passports: the US or the Scandinavian countries?

# Answer: 97.0% of Scandinavians had a valid passport and 96.5% of Americans. Of course, US 
# participants of a travel-focused subreddit are not likely to be representative for the US 
# population :)
filter(travel, country_of_nationality %in% scandinavian_countries | country_of_nationality == "United States") %>% 
  select(country_of_nationality, passport) %>% 
  arrange(country_of_nationality, passport) %>% View()

# Scandinavian countries: 32 of 33 rows remaining (96.970%)
filter(travel, country_of_nationality %in% scandinavian_countries) %>% 
  select(passport) %>% filter(passport == "Yes - valid") %>% 
  count()

# US: 413 of 428 rows remaining (96.495%)
filter(travel, country_of_nationality == "United States") %>% 
  select(passport) %>% filter(passport == "Yes - valid") %>% count()





#------------------------------------------------------------------------------------------
# 6. Which participant from the Benelux travels most often per year? 
#
# The Benelux countries consist of Belgium, Luxembourg and the Netherlands. Find 
# out which participant from these three countries has the highest number of 
# travel days per year. How many days do they travel per year and what country 
# are they from?

# Answer: A participant from the Netherlands travels 90 days per year.
benelux_countries <- c("Belgium", "Luxembourg", "Netherlands")
filter(travel, country_of_nationality %in% benelux_countries) %>% 
  select(country_of_nationality, travel_days_per_year) %>% 
  arrange(travel_days_per_year) %>% tail(1)





#------------------------------------------------------------------------------------------
# 7. Find out which participant has the highest percentage of business days traveled. In case 
#    of ties, find the participant with the highest number of business days traveled in total.
#    Also include their age, gender and nationality.
#
# Hint: Be careful, some people have filled out the survey incorrectly and have listed more 
# business travel days than total travel days, so you need to filter these out.

# Answer: A 22-29-year-old male participant from India traveled 100% for busines, 75 days in total.
travel %>% select(age, gender, country_of_nationality, travel_days_business, travel_days_per_year) %>%  
  mutate(business_share = travel_days_business / travel_days_per_year) %>% 
  filter(travel_days_per_year >= travel_days_business) %>% 
  arrange(desc(business_share), desc(travel_days_business)) %>%
  head(1)
