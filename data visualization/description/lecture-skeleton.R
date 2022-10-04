library("tidyverse")
library("readxl")

# Read in the data.
survey <- read_xlsx("instagram-questionnaire.xlsx")

# Fix the variable type of the 'profile_since' column.
# Get a quick summary.
sumary <- summarize(survey, mean(daily_time_in_mins, na.rm = TRUE))



### Mean

# Mean time spent on Instagram in minutes.
mean_daily <- mean(survey$daily_time_in_mins, na.rm = TRUE)
# Mean number of followers, followed, difference.


# Mean time grouped by age and gender.
survey %>%
  group_by(age) %>%
  summarize(mean = mean(daily_time_in_mins, na.rm = TRUE)) -> age_group

# survey  %>%
# group_by(follower_count)  %>%
# summarize(mean = mean(follower_count, na.rm = TRUE)) -> followers
mean <- mean(survey$follower_count, na.rm = TRUE)
### Median

survey %>%
  group_by(gender) %>%
  summarize(median = mean(daily_time_in_mins, na.rm = TRUE)) -> median_daily


### Mode
calculate_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

survey %>%
  group_by(gender) %>%
  summarize(mode = calculate_mode(daily_time_in_mins)) -> mode_daily


# Together
survey %>%
  group_by(gender) %>%
  summarize(
    mean = mean(daily_time_in_mins, na.rm = TRUE),
    median = median(daily_time_in_mins, na.rm = TRUE),
    mode = calculate_mode(daily_time_in_mins),
    max = max(daily_time_in_mins, na.rm = TRUE),
    min = min(daily_time_in_mins, na.rm = TRUE)
  ) -> grouped_sumary

### Range

inbuilt_range <- range(survey$daily_time_in_mins, na.rm = TRUE)

survey %>%
  group_by(gender) %>%
  summarize(range = max(daily_time_in_mins, na.rm = TRUE) -
    min(daily_time_in_mins, na.rm = TRUE)) -> better_range

# MEAN DEVIATION


### Standard deviation

survey %>%
  group_by(gender) %>%
  summarize(
    range = max(daily_time_in_mins, na.rm = TRUE) -
      min(daily_time_in_mins, na.rm = TRUE),
    stdev = sd(daily_time_in_mins, na.rm = TRUE),
    variance = sd(daily_time_in_mins, na.rm = TRUE) * sd(daily_time_in_mins, na.rm = TRUE)
  ) -> standard_deviation

### Correlation
