### Example: Reading in a CSV file
library("tidyverse")

data <- read.csv("hockey-stats.csv")

### Example: Reading in an Excel file

heads <- head(data)

### Example: Sorting data

arranged <- arrange(data, desc(Born))

### Example: Selecting columns

names <- select(data, Name)

### Activating the 'tidylog' package

### Example: Piping

### Example: Filtering data with slice()
# Keep only the first three rows

# Keep only the last three rows

# What if you don't know exactly how many rows there are?

### Example: Filtering data with filter()
# All Canadian hockey players.
find_row_one_four_eleven <- slice(data, 1, 4, 11)
canadian <- filter(data, Country == "CAN")
# We can save both sets of players in different tibbles if we want!
# All players who’ve scored at least 700 goals
goals_filter <- filter(data, G > 700)
# All players who’ve scored more goals than assists
goals_more_assist <- filter(data, G > A)
# Which players have more than 1700 points (goals + assists) combined?
goals_more_goal_ass <- filter(data, G + A > 1700)
# Which players are Canadian- or US-born?
can_and_us <- filter(data, Country == "CAN" | Country == "US")
# Which players scored between 700 and 800 goals?
goals_between <- filter(data, G >= 700 & G <= 800)
# Find all playes born in the 1970s
born <- filter(data, str_detect(Born, "197"))
# Find all players called 'Joe' or 'Mark'
data$Names <- rownames(data)
contai <- dplyr::filter(data, grepl("Mark|Joe", Name))
# Get the top 5 hockey players who scored 500 goals or more, sorted by goals.
goals_top <- head(arrange(filter(data, G >= 500), desc(G)), 5)
### Example: Saving to a CSV file.

mean_goals <- mean(data$G, na.rm = TRUE)

group <- data %>%
    mutate(P = G + A) %>%
    group_by(Country, Position) %>%
    summarize(max_points = max(P, na.rm = TRUE)) %>%
    arrange(desc(max_points))
    
# write_csv(goals_top, "./topGoals.csv")
### Example: Saving to an Excel file.
