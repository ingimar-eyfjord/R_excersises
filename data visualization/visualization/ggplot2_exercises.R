# Visualizing Data With ggplot - Exercise Sheet
#---------------------------------------------

library(tidyverse)
library(palmerpenguins)
library(scales)
#------------------------------------------------
# Section 1 - More visualizations on the penguins
#------------------------------------------------
penguins_data <- penguins

# 1.1 Create a scatter plot that shows the relation
# between body_mass_g and flipper_length_mm for the
# three different species

# bill_scatter <- ggplot(data=penguins_data,
#                     mapping=aes(x=bill_length_mm,
#                                 y=bill_depth_mm))+geom_point()

bill_scatter <- penguins_data %>%
    ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
    geom_point(aes(color = species, shape = species)) +
    geom_smooth(method = "lm")


# 1.2 Adapt the plot in the following way
# Change the shape of the points depending on the species
# Change the size of the points to '2'
# Add the following line to change the colors
# +scale_color_manual(values = c("darkorange","darkorchid","cyan4"))


bm_fl <- penguins_data %>% ggplot(aes(x = body_mass_g, y = flipper_length_mm)) +
    geom_point(aes(color = species, shape = species)) +
    geom_smooth()


# 1.3 Create a bar chart that shows the number of penguins for
# each sex

# barplot <- penguins_data %>%
#     group_by(species) %>%
#     summarise(count = n()) %>%
#     ggplot(mapping = aes(y = species, x = count, color = species, fill = species)) +
#     geom_col()

barplot <- penguins_data %>%
    group_by(species, island) %>%
    summarise(count = n()) %>%
    ggplot(mapping = aes(x = species, y = count, fill = island)) +
    geom_col(position = position_dodge2(preserve = "single")) +
    facet_wrap(vars(species))


distribution <- penguins_data %>% ggplot(aes(y = body_mass_g)) +ß
    geom_boxplot()

# 1.3.1 Then create the same in percentage


percentages_column <- penguins_data %>%
    count(species) %>%
    mutate(percentage = n / sum(n)) %>%
    ggplot(aes(x = percentage, y = fct_reorder(species, percentage))) +
    geom_col() +
    labs(y = "", x = "Percentage [%]", title = "Share of speicies") +
    scale_x_continuous(labels = label_percent(accuracy = 1))+theme_minimal(base_size=16)




# 1.3.2 Try to make the following adjustments (use Google for help!)
# Change the theme of the plot to +theme_minimal()
# Modify the scale so it shows percentages (hin: package scales)
# install.packages(scales)





#---------------------------------------------------------
# Section 2: Visualizing the reddit travel survey  data-set
#---------------------------------------------------------

reddit_travel_survey <- read_csv("reddit-travel-survey.csv")

# 2.1 Create an overview of the survey participants demographics
# by creating a bar chart for gender, age-group and education
# use +theme_minimal() and appropriate axis labels for all plots



# 2.2 Have a look at the package patchwork (https://github.com/thomasp85/patchwork)
# What does it do? How can you use it in combination
# with the three previous plots?


# 2.3 Does the importance of photos have an effect on participants
# estimation of their favourite country
# Look at countries that got mentioned five times or more




# 2.4 Do participants with more travel also visit more countries?
