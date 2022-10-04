library(tidyverse)
library(dplyr)

data1 <- table4a

clean_longer <- data1 %>%
    pivot_longer(cols = -country, names_to = "year", values_to = "cases")

data2 <- table2

clean_wider <- data2 %>%
    pivot_wider(names_from = type, values_from = count)

relig_income_example <- relig_income

relig_income_example %>%
    pivot_longer(cols = -religion, names_to = "income_group", values_to = "count") -> relig_income_example_tidy

us_rent <- us_rent_income
us_rent_income %>%
    pivot_wider(names_from = variable, values_from = c("estimate", "moe")) -> test



specs <- colnames(who_data)
who %>% pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = c("diagnosis", "gender", "age"),
    names_pattern = "new_?(.*)_(.)(.*)",
    names_transform = list(
        gender = ~ readr::parse_factor(.x, levels = c("f", "m")),
        age = ~ readr::parse_factor(
            .x,
            levels = c("014", "1524", "2534", "3544", "4554", "5564", "65"),
            ordered = TRUE
        )
    ),
    values_to = "count",
) -> who_data

