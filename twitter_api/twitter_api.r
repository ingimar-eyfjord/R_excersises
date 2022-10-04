require(rtweet)
require(tidyverse)

auth_setup_default()
# rstats <- get_timeline('@toinebogers', n=3200)
plot <- ts_plot(rstats)
# highest_tweet <- top(arrange(darstatsta, desc(favorite_count)))

rstats_tweet_users <- users_data(rstats)

user_descriptions <- rstats_tweet_users$description

# related_hastags <- rstats$entities

related_hastags <- rstats %>%
    select(id_str, entities) %>%
    unnest_auto(entities) %>%
    unnest(hashtags)

counting <- related_hastags %>%
    group_by(text) %>%
    count() %>%
    arrange(desc(n))

source <- rstats %>%
    group_by(source) %>%
    summarise(freq = n()) %>%
    arrange(desc(freq))
# (?<=\[)(.*?)(?=\])

favourites <- get_favorites("@toinebogers", n = 3000)

related_fav_hastags <- favourites %>%
    select(id_str, entities) %>%
    unnest_auto(entities) %>%
    unnest(hashtags)

counting_fav <- related_fav_hastags %>%
    group_by(text) %>%
    count() %>%
    arrange(desc(n))
