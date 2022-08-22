library(tidyverse)
library(lubridate)
library(xml2)
library(ggridges)
theme_set(theme_light())

rawdata <-
    read_xml("http://www.spartanburgsheriff.org/bookings/jailrostera.xml")

inmates_raw <- xml_find_all(rawdata, "ins") %>%
    as_list()

inmates <- inmates_raw[[1]]

extract_data <- function(data, index) {
    lastname <- data[index]$`in`$nl
    firstname <- data[index]$`in`$nf
    age <- data[index]$`in`$age
    race <- data[index]$`in`$race
    sex <- data[index]$`in`$sex
    ht <- data[index]$`in`$ht
    wt <- data[index]$`in`$wt
    booking <- data[index]$`in`$bd
    crime <- tolower(data[index]$`in`$ar$of$ol)
    street <- data[index]$`in`$street
    city <- data[index]$`in`$csz
    tibble(lastname, firstname, age, race, sex,
           ht, wt, booking, crime, street, city) %>%
        mutate(across(everything(), unlist))
}

inmate_data <-
    map_dfr(seq_along(inmates), ~ extract_data(inmates, .x)) %>%
    mutate(race = case_when(
        race == "W" ~ "White",
        race == "B" ~ "Black",
        TRUE ~ "Other"
    )) %>%
    separate(city, c("town", "state_zip"), sep = ", ") %>%
    separate(state_zip, c("state", "zipcode"), sep = " ") %>%
    mutate(across(c(lastname, firstname, street, town), str_to_title)) %>%
    mutate(across(c(race, sex, state, town), factor)) %>%
    separate(booking, c("time", "date"), sep = " ") %>%
    mutate(booking = mdy_hms(paste(date, time), tz = "EST")) %>%
    mutate(time_in_jail = (now() - booking) / dmonths(1)) %>%
    mutate(ht = str_remove(ht, "\"")) %>%
    separate(ht, c("feet", "inches"), sep = "'") %>%
    mutate(across(c(time_in_jail, age, wt, feet:inches), as.numeric)) %>%
    mutate(ht = 0.01 * (feet * 30.54 + inches * 2.54)) %>%
    mutate(wt = wt / 2.2) %>%
    mutate(bmi = (wt / ht^2)) %>%
    select(-feet, -inches, -date, -time)

save(inmate_data, file = "Rdata/inmates.RData")

summary(inmate_data)

# where do most criminals come from?
inmate_data %>%
    group_by(town, state) %>%
    summarise(n = n(), .groups = "drop") %>%
    slice_max(n, n = 20) %>%
    mutate(city = interaction(town, state)) %>%
    filter(!is.na(city)) %>%
    ggplot() +
    aes(y = fct_reorder(city, n), x = n) +
    geom_col()


which_town <- function(data, town_req, state_req = "SC") {
    data %>%
        filter(town == town_req, state == state_req) %>%
        select(firstname, lastname, street, crime, time_in_jail) %>%
        unite("name", c(lastname, firstname),  sep = ", ") %>%
        arrange(-time_in_jail) %>%
        knitr::kable()
}

# Boiling Springs inmates
which_town(inmate_data, "Boiling Springs")
# Inman inmates
which_town(inmate_data, "Inman")
# Chesnee inmates
which_town(inmate_data, "Chesnee")

# by race and gender
inmate_data %>%
    group_by(race, sex) %>%
    summarise(n = n()) %>%
    ggplot() +
    aes(x = race, y = sex, fill = n) +
    geom_tile()

# distribution of jail time by race, gender
inmate_data %>%
    ggplot() +
    aes(x = time_in_jail, y = race) +
    facet_wrap(~sex) +
    geom_density_ridges(alpha = .5) +
    scale_x_log10(
        label = scales::number_format(accuracy = .1),
        breaks = 10^(-2:3),
        limit = c(.01, NA)
    )