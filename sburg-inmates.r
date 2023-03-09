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
    tibble(
        lastname, firstname, age, race, sex,
        ht, wt, booking, crime, street, city
    ) %>%
        mutate(across(everything(), unlist))
}

inmate_data <-
    map_dfr(seq_along(inmates), ~ extract_data(inmates, .x)) %>%
    mutate(race = fct_lump_prop(factor(race), .1)) %>%
    separate(city, c("town", "state_zip"), sep = ", ") %>%
    separate(state_zip, c("state", "zipcode"), sep = " ") %>%
    mutate(across(c(lastname, firstname, street, town), str_to_title)) %>%
    mutate(across(c(sex, state, town), factor)) %>%
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

#
# in Glenlake?
#

crossing(
        gl = read_csv("sources/glstreets.csv") %>% pull(streetname),
        inm = inmate_data %>%
            filter(town == "Boiling Springs") %>%
            pull(street)
    ) %>%
    mutate(in_glenlake = str_detect(inm, gl)) %>%
    filter(in_glenlake) %>%
    inner_join(inmate_data, by = c("inm" = "street")) %>%
    select(lastname, firstname, street = inm, town, crime) %>%
    knitr::kable()


short_crime <- function(crime_long, max_len = 30) {
    paste0(
        substr(crime_long, 1, max_len),
        ifelse(nchar(crime_long) > max_len, "...", "")
    )
}

which_town <- function(data, town_req, state_req = "SC") {
    data %>%
        filter(town == town_req, state == state_req) %>%
        select(firstname, lastname, street, crime, time_in_jail) %>%
        unite("name", c(lastname, firstname), sep = ", ") %>%
        mutate(crime = short_crime(crime)) %>%
        arrange(-time_in_jail)
}

# Boiling Springs inmates
which_town(inmate_data, "Boiling Springs") %>% knitr::kable()
# Inman inmates
which_town(inmate_data, "Inman") %>% knitr::kable()
# Chesnee inmates
which_town(inmate_data, "Chesnee") %>% knitr::kable()

map_dfr(
    c("Boiling Springs", "Inman", "Chesnee"),
    ~ which_town(inmate_data, .x)
) %>%
    arrange(-time_in_jail) %>%
    mutate(across(time_in_jail, ~ . * 30)) %>%
    filter(time_in_jail <= 7) %>%
    knitr::kable()

inmate_data %>%
    select(firstname, lastname, street, crime, time_in_jail) %>%
    unite("name", c(lastname, firstname), sep = ", ") %>%
    mutate(crime = short_crime(crime)) %>%
    arrange(-time_in_jail) %>%
    mutate(across(time_in_jail, ~ . * 30)) %>%
    filter(time_in_jail <= 3) %>%
    knitr::kable()

inmate_data %>%
    filter(str_detect(crime, "shop") | str_detect(crime, "goods")) %>%
    select(lastname, firstname, age, race, street, town, crime) %>%
    arrange(age) %>% View()

inmate_data %>%
    count(crime, sort = TRUE) %>% View()

# where do most criminals come from?
inmate_data %>%
    group_by(town, state) %>%
    summarise(n = n(), .groups = "drop") %>%
    slice_max(n, n = 20) %>%
    mutate(city = interaction(town, state)) %>%
    filter(!is.na(city)) %>%
    ggplot() +
    aes(y = fct_reorder(city, n), x = n) +
    # coord_cartesian(xlim = c(0, 500)) +
    scale_x_continuous(limits = c(0, 450)) +
    ggbreak::scale_x_break(c(60, 350)) +
    labs(y = NULL, x = "Number of inmates") +
    geom_col()



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

bmi_lines <-
    crossing(
        ht = seq(1.4, 2.1, .1),
        bmi = seq(15, 40, 5)
    ) %>%
    mutate(wt = bmi * ht^2)

bmi_labels <-
    bmi_lines %>%
    filter(ht == max(ht))


inmate_data %>%
    ggplot() +
    aes(x = ht, y = wt, color = sex) +
    geom_point() +
    stat_ellipse() +
    geom_line(
        data = bmi_lines,
        lty = 3,
        color = "gray50",
        aes(
            group = factor(bmi),
            color = NULL
        )
    ) +
    geom_label(
        data = bmi_labels,
        aes(
            x = ht, y = wt,
            label = bmi,
            color = NULL
        )
    ) +
    scale_color_manual(values = c("F" = "pink", "M" = "dodgerblue")) +
    labs(x = "Height (in m)", y = "Weight (in kg)") +
    theme(legend.position = "none")

inmate_data %>%
    group_by(sex, race) %>%
    summarize(
        n = n(),
        across(c(ht, wt, bmi), ~ median(.x, na.rm = TRUE))
    ) %>%
    mutate(bmi_m = wt / ht^2)