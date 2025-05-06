library(tidyverse)
library(rvest)
library(xml2)

crime_cat <- read_csv("sources/crime_categories.csv", show_col_types = FALSE)

url <- "http://www.spartanburgsheriff.org/bookings.php"

height_conversion <- function(ht_ft) {
    ht_ft <- str_remove_all(ht_ft, '\\"')
    ht_vec <- sapply(str_split(ht_ft, pattern = "\'"), as.numeric)
    round(sum(ht_vec * c(30.54, 2.54)), digits = 2)
}

weight_conversion <- function(wt) {
    round(as.numeric(wt) / 2.2, digits = 2)
}

read_arrest_url <- function(url) {
    arrest <- read_html(url)
    name_element <- arrest %>%
        html_elements("h1") %>%
        as_list()
    name <- str_to_title(str_trim(name_element[[1]][[1]]))

    offense <- arrest %>%
        html_element("table") %>%
        html_table() %>%
        mutate(Offense = str_to_title(Offense)) %>%
        pull(Offense)

    details_list <- html_elements(arrest, "span") %>% html_text()
    address <- str_to_title(details_list[2])
    date_of_birth <- mdy(details_list[3])
    height <- height_conversion(details_list[5])
    weight <- weight_conversion(details_list[6])
    datetime_arrested <- mdy_hm(details_list[7])
    race_gender <- str_to_title(details_list[4])
    race_gender <- sapply(str_split(race_gender, "/"), str_trim)
    race <- race_gender[1]
    gender <- race_gender[2]
    tibble(name, offense, address, date_of_birth, height, weight, race, gender, datetime_arrested)
}

categorize_offense <- function(offense) {
    offense_lower <- tolower(offense)
    catg <- crime_cat$category[str_detect(offense_lower, crime_cat$search_string)]
    if (length(catg) == 0) {
        catg <- "Unknown"
    } else if (length(catg) > 1) {
        catg <- first(catg)
    }
    catg
}


all_links <- read_html(url) %>%
    html_elements("a") %>%
    html_attr("href")


arrest_urls <- paste0("http://www.spartanburgsheriff.org/", all_links[str_detect(all_links, "myscripts")])

recent_arrest <-
    purrr::map_dfr(arrest_urls, read_arrest_url) %>%
    mutate(
        cat_offense = purrr::map_chr(offense, categorize_offense)
    )

#
if (!file.exists("sources/recent_arrests.csv")) {
    write_csv(recent_arrest, "sources/recent_arrests.csv")
} else {
    read_csv("sources/recent_arrests.csv", show_col_types = FALSE) %>%
        bind_rows(recent_arrest) %>%
        distinct() %>%
        write_csv("sources/recent_arrests.csv")
}
