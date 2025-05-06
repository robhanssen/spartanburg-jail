library(tidyverse)
library(rvest)
library(xml2)
library(future)

plan(multisession)


crime_cat <- read_csv("crime_categories.csv", show_col_types = FALSE)

url <- "http://www.spartanburgsheriff.org/bookings.php"

all_links <- read_html(url) %>%
    html_elements("a") %>%
    html_attr("href")


arrest_urls <- paste0("http://www.spartanburgsheriff.org/", all_links[str_detect(all_links, "myscripts")])

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

    tibble(name, offense)
}

categorize_offense <- function(offense) {
    offense_lower <- tolower(offense)
    catg <- crime_cat$category[str_detect(offense_lower, crime_cat$search_string)]
    if (length(catg) == 0) catg <- "Unknown"
    else if (length(catg) > 1) catg <- first(catg)
    catg
}

recent_arrest <-
    furrr::future_map_dfr(arrest_urls, read_arrest_url) %>%
    mutate(
        cat_offense = furrr::future_map_chr(offense, categorize_offense)
    )

recent_arrest %>%
    filter(cat_offense == "Unknown")

recent_arrest %>%
    count(cat_offense, sort = TRUE) %>%
    ggplot(aes(y = fct_reorder(cat_offense, n), x = n)) +
    geom_col()