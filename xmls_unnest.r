library(tidyverse)
library(xml2)
library(lubridate)

null_weeder <- function(x) {
    if (is.null(x)) {
        return(NA)
    } else {
        return(x)
    }
}

rawdata <-
    read_xml("http://www.spartanburgsheriff.org/bookings/jailrostera.xml")

inmates_raw <- xml_find_all(rawdata, "ins") %>%
    as_list()

inmates_df1 <-
    tibble(inmates_raw) %>%
    unnest(inmates_raw) %>%
    rowid_to_column() %>%
    unnest_wider(inmates_raw) %>%
    mutate(across(
        c(nl:nf, csz:racegen, sex:wt, dtin:tmout, urlSAVAN, lastdtout:lasttmout),
        unlist
    )) %>%
    mutate(nm = sapply(
        seq_along(nm),
        function(t) null_weeder(nm[[t]])
    )) %>%
    mutate(nm = unlist(nm)) %>%
    mutate(street = sapply(
        seq_along(street),
        function(t) null_weeder(street[[t]])
    )) %>%
    mutate(street = unlist(street)) %>%
    mutate(across(c(dob, dtin), mdy)) %>%
    separate(bd, sep = " ", into = c("tm", "dt")) %>%
    unite("bd", c(dt, tm), sep = " ") %>%
    mutate(bd = as_datetime(bd, format = "%m/%d/%y %H:%M:%S")) %>%
    mutate(across(c(nl:nf, csz, racegen, nm, street), str_to_title)) %>%
    mutate(race = sapply(
        seq_along(race),
        function(t) null_weeder(race[[t]])
    )) %>%
    mutate(race = unlist(race)) %>%
    mutate(across(c(age, wt), as.numeric))

offenses <-
    inmates_df1 %>%
    select(rowid, ar) %>%
    unnest_longer(ar) %>%
    pivot_wider(names_from = "ar_id", values_from = "ar", values_fn = list) %>%
    select(rowid, of) %>%
    unnest(of) %>%
    unnest_wider(of) %>%
    select(rowid, ol) %>%
    unnest(ol) %>%
    unnest(ol) %>%
    mutate(offense = str_to_title(ol)) %>%
    select(rowid, offense)

inmate_offense <-
    right_join(inmates_df1, offenses, by = "rowid", multiple = "all")
