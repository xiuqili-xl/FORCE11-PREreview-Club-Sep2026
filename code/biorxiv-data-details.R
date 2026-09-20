# Goal ----
# This script pulls data from bioRxiv API (/details/ endpoint) 
# Compared to the /pubs/ endpoint that Yin et al reported on using,
# this endpoint provides more information about versioning, etc.
# So it's useful for looking into what preprints are included in or excluded from the analysis
# Downside: extracting data takes time...


# Load package ----
library(tidyverse)
library(here)

library(rbiorxiv)           # R client for interacting with bioRxiv API
library(httr2)
library(purrr)


# Get bioRxiv summary data ----
biorxiv_yearly_stats <- biorxiv_summary(interval = "y", format = "df")
biorxiv_yearly_stats

biorxiv_monthly_stats <- biorxiv_summary(interval = "m", format = "df")
biorxiv_monthly_stats



# Get bioRxiv preprint level data (/details/ end point) ----

## Write a function to extract data using bioRxiv API ----
## Reason: discovered an incompatibility between rbiorxiv 0.2.2 with the current bioRxiv API as of 2026-09-18
## If not pulling new data from API, jump directly to "Read in extracted data" to use data from 2026-09-18

get_biorxiv_data <- function(from, to, server = "biorxiv") {
  fields <- c("title", "doi", "date", "version", "type", 
              "license", "category", "published", "server")
  
  # validate arguments before constructing the URL
  from_date <- as.Date(from)
  to_date <- as.Date(to)
  
  if (is.na(from_date) || is.na(to_date)) {
    stop("`from` and `to` must use YYYY-MM-DD format.", call. = FALSE)
  }
  
  if (from_date > to_date) {
    stop("`from` must not be later than `to`.", call. = FALSE)
  }
  
  server <- match.arg(server, c("biorxiv", "medrxiv"))
  
  build_request <- function(cursor) {
    url <- sprintf("https://api.biorxiv.org/details/%s/%s/%s/%s",
                   server, from, to, cursor)
    
    request(url) %>%
      req_user_agent("R bioRxiv metadata request") %>%
      req_retry(max_tries = 4, max_seconds = 300, retry_on_failure = TRUE) %>%
      req_timeout(seconds = 90) %>%
      req_throttle(capacity = 2, fill_time_s = 1, realm = "biorxiv-api")
  }
  
  # first request determines the total number of records
  first_response <- build_request(0L) %>%
    req_perform()
  
  first_page <- first_response %>%
    resp_body_json(simplifyVector = TRUE)
  
  total <- as.integer(first_page$messages$total[[1]])
  
  if (is.na(total) || total == 0L) {
    return(tibble(title = character(), doi = character(), date = as.Date(character()),
                  version = integer(), type = character(), license = character(),
                  category = character(), published = character(), server = character()))
  }
  
  remaining_cursors <- if (total > 30L) {
    seq.int(30L, total - 1L, by = 30L)
  } else {
    integer()
  }
  
  if (length(remaining_cursors) > 0L) {
    requests <- map(remaining_cursors, build_request)
    
    responses <- req_perform_parallel(requests, max_active = 2, 
                                      on_error = "stop", progress = TRUE)
    remaining_pages <- map(
      responses,
      \(response) {
        response %>%
          resp_body_json(simplifyVector = TRUE) %>%
          pluck("collection")
      }
    )
  } else {
    remaining_pages <- list()
  }
  
  c(list(first_page$collection), remaining_pages) %>%
    bind_rows() %>%
    select(all_of(fields)) %>%
    mutate(date = as.Date(date),
           version = as.integer(version))
}

## quick test
test <- get_biorxiv_data(from = "2018-01-01", to = "2018-01-05")
glimpse(test)


## Use a for loop to extract data on a monthly interval ----
monthly_seq <- seq(from = ymd("2013-06-01"), to = ymd("2026-01-01"), by = "month")
month_start <- monthly_seq[1: length(monthly_seq)-1]
month_end <- (monthly_seq - 1)[2: length(monthly_seq)]

length(month_start) == length(month_end)         # sanity check

output_dir <- here("data_biorxiv_details")

for (i in 1:length(month_start)) {
  message("Downloading bioRxiv data from ", month_start[i], " to ", month_end[i])
  
  output_file <- file.path(output_dir, paste0("biorxiv-", month_start[i], ".rds"))
  biorxiv_monthly_data <- get_biorxiv_data(from = month_start[i], to = month_end[i])
  
  saveRDS(biorxiv_monthly_data, file = output_file, compress = "gzip", version = 3)
  message("Saved ", nrow(biorxiv_monthly_data), " records to ", basename(output_file))
  
  Sys.sleep(0.5)          # brief pause between monthly API requests
}


# Read in extracted data ----
biorxiv_monthly_files <- list.files(output_dir, full.names = TRUE)
biorxiv_monthly_files

month_names <- biorxiv_monthly_files %>%
  basename() %>%
  str_remove("biorxiv-") %>%
  str_remove("-01") %>%
  str_remove("\\.rds$")

## read in all month as a named list
biorxiv_monthly_data <- biorxiv_monthly_files %>%
  setNames(month_names) %>%
  purrr::map(readRDS)

summary(biorxiv_monthly_data)
glimpse(biorxiv_monthly_data)

## access one month
biorxiv_monthly_data[["2017-08"]]

## combine all month
for (mon in names(biorxiv_monthly_data)) {
  biorxiv_monthly_data[[mon]] <- biorxiv_monthly_data[[mon]] %>%
    mutate(month = mon)           # add a new column to df within lists
}

biorxiv_combined_data <- bind_rows(biorxiv_monthly_data)


# Examine preprint level data ----
glimpse(biorxiv_combined_data)

unique(biorxiv_combined_data$version)            # 1 - 25
unique(biorxiv_combined_data$type)               # will need to clean up if to use
unique(biorxiv_combined_data$server)             # only bioRxiv

## No of new vs updated preprints per month
biorxiv_combined_data %>%
  mutate(upload = if_else(version == 1, "new", "update")) %>%
  count(upload, month) %>%
  pivot_wider(id_cols = "month", names_from = "upload", values_from = "n") %>%
  # join stats retrieved with `rbiorxiv` and test if the number of new papers match
  left_join(biorxiv_monthly_stats %>% select(month, new_papers), by = "month") %>%
  mutate(match_stat = (new == new_papers)) %>%
  view()         # match_stats all TRUE

## so... new papers are marked version == 1
## note, different versions of the same bioRxiv preprint share the same doi
## so to pull original preprint date from details dataset, we should filter for version == 1



# Examine preprint_doi from biorxiv-data-pubs.R ----
# This df contains doi from preprints from (1) Yin et al full_corpose, or 
# (2) meet the Yin et al selection criteria in our analysis
preprint_doi <- read_csv(here("data_processed", "preprint_doi.csv"))

## Join with other datasets ----
preprint_doi_joined <- preprint_doi %>%
  left_join(biorxiv_combined_data %>% filter(version == 1) %>% select(doi, date, version, published), 
            by = join_by("biorxiv_doi" == "doi")) %>%
  mutate(upload = if_else(version == 1, "new", "update"))

## Investigate versioning ----
preprint_doi_joined %>%
  count(review_set, manuscript_set, upload)
## so... all preprints in preprint_doi can be linked to a new upload as identified by version == 1 in `/pubs/`







