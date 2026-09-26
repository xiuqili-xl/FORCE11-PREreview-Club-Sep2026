# Goal ----
# This script pulls data from bioRxiv API (/pubs/ endpoint), which is what Yin et al reported using
# The metadata fields include preprint_date and publish_date (https://api.biorxiv.org/),
# which are most relevant fields for looking into Yin et al's inclusion/exclusion criteria


# Load package ----
library(tidyverse)
library(here)

library(rbiorxiv)           # R client for interacting with bioRxiv API
library(rentrez)

library(httr2)
library(purrr)
library(xml2)

library(cld2)         # for checking English language


# Import data ----
# Here, we'll import data from the latest commit
full_corpus <- read_csv("https://raw.githubusercontent.com/rustlab1/PreprintPaperTracker/refs/heads/main/analysis/data/full_corpus_labels.csv")
nrow(full_corpus)            # 72644, as reported in the manuscript


# Get bioRxiv summary data ----
biorxiv_yearly_stats <- biorxiv_summary(interval = "y", format = "df")
biorxiv_yearly_stats

biorxiv_monthly_stats <- biorxiv_summary(interval = "m", format = "df")
biorxiv_monthly_stats


# Get bioRxiv preprint-publish data (/pubs/ end point) ----

## Write a function to extract data using bioRxiv API ----
## This section mirror the section in `biorxiv-date-details.R`
## If not pulling new data from API, jump directly to "Read in extracted data" to use data from 2026-09-18

get_biorxiv_pubs <- function(from, to, server = "biorxiv") {
  fields <- c("biorxiv_doi", "published_doi", "preprint_platform", 
              "preprint_category", "preprint_abstract", "preprint_date", "published_date")
  
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
    url <- sprintf("https://api.biorxiv.org/pubs/%s/%s/%s/%s",
                   server, from, to, cursor)
    
    request(url) %>%
      req_user_agent("R bioRxiv publication metadata request") %>%
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
    return(tibble(biorxiv_doi = character(), published_doi = character(),
                  preprint_platform = character(), preprint_category = character(),
                  preprint_abstract = character(), preprint_date = as.Date(character()), 
                  published_date = as.Date(character())))
  }
  
  remaining_cursors <- if (total > 100L) {
    seq.int(100L, total - 1L, by = 100L)
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
    rename(biorxiv_doi = preprint_doi) %>%
    select(all_of(fields)) %>%
    mutate(
      preprint_category = str_trim(preprint_category),
      preprint_abstract = str_trim(preprint_abstract),
      preprint_date = as.Date(preprint_date),
      published_date = as.Date(published_date)
    )
}

## quick test
pubs_test <- get_biorxiv_pubs(from = "2018-01-01", to = "2018-01-05")
glimpse(pubs_test)


## Use a for loop to extract data on a monthly interval ----
quarter_seq <- seq(from = ymd("2013-10-01"), to = ymd("2026-01-01"), by = "quarter")
quarter_start <- head(quarter_seq, -1)
quarter_end <- tail(quarter_seq, -1) - days(1)

length(quarter_start) == length(quarter_end)         # sanity check

pubs_output_dir <- here("data_biorxiv_pubs")
dir.create(pubs_output_dir, recursive = TRUE, showWarnings = FALSE)

for (i in 1:length(quarter_start)) {
  message("Downloading bioRxiv preprint-pubication data from ", 
          quarter_start[i], " to ", quarter_end[i])
  
  output_file <- file.path(pubs_output_dir, paste0("biorxiv-", quarter_start[i], ".rds"))
  biorxiv_pubs_quarterly <- get_biorxiv_pubs(from = quarter_start[i], to = quarter_end[i])
  
  saveRDS(biorxiv_pubs_quarterly, file = output_file, compress = "gzip", version = 3)
  message("Saved ", nrow(biorxiv_pubs_quarterly), " records to ", basename(output_file))

  Sys.sleep(0.5)          # brief pause between monthly API requests
}


# Read in extracted pubs data ----
biorxiv_pubs_files <- list.files(pubs_output_dir, full.names = TRUE)
biorxiv_pubs_files

pubs_quarter_names <- biorxiv_pubs_files %>%
  basename() %>%
  str_remove("biorxiv-") %>%
  str_remove("-01") %>%
  str_remove("\\.rds$")

## read in all month as a named list
biorxiv_pubs_quarterly_data <- biorxiv_pubs_files %>%
  setNames(pubs_quarter_names) %>%
  purrr::map(readRDS)

summary(biorxiv_pubs_quarterly_data)
glimpse(biorxiv_pubs_quarterly_data)

## access one month
biorxiv_pubs_quarterly_data[["2018-01"]]

## combine all month
for (quart in names(biorxiv_pubs_quarterly_data)) {
  biorxiv_pubs_quarterly_data[[quart]] <- biorxiv_pubs_quarterly_data[[quart]] %>%
    mutate(quarter = quart)           # add a new column to df within lists
}

biorxiv_pubs_combined_data <- bind_rows(biorxiv_pubs_quarterly_data)

write_csv(biorxiv_pubs_combined_data %>% select(-preprint_abstract),
          here("data_processed", "biorxiv_preprint_publication_pairs.csv"))


# Examine preprint level data ----
glimpse(biorxiv_pubs_combined_data)

unique(biorxiv_pubs_combined_data$preprint_platform)     # bioRxiv
summary(biorxiv_pubs_combined_data$preprint_date)        # Min: 2013-11-07; Max: 2026-02-26
summary(biorxiv_pubs_combined_data$published_date)       # Min: 2013-12-10; Max: 2025-12-31

biorxiv_pubs_combined_data %>%
  count(biorxiv_doi) %>%
  filter(n > 1)               
## only 4 out of 162,259 has duplicate doi's
## since all versions of the same bioRxiv preprint share the same doi
## presumably, the /pubs/ endpoint is for specific preprints, accounting for all verisions
## question now: is preprint_date the date for version 1 or later?


## Based on Yin et al's inclusion criteria in the Methods section
## "all bioRxiv records posted between 2018 and 2025 that had a DOI corresponding to 
## a peer reviewed original research article and that were published between January 2021 
## and February 2025 were retrieved."

pairs_within_date_range <- biorxiv_pubs_combined_data %>%  # 162,259 preprint-publication pairs
  filter(preprint_date >= ymd("2018-01-01"), 
         preprint_date <= ymd("2025-12-31")) %>%           # 148,743 pairs
  filter(published_date >= ymd("2021-01-01"), 
         published_date <= ymd("2025-02-28"))              # 86,246 pairs

nrow(pairs_within_date_range)        # 86,246 pairs

## Also based on Yin et al's inclusion criteria in the Methods section
## Pairs were included in the analysis if both abstracts were in English and 
## contained at least 100 characters.

## At the end of this script is a function for pulling abstract from PubMed
## Since we have over 82k dois, this will take a long time (3 queries/second without API key)
## So, we'll just use preprint abstract as an approximate

pairs_meeting_criteria <- pairs_within_date_range %>%
  filter(nchar(preprint_abstract) >= 100) %>%
  filter(cld2::detect_language(preprint_abstract) == "en")

nrow(pairs_meeting_criteria)          # 86,230 pairs


## Venn diagram of preprint doi in pairs_meeting_criteria vs full_corpus ----
preprint_doi_list <- list(biorxiv_api = pairs_meeting_criteria$biorxiv_doi, 
                          manuscript = full_corpus$biorxiv_doi)

ggVennDiagram::ggVennDiagram(preprint_doi_list) +
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF") +
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "Preprint doi from bioRxiv API vs Yin et al manuscript",
       subtitle = "Based on selection criteria reported in Methods")

ggsave(filename = "exploration_preprint-doi_venn-diagram.png", path = here("graphs"),
       width = 5, height = 4, dpi = 300, unit = "in", bg = "white")


## Explore 113 preprints in Yin et al manuscript (full_corpus) but not in 
## bioRxiv API data (pairs_meeting_criteria)
full_corpus %>%
  filter(!biorxiv_doi %in% pairs_meeting_criteria$biorxiv_doi) %>%
  count(year)
## contains the 101 record published between 2014-2017

full_corpus_discrepency <- full_corpus %>%
  filter(!biorxiv_doi %in% pairs_meeting_criteria$biorxiv_doi) %>%
  filter(year >= 2018)


## test if these are in pairs_within_date_range
pairs_within_date_range %>%
  filter(biorxiv_doi %in% full_corpus_discrepency$biorxiv_doi) %>%
  mutate(abstract_word_count = nchar(preprint_abstract),
         abstract_language = cld2::detect_language(preprint_abstract)) %>%
  select(biorxiv_doi, preprint_date, published_doi, published_date,
         abstract_language, abstract_word_count, preprint_abstract, preprint_category) %>%
  view()
## looks like these 12 got filtered out bc of inaccuracies in language assignment


## Potential causes for 16% in biorxiv_api but not in Yin et al manuscript
## (1) bioRxiv continuously update preprint-publication pair
## (2) the biorxiv_api set does not consider versioning, since that's not a variable in /pubs/ endpoint
##     though, different versions of the same bioRxiv preprint share doi, and there is minimal doi repeat
##.    this is unlikely to be a strong factor
## (3) biorxiv_api hasn't been filtered based on published abstract (100 words, English)



## Export preprint_doi dataset ----
preprint_doi <- tibble(
  biorxiv_doi = unique(c(pairs_meeting_criteria$biorxiv_doi, full_corpus$biorxiv_doi))
) %>%
  mutate(review_set = (biorxiv_doi %in% pairs_meeting_criteria$biorxiv_doi),
         manuscript_set = (biorxiv_doi %in% full_corpus$biorxiv_doi))

preprint_doi %>%
  count(review_set, manuscript_set)          # should match numbers in Venn Diagram

write_csv(preprint_doi, here("data_processed", "preprint_doi.csv"))




# BACKUP | Pull abstract from PubMed ----
## First, define function for pulling abstract info from PubMed
get_abstract <- function(doi) {
  # search pubmed for DOI
  search <- entrez_search(db = "pubmed",
                          term = paste0(doi, "[AID]"),   ## [AID] restrict search to article identifiers
                          retmax = 1)
  
  Sys.sleep(0.35)
  
  # no matching article -- return the DOI with missing values.
  if (length(search$ids) == 0) {
    return(data.frame(doi = doi, pmid = NA_character_, abstract = NA_character_))
  }
  
  # extract the PMID of the article
  pmid <- search$ids[1]
  
  # fetch PubMed XML as text, then convert it to an xml2 object
  record_text <- entrez_fetch(db = "pubmed", id = pmid, 
                              rettype = "xml", parsed = FALSE)
  record <- xml2::read_xml(record_text)
  
  # find all abstract sections in the XML record -- there could be more than 1
  abstract_nodes <- xml2::xml_find_all(record, ".//AbstractText")
  
  # combine the abstract sections into one text string. return NA if no abstract
  abstract <- if (length(abstract_nodes) > 0) {
    paste(xml_text(abstract_nodes), collapse = " ")
  } else {
    NA_character_
  }
  
  Sys.sleep(0.35)
  
  # return one row containing DOI, PubMed ID, and abstract.
  data.frame(doi = doi,
             pmid = pmid,
             abstract = abstract)
}


## Pull abstract from PubMed based on published DOI
published_abstracts <- do.call(
  rbind,
  lapply(pairs_within_date_range$published_doi[1:5], get_abstract)
)



# BACKUP | Extract data from bioRxiv API by doi ----
get_biorxiv_pubs_by_doi <- function(biorxiv_doi, server = "biorxiv") {
  server <- match.arg(server, c("biorxiv", "medrxiv"))
  
  doi <- biorxiv_doi %>%
    as.character() %>%
    str_trim() %>%
    str_remove(regex("^https?://(dx\\.)?doi\\.org/", ignore_case = TRUE))
  
  if (any(is.na(doi) | doi == "")) {
    stop("`biorxiv_doi` cannot contain missing or empty values.",
         call. = FALSE)
  }
  
  if (any(!str_detect(doi, regex("^10\\.1101/", ignore_case = TRUE)))) {
    stop("All values must be bioRxiv or medRxiv DOIs beginning with `10.1101/`.",
         call. = FALSE)
  }
  
  fetch_one <- function(current_doi) {
    url <- sprintf(
      "https://api.biorxiv.org/pubs/%s/%s",
      server,
      current_doi
    )
    
    response <- request(url) %>%
      req_user_agent("R bioRxiv publication metadata request") %>%
      req_retry(
        max_tries = 4,
        max_seconds = 300,
        retry_on_failure = TRUE
      ) %>%
      req_timeout(seconds = 90) %>%
      req_perform()
    
    body <- resp_body_json(response, simplifyVector = TRUE)
    records <- body$collection
    
    # Preserve the requested DOI when the API has no publication record.
    if (is.null(records) || length(records) == 0L) {
      return(
        tibble(
          biorxiv_doi = current_doi,
          preprint_date = as.Date(NA),
          published_doi = NA_character_,
          published_date = as.Date(NA),
          api_match = FALSE
        )
      )
    }
    
    records %>%
      as_tibble() %>%
      rename_with(
        ~ "biorxiv_doi",
        any_of("preprint_doi")
      ) %>%
      transmute(
        biorxiv_doi,
        preprint_date = as.Date(preprint_date),
        published_doi,
        published_date = as.Date(published_date),
        api_match = TRUE
      )
  }
  
  doi %>%
    unique() %>%
    map_dfr(\(current_doi) {
      result <- fetch_one(current_doi)
      Sys.sleep(0.5)
      result
    })
}
