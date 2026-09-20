# Goal ----
# Explore how does time to publication change over the years
# From the Results sectio, "The preprint-to-publication interval also shortened over this period 
# (median 666 days in 2019 to 160 days in 2024)"


# Load packages ----
library(tidyverse)
library(here)


# Import data ----
# manuscript data directly from GitHub; note we are looking at Commit 49628b9
full_corpus <- read_csv("https://raw.githubusercontent.com/rustlab1/PreprintPaperTracker/49628b97edf308f520fb0680ca7c4c7a4d36bf89/analysis/data/full_corpus_labels.csv")
glimpse(full_corpus)

# my API pull: note this goes from end of 2013 to early 2026
biorxiv_pubs <- read_csv(here("data_processed", "biorxiv_preprint_publication_pairs.csv"))
summary(biorxiv_pubs)



# Manuscript corpus ----
time_to_pub_manuscript <- full_corpus %>%
  mutate(preprint_year = year(preprint_date),
         time_to_pub = published_date - preprint_date) %>%
  select(biorxiv_doi, preprint_year, time_to_pub) %>%
  group_by(preprint_year) %>%
  mutate(no_pairs = n(),
         no_pairs_label = paste0(no_pairs, " pairs")) %>%
  ungroup() %>%
  filter(preprint_year >= 2019, preprint_year <= 2024)

time_to_pub_manuscript_stats <- time_to_pub_manuscript %>%
  group_by(preprint_year, no_pairs_label) %>%
  summarise(time_to_pub_median = median(time_to_pub)) %>%
  ungroup() %>%
  # assume data was extracted Apr 2026 
  mutate(time_to_pub_cutoff = ymd("2026-04-15") - ymd(paste0(preprint_year, "-01-01")))


ggplot() +
  geom_histogram(data = time_to_pub_manuscript,
                 mapping = aes(x = time_to_pub, y = after_stat(density)),
                 fill = "grey70") +
  # add median
  geom_vline(data = time_to_pub_manuscript_stats, 
             mapping = aes(xintercept = time_to_pub_median), color = "darkblue") +
  geom_text(data = time_to_pub_manuscript_stats, 
            mapping = aes(x = time_to_pub_median + 50, y = 0.004, label = "median"), 
            color = "darkblue", hjust = 0, size = 4) +
  # add cutoff
  geom_vline(data = time_to_pub_manuscript_stats, 
             mapping = aes(xintercept = time_to_pub_cutoff), color = "brown3") +
  geom_text(data = time_to_pub_manuscript_stats, 
            mapping = aes(x = time_to_pub_cutoff + 50, y = 0.004, label = "cutoff"), 
            color = "brown3", hjust = 0, size = 4) +
  # add number of preprint pairs within each group
  geom_text(data = time_to_pub_manuscript_stats, 
            mapping = aes(x = -150, y = 0.004, label = no_pairs_label), 
            color = "darkmagenta", hjust = 0.3, size = 3) +
  scale_x_continuous(limits = c(-150, 2000)) +
  facet_wrap(~ preprint_year, ncol = 1, strip.position = "right") +
  theme_bw() +
  labs(title = "Preprint to publication delay",
       subtitle = "Data source: Yin et al. corpus, preprint-publication-pairs between 2019 and 2024",
       x = "days")

ggsave(path = here("graphs"), filename = "exploration_time-to-publication_manuscript-data.png",
       width = 6, height = 6, dpi = 300, unit = "in")  




# Data from bioRxiv API (/pubs/ endpoint) ----
time_to_pub_biorxiv <- biorxiv_pubs %>%
  mutate(preprint_year = year(preprint_date),
         time_to_pub = published_date - preprint_date) %>%
  select(biorxiv_doi, preprint_year, time_to_pub) %>%
  group_by(preprint_year) %>%
  mutate(no_pairs = n(),
         no_pairs_label = paste0(no_pairs, " pairs")) %>%
  ungroup() %>%
  filter(preprint_year >= 2019, preprint_year <= 2024)

time_to_pub_biorxiv_stats <- time_to_pub_biorxiv %>%
  group_by(preprint_year, no_pairs_label) %>%
  summarise(time_to_pub_median = median(time_to_pub)) %>%
  ungroup() %>%
  # assume data was extracted Apr 2026 
  mutate(time_to_pub_cutoff = ymd("2026-04-15") - ymd(paste0(preprint_year, "-01-01")))


ggplot() +
  geom_histogram(data = time_to_pub_biorxiv,
                 mapping = aes(x = time_to_pub, y = after_stat(density)),
                 fill = "grey70") +
  # add median
  geom_vline(data = time_to_pub_biorxiv_stats, 
             mapping = aes(xintercept = time_to_pub_median), color = "darkblue") +
  geom_text(data = time_to_pub_biorxiv_stats, 
            mapping = aes(x = time_to_pub_median + 50, y = 0.003, label = "median"), 
            color = "darkblue", hjust = 0, size = 4) +
  # add cutoff
  geom_vline(data = time_to_pub_biorxiv_stats, 
             mapping = aes(xintercept = time_to_pub_cutoff), color = "brown3") +
  geom_text(data = time_to_pub_biorxiv_stats, 
            mapping = aes(x = time_to_pub_cutoff + 50, y = 0.003, label = "cutoff"), 
            color = "brown3", hjust = 0, size = 4) +
  # add number of preprint pairs within each group
  geom_text(data = time_to_pub_biorxiv_stats, 
            mapping = aes(x = -150, y = 0.003, label = no_pairs_label), 
            color = "darkmagenta", hjust = 0.3, size = 3) +
  scale_x_continuous(limits = c(-150, 2000)) +
  facet_wrap(~ preprint_year, ncol = 1, strip.position = "right") +
  theme_bw() +
  labs(title = "Preprint to publication delay",
       subtitle = "Data source: bioRxiv API (/pubs/), preprint-publication-pairs between 2019 and 2024",
       x = "days")

ggsave(path = here("graphs"), filename = "exploration_time-to-publication_biorxiv-api-data.png",
       width = 6, height = 6, dpi = 300, unit = "in")  




