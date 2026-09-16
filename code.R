# Load libraries ----
library(tidyverse)
library(here)


# Import data ----
# directly from GitHub; note we are looking at Commit 49628b9
full_corpus <- read_csv("https://raw.githubusercontent.com/rustlab1/PreprintPaperTracker/49628b97edf308f520fb0680ca7c4c7a4d36bf89/analysis/data/full_corpus_labels.csv")


# Explore data ----
nrow(full_corpus)
head(full_corpus, 10)
glimpse(full_corpus)


# Preprint and published years ----
summary(full_corpus$preprint_date)
summary(full_corpus$published_date)

full_corpus %>%
  mutate(preprint_year = year(preprint_date)) %>% 
  count(preprint_year)
# note, the corpus include preprints from 2014, 2015, 2016, and 2017

full_corpus %>%
  mutate(published_year = year(published_date)) %>% 
  count(published_year)


# Subject field ----
full_corpus %>%
  count(preprint_category, name = "no_preprint") %>%
  mutate(percent = no_preprint / sum(no_preprint) * 100) %>%
  arrange(desc(percent))
# matching Suppl Figure 3

revision_by_field <- full_corpus %>%
  count(preprint_category, primary_label, name = "no_preprint") %>%
  group_by(preprint_category) %>%
  mutate(no_category = sum(no_preprint)) %>%
  ungroup() %>%
  filter(no_category >= 1500) %>%
  mutate(percent = no_preprint / no_category * 100)

revision_by_field_order <- revision_by_field %>%
  filter(primary_label == "major") %>%
  arrange(desc(percent))

revision_by_field$preprint_category <- factor(revision_by_field$preprint_category, 
                                              levels = unique(revision_by_field_order$preprint_category))

ggplot(data = revision_by_field,
       mapping = aes(x = percent, y = preprint_category, fill = primary_label)) +
  geom_col(width = 0.85) +
  scale_x_continuous(expand = 0.01) +
  scale_y_discrete(limits = rev, name = NULL) +
  scale_fill_manual(values = c("major" = "salmon", "minor" = "gold", "unchanged" = "palegreen4")) +
  theme_bw() +
  labs(title = "Figure 1D. Revision rate by field")

ggsave(path = here("graphs"), filename = "figure_1d_revision-rate-by-field.png",
       width = 6, height = 4, dpi = 300, unit = "in")  




