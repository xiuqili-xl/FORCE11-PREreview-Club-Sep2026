# Goal ----
# Pull data from GitHub repo and recreate some of the figures in the Yin et al manuscript


# Load libraries ----
library(tidyverse)
library(here)


# Import data ----
# directly from GitHub; note we are looking at Commit 49628b9
full_corpus <- read_csv("https://raw.githubusercontent.com/rustlab1/PreprintPaperTracker/49628b97edf308f520fb0680ca7c4c7a4d36bf89/analysis/data/full_corpus_labels.csv")


# Explore data ----
nrow(full_corpus)              # 72,644
head(full_corpus, 10)
glimpse(full_corpus)


# Preprint and published years ----
summary(full_corpus$preprint_date)
## range from 2014-01-13 to 2025-02-23

summary(full_corpus$published_date)

full_corpus %>%
  mutate(preprint_year = year(preprint_date)) %>% 
  count(preprint_year)
# note, the corpus include 101 preprints from 2014 - 2017

full_corpus %>%
  mutate(published_year = year(published_date)) %>% 
  count(published_year)


# Overall change (Figure 1a and 1b) ----
primary_content_graph <- full_corpus %>%
  count(primary_label) %>%
  mutate(percent = n / sum(n) * 100,
         primary_label = factor(primary_label, levels = c("unchanged", "minor", "major"))) %>%
  arrange(primary_label) %>%
  mutate(label_position = cumsum(percent) - 1/2 * percent,
         label_text = paste0(round(percent, 1), "%"))

ggplot(data = primary_content_graph) +
  geom_col(mapping = aes(x = 3, y = percent, fill = primary_label), 
           width = 1, position = position_stack(reverse = TRUE)) +
  geom_label(mapping = aes(x = 3, y = label_position, label = label_text),
            size = 4, fill = "white", alpha = 0.8, linewidth = 0) +
  annotate("text", x = 1.2, y = 0, label = "Primary claim\ncontent change", size = 5.5) +
  scale_x_continuous(limits = c(1.2, 3.5), expand = c(0, 0)) +
  scale_fill_manual(values = c("major" = "salmon", "minor" = "gold", "unchanged" = "palegreen4")) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(plot.caption = element_text(face = "italic")) +
  labs(title = "Figure 1a", 
       caption = "Data source: GitHub repo `full_corpus_labels.csv`\nNumber of claims: 72,644")

ggsave(path = here("graphs"), filename = "figure_1a_main-claim-content-change.png",
       width = 5, height = 4, dpi = 300, unit = "in", bg = "white")  


primary_hedging_graph <- full_corpus %>%
  count(primary_hedging) %>%
  mutate(percent = n / sum(n) * 100,
         primary_hedging = factor(primary_hedging, levels = c("unchanged", "weakened", "strengthened", NA))) %>%
  arrange(primary_hedging) %>%
  mutate(label_position = cumsum(percent) - 1/2 * percent,
         label_text = paste0(round(percent, 1), "%"))

ggplot(data = primary_hedging_graph) +
  geom_col(mapping = aes(x = 3, y = percent, fill = primary_hedging), 
           width = 1, position = position_stack(reverse = TRUE)) +
  geom_label(mapping = aes(x = 3.4, y = label_position, label = label_text),
             size = 4, fill = "white", alpha = 0.8, linewidth = 0) +
  annotate("text", x = 1.2, y = 0, label = "Primary claim\nhedging shift", size = 5.5) +
  scale_x_continuous(limits = c(1.2, 3.5), expand = c(0, 0)) +
  scale_fill_manual(values = c("unchanged" = "grey70", "weakened" = "deepskyblue", "strengthened" = "salmon"),
                    na.value = "grey85") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(plot.caption = element_text(face = "italic")) +
  labs(title = "Figure 1b", 
       caption = "Data source: GitHub repo `full_corpus_labels.csv`\nNumber of claims: 72,644")

ggsave(path = here("graphs"), filename = "figure_1b_main-claim-hedging-shift.png",
       width = 5, height = 4, dpi = 300, unit = "in", bg = "white")  



# By subject field (Figure 1d) ----
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
  geom_col(width = 0.8) +
  scale_x_continuous(expand = 0.01) +
  scale_y_discrete(limits = rev, name = NULL) +
  scale_fill_manual(values = c("major" = "salmon", "minor" = "gold", "unchanged" = "palegreen4")) +
  theme_bw() +
  theme(plot.caption = element_text(face = "italic")) +
  labs(title = "Figure 1D. Main claim content change by field",
       caption = "Data source: GitHub repo `full_corpus_labels.csv`\n Fields with 1500+ preprints")

ggsave(path = here("graphs"), filename = "figure_1d_main-claim-change-by-field.png",
       width = 6, height = 4, dpi = 300, unit = "in")  



# Over the years (Figure 2c)  ----
primary_content_over_time <- full_corpus %>%
  mutate(preprint_year = year(preprint_date)) %>% 
  count(year, primary_label) %>%
  group_by(year) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(year >= 2018,     # remove the 101 preprints from 2014-2017
         year < 2025)      # remove 2025 bc it's not in the graph                 

ggplot(data = primary_content_over_time,
       mapping = aes(x = year, y = percent, group = primary_label, color = primary_label)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("major" = "salmon", "minor" = "gold", "unchanged" = "palegreen4")) +
  theme_bw() +
  theme(plot.caption = element_text(face = "italic")) +
  labs(title = "Figure 2c. Primary claim content change over time",
       x = "Preprint year", y = "% of preprint-publication pairs",
       caption = "Data source: GitHub repo `full_corpus_labels.csv`\nLimit to preprints posted between 2018-2024")

ggsave(path = here("graphs"), filename = "figure_2c_main-claim-change-over-time.png",
       width = 6, height = 4, dpi = 300, unit = "in")  



# By preprint-to-publication time (Figure 2c)  ----
primary_content_by_pub_time <- full_corpus %>%
  mutate(time_to_pub = published_date - preprint_date) %>%
  select(biorxiv_doi, primary_label, preprint_date, published_date, time_to_pub) %>%
  mutate(time_to_pub_tertile = ntile(time_to_pub, 3),
         time_to_pub_tertile = case_when(time_to_pub_tertile == 1 ~ "fast",
                                         time_to_pub_tertile == 2 ~ "medium",
                                         time_to_pub_tertile == 3 ~ "slow"),
         time_to_pub_tertile = factor(time_to_pub_tertile, levels = c("slow", "medium", "fast")))


# median publication time for each group
primary_content_by_pub_time_graph <- primary_content_by_pub_time %>%
  group_by(time_to_pub_tertile) %>%
  mutate(median_to_pub = median(time_to_pub),
         group_label = paste0(time_to_pub_tertile, "\n(", median_to_pub, "d)")) %>%
  ungroup() %>%
  group_by(group_label, primary_label) %>%
  summarize(n = n()) %>%
  ungroup(primary_label) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

ggplot(data = primary_content_by_pub_time_graph,
       mapping = aes(x = group_label, y = percent, fill = primary_label)) +
  geom_col(width = 0.85) +
  scale_x_discrete(name = NULL) +
  scale_fill_manual(values = c("major" = "salmon", "minor" = "gold", "unchanged" = "palegreen4")) +
  theme_bw()+
  theme(plot.caption = element_text(face = "italic")) +
  labs(title = "Figure 2d. Primary claim content change by time to publication",
       caption = "Data source: GitHub repo `full_corpus_labels.csv`\nNumber of pairs: 72,644")
  
ggsave(path = here("graphs"), filename = "figure_2d_main-claim-change-by-time-to-publication.png",
       width = 6, height = 4, dpi = 300, unit = "in")  


