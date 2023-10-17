library(tidyverse)
library(lubridate)
setwd("~/student-files/22f-pstat197a")

# import raw responses
# raw <- read_csv('intake-survey-responses-raw.csv')
# view(raw)
# 
# output column names to create metadata file DO NOT RE-EXECUTE
# tibble(question.text = colnames(raw)) %>%
#   write_csv(file = "survey-metadata.csv")

# read in metadata for variable name replacement
metadata <- read_csv('survey-metadata.csv')

# read in responses
responses_raw <- read_csv('ds-capstone-intake-F23.csv', 
                          col_names = pull(metadata, variable.name),
                          skip = 1) %>%
  mutate(response.id = row_number())

# separate by survey section
personal_info <- responses_raw %>% 
  select(response.id, starts_with('p.')) %>%
  rename_with(~gsub('p.', '', .x))

background_raw <- responses_raw %>% 
  select(response.id, starts_with('b.')) %>%
  rename_with(~gsub('b.', '', .x))

interest_raw <- responses_raw %>%
  select(response.id, starts_with('pr.')) %>%
  rename_with(~gsub('pr.', '', .x))

# clean for distribution to class by removing identifiable info
background_raw %>%
  # coerce string of classes taken into individual binary indicators
  separate_rows(updv.clas, sep = ';') %>%
  mutate(value = 'yes') %>%
  pivot_wider(names_from = updv.clas, 
              values_from = value, 
              values_fill = "no") %>%
  # drop long text (privacy)
  select(-ends_with('.txt')) %>%
  # shorten proficiency descriptions
  mutate(across(ends_with('.prof'), 
                ~str_sub(.x, start = 1, end = 3))) %>%
  # combine research experience (for privacy) drop distinction between ds/nonds
  mutate(across(contains('rsrch'), function(.x){.x == 'Yes'})) %>%
  mutate(rsrch = dsrsrch + othrsrch > 0) %>%
  select(-dsrsrch, -othrsrch) %>%
  # filter by consent to share
  filter(consent == 'Yes') %>%
  # export
  write_csv('background-clean.csv')

interest_raw %>%
  # drop long text (privacy)
  select(-ends_with('.txt')) %>%
  # shorten project type descriptions
  mutate(type = str_sub(jtype, start = 24, end = 26)) %>%
  mutate(type = if_else(type == '', 'both', type)) %>%
  select(-jtype) %>%
  # filter by consent
  filter(consent == 'Yes') %>%
  # export
  write_csv('interest-clean.csv')
