library(tidyverse)
library(googlesheets4)
library(janitor)
library(glue)
library(here)

# Authenticate Google Sheets
gs4_auth()

# Get `data_sheet_id`
source(here(".secrets.R"))

consumption_df <- read_sheet(data_sheet_id, sheet = "Consumption")
consumption_df %>% 
  clean_names %>% 
  identity -> consumption_df

glimpse(consumption_df)

consumption_df %>% 
  filter(drink_type == "Water") %>% 
  count(volume_in_ounces)

consumption_df %>% 
  mutate(date = date(timestamp)) %>% 
  filter(drink_type == "Water") %>% 
  group_by(date) %>% 
  summarise(daily_water_vol = sum(volume_in_ounces, na.rm = T)) %>% 
  ggplot(aes(date, daily_water_vol)) +
  geom_line()

phys_df <- read_sheet(data_sheet_id, sheet = "Physiology")

phys_df %>% 
  arrange(Timestamp) %>% 
  # Get date from timestamp
  mutate(date = date(Timestamp)) %>% 
  # Dedupe dates, taking min of weights measured
  summarise(wt = min(`Weight in Pounds`), .by = date) %>% 
  # Fill in missing dates
  complete(date = seq.Date(min(date), max(date), by = "day")) %>% 
  # ID streak info
  mutate(
    # Get value to determine streak of
    wt_measured = !is.na(wt),
    # Streak scaffolding
    lagged = lag(wt_measured),
    start = wt_measured != lagged,
    start = case_when(
      is.na(start) ~ TRUE,
      TRUE ~ start
    ),
    # ID each streak
    streak_id = cumsum(start)
  ) %>%
  # Get cumulative streak length
  group_by(streak_id) %>% 
  mutate(streak = row_number()) %>% 
  # Including negative values for missed streak days
  mutate(streak = streak * ifelse(wt_measured, 1, -1)) %>% 
  # Collapse date records/rows into streak records/rows
  summarise(
    start_date = min(date),
    end_date = max(date),
    streak_length = n()*sign(min(streak))
  ) %>% 
  identity -> streak_df

max_streak <- max(streak_df$streak_length)
worst_streak <- min(streak_df$streak_length)
cur_streak <- streak_df %>% 
  slice_tail(n = 1) %>% 
  pull(streak_length)

streak_assessment_str <- "Max streak: {max_streak}.\nWorst streak: {worst_streak}.\nCurrent streak: {cur_streak}."
glue(streak_assessment_str)

view(streak_df)
