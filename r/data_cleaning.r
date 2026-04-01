library(here)
library(tidyverse)
library(ggplot2)

# load raw data
netflix <- read.csv(here("data/raw/netflix_titles.csv"))
imdb_netflix <- read.csv(here("data/raw/Netflix Movies_TVShows Dataset.csv"))

# before cleaning
before_structure <- data.frame(
  column = names(netflix),
  type = sapply(netflix, class)
)

before_missing <- data.frame(
  column = names(netflix),
  missing_count = colSums(is.na(netflix)),
  missing_percent = colSums(is.na(netflix)) / nrow(netflix) * 100
)

write.csv(before_structure,
          here("data/processed/before_structure.csv"),
          row.names = FALSE)
write.csv(before_missing,
          here("data/processed/before_missing.csv"),
          row.names = FALSE)

# begin cleaning
imdb_netflix <- imdb_netflix |>
  rename(title = original_title)

imdb_netflix <- imdb_netflix[!duplicated(imdb_netflix$title), ]

clean_title <- function(x) {
  x %>%
    tolower() %>%
    trimws() %>%
    gsub("[^a-z0-9 ]", "", .)
}

netflix$title <- clean_title(netflix$title)
imdb_netflix$title <- clean_title(imdb_netflix$title)

final_data <- merge(netflix, imdb_netflix, by = "title", all.x = TRUE)

dir.create(here("data/processed"), showWarnings = FALSE)

# export cleaned data
structure_df <- data.frame(
  column = names(final_data),
  type = sapply(final_data, class)
)
write.csv(structure_df,
          here("data/processed/final_structure.csv"),
          row.names = FALSE)

missing_df <- data.frame(
  column = names(final_data),
  missing_count = colSums(is.na(final_data)),
  missing_percent = colSums(is.na(final_data)) / nrow(final_data) * 100
)

write.csv(missing_df,
          here("data/processed/final_missing.csv"),
          row.names = FALSE)