library(here)
library(tidyverse)
library(ggplot2)
dir.create(here("data/processed"), showWarnings = FALSE)

# load raw data
netflix <- read.csv(here("data/raw/netflix_titles.csv"))
imdb_netflix <- read.csv(here("data/raw/Netflix Movies_TVShows Dataset.csv"))
cat("ORIGINAL DATASET SUMMARY\n")
cat("Netflix dataset rows:", nrow(netflix), "\n")
cat("IMDb dataset rows:", nrow(imdb_netflix), "\n")

netflix[netflix == ""] <- NA
imdb_netflix[imdb_netflix == ""] <- NA

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

dup_count <- sum(duplicated(imdb_netflix$title))
cat("Duplicate titles in IMDb dataset BEFORE cleaning:", dup_count, "\n")

# Remove duplicates
imdb_netflix <- imdb_netflix[!duplicated(imdb_netflix$title), ]

cat("Row count BEFORE cleaning:", nrow(netflix), "\n")

clean_title <- function(x) {
  x %>%
    tolower() %>%
    trimws() %>%
    gsub("[^a-z0-9 ]", "", .)
}

netflix$title <- clean_title(netflix$title)
imdb_netflix$title <- clean_title(imdb_netflix$title)

netflix$title[netflix$title == ""] <- NA
imdb_netflix$title[imdb_netflix$title == ""] <- NA
netflix <- netflix[!is.na(netflix$title), ]
imdb_netflix <- imdb_netflix[!is.na(imdb_netflix$title), ]

netflix <- netflix[!duplicated(netflix), ]

netflix$duration_num <- suppressWarnings(
  as.numeric(gsub("[^0-9]", "", netflix$duration))
)

# remove TV show durations because they default to 1
netflix$duration_num[netflix$type == "TV Show"] <- NA

netflix$country[netflix$country == ""] <- NA

netflix$country <- sapply(strsplit(netflix$country, ","), `[`, 1)
netflix$country <- trimws(netflix$country)

netflix$date_added <- as.Date(netflix$date_added, format = "%B %d, %Y")

final_data <- merge(netflix, imdb_netflix, by = "title", all.x = TRUE)
cat("Row count AFTER cleaning:", nrow(final_data), "\n")

write.csv(final_data,
          here("data/processed/final_data.csv"),
          row.names = FALSE)

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