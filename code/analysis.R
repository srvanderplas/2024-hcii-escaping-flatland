library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(RSQLite)

db_files <- list.files("data", pattern = "\\.db", full.names = T, recursive = T)
tmp <- purrr::map(db_files, function(x) {
  db <- dbConnect(SQLite(), x)
  tables <- dbListTables(db)
  res <- purrr::map(tables, dbReadTable, conn = db)
  names(res) <- tables
  dbDisconnect(db)
  res
})

data218 <- tibble(
  db = db_files,
  results = map(tmp, ~pluck(., "results")),
  users = map(tmp, ~pluck(., "user")),
  userMat = map(tmp, ~pluck(., "userMatrix", .default = NA))
) %>%
  mutate(results = purrr::map(results, ~mutate(., graphCorrecter = as.character(graphCorrecter))))

res218 <- unnest(data218, "results")

user218 <- unnest(data218, "userMat")
