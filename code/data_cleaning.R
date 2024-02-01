library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(RSQLite)

# We used different databases each semester
db_files <- list.files("data", pattern = "\\.db", full.names = T, recursive = T)

# This pulls out all of the tables for each database file
db_tables <- purrr::map(db_files, function(x) {
  db <- dbConnect(SQLite(), x)
  tables <- dbListTables(db)
  res <- purrr::map(tables, dbReadTable, conn = db)
  names(res) <- tables
  dbDisconnect(db)
  res
})

# Get datasets about each kit
load("stimuli/kits.Rdata") # kitsWithData
load("stimuli/set85data.Rdata") # datasets


trueRatios <- datasets %>%
  mutate(ratio.df = map(data, function(x)(x[!is.na(x[,'IDchr']),4])),
         trueRatio = map(ratio.df, function(x)(x[1,'Height'] / x[2,'Height']))) %>%
  unnest(trueRatio) %>%
  # Remove practice cases
  filter(fileID != 15) %>%
  select(fileID, true_ratio = Height)


# Get tables from each database in columns by table name
data218 <- tibble(
  db = db_files,
  results = map(db_tables, ~pluck(., "results")),
  users = map(db_tables, ~pluck(., "user")),
  userMat = map(db_tables, ~pluck(., "userMatrix", .default = NA))
) %>%
  # This is a NA column in some databases so we have to fix the type
  mutate(results = purrr::map(
    results, ~mutate(., graphCorrecter = as.character(graphCorrecter))))

ed_levels <- c("High School or Less",
               "Some Undergraduate Courses",
               "Undergraduate Degree",
               "Some Graduate Courses",
               "Graduate Degree",
               "Prefer not to answer")

age_levels <- c("Under 19", "19-25", "26-30",
                "31-35", "36-40", "41-45", "46-50",
                "51-55", "56-60", "Over 60",
                "Prefer not to answer")

user218 <- unnest(data218, "users") %>%
  select(-c("results", "userMat"))  %>%
  mutate(across(matches("(?:app|plot|exp)(?:Start|End)[tT]ime"),
                as.POSIXct)) %>%
  mutate(participantID = paste0(nickname, participantUnique)) %>%
  mutate(age = factor(age, levels = age_levels, ordered = T),
         education = factor(education, levels = ed_levels, ordered = T))

# Track users lost with each step
userno <- tibble(step = "orig", n = nrow(user218))

user218 <- user218 %>% filter(!str_detect(participantUnique, "[Tt]est"))
userno <- bind_rows(userno, tibble(step = "remove test cases", n = nrow(user218)))

user218 <- user218 %>% filter(age != 'Under 19')
userno <- bind_rows(userno, tibble(step = "above age of consent", n = nrow(user218)))

user218 <- user218 %>% filter(stat218 == "TRUE")
userno <- bind_rows(userno, tibble(step = "Stat 218", n = nrow(user218)))

user218 <- user218 %>% filter(consent == "TRUE")
userno <- bind_rows(userno, tibble(step = "Consent", n = nrow(user218)))

shape_correct <- data.frame(
  shapeOrder = c(1, 1, 2, 2),
  shapeCorrect = c(T, F, T, F),
  whichIsSmaller = c("Triangle (▲)", "Circle (●)", "Circle (●)", "Triangle (▲)")
)
full_results <- data218 %>%
  select(-c("userMat", "users"))%>%
  unnest("results") %>%
  mutate(participantID = paste0(nickname, participantUnique)) %>%
  mutate(across(matches("(?:app|plot|exp)(?:Start|End)[tT]ime"),
                as.POSIXct)) %>%
  # Keep only results from valid users
  filter(participantID %in% user218$participantID) %>%
  # Add in true ratio information
  left_join(trueRatios, by = 'fileID') %>%
  mutate(log2diff = log2(abs(byHowMuch - true_ratio*100) + 1/8),
         absdiff = (byHowMuch - true_ratio*100)/(true_ratio*100),
         ratioLabel = round(100*true_ratio, 1)) %>%
  arrange(appStartTime) %>%
  filter(!is.na(ratio)) %>%
  filter(whichIsSmaller != "") %>%
  # Identify correct shapes - if NA, the answer was always triangle (== 1)
  mutate(shapeOrder = ifelse(is.na(shapeOrder), 1, shapeOrder)) %>%
  left_join(shape_correct, by = c("shapeOrder", "whichIsSmaller")) %>%
  # Add in standard label order
  mutate(plot = factor(plot, levels = c("2dDigital", "3dStatic", "3dDigital", "3dPrint"),
                       labels = c("2D", "3D Fixed", "3D Render", "3D Print"), ordered = T)) %>%
  # calculate time diff
  mutate(trial_secs = as.numeric(difftime(plotEndTime, plotStartTime, units = "secs")))

unique_user_data <- full_results %>%
  group_by(appStartTime, nickname, participantUnique, kit, db) %>%
  nest(trials = -c(appStartTime, nickname, participantUnique, participantID, kit, db)) %>%
  mutate(ntrials = purrr::map_int(trials, ~nrow(.))) %>%
  filter(ntrials >= 10) %>%
  mutate(avg_trial_time = purrr::map_dbl(trials, ~mean(.$trial_secs)),
         sd_trial_time = purrr::map_dbl(trials, ~sd(.$trial_secs)),
         response_sd = purrr::map_dbl(trials, ~sd(.$byHowMuch)),
         flatline_idx = purrr::map_dbl(trials, ~sum(.$byHowMuch == 50.0)/nrow(.))) %>%
  ungroup() %>%
  mutate(id = row_number()) %>%
  unnest(trials) %>%
  # Remove incorrectly answered trials
  filter(shapeCorrect) %>%
  nest(trials = -c(appStartTime, nickname, participantUnique, participantID, kit, db, ntrials, avg_trial_time, sd_trial_time, response_sd, flatline_idx)) %>%
  mutate(ntrials_correct = purrr::map_int(trials, ~nrow(.)))


user218 <- user218 %>% filter(participantID %in% unique_user_data$participantID)
userno <- bind_rows(userno, tibble(step = "Data exists", n = nrow(user218)))

reasonable_user_data <- unique_user_data %>%
  filter(flatline_idx < 0.5)

user218 <- user218 %>% filter(participantID %in% reasonable_user_data$participantID)
userno <- bind_rows(userno, tibble(step = "Selects 50% less than half the time", n = nrow(user218)))

results <- unnest(reasonable_user_data, trials)


userMat218 <- unnest(data218, "userMat") %>%
  mutate(participantID = paste0(nickname, participantUnique)) %>%
  filter(participantID %in% user218$participantID)


