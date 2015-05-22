library(RJSONIO)
library(dplyr)
# function to import strava data from StravaSample1 json files

files<- paste("strava_df/StravaSample1/",1:1000, ".json", sep = "")
js_list <- lapply(files, function(i) fromJSON(i, nullValue = NA))
id_len<-sapply(js_list, length)
str_list<-lapply(js_list, function(i) do.call(rbind, i))
str_df <- as.data.frame(do.call(rbind, str_list), stringsAsFactors = FALSE)

#work to wrangle data into proper types/classes

str_df <- tbl_df(str_df)

coln <- c("latitude", "longitude", "elapsed_time", "moving_time", "distance", "elev_gain", "max_speed", "avg_hr", "max_hr", "avg_cadence")
str_df[coln] <- lapply(str_df[coln], as.numeric)
str_df$type = as.factor(as.character(str_df$type))
str_df$workout_type = as.factor(as.character(str_df$workout_type))
str_df$name = as.character(str_df$name)
str_df$description <- as.character(str_df$description)
str_date <- strsplit(as.character(str_df$start_date_local), " ")
str_df$start_date <- as.Date(sapply(str_date, function(i) i[1]))
str_df$start_time <- sapply(str_date, function(i) i[2])
str_df$has_streams <- as.logical(str_df$has_streams)
str_df$start_date_local <- NULL
str_df$id <- as.factor(rep(1:1000, times = id_len))
str_df$avg_speed <- str_df$distance/str_df$moving_time

runner_info <- read.csv("C:/Users/Sally/Desktop/incomplete_code/strava/Efforts1/Efforts/anonAthleteInfo_1000.csv")

#filling in NA's for sex based on name
runner_info$sex[which(is.na(runner_info$sex))] <- c(rep("M", 15), "F", rep("M", 6), "F", "M")

str_df$sex <- runner_info$sex[as.numeric(as.character(str_df$id))]


save(str_df, file = "unclean_str.rda")





