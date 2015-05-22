#code to clean strava runs even more
library(ggplot2)
library(dplyr)

load("strava_df/unclean_str.rda")

# Looking only at runs.
# Removing distances less than 100 because no races less than 100m typically
# Removing distances more than 100000m since most races less than this.
# Moving_time < 72000 means runs of less than 20 hours of movement
# Removing avg_speeds greater than 10.43m/s since this is the world record for 100m
  # Any run should logically be slower than the world record at a small distance.
# Removing runs with a negative moving time.
# Removing max_speeds more than 15m
# Removing elev_gain, avg_hr, max_hr all greater than 500 based off density plots

str_run <- str_df %>% filter(type == "Run", distance >= 100, 
                             distance <= 100000, moving_time < 72000,
                             avg_speed < 10.43, avg_speed > 0.75, moving_time > 0,
                             max_speed < 15, elev_gain < 3000
                             )

str_run %>% ggplot(aes(distance, avg_speed)) + geom_point()

#world record speeds for men based off http://www.iaaf.org/records/by-category/world-records#results-tab-sub=0
# 100m less than 10.43 avg_speed
# 400m less than 9.26
# 1000m less than 7.63
# 2000m less than 7.02
# 5000m less than 6.6
# 10000m less than 6.34
# 20000m less than 5.9
# 25000m less than 5.75
# 30000m less than 5.76
# marathon less than 5.71
# 50000m or more less than 5.5
str_run <- str_run %>%
  filter(!(distance > 400 & avg_speed > 9.26 & sex == "M")) %>% 
  filter(!(distance > 1000 & avg_speed > 7.63 & sex == "M")) %>% 
  filter(!(distance > 2000 & avg_speed > 7.02 & sex == "M")) %>% 
  filter(!(distance > 5000 & avg_speed > 6.60 & sex == "M")) %>%
  filter(!(distance > 10000 & avg_speed > 6.34 & sex == "M")) %>%
  filter(!(distance > 20000 & avg_speed > 5.90 & sex == "M")) %>%
  filter(!(distance > 25000 & avg_speed > 5.75 & sex == "M")) %>%
  filter(!(distance > 30000 & avg_speed > 5.76 & sex == "M")) %>%
  filter(!(distance > 42000 & avg_speed > 5.71 & sex == "M")) %>%
  filter(!(distance > 50000 & avg_speed > 5.5 & sex == "M"))


# world record speeds for women based off http://www.iaaf.org/records/by-category/world-records#results-tab-sub=0
# 100m less than 8.98 avg_speed
# 400m less than 8.40
# 1000m less than 6.71
# 2000m less than 6.04
# 5000m less than 5.64
# 10000m less than 5.64
# 20000m less than 5.54
# 25000m less than 4.784
# 30000m less than 4.72
# marathon less than 4.6 (not using Paula Radcliffe's outlier record)

str_run <- str_run %>%
  filter(!(distance > 100 & avg_speed > 8.98 & sex == "F")) %>% 
  filter(!(distance > 400 & avg_speed > 8.40 & sex == "F")) %>% 
  filter(!(distance > 1000 & avg_speed > 6.71 & sex == "F")) %>% 
  filter(!(distance > 2000 & avg_speed > 6.04 & sex == "F")) %>% 
  filter(!(distance > 5000 & avg_speed > 5.64 & sex == "F")) %>%
  filter(!(distance > 10000 & avg_speed > 5.64 & sex == "F")) %>%
  filter(!(distance > 20000 & avg_speed > 5.54 & sex == "F")) %>%
  filter(!(distance > 25000 & avg_speed > 4.78 & sex == "F")) %>%
  filter(!(distance > 30000 & avg_speed > 4.72 & sex == "F")) %>%
  filter(!(distance > 42000 & avg_speed > 4.6 & sex == "F"))

str_run <- str_run %>% filter((elapsed_time - moving_time) >= 0)


str_run %>% mutate(race = workout_type == 1) %>% arrange(race) %>%
ggplot(aes(distance, avg_speed, color = race)) + geom_point() +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "Distance (m)", y = "Avg_Speed (m/sec)" , title = "Scatterplot of Avg_Speed Against Distance")

#code to see which races were removed from original strava set
str_race <- str_run %>% filter(workout_type == 1)
dirty_race <- str_df %>% filter(type == "Run", workout_type == 1)
d_race<-anti_join(dirty_race, str_race)

