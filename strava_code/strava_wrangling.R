library(ggplot2)
library(lubridate)
library(dplyr)

load("strava_df/new_str_run.RData")
runner_info <- read.csv("C:/Users/Sally/Desktop/data_analysis/strava_eda/strava_df/anonAthleteInfo_1000.csv")


ggplot(str_run, aes(distance, moving_time)) + 
  geom_point() + 
  facet_wrap(~ workout_type, ncol = 2) + 
  ggtitle("Scatterplots of Moving Time Against Distance for Each Run Type")

#counting amount of workout_type
ggplot(str_run, aes(workout_type)) + 
  geom_bar() + ggtitle("Barplot of Counts for Each Run Type")

#histogram of number of runs for each runner
id_count<-str_run %>% group_by(id) %>% summarise(num_runs = n())
id_count %>% filter(num_runs < 1000) %>% 
  ggplot(aes(num_runs)) + 
    geom_histogram() +
    ggtitle("Histogram of Number of Runs for Each Runner\n(Greater than 1000 Removed)")



#getting years and months of start date
str_run<-mutate(str_run, year = year(start_date), month = month(start_date))
month_name <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(month_name) <- 1:12
str_run$month <- as.factor(month_name[str_run$month])

#more runs toward the later months 
#(could be because app may have gotten more popular over time)
#highest number of recorded runs in oct, followed by august, lowest in feb then jan (when 2014 not counted)
str_run %>% filter(year > 2006) %>% 
  ggplot(aes(month, fill = factor(year))) + 
    geom_bar() + 
      scale_x_discrete(limits=month_name) + 
        ggtitle("Barplot of Number of Runs by Month")

#similar results for distance run
str_run %>% 
  filter(year > 2006) %>% 
  group_by(month, year) %>% 
  summarise(sum_dist = sum(distance)) %>% 
  mutate(km_run = sum_dist/1000) %>%
  ggplot(aes(month, km_run,  fill = factor(year))) + 
              geom_bar() + 
                scale_x_discrete(limits = month_name) + 
                    ggtitle("Barplot of Kilometers Run for Each Month")

#looking at number of recorded races
# pretty big difference in number of races recorded for the beginning of the year vs the end of the year (much more races toward the end) for 2012-2013
# many more races recorded in 2014 compared to 2013 for the months jan- mar
# possibly because strava started to become more well known as an app/company
str_run %>% 
  filter(year > 2006, workout_type == 1) %>% 
    ggplot(aes(month, fill = factor(year))) + 
      geom_bar() + 
        scale_x_discrete(limits=month_name) + 
          ggtitle("Barplot of Number of Races by Month")

#looking into when profiles were created
#seems like most members joined in 2012, followed by 2013
#no obvious pattern to whether members joined more often during a certain month
#overall though, most joined during july and march (when removing 2014)
tbl_df(runner_info) %>% 
  mutate(year_joined = year(created_at), month_joined = month_name[month(created_at)]) %>%
  ggplot(aes(month_joined, fill = factor(year_joined))) + 
      geom_bar() + 
        scale_x_discrete(limits = month_name) + 
          ggtitle("Barplot of Members Joined for Each Month")

#look at lineplots for the month/year counts of runs and races
str_run %>% filter(year(start_date) > 2012) %>%
  mutate(mon_yr = as.Date(format(start_date, "%m/%y"), format = "%m/%y")) %>% 
  group_by(mon_yr) %>% summarise(count = n()) %>%
  ggplot(aes(mon_yr, count)) + geom_point()

#for runs
#no obvious difference in avg speed for each month for runs
p0 <- str_run %>% 
        filter(year > 2006) %>% 
          ggplot(aes(month, avg_speed)) + 
            geom_boxplot() + 
              scale_x_discrete(limits = month_name) + 
                ggtitle("Boxplot of Average Speeds for Runs for Each Month")

# compute lower and upper whiskers
ylim1 = boxplot.stats(str_run[str_run$year > 2006,]$avg_speed)$stats[c(1, 5)]

# scale y limits based on ylim1
p1 = p0 + coord_cartesian(ylim = ylim1*1.05)
print(p1)


#for races
#removing outliers to see quantiles of boxplots better
#seems like May avg speeds are a little slower, while July avg speeds are a little faster
#overall though, not too big of a difference between the months
p0 <- str_run %>% 
        filter(year > 2006, workout_type == 1) %>% 
          ggplot(aes(month, avg_speed)) + 
            geom_boxplot() + 
              scale_x_discrete(limits = month_name) + 
                ggtitle("Boxplot of Average Speeds for Races for Each Month")
# compute lower and upper whiskers
ylim1 = boxplot.stats(str_run[str_run$year > 2006 & str_run$workout_type == 1,]$avg_speed)$stats[c(1, 5)]
# scale y limits based on ylim1
p1 = p0 + coord_cartesian(ylim = ylim1*1.05)
print(p1)



#finding ids of people who ran at least one race
racer_id <- str_run %>% filter(workout_type == 1) %>% select(id) %>% unique()
runs_racer <- str_run %>% filter(id %in% unlist(racer_id))

#looking at time diff between elapsed time and moving_time
#zooming in to ignore outliers
# almost no difference between elapsed time and moving time for most races
# shouldn't matter too much in the end whether to predict moving time or elapsed time
#for non-races, more runs did have a difference but most were less than a 400 second difference
runs_racer$time_diff <- runs_racer$elapsed_time - runs_racer$moving_time
p0 <- runs_racer %>% 
        ggplot(aes(workout_type, time_diff)) + 
          geom_boxplot() + 
            ggtitle("Boxplot of Time Differences for Workout Types")
# compute lower and upper whiskers
ylim1 = boxplot.stats(runs_racer$time_diff)$stats[c(1, 5)]
# scale y limits based on ylim1
p1 = p0 + coord_cartesian(ylim = ylim1*2)
print(p1)

#do more cleaning b/c some moving times are greater than the elapsed times
#more variance in the time diff for lower distances (not what you may expect)
runs_racer %>% 
  filter(time_diff <1000000) %>% 
  ggplot(aes(distance, time_diff)) + 
  geom_point() +
  labs(y = "time_diff(elapsed_time - moving_time)", title = "Scatterplot of Difference Between Elapsed and Moving Time\nAgainst Distance for Runs")
#looking at just races
#a bit more variance in the lower distances but not a huge difference
runs_racer %>% 
  filter(workout_type == 1, time_diff <20000) %>%
  ggplot(aes(distance, time_diff)) + 
  geom_point() +
  labs(y = "time_diff(elapsed_time - moving_time)", title = "Scatterplot of Difference Between Elapsed and Moving Time\nAgainst Distance for Races")

#scatterplot of avg_speed against distance
#interesting pattern where as distance increases, the variance of average speed funnels down
str_run %>% mutate(race = workout_type == 1) %>% arrange(race) %>%
  ggplot(aes(distance, avg_speed, color = race)) + geom_point() +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "Distance (m)", y = "Avg_Speed (m/sec)" , title = "Scatterplot of Avg_Speed Against Distance")

#looking at female vs male in terms of avg_speed against distance
str_run %>% mutate(is_male = sex == "M") %>% arrange(desc(is_male)) %>%
  ggplot(aes(distance, avg_speed, color = is_male)) + geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  labs(x = "Distance (m)", y = "Avg_Speed (m/sec)" , title = "Scatterplot of Avg_Speed Against Distance")

# looking at effect of elev gain on speed vs distance
str_run %>% 
          mutate(elev_gain_interval = cut(elev_gain, breaks =c(0,100,500,1000,2000,5000),
                                  labels =c("0_100", "100_500", "500_1000", "1000_2000", "2000_5000"))) %>%
          ggplot(aes(distance, avg_speed, color = elev_gain_interval)) + geom_point() +
          geom_smooth(method = "lm") +
          ggtitle("Scatterplot of Avg_Speed Against Distance\nSplit By Elevation Gain")
