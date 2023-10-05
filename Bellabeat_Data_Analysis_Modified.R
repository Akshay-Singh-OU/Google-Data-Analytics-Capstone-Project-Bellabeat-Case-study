library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(lubridate)

new_daily_activity <- read.csv("your_new_daily_activity_data.csv", header = TRUE)
new_daily_sleep <- read.csv("your_new_daily_sleep_data.csv", header = TRUE)
new_daily_steps <- read.csv("your_new_daily_steps_data.csv", header = TRUE)
new_hourly_calories <- read.csv("your_new_hourly_calories_data.csv", header = TRUE)
new_hourly_intensities <- read.csv("your_new_hourly_intensities_data.csv", header = TRUE)
new_hourly_steps <- read.csv("your_new_hourly_steps_data.csv", header = TRUE)
new_weight <- read.csv("your_new_weight_data.csv", header = TRUE)

head(new_daily_activity)
head(new_daily_sleep)
head(new_daily_steps)
head(new_hourly_calories)
head(new_hourly_intensities)
head(new_hourly_steps)
head(new_weight)

new_daily_activity <- new_daily_activity %>%
  distinct() %>%
  drop_na()
  
new_daily_sleep <- new_daily_sleep %>%
  distinct() %>%
  drop_na()

new_daily_steps <- new_daily_steps %>%
  distinct() %>%
  drop_na()

new_hourly_calories <- new_hourly_calories %>%
  distinct() %>%
  drop_na()

new_hourly_intensities <- new_hourly_intensities %>%
  distinct() %>%
  drop_na()

new_hourly_steps <- new_hourly_steps %>%
  distinct() %>%
  drop_na()

clean_names(new_daily_activity)
clean_names(new_daily_sleep)
clean_names(new_daily_steps)
clean_names(new_hourly_calories)
clean_names(new_hourly_intensities)
clean_names(new_hourly_steps)


new_daily_activity <- new_daily_activity %>%
  rename(date = activitydate) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

new_daily_sleep <- new_daily_sleep %>%
  rename(date = sleepday) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))

new_hourly_calories <- new_hourly_calories %>% 
  rename(date_time = activityhour) %>% 
  mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))

new_hourly_intensities <- new_hourly_intensities %>% 
  rename(date_time = activityhour) %>% 
  mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))

new_hourly_steps <- new_hourly_steps %>% 
  rename(date_time = activityhour) %>% 
  mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))

new_daily_activity %>%  
  select(total_steps,
         total_distance,
         sedentary_minutes,
         calories) %>%
  summary()

Create a scatter plot for Total Steps vs. Calories
ggplot(data = new_daily_activity, aes(x = total_steps, y = calories)) + 
  geom_point() + 
  geom_smooth() + 
  labs(title = "Total Steps vs. Calories")

Analyze and visualize daily steps per weekday
new_weekday_steps <- new_daily_activity %>%
  mutate(weekday = weekdays(date))
new_weekday_steps$weekday <- ordered(new_weekday_steps$weekday, levels = c(
  "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
))

new_weekday_steps <- new_weekday_steps %>%
  group_by(weekday) %>%
  summarize(daily_steps = mean(total_steps))

head(new_weekday_steps)

ggplot(new_weekday_steps, aes(weekday, daily_steps)) +
  geom_col(fill = "#d62d58") +
  geom_hline(yintercept = 7500) +
  labs(title = "Daily Steps per Weekday", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))

Analyze and visualize minutes asleep per weekday
new_weekday_sleep <- new_daily_sleep %>%
  mutate(weekday = weekdays(date))
new_weekday_sleep$weekday <- ordered(new_weekday_sleep$weekday, levels = c(
  "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
))

new_weekday_sleep <- new_weekday_sleep %>%
  group_by(weekday) %>%
  summarize(daily_sleep = mean(total_minutes_asleep))

head(new_weekday_sleep)

ggplot(new_weekday_sleep, aes(weekday, daily_sleep)) +
  geom_col(fill = "#db7992") +
  geom_hline(yintercept = 480) +
  labs(title = "Minutes Asleep per Weekday", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))


Analyze hourly steps throughout the day
new_hourly_steps %>%
  separate(date_time, into = c("date", "time"), sep = " ") %>%
  mutate(date = ymd(date)) 

head(new_hourly_steps)

new_hourly_steps %>%
  group_by(time) %>%
  summarize(average_steps = mean(step_total)) %>%
  ggplot() +
  geom_col(mapping = aes(x = time, y = average_steps, fill = average_steps)) + 
  labs(title = "Hourly Steps throughout the Day", x = "", y = "") + 
  scale_fill_gradient(low = "red", high = "green") +
  theme(axis.text.x = element_text(angle = 90))

# Example: Analyze daily use of smart devices based on user activity
new_daily_use <- new_daily_activity_sleep %>%
  group_by(id) %>%
  summarize(days_used = sum(n())) %>%
  mutate(user_type = case_when(
    days_used >= 1 & days_used <= 10 ~ "low user",
    days_used >= 11 & days_used <= 20 ~ "moderate user", 
    days_used >= 21 & days_used <= 31 ~ "high user", 
  ))

head(new_daily_use)

Visualize the percentage of user types
new_daily_use_percent <- new_daily_use %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

new_daily_use_percent$user_type <- factor(new_daily_use_percent$user_type, levels = c(
  "high user", "moderate user", "low user"
))

head(new_daily_use_percent)

new_daily_use_percent %>%
  ggplot(aes(x = "", y = total_percent, fill = user_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(), 
    panel.grid = element_blank(), 
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c(
    "#d62d58", "#db7980", "#fc9fb7"
  ), labels = c(
    "High user - 21 to 31 days",
    "Moderate user - 11 to 20 days",
    "Low user - 1 to 10 days"
  )) +
  labs(title = "Daily Use of Smart Devices")



