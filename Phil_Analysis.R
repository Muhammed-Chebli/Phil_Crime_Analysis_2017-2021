#------------------------------------------FINAL ANALYSIS------------------------------------------

#install and load tools
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("hms")
library(tidyverse) #calculations
library(lubridate) #dates
library(janitor) #cleaning
library(hms) #times

#load, and name, csv files; 5 years worth of data from https://www.opendataphilly.org/
pc_2021 <- read_csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272021-01-01%27%20AND%20dispatch_date_time%20%3C%20%272022-01-01%27")
pc_2020 <- read_csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272020-01-01%27%20AND%20dispatch_date_time%20%3C%20%272021-01-01%27")
pc_2019 <- read_csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272019-01-01%27%20AND%20dispatch_date_time%20%3C%20%272020-01-01%27")
pc_2018 <- read_csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272018-01-01%27%20AND%20dispatch_date_time%20%3C%20%272019-01-01%27")
pc_2017 <- read_csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272017-01-01%27%20AND%20dispatch_date_time%20%3C%20%272018-01-01%27")

#concatenate data into a 5 year dataframe
crime_df <- bind_rows(pc_2021,pc_2020,pc_2019,pc_2018,pc_2017)

#remove yearly dataframes to clean environment
rm(pc_2017, pc_2018, pc_2019, pc_2020, pc_2021)

#create a backup dataframe to remove/add columns
crime_data <- crime_df

#create a list of our original columns
colnames(crime_data)

#remove unnecessary columns
crime_data[3:6] <- list(NULL)
crime_data[4:8] <- list(NULL)

#create new columns for hour, year, and month
crime_data$hour <- hour(crime_data$dispatch_time) #hour
crime_data$year <- format(as.Date(crime_data$dispatch_date), "%Y") #year
crime_data$month <- format(as.Date(crime_data$dispatch_date), "%m") #month

#rename column "text_general_code" to "reported_crime"
crime_data <- crime_data_clean %>% rename(reported_crime = text_general_code)
  
#change text_general_code value from "DRIVING UNDER THE INFLUENCE" to  "DUI"
crime_data$reported_crime[crime_data$reported_crime == "DRIVING UNDER THE INFLUENCE"] <- "DUI"

#create new column for the seasons: Winter, Spring, Summer, Fall
crime_data <- crime_data %>% mutate(season = case_when
                                    (month == "12" ~ "Winter",
                                     month == "01" ~ "Winter",
                                     month == "02" ~ "Winter",
                                     month == "03" ~ "Spring",
                                     month == "04" ~ "Spring",
                                     month == "05" ~ "Spring",
                                     month == "06" ~ "Summer",
                                     month == "07" ~ "Summer",
                                     month == "08" ~ "Summer",
                                     month == "09" ~ "Fall",
                                     month == "10" ~ "Fall",
                                     month == "11" ~ "Fall"))
                                     
                                     
#create new column for time of day: Morning, Night, Afternoon, Evening
crime_data <- crime_data %>% mutate(time_of_day = case_when(
              hour == "0" ~ "Night",
                                                           
              hour == "1" ~ "Night",
                                                          
              hour == "2" ~ "Night",
                                                           
              hour == "3" ~ "Night",
                                                           
              hour == "4" ~ "Night",
                                                           
              hour == "5" ~ "Night",
                                                           
              hour == "6" ~ "Morning",
                                                           
              hour == "7" ~ "Morning",
                                                           
              hour == "8" ~ "Morning",
                                                           
              hour == "9" ~ "Morning",
                                                           
              hour == "10" ~ "Morning",
                                                           
              hour == "11" ~ "Morning",
                                                           
              hour == "12" ~ "Afternoon",
                                                           
              hour == "13" ~ "Afternoon",
                                                           
              hour == "14" ~ "Afternoon",
                                                           
              hour == "15" ~ "Afternoon",
                                                           
              hour == "16" ~ "Afternoon",
                                                           
              hour == "17" ~ "Afternoon",
                                                           
              hour == "18" ~ "Evening",
                                                           
              hour == "19" ~ "Evening",
                                                           
              hour == "20" ~ "Evening",
                                                           
              hour == "21" ~ "Evening",
                                                           
              hour == "22" ~ "Evening",
                                                           
              hour == "23" ~ "Evening"))
 
#clean the data

crime_data <- na.omit(crime_data) #removes rows with na values
crime_data <- distinct(crime_data) #removes duplicates

#------------------------------------------REPORTED CRIMES CALCS------------------------------------------

#total number of crimes reported
nrow(crime_data)

#-------------CRIME TYPE-------------
crime_data %>%
  group_by(reported_crime) %>%
  count(reported_crime)%>%
  print(n=32) #view entire tibble
  
#-------------YEAR-------------
crime_data %>%
  group_by(year) %>%
  count(year) 

#-------------SEASONS-------------
crime_data %>%
  group_by(season) %>%
  count(season)

#-------------MONTH-------------
crime_data %>%
  group_by(month) %>%
  count(month) %>%
  print(n=12) #view entire tibble
  
#-------------HOUR-------------
crime_data %>%
  group_by(hour) %>%
  count(hour) %>%
  print(n=24) #view entire tibble
  
#-------------TIME OF DAY-------------

#----morning----
crime_data %>% 
  group_by(time_of_day) %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)
  
#---afternoon---
crime_data %>% 
  group_by(time_of_day) %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#---evening---
crime_data %>% 
  group_by(time_of_day) %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#---night---
crime_data %>% 
  group_by(time_of_day) %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#-----OR-----
crime_data %>%
  group_by(time_of_day) %>%
  count(time_of_day)
  
#-------------ANY CRIME PER TIME OF DAY-------------
> crime_data_clean %>% 
+ group_by(time_of_day, reported_crime) %>% 
+ filter(reported_crime == "...") %>%  #replace "..." with any of the 31 reported crimes.
+ count(time_of_day)

#-----------------------------------VISUALIZED DATA - CRIME PER YEAR & TIME OF DAY-----------------------------------
ggplot(data = crime_data, #select dataframe
       mapping = aes(x = year, color = time_of_day))+ #select x and y axes, add color for time of days
       geom_line(stat = "count", size = 1.05)+ #count crimes per year, change line size
       ylim(0,60000)+ #limit y axis from 0-60,000
       labs(title = "Reported Crime Per Year, Time of day")+ #add title
       labs(color = "")+ #remove legend title
       xlab("Year")+ #change x-axis to "Year"
       ylab("# of Reported Crimes")+ #change y-axis to "# of Reported Crimes"
       theme_gray()+ #add a theme
       theme(legend.position = "bottom")+ #change legend position to bottom
       scale_color_manual(values = c("blueviolet","cornflowerblue","cyan2","deepskyblue3")) #change line colors
