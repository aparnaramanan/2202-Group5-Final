
# Caton Gayle 
# cmg4ft
# 2202 Final Project - Group 5
# Quantifying Wellness 


################## Importing Packages ##############################################
library(readr)
library(tidyverse)
library(zoo)
library(lubridate)
library(anytime)
################## Cleaning and Binning Activities #################################

# Importing each Activities dataset

Caton_Activities_Raw <- read_csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Activities/5act.csv")
Aparna_Activities_Raw <- read_csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Activities/3act.csv")
Emma_Activities_Raw <- read_csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Activities/1act.csv")
Ian_Activities_Raw <- read_csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Activities/2act.csv")
Zach_Activities_Raw <- read_csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Activities/4act.csv")

# Removing unnecessary columns
Caton_Activities_Raw <- Caton_Activities_Raw[,c(2,3,4)]
Aparna_Activities_Raw <- Aparna_Activities_Raw[,c(2,3,4)]
Emma_Activities_Raw <- Emma_Activities_Raw[,c(2,3,4)]
Ian_Activities_Raw <- Ian_Activities_Raw[,c(2,3,4)]
Zach_Activities_Raw <- Zach_Activities_Raw[,c(2,3,4)]

# Removing null or Na values
Caton_Activities_Raw <- filter(Caton_Activities_Raw, activity_name != "unknown")
Caton_Activities_Raw <- filter(Caton_Activities_Raw, activity_name != "tilting")
Aparna_Activities_Raw <- filter(Aparna_Activities_Raw,is.na(activities) == FALSE)
Emma_Activities_Raw <- filter(Emma_Activities_Raw,is.na(activities) == FALSE)
Ian_Activities_Raw <- filter(Ian_Activities_Raw,is.na(activities) == FALSE)
Zach_Activities_Raw <- filter(Zach_Activities_Raw,is.na(activities) == FALSE)

# Converting timestamp to readable format
activityTime_Caton <- data.matrix(as.vector(Caton_Activities_Raw$timestamp / 1000))
activityTime_Aparna <- data.matrix(as.vector(Aparna_Activities_Raw$timestamp / 1000))
activityTime_Emma <- data.matrix(as.vector(Emma_Activities_Raw$timestamp / 1000))
activityTime_Ian <- data.matrix(as.vector(Ian_Activities_Raw$timestamp / 1000))
activityTime_Zach <- data.matrix(as.vector(Zach_Activities_Raw$timestamp / 1000))

Caton_Activities_Raw$date <- anydate(activityTime_Caton)
Aparna_Activities_Raw$date <- anydate(activityTime_Aparna)
Emma_Activities_Raw$date <- anydate(activityTime_Emma)
Ian_Activities_Raw$date <- anydate(activityTime_Ian)
Zach_Activities_Raw$date <- anydate(activityTime_Zach)

Caton_Activities_Raw <- select(Caton_Activities_Raw, date, activities = activity_name, timestamp=NULL, device_id=NULL)
Aparna_Activities_Raw <- select(Aparna_Activities_Raw, date, activities, timestamp=NULL, device_id=NULL)
Emma_Activities_Raw <- select(Emma_Activities_Raw, date, activities, timestamp=NULL, device_id=NULL)
Ian_Activities_Raw <- select(Ian_Activities_Raw, date, activities, timestamp=NULL, device_id=NULL)
Zach_Activities_Raw <- select(Zach_Activities_Raw, date, activities, timestamp=NULL, device_id=NULL)


# change activities to a common version

Caton_Activities_Raw$activities <- ifelse(Caton_Activities_Raw$activities == "still", "still",
                                          ifelse(Caton_Activities_Raw$activities == "on_foot", "walking",
                                                 ifelse(Caton_Activities_Raw$activities == "in_vehicle", "vehicle",
                                                        ifelse(Caton_Activities_Raw$activities == "on_bicycle", "bicycle", "None"))))

Aparna_Activities_Raw$activities <- ifelse(Aparna_Activities_Raw$activities == "[\\\"walking\\\"]", "walking",
                                           ifelse(Aparna_Activities_Raw$activities == "[\\\"stationary\\\"]", "still",
                                                  ifelse(Aparna_Activities_Raw$activities == "[\\\"running\\\"]", "walking",
                                                         ifelse(Aparna_Activities_Raw$activities == "[\\\"automotive\\\"]", "vehicle",
                                                                ifelse(Aparna_Activities_Raw$activities == "[\\\"stationary\\", "still",
                                                                       ifelse(Aparna_Activities_Raw$activities == "[\\\"cycling\\\"]", "bicycle", "None"))))))

Emma_Activities_Raw$activities <- ifelse(Emma_Activities_Raw$activities == "[\"stationary\"]", "still",
                                         ifelse(Emma_Activities_Raw$activities == "[\"walking\"]", "walking",
                                                ifelse(Emma_Activities_Raw$activities == "[\"running\"]", "walking",
                                                       ifelse(Emma_Activities_Raw$activities == "[\"automotive\"]", "vehicle",
                                                              ifelse(Emma_Activities_Raw$activities == "[\"stationary\",\"automotive\"]", "vehicle",
                                                                     ifelse(Emma_Activities_Raw$activities == "[\"cycling\"]", "bicycle", "None"))))))

Ian_Activities_Raw$activities <- ifelse(Ian_Activities_Raw$activities == "[\\\"walking\\\"]", "walking",
                                        ifelse(Ian_Activities_Raw$activities == "[\\\"stationary\\\"]", "still",
                                               ifelse(Ian_Activities_Raw$activities == "[\\\"running\\\"]", "walking",
                                                      ifelse(Ian_Activities_Raw$activities == "[\\\"automotive\\\"]", "vehicle",
                                                             ifelse(Ian_Activities_Raw$activities == "[\\\"stationary\\", "still",
                                                                    ifelse(Ian_Activities_Raw$activities == "[\\\"cycling\\\"]", "bicycle", "None"))))))

Zach_Activities_Raw$activities <- ifelse(Zach_Activities_Raw$activities == "[\\\"walking\\\"]", "walking",
                                         ifelse(Zach_Activities_Raw$activities == "[\\\"stationary\\\"]", "still",
                                                ifelse(Zach_Activities_Raw$activities == "[\\\"running\\\"]", "walking",
                                                       ifelse(Zach_Activities_Raw$activities == "[\\\"automotive\\\"]", "vehicle",
                                                              ifelse(Zach_Activities_Raw$activities == "[\\\"stationary\\", "still",
                                                                     ifelse(Zach_Activities_Raw$activities == "[\\\"cycling\\\"]", "bicycle", "None"))))))


############# Tabling Caton Activities (5) #############################################################

act_table5 <- as.data.frame(table(Caton_Activities_Raw))
act_table5 <- arrange(act_table5, act_table5$date)

# manually split columns and create new dataframe
table5_bicycle_indexes <- seq(1, 361, 4)
table5_still_indexes <- seq(2, 362, 4)
table5_vehicle_indexes <- seq(3, 363, 4)
table5_walking_indexes <- seq(4, 364, 4)

# create df for each unique date
table5_used_dates_indexes <- seq(1, 361, 4)
table5_surveyDates <- data.frame(date=act_table5$date[table5_used_dates_indexes])

# create df for each count 
table5_bicycle_count <- data.frame(bicycle_count=act_table5$Freq[table5_bicycle_indexes])
table5_still_count <- data.frame(still_count=act_table5$Freq[table5_still_indexes])
table5_vehicle_count <- data.frame(vehicle_count=act_table5$Freq[table5_vehicle_indexes])
table5_walking_count <- data.frame(walking_count=act_table5$Freq[table5_walking_indexes])

# combine new count values into df
activities_5 <- data.frame(table5_surveyDates, table5_still_count, table5_vehicle_count, table5_bicycle_count, table5_walking_count)

# sum vehicle, bicycle, and walking into moving category
activities_5$moving <- rowSums(activities_5[,c("vehicle_count", "bicycle_count","walking_count")])

# binning by week
# activities_5$date <- as.Date(activities_5$date)
# activities_5 <- activities_5 %>% mutate(week = ((as.numeric(date) %/% 7)-2611)) 
# activities_5 <- select(activities_5, date, week, still_count, vehicle_count, bicycle_count, walking_count, moving)

# remove dates after the "spring break" cutoff being 3/6/2020
# activities_5 <- activities_5 %>% filter(activities_5$date <= "2020-03-06")

# remove redundant columns
activities_5 <- activities_5 %>% select(date, week, still_count, moving)

# add id column
activities_5$id <- 5

############# Tabling Aparna Activities (3) ##################################################

act_table3 <- as.data.frame(table(Aparna_Activities_Raw))
act_table3 <- arrange(act_table3, act_table3$date)

# manually split columns and create new dataframe
table3_bicycle_indexes <- seq(1, 193, 4)
table3_still_indexes <- seq(2, 194, 4)
table3_vehicle_indexes <- seq(3, 195, 4)
table3_walking_indexes <- seq(4, 196, 4)

# create df for each unique date
table3_used_dates_indexes <- seq(1, 193, 4)
table3_surveyDates <- data.frame(date=act_table3$date[table3_used_dates_indexes])

# create df for each count 
table3_bicycle_count <- data.frame(bicycle_count=act_table3$Freq[table3_bicycle_indexes])
table3_still_count <- data.frame(still_count=act_table3$Freq[table3_still_indexes])
table3_vehicle_count <- data.frame(vehicle_count=act_table3$Freq[table3_vehicle_indexes])
table3_walking_count <- data.frame(walking_count=act_table3$Freq[table3_walking_indexes])

# combine new count values into df
activities_3 <- data.frame(table3_surveyDates, table3_still_count, table3_vehicle_count, table3_bicycle_count, table3_walking_count)

# sum vehicle, bicycle, and walking into moving category
activities_3$moving <- rowSums(activities_3[,c("vehicle_count", "bicycle_count","walking_count")])

# binning by week
# activities_3$date <- as.Date(activities_3$date)
# activities_3 <- activities_3 %>% mutate(week = ((as.numeric(date) %/% 7)-2611)) 
# activities_3 <- select(activities_3, date, week, still_count, vehicle_count, bicycle_count, walking_count, moving)

# remove dates after the "spring break" cutoff being 3/6/2020
# activities_3 <- activities_3 %>% filter(activities_3$date <= "2020-03-06")

# remove redundant columns
activities_3 <- activities_3 %>% select(date, week, still_count, moving)

# add id column
activities_3$id <- 3


############# Tabling Emma Activities (1) #############################################################

act_table1 <- as.data.frame(table(Emma_Activities_Raw))
act_table1 <- arrange(act_table1, act_table1$date)

# manually split columns and create new dataframe
table1_bicycle_indexes <- seq(1, 221, 4)
table1_still_indexes <- seq(2, 222, 4)
table1_vehicle_indexes <- seq(3, 223, 4)
table1_walking_indexes <- seq(4, 224, 4)

# create df for each unique date
table1_used_dates_indexes <- seq(1, 221, 4)
table1_surveyDates <- data.frame(date=act_table1$date[table1_used_dates_indexes])

# create df for each count 
table1_bicycle_count <- data.frame(bicycle_count=act_table1$Freq[table1_bicycle_indexes])
table1_still_count <- data.frame(still_count=act_table1$Freq[table1_still_indexes])
table1_vehicle_count <- data.frame(vehicle_count=act_table1$Freq[table1_vehicle_indexes])
table1_walking_count <- data.frame(walking_count=act_table1$Freq[table1_walking_indexes])

# combine new count values into df
activities_1 <- data.frame(table1_surveyDates, table1_still_count, table1_vehicle_count, table1_bicycle_count, table1_walking_count)

# sum vehicle, bicycle, and walking into moving category
activities_1$moving <- rowSums(activities_1[,c("vehicle_count", "bicycle_count","walking_count")])

# binning by week
# activities_1$date <- as.Date(activities_1$date)
# activities_1 <- activities_1 %>% mutate(week = ((as.numeric(date) %/% 7)-2611)) 
# activities_1 <- select(activities_1, date, week, still_count, vehicle_count, bicycle_count, walking_count, moving)

# remove dates after the "spring break" cutoff being 3/6/2020
# activities_1 <- activities_1 %>% filter(activities_1$date <= "2020-03-06")

# remove redundant columns
activities_1 <- activities_1 %>% select(date, week, still_count, moving)

# add id column
activities_1$id <- 1

############# Tabling Zach Activities (4) #############################################################

act_table4 <- as.data.frame(table(Zach_Activities_Raw))
act_table4 <- arrange(act_table4, act_table4$date)

# manually split columns and create new dataframe
table4_bicycle_indexes <- seq(1, 353, 4)
table4_still_indexes <- seq(2, 354, 4)
table4_vehicle_indexes <- seq(3, 355, 4)
table4_walking_indexes <- seq(4, 356, 4)

# create df for each unique date
table4_used_dates_indexes <- seq(1, 353, 4)
table4_surveyDates <- data.frame(date=act_table4$date[table4_used_dates_indexes])

# create df for each count 
table4_bicycle_count <- data.frame(bicycle_count=act_table4$Freq[table4_bicycle_indexes])
table4_still_count <- data.frame(still_count=act_table4$Freq[table4_still_indexes])
table4_vehicle_count <- data.frame(vehicle_count=act_table4$Freq[table4_vehicle_indexes])
table4_walking_count <- data.frame(walking_count=act_table4$Freq[table4_walking_indexes])

# combine new count values into df
activities_4 <- data.frame(table4_surveyDates, table4_still_count, table4_vehicle_count, table4_bicycle_count, table4_walking_count)

# sum vehicle, bicycle, and walking into moving category
activities_4$moving <- rowSums(activities_4[,c("vehicle_count", "bicycle_count","walking_count")])

# binning by week
# activities_4$date <- as.Date(activities_4$date)
# activities_4 <- activities_4 %>% mutate(week = ((as.numeric(date) %/% 7)-2611)) 
# activities_4 <- select(activities_4, date, week, still_count, vehicle_count, bicycle_count, walking_count, moving)

# remove dates after the "spring break" cutoff being 3/6/2020
# activities_4 <- activities_4 %>% filter(activities_4$date <= "2020-03-06")

# remove redundant columns
activities_4 <- activities_4 %>% select(date, week, still_count, moving)

# add id column
activities_4$id <- 4

############# Tabling Claire Activities (2) #############################################################

act_table2 <- as.data.frame(table(Ian_Activities_Raw))
act_table2 <- arrange(act_table2, act_table2$date)

# manually split columns and create new dataframe
table2_bicycle_indexes <- seq(1, 141, 4)
table2_still_indexes <- seq(2, 142, 4)
table2_vehicle_indexes <- seq(3, 143, 4)
table2_walking_indexes <- seq(4, 144, 4)

# create df for each unique date
table2_used_dates_indexes <- seq(1, 141, 4)
table2_surveyDates <- data.frame(date=act_table2$date[table2_used_dates_indexes])

# create df for each count 
table2_bicycle_count <- data.frame(bicycle_count=act_table2$Freq[table2_bicycle_indexes])
table2_still_count <- data.frame(still_count=act_table2$Freq[table2_still_indexes])
table2_vehicle_count <- data.frame(vehicle_count=act_table2$Freq[table2_vehicle_indexes])
table2_walking_count <- data.frame(walking_count=act_table2$Freq[table2_walking_indexes])

# combine new count values into df
activities_2 <- data.frame(table2_surveyDates, table2_still_count, table2_vehicle_count, table2_bicycle_count, table2_walking_count)

# sum vehicle, bicycle, and walking into moving category
activities_2$moving <- rowSums(activities_2[,c("vehicle_count", "bicycle_count","walking_count")])

# binning by week
# activities_2$date <- as.Date(activities_2$date)
# activities_2 <- activities_2 %>% mutate(week = ((as.numeric(date) %/% 7)-2611)) 
# activities_2 <- select(activities_2, date, week, still_count, vehicle_count, bicycle_count, walking_count, moving)

# remove dates after the "spring break" cutoff being 3/6/2020
# activities_2 <- activities_2 %>% filter(activities_2$date <= "2020-03-06")
# activities_2 <- activities_2 %>% filter(activities_2$date >= "2020-01-23")

# remove redundant columns
activities_2 <- activities_2 %>% select(date, week, still_count, moving)

# add id column
activities_2$id <- 2


############# Merging first 5 members #############################################################

activities_merged <- rbind(activities_1,activities_2,activities_3,activities_4,activities_5)

activities_pre <-  filter(activities_merged, activities_merged$date <= "2020-03-06")
activities_post <-  filter(activities_merged, activities_merged$date > "2020-03-06")


############# Cleaning and standardizing new people ###############################################

person6 <- read_csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Activities/6act.csv")
person7 <- read_csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Activities/7act.csv")
person8 <- read_csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Activities/8act.csv")
person9 <- read_csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Activities/9act.csv")
person10 <- read_csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Activities/10act.csv")
person11 <- read_csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Activities/11act.csv")
person12 <- read_csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Activities/12act.csv")
person13 <- read_csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Activities/13act.csv")

# Removing unnecessary columns

person6 <- person6[,c(2,3,4)]
person7 <- person7[,c(2,3,4)]
person8 <- person8[,c(2,3,4)]
person9 <- person9[,c(2,3,4)]
person10 <- person10[,c(2,3,4)]
person11 <- person11[,c(2,3,4)]
person12 <- person12[,c(2,3,4)]
person13 <- person13[,c(2,3,4)]

# Removing null or Na values

person6 <- filter(person6,is.na(activities) == FALSE)
person7 <- filter(person7,is.na(activities) == FALSE)
person8 <- filter(person8,is.na(activities) == FALSE)
person9 <- filter(person9,is.na(activities) == FALSE)
person10 <- filter(person10,is.na(activities) == FALSE)
person11 <- filter(person11,is.na(activities) == FALSE)
person12 <- filter(person12,is.na(activities) == FALSE)
person13 <- filter(person13,is.na(activities) == FALSE)

# Converting timestamp to readable format

activityTime_person6 <- data.matrix(as.vector(person6$timestamp / 1000))
activityTime_person7 <- data.matrix(as.vector(person7$timestamp / 1000))
activityTime_person8 <- data.matrix(as.vector(person8$timestamp / 1000))
activityTime_person9 <- data.matrix(as.vector(person9$timestamp / 1000))
activityTime_person10 <- data.matrix(as.vector(person10$timestamp / 1000))
activityTime_person11 <- data.matrix(as.vector(person11$timestamp / 1000))
activityTime_person12 <- data.matrix(as.vector(person12$timestamp / 1000))
activityTime_person13 <- data.matrix(as.vector(person13$timestamp / 1000))

person6$date <- anydate(activityTime_person6)
person7$date <- anydate(activityTime_person7)
person8$date <- anydate(activityTime_person8)
person9$date <- anydate(activityTime_person9)
person10$date <- anydate(activityTime_person10)
person11$date <- anydate(activityTime_person11)
person12$date <- anydate(activityTime_person12)
person13$date <- anydate(activityTime_person13)

person6 <- select(person6, date, activities, timestamp=NULL, device_id=NULL)
person7 <- select(person7, date, activities, timestamp=NULL, device_id=NULL)
person8 <- select(person8, date, activities, timestamp=NULL, device_id=NULL)
person9 <- select(person9, date, activities, timestamp=NULL, device_id=NULL)
person10 <- select(person10, date, activities, timestamp=NULL, device_id=NULL)
person11 <- select(person11, date, activities, timestamp=NULL, device_id=NULL)
person12 <- select(person12, date, activities, timestamp=NULL, device_id=NULL)
person13 <- select(person13, date, activities, timestamp=NULL, device_id=NULL)

# change activities to a common version

person6$activities <- ifelse(person6$activities == "[\"walking\"]", "walking",
                             ifelse(person6$activities == "[\"stationary\"]", "still",
                                    ifelse(person6$activities == "[\"running\"]", "walking",
                                           ifelse(person6$activities == "[\"automotive\"]", "vehicle",
                                                  ifelse(person6$activities == "[\"stationary\",\"automotive\"]", "still",
                                                         ifelse(person6$activities == "[\"cycling\"]", "bicycle", "None"))))))

person7$activities <- ifelse(person7$activities == "[\"walking\"]", "walking",
                             ifelse(person7$activities == "[\"stationary\"]", "still",
                                    ifelse(person7$activities == "[\"running\"]", "walking",
                                           ifelse(person7$activities == "[\"automotive\"]", "vehicle",
                                                  ifelse(person7$activities == "[\"stationary\",\"automotive\"]", "still",
                                                         ifelse(person7$activities == "[\"cycling\"]", "bicycle", "None"))))))

person8$activities <- ifelse(person8$activities == "[\"walking\"]", "walking",
                             ifelse(person8$activities == "[\"stationary\"]", "still",
                                    ifelse(person8$activities == "[\"running\"]", "walking",
                                           ifelse(person8$activities == "[\"automotive\"]", "vehicle",
                                                  ifelse(person8$activities == "[\"stationary\",\"automotive\"]", "still",
                                                         ifelse(person8$activities == "[\"cycling\"]", "bicycle", "None"))))))

person9$activities <- ifelse(person9$activities == "[\"walking\"]", "walking",
                             ifelse(person9$activities == "[\"stationary\"]", "still",
                                    ifelse(person9$activities == "[\"running\"]", "walking",
                                           ifelse(person9$activities == "[\"automotive\"]", "vehicle",
                                                  ifelse(person9$activities == "[\"stationary\",\"automotive\"]", "still",
                                                         ifelse(person9$activities == "[\"cycling\"]", "bicycle", "None"))))))

person10$activities <- ifelse(person10$activities == "[\"walking\"]", "walking",
                              ifelse(person10$activities == "[\"stationary\"]", "still",
                                     ifelse(person10$activities == "[\"running\"]", "walking",
                                            ifelse(person10$activities == "[\"automotive\"]", "vehicle",
                                                   ifelse(person10$activities == "[\"stationary\",\"automotive\"]", "still",
                                                          ifelse(person10$activities == "[\"cycling\"]", "bicycle", "None"))))))

person11$activities <- ifelse(person11$activities == "[\"walking\"]", "walking",
                              ifelse(person11$activities == "[\"stationary\"]", "still",
                                     ifelse(person11$activities == "[\"running\"]", "walking",
                                            ifelse(person11$activities == "[\"automotive\"]", "vehicle",
                                                   ifelse(person11$activities == "[\"stationary\",\"automotive\"]", "still",
                                                          ifelse(person11$activities == "[\"cycling\"]", "bicycle", "None"))))))

person12$activities <- ifelse(person12$activities == "[\"walking\"]", "walking",
                              ifelse(person12$activities == "[\"stationary\"]", "still",
                                     ifelse(person12$activities == "[\"running\"]", "walking",
                                            ifelse(person12$activities == "[\"automotive\"]", "vehicle",
                                                   ifelse(person12$activities == "[\"stationary\",\"automotive\"]", "still",
                                                          ifelse(person12$activities == "[\"cycling\"]", "bicycle", "None"))))))

person13$activities <- ifelse(person13$activities == "[\"walking\"]", "walking",
                              ifelse(person13$activities == "[\"stationary\"]", "still",
                                     ifelse(person13$activities == "[\"running\"]", "walking",
                                            ifelse(person13$activities == "[\"automotive\"]", "vehicle",
                                                   ifelse(person13$activities == "[\"stationary\",\"automotive\"]", "still",
                                                          ifelse(person13$activities == "[\"cycling\"]", "bicycle", "None"))))))

############# Tabling person 6 #############################################################

act_table6 <- as.data.frame(table(person6))
act_table6 <- arrange(act_table6, act_table6$date)

# manually split columns and create new dataframe
table6_bicycle_indexes <- seq(1, 365, 4)
table6_still_indexes <- seq(2, 366, 4)
table6_vehicle_indexes <- seq(3, 367, 4)
table6_walking_indexes <- seq(4, 368, 4)

# create df for each unique date
table6_used_dates_indexes <- seq(1, 365, 4)
table6_surveyDates <- data.frame(date=act_table6$date[table6_used_dates_indexes])

# create df for each count
table6_bicycle_count <- data.frame(bicycle_count=act_table6$Freq[table6_bicycle_indexes])
table6_still_count <- data.frame(still_count=act_table6$Freq[table6_still_indexes])
table6_vehicle_count <- data.frame(vehicle_count=act_table6$Freq[table6_vehicle_indexes])
table6_walking_count <- data.frame(walking_count=act_table6$Freq[table6_walking_indexes])

# combine new count values into df
activities_6 <- data.frame(table6_surveyDates, table6_still_count, table6_vehicle_count, table6_bicycle_count, table6_walking_count)

# sum vehicle, bicycle, and walking into moving category
activities_6$moving <- rowSums(activities_6[,c("vehicle_count", "bicycle_count","walking_count")])

# remove redundant columns
activities_6 <- activities_6 %>% select(date, still_count, moving)

# add id column
activities_6$id <- 6


############# Tabling person 7 #############################################################

act_table7 <- as.data.frame(table(person7))
act_table7 <- arrange(act_table7, act_table7$date)

# manually split columns and create new dataframe
table7_bicycle_indexes <- seq(1, 365, 4)
table7_still_indexes <- seq(2, 366, 4)
table7_vehicle_indexes <- seq(3, 367, 4)
table7_walking_indexes <- seq(4, 368, 4)

# create df for each unique date
table7_used_dates_indexes <- seq(1, 365, 4)
table7_surveyDates <- data.frame(date=act_table7$date[table7_used_dates_indexes])

# create df for each count
table7_bicycle_count <- data.frame(bicycle_count=act_table7$Freq[table7_bicycle_indexes])
table7_still_count <- data.frame(still_count=act_table7$Freq[table7_still_indexes])
table7_vehicle_count <- data.frame(vehicle_count=act_table7$Freq[table7_vehicle_indexes])
table7_walking_count <- data.frame(walking_count=act_table7$Freq[table7_walking_indexes])

# combine new count values into df
activities_7 <- data.frame(table7_surveyDates, table7_still_count, table7_vehicle_count, table7_bicycle_count, table7_walking_count)

# sum vehicle, bicycle, and walking into moving category
activities_7$moving <- rowSums(activities_7[,c("vehicle_count", "bicycle_count","walking_count")])

# remove redundant columns
activities_7 <- activities_7 %>% select(date, still_count, moving)

# add id column
activities_7$id <- 7

############# Tabling person 8 #############################################################

act_table8 <- as.data.frame(table(person8))
act_table8 <- arrange(act_table8, act_table8$date)

# manually split columns and create new dataframe
table8_bicycle_indexes <- seq(1, 313, 4)
table8_still_indexes <- seq(2, 314, 4)
table8_vehicle_indexes <- seq(3, 315, 4)
table8_walking_indexes <- seq(4, 316, 4)

# create df for each unique date
table8_used_dates_indexes <- seq(1, 313, 4)
table8_surveyDates <- data.frame(date=act_table8$date[table8_used_dates_indexes])

# create df for each count
table8_bicycle_count <- data.frame(bicycle_count=act_table8$Freq[table8_bicycle_indexes])
table8_still_count <- data.frame(still_count=act_table8$Freq[table8_still_indexes])
table8_vehicle_count <- data.frame(vehicle_count=act_table8$Freq[table8_vehicle_indexes])
table8_walking_count <- data.frame(walking_count=act_table8$Freq[table8_walking_indexes])

# combine new count values into df
activities_8 <- data.frame(table8_surveyDates, table8_still_count, table8_vehicle_count, table8_bicycle_count, table8_walking_count)

# sum vehicle, bicycle, and walking into moving category
activities_8$moving <- rowSums(activities_8[,c("vehicle_count", "bicycle_count","walking_count")])

# remove redundant columns
activities_8 <- activities_8 %>% select(date, still_count, moving)

# add id column
activities_8$id <- 8
############# Tabling person 9 #############################################################

act_table9 <- as.data.frame(table(person9))
act_table9 <- arrange(act_table9, act_table9$date)

# manually split columns and create new dataframe
table9_bicycle_indexes <- seq(1, 365, 4)
table9_still_indexes <- seq(2, 366, 4)
table9_vehicle_indexes <- seq(3, 367, 4)
table9_walking_indexes <- seq(4, 368, 4)

# create df for each unique date
table9_used_dates_indexes <- seq(1, 365, 4)
table9_surveyDates <- data.frame(date=act_table9$date[table9_used_dates_indexes])

# create df for each count
table9_bicycle_count <- data.frame(bicycle_count=act_table9$Freq[table9_bicycle_indexes])
table9_still_count <- data.frame(still_count=act_table9$Freq[table9_still_indexes])
table9_vehicle_count <- data.frame(vehicle_count=act_table9$Freq[table9_vehicle_indexes])
table9_walking_count <- data.frame(walking_count=act_table9$Freq[table9_walking_indexes])

# combine new count values into df
activities_9 <- data.frame(table9_surveyDates, table9_still_count, table9_vehicle_count, table9_bicycle_count, table9_walking_count)

# sum vehicle, bicycle, and walking into moving category
activities_9$moving <- rowSums(activities_9[,c("vehicle_count", "bicycle_count","walking_count")])

# remove redundant columns
activities_9 <- activities_9 %>% select(date, still_count, moving)

# add id column
activities_9$id <- 9

############# Tabling person 10 #############################################################

act_table10 <- as.data.frame(table(person10))
act_table10 <- arrange(act_table10, act_table10$date)

# manually split columns and create new dataframe
table10_bicycle_indexes <- seq(1, 333, 4)
table10_still_indexes <- seq(2, 334, 4)
table10_vehicle_indexes <- seq(3, 335, 4)
table10_walking_indexes <- seq(4, 336, 4)

# create df for each unique date
table10_used_dates_indexes <- seq(1, 333, 4)
table10_surveyDates <- data.frame(date=act_table10$date[table10_used_dates_indexes])

# create df for each count
table10_bicycle_count <- data.frame(bicycle_count=act_table10$Freq[table10_bicycle_indexes])
table10_still_count <- data.frame(still_count=act_table10$Freq[table10_still_indexes])
table10_vehicle_count <- data.frame(vehicle_count=act_table10$Freq[table10_vehicle_indexes])
table10_walking_count <- data.frame(walking_count=act_table10$Freq[table10_walking_indexes])

# combine new count values into df
activities_10 <- data.frame(table10_surveyDates, table10_still_count, table10_vehicle_count, table10_bicycle_count, table10_walking_count)

# sum vehicle, bicycle, and walking into moving category
activities_10$moving <- rowSums(activities_10[,c("vehicle_count", "bicycle_count","walking_count")])

# remove redundant columns
activities_10 <- activities_10 %>% select(date, still_count, moving)

# add id column
activities_10$id <- 10

############# Tabling person 11 #############################################################

act_table11 <- as.data.frame(table(person11))
act_table11 <- arrange(act_table11, act_table11$date)

# manually split columns and create new dataframe
table11_bicycle_indexes <- seq(1, 177, 4)
table11_still_indexes <- seq(2, 178, 4)
table11_vehicle_indexes <- seq(3, 179, 4)
table11_walking_indexes <- seq(4, 180, 4)

# create df for each unique date
table11_used_dates_indexes <- seq(1, 177, 4)
table11_surveyDates <- data.frame(date=act_table11$date[table11_used_dates_indexes])

# create df for each count
table11_bicycle_count <- data.frame(bicycle_count=act_table11$Freq[table11_bicycle_indexes])
table11_still_count <- data.frame(still_count=act_table11$Freq[table11_still_indexes])
table11_vehicle_count <- data.frame(vehicle_count=act_table11$Freq[table11_vehicle_indexes])
table11_walking_count <- data.frame(walking_count=act_table11$Freq[table11_walking_indexes])

# combine new count values into df
activities_11 <- data.frame(table11_surveyDates, table11_still_count, table11_vehicle_count, table11_bicycle_count, table11_walking_count)

# sum vehicle, bicycle, and walking into moving category
activities_11$moving <- rowSums(activities_11[,c("vehicle_count", "bicycle_count","walking_count")])

# remove redundant columns
activities_11 <- activities_11 %>% select(date, still_count, moving)

# add id column
activities_11$id <- 11

############# Tabling person 12 #############################################################

act_table12 <- as.data.frame(table(person12))
act_table12 <- arrange(act_table12, act_table12$date)

# manually split columns and create new dataframe
table12_bicycle_indexes <- seq(1, 277, 4)
table12_still_indexes <- seq(2, 278, 4)
table12_vehicle_indexes <- seq(3, 279, 4)
table12_walking_indexes <- seq(4, 280, 4)

# create df for each unique date
table12_used_dates_indexes <- seq(1, 277, 4)
table12_surveyDates <- data.frame(date=act_table12$date[table12_used_dates_indexes])

# create df for each count
table12_bicycle_count <- data.frame(bicycle_count=act_table12$Freq[table12_bicycle_indexes])
table12_still_count <- data.frame(still_count=act_table12$Freq[table12_still_indexes])
table12_vehicle_count <- data.frame(vehicle_count=act_table12$Freq[table12_vehicle_indexes])
table12_walking_count <- data.frame(walking_count=act_table12$Freq[table12_walking_indexes])

# combine new count values into df
activities_12 <- data.frame(table12_surveyDates, table12_still_count, table12_vehicle_count, table12_bicycle_count, table12_walking_count)

# sum vehicle, bicycle, and walking into moving category
activities_12$moving <- rowSums(activities_12[,c("vehicle_count", "bicycle_count","walking_count")])

# remove redundant columns
activities_12 <- activities_12 %>% select(date, still_count, moving)

# add id column
activities_12$id <- 12

############# Tabling person 13 #############################################################

act_table13 <- as.data.frame(table(person13))
act_table13 <- arrange(act_table13, act_table13$date)

# manually split columns and create new dataframe
table13_bicycle_indexes <- seq(1, 201, 4)
table13_still_indexes <- seq(2, 202, 4)
table13_vehicle_indexes <- seq(3, 203, 4)
table13_walking_indexes <- seq(4, 204, 4)

# create df for each unique date
table13_used_dates_indexes <- seq(1, 201, 4)
table13_surveyDates <- data.frame(date=act_table13$date[table13_used_dates_indexes])

# create df for each count
table13_bicycle_count <- data.frame(bicycle_count=act_table13$Freq[table13_bicycle_indexes])
table13_still_count <- data.frame(still_count=act_table13$Freq[table13_still_indexes])
table13_vehicle_count <- data.frame(vehicle_count=act_table13$Freq[table13_vehicle_indexes])
table13_walking_count <- data.frame(walking_count=act_table13$Freq[table13_walking_indexes])

# combine new count values into df
activities_13 <- data.frame(table13_surveyDates, table13_still_count, table13_vehicle_count, table13_bicycle_count, table13_walking_count)

# sum vehicle, bicycle, and walking into moving category
activities_13$moving <- rowSums(activities_13[,c("vehicle_count", "bicycle_count","walking_count")])

# remove redundant columns
activities_13 <- activities_13 %>% select(date, still_count, moving)

# add id column
activities_13$id <- 13


############# Merging all datasets #############################################################

activities_merged <- select(activities_merged, date, still_count, moving, id)

activities_complete <- rbind(activities_merged, activities_6, activities_7, activities_8,
                             activities_9, activities_10, activities_11, activities_12, activities_13)

activities_complete$date <- as.Date(activities_complete$date)

activities_pre <-  filter(activities_complete, activities_complete$date <= "2020-03-06")
activities_post <- filter(activities_complete, activities_complete$date > "2020-03-06")



