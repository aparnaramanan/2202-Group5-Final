# Zachary Kay - Cleaning and Binning All Call Data

#Importing Packages
library(readr)
library(tidyverse)
library(zoo)
library(lubridate)
library(anytime)

# Importing each call data frame
Caton_call <- read.csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Calls/5call.csv")
Aparna_call <- read.csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Calls/3call.csv")
Emma_call <- read.csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Calls/1call.csv")
Zach_call <- read.csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Calls/4call.csv")
Claire_call <-read.csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Calls/2call.csv")
Student6_call <- read.csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Calls/6call.csv")
Student7_call <- read.csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Calls/7call.csv")
Student8_call <- read.csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Calls/8call.csv")
Student9_call <- read.csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Calls/9call.csv")
Student10_call <- read.csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Calls/10call.csv")
Student11_call <- read.csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Calls/11call.csv")
Student12_call <- read.csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Calls/12call.csv")
Student13_call <- read.csv("/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/Calls/13call.csv")

# Removing unnecessary columns
Caton_call <- Caton_call[,c(2,5)]
Aparna_call <- Aparna_call[,c(2,5)]
Emma_call <- Emma_call[,c(2,5)]
Zach_call <- Zach_call[,c(2,5)]
Claire_call <- Claire_call[,c(2,5)]
Student6_call <- Student6_call[,c(2,5)]
Student7_call <- Student7_call[,c(2,5)]
Student8_call <- Student8_call[,c(2,5)]
Student9_call <- Student9_call[,c(2,5)]
Student10_call <- Student10_call[,c(2,5)]
Student11_call <- Student11_call[,c(2,5)]
Student12_call <- Student12_call[,c(2,5)]
Student13_call <- Student13_call[,c(2,5)]

# Removing null or Na values
Caton_call <- filter(Caton_call,is.na(call_duration) == FALSE)
Aparna_call <- filter(Aparna_call,is.na(call_duration) == FALSE)
Emma_call <- filter(Emma_call,is.na(call_duration) == FALSE)
Zach_call <- filter(Zach_call,is.na(call_duration) == FALSE)
Claire_call <- filter(Claire_call,is.na(call_duration) == FALSE)
Student6_call <- filter(Student6_call,is.na(call_duration) == FALSE)
Student7_call <- filter(Student7_call,is.na(call_duration) == FALSE)
Student8_call <- filter(Student8_call,is.na(call_duration) == FALSE)
Student9_call <- filter(Student9_call,is.na(call_duration) == FALSE)
Student10_call <- filter(Student10_call,is.na(call_duration) == FALSE)
Student11_call <- filter(Student11_call,is.na(call_duration) == FALSE)
Student12_call <- filter(Student12_call,is.na(call_duration) == FALSE)
Student13_call <- filter(Student13_call,is.na(call_duration) == FALSE)

# Converting timestamp to readable format
callTime_Caton <- data.matrix(as.vector(Caton_call$timestamp / 1000))
callTime_Aparna <- data.matrix(as.vector(Aparna_call$timestamp / 1000))
callTime_Emma <- data.matrix(as.vector(Emma_call$timestamp / 1000))
callTime_Zach <- data.matrix(as.vector(Zach_call$timestamp / 1000))
callTime_Claire <- data.matrix(as.vector(Claire_call$timestamp / 1000))
callTime_Student6 <- data.matrix(as.vector(Student6_call$timestamp / 1000))
callTime_Student7 <- data.matrix(as.vector(Student7_call$timestamp / 1000))
callTime_Student8 <- data.matrix(as.vector(Student8_call$timestamp / 1000))
callTime_Student9 <- data.matrix(as.vector(Student9_call$timestamp / 1000))
callTime_Student10 <- data.matrix(as.vector(Student10_call$timestamp / 1000))
callTime_Student11 <- data.matrix(as.vector(Student11_call$timestamp / 1000))
callTime_Student12 <- data.matrix(as.vector(Student12_call$timestamp / 1000))
callTime_Student13 <- data.matrix(as.vector(Student13_call$timestamp / 1000))


Caton_call$date <- anydate(callTime_Caton)
Aparna_call$date <- anydate(callTime_Aparna)
Emma_call$date <- anydate(callTime_Emma)
Zach_call$date <- anydate(callTime_Zach)
Claire_call$date <- anydate(callTime_Claire)
Student6_call$date <- anydate(callTime_Student6)
Student7_call$date <- anydate(callTime_Student7)
Student8_call$date <- anydate(callTime_Student8)
Student9_call$date <- anydate(callTime_Student9)
Student10_call$date <- anydate(callTime_Student10)
Student11_call$date <- anydate(callTime_Student11)
Student12_call$date <- anydate(callTime_Student12)
Student13_call$date <- anydate(callTime_Student13)

Caton_call <- select(Caton_call, date, call_duration)
Aparna_call <- select(Aparna_call, date, call_duration)
Emma_call <- select(Emma_call, date, call_duration)
Zach_call <- select(Zach_call, date, call_duration)
Claire_call <- select(Claire_call, date, call_duration)
Student6_call <- select(Student6_call, date, call_duration)
Student7_call <- select(Student7_call, date, call_duration)
Student8_call <- select(Student8_call, date, call_duration)
Student9_call <- select(Student9_call, date, call_duration)
Student10_call <- select(Student10_call, date, call_duration)
Student11_call <- select(Student11_call, date, call_duration)
Student12_call <- select(Student12_call, date, call_duration)
Student13_call <- select(Student13_call, date, call_duration)

# change activities to a common version
Caton_call$call_duration <- ifelse(Caton_call$call_duration >= 1, 1,
                                   ifelse(Caton_call$call_duration == 0, NA, "None"))

Aparna_call$call_duration <- ifelse(Aparna_call$call_duration >= 1, 1,
                                    ifelse(Aparna_call$call_duration == 0, NA, "None"))

Emma_call$call_duration <- ifelse(Emma_call$call_duration >= 1, 1,
                                  ifelse(Emma_call$call_duration == 0, NA, "None"))

Zach_call$call_duration <- ifelse(Zach_call$call_duration >= 1, 1,
                                  ifelse(Zach_call$call_duration == 0, NA, "None"))

Claire_call$call_duration <- ifelse(Claire_call$call_duration >= 1, 1,
                                    ifelse(Claire_call$call_duration == 0, NA, "None"))

Student6_call$call_duration <- ifelse(Student6_call$call_duration >= 1, 1,
                                      ifelse(Student6_call$call_duration == 0, NA, "None"))

Student7_call$call_duration <- ifelse(Student7_call$call_duration >= 1, 1,
                                      ifelse(Student7_call$call_duration == 0, NA, "None"))

Student8_call$call_duration <- ifelse(Student8_call$call_duration >= 1, 1,
                                      ifelse(Student8_call$call_duration == 0, NA, "None"))

Student9_call$call_duration <- ifelse(Student9_call$call_duration >= 1, 1,
                                      ifelse(Student9_call$call_duration == 0, NA, "None"))

Student10_call$call_duration <- ifelse(Student10_call$call_duration >= 1, 1,
                                       ifelse(Student10_call$call_duration == 0, NA, "None"))

Student11_call$call_duration <- ifelse(Student11_call$call_duration >= 1, 1,
                                       ifelse(Student11_call$call_duration == 0, NA, "None"))

Student12_call$call_duration <- ifelse(Student12_call$call_duration >= 1, 1,
                                       ifelse(Student12_call$call_duration == 0, NA, "None"))

Student13_call$call_duration <- ifelse(Student13_call$call_duration >= 1, 1,
                                       ifelse(Student13_call$call_duration == 0, NA, "None"))

#Removes NA values where the phone duration was 0
Caton_call <- filter(Caton_call,is.na(call_duration) == FALSE)
Aparna_call <- filter(Aparna_call,is.na(call_duration) == FALSE)
Emma_call <- filter(Emma_call,is.na(call_duration) == FALSE)
Zach_call <- filter(Zach_call,is.na(call_duration) == FALSE)
Claire_call <- filter(Zach_call,is.na(call_duration) == FALSE)
Student6_call <- filter(Student6_call,is.na(call_duration) == FALSE)
Student7_call <- filter(Student7_call,is.na(call_duration) == FALSE)
Student8_call <- filter(Student8_call,is.na(call_duration) == FALSE)
Student9_call <- filter(Student9_call,is.na(call_duration) == FALSE)
Student10_call <- filter(Student10_call,is.na(call_duration) == FALSE)
Student11_call <- filter(Student11_call,is.na(call_duration) == FALSE)
Student12_call <- filter(Student12_call,is.na(call_duration) == FALSE)
Student13_call <- filter(Student13_call,is.na(call_duration) == FALSE)

#finds relative frequency of call types (number of calls in a day) for each person
#renames frequency to Number of Calls
Caton_call_day <- as.data.frame(table(Caton_call))
Caton_call_day <- arrange(Caton_call_day, Caton_call_day$date)
Caton_call_day <- Caton_call_day %>% 
  rename(Number_of_calls = Freq)

Aparna_call_day <- as.data.frame(table(Aparna_call))
Aparna_call_day <- arrange(Aparna_call_day, Aparna_call_day$date)
Aparna_call_day <- Aparna_call_day %>% 
  rename(Number_of_calls = Freq)

Emma_call_day <- as.data.frame(table(Emma_call))
Emma_call_day <- arrange(Emma_call_day, Emma_call_day$date)
Emma_call_day <- Emma_call_day %>% 
  rename(Number_of_calls = Freq)

Zach_call_day <- as.data.frame(table(Zach_call))
Zach_call_day <- arrange(Zach_call_day, Zach_call_day$date)
Zach_call_day <- Zach_call_day %>% 
  rename(Number_of_calls = Freq)

Claire_call_day <- as.data.frame(table(Claire_call))
Claire_call_day <- arrange(Claire_call_day, Claire_call_day$date)
Claire_call_day <- Claire_call_day %>% 
  rename(Number_of_calls = Freq)

Student6_call_day <- as.data.frame(table(Student6_call))
Student6_call_day <- arrange(Student1_call_day, Student6_call_day$date)
Student6_call_day <- Student6_call_day %>% 
  rename(Number_of_calls = Freq)

Student7_call_day <- as.data.frame(table(Student7_call))
Student7_call_day <- arrange(Student7_call_day, Student7_call_day$date)
Student7_call_day <- Student7_call_day %>% 
  rename(Number_of_calls = Freq)

Student8_call_day <- as.data.frame(table(Student8_call))
Student8_call_day <- arrange(Student8_call_day, Student8_call_day$date)
Student8_call_day <- Student8_call_day %>% 
  rename(Number_of_calls = Freq)

Student9_call_day <- as.data.frame(table(Student9_call))
Student9_call_day <- arrange(Student9_call_day, Student9_call_day$date)
Student9_call_day <- Student9_call_day %>% 
  rename(Number_of_calls = Freq)

Student10_call_day <- as.data.frame(table(Student10_call))
Student10_call_day <- arrange(Student10_call_day, Student10_call_day$date)
Student10_call_day <- Student10_call_day %>% 
  rename(Number_of_calls = Freq)

Student11_call_day <- as.data.frame(table(Student11_call))
Student11_call_day <- arrange(Student11_call_day, Student11_call_day$date)
Student11_call_day <- Student11_call_day %>% 
  rename(Number_of_calls = Freq)

Student12_call_day <- as.data.frame(table(Student12_call))
Student12_call_day <- arrange(Student12_call_day, Student12_call_day$date)
Student12_call_day <- Student12_call_day %>% 
  rename(Number_of_calls = Freq)

Student13_call_day <- as.data.frame(table(Student13_call))
Student13_call_day <- arrange(Student13_call_day, Student13_call_day$date)
Student13_call_day <- Student13_call_day %>% 
  rename(Number_of_calls = Freq)

#selects the date and the number of calls in each day
Caton_call_day <- Caton_call_day[,c(1,3)]
Aparna_call_day <- Aparna_call_day[,c(1,3)]
Emma_call_day <- Emma_call_day[,c(1,3)]
Zach_call_day <- Zach_call_day[,c(1,3)]
Claire_call_day <- Claire_call_day[,c(1,3)]
Student6_call_day <- Student6_call_day[,c(1,3)]
Student7_call_day <- Student7_call_day[,c(1,3)]
Student8_call_day <- Student8_call_day[,c(1,3)]
Student9_call_day <- Student9_call_day[,c(1,3)]
Student10_call_day <- Student10_call_day[,c(1,3)]
Student11_call_day <- Student11_call_day[,c(1,3)]
Student12_call_day <- Student12_call_day[,c(1,3)]
Student13_call_day <- Student13_call_day[,c(1,3)]

#Cleans data by removing sound values
Caton_call_day$Number_of_calls <- ifelse(Caton_call_day$Number_of_calls >= 51, Caton_call_day$Number_of_calls-47,
                                         ifelse(Caton_call_day$Number_of_calls >= 41, Caton_call_day$Number_of_calls-37,
                                                ifelse(Caton_call_day$Number_of_calls >= 31, Caton_call_day$Number_of_calls-27,
                                                       ifelse(Caton_call_day$Number_of_calls >= 21, Caton_call_day$Number_of_calls-17,
                                                              ifelse(Caton_call_day$Number_of_calls >= 15, Caton_call_day$Number_of_calls-7,
                                                                     ifelse(Caton_call_day$Number_of_calls <= 14, Caton_call_day$Number_of_calls, "None"))))))

Aparna_call_day$Number_of_calls <- ifelse(Aparna_call_day$Number_of_calls >= 51, Aparna_call_day$Number_of_calls-47,
                                          ifelse(Aparna_call_day$Number_of_calls >= 41, Aparna_call_day$Number_of_calls-37,
                                                 ifelse(Aparna_call_day$Number_of_calls >= 31, Aparna_call_day$Number_of_calls-27,
                                                        ifelse(Aparna_call_day$Number_of_calls >= 21, Aparna_call_day$Number_of_calls-17,
                                                               ifelse(Aparna_call_day$Number_of_calls >= 15, Aparna_call_day$Number_of_calls-7,
                                                                      ifelse(Aparna_call_day$Number_of_calls <= 14, Aparna_call_day$Number_of_calls, "None"))))))

Emma_call_day$Number_of_calls <- ifelse(Emma_call_day$Number_of_calls >= 51, Emma_call_day$Number_of_calls-47,
                                        ifelse(Emma_call_day$Number_of_calls >= 41, Emma_call_day$Number_of_calls-37,
                                               ifelse(Emma_call_day$Number_of_calls >= 31, Emma_call_day$Number_of_calls-27,
                                                      ifelse(Emma_call_day$Number_of_calls >= 21, Emma_call_day$Number_of_calls-17,
                                                             ifelse(Emma_call_day$Number_of_calls >= 15, Emma_call_day$Number_of_calls-7,
                                                                    ifelse(Emma_call_day$Number_of_calls <= 14, Emma_call_day$Number_of_calls, "None"))))))

Claire_call_day$Number_of_calls <- ifelse(Claire_call_day$Number_of_calls >= 51, Claire_call_day$Number_of_calls-47,
                                          ifelse(Claire_call_day$Number_of_calls >= 41, Claire_call_day$Number_of_calls-37,
                                                 ifelse(Claire_call_day$Number_of_calls >= 31, Claire_call_day$Number_of_calls-27,
                                                        ifelse(Claire_call_day$Number_of_calls >= 21, Claire_call_day$Number_of_calls-17,
                                                               ifelse(Claire_call_day$Number_of_calls >= 15, Claire_call_day$Number_of_calls-7,
                                                                      ifelse(Claire_call_day$Number_of_calls <= 14, Claire_call_day$Number_of_calls, "None"))))))

Zach_call_day$Number_of_calls <- ifelse(Zach_call_day$Number_of_calls >= 51, Zach_call_day$Number_of_calls-47,
                                        ifelse(Zach_call_day$Number_of_calls >= 41, Zach_call_day$Number_of_calls-37,
                                               ifelse(Zach_call_day$Number_of_calls >= 31, Zach_call_day$Number_of_calls-27,
                                                      ifelse(Zach_call_day$Number_of_calls >= 21, Zach_call_day$Number_of_calls-17,
                                                             ifelse(Zach_call_day$Number_of_calls >= 15, Zach_call_day$Number_of_calls-7,
                                                                    ifelse(Zach_call_day$Number_of_calls <= 14, Zach_call_day$Number_of_calls, "None"))))))

Student6_call_day$Number_of_calls <- ifelse(Student6_call_day$Number_of_calls >= 51, Student6_call_day$Number_of_calls-47,
                                            ifelse(Student6_call_day$Number_of_calls >= 41, Student6_call_day$Number_of_calls-37,
                                                   ifelse(Student6_call_day$Number_of_calls >= 31, Student6_call_day$Number_of_calls-27,
                                                          ifelse(Student6_call_day$Number_of_calls >= 21, Student6_call_day$Number_of_calls-17,
                                                                 ifelse(Student6_call_day$Number_of_calls >= 15, Student6_call_day$Number_of_calls-7,
                                                                        ifelse(Student6_call_day$Number_of_calls <= 14, Student6_call_day$Number_of_calls, "None"))))))

Student7_call_day$Number_of_calls <- ifelse(Student7_call_day$Number_of_calls >= 51, Student7_call_day$Number_of_calls-47,
                                            ifelse(Student7_call_day$Number_of_calls >= 41, Student7_call_day$Number_of_calls-37,
                                                   ifelse(Student7_call_day$Number_of_calls >= 31, Student7_call_day$Number_of_calls-27,
                                                          ifelse(Student7_call_day$Number_of_calls >= 21, Student7_call_day$Number_of_calls-17,
                                                                 ifelse(Student7_call_day$Number_of_calls >= 15, Student7_call_day$Number_of_calls-7,
                                                                        ifelse(Student7_call_day$Number_of_calls <= 14, Student7_call_day$Number_of_calls, "None"))))))

Student8_call_day$Number_of_calls <- ifelse(Student8_call_day$Number_of_calls >= 51, Student8_call_day$Number_of_calls-47,
                                            ifelse(Student8_call_day$Number_of_calls >= 41, Student8_call_day$Number_of_calls-37,
                                                   ifelse(Student8_call_day$Number_of_calls >= 31, Student8_call_day$Number_of_calls-27,
                                                          ifelse(Student8_call_day$Number_of_calls >= 21, Student8_call_day$Number_of_calls-17,
                                                                 ifelse(Student8_call_day$Number_of_calls >= 15, Student8_call_day$Number_of_calls-7,
                                                                        ifelse(Student8_call_day$Number_of_calls <= 14, Student8_call_day$Number_of_calls, "None"))))))

Student9_call_day$Number_of_calls <- ifelse(Student9_call_day$Number_of_calls >= 70, 8,
                                            ifelse(Student9_call_day$Number_of_calls >= 51, Student9_call_day$Number_of_calls-47,
                                                   ifelse(Student9_call_day$Number_of_calls >= 41, Student9_call_day$Number_of_calls-37,
                                                          ifelse(Student9_call_day$Number_of_calls >= 31, Student9_call_day$Number_of_calls-27,
                                                                 ifelse(Student9_call_day$Number_of_calls >= 21, Student9_call_day$Number_of_calls-17,
                                                                        ifelse(Student9_call_day$Number_of_calls >= 15, Student9_call_day$Number_of_calls-7,
                                                                               ifelse(Student9_call_day$Number_of_calls <= 14, Student9_call_day$Number_of_calls, "None")))))))

Student10_call_day$Number_of_calls <- ifelse(Student10_call_day$Number_of_calls >= 51, Student10_call_day$Number_of_calls-47,
                                             ifelse(Student10_call_day$Number_of_calls >= 41, Student10_call_day$Number_of_calls-37,
                                                    ifelse(Student10_call_day$Number_of_calls >= 31, Student10_call_day$Number_of_calls-27,
                                                           ifelse(Student10_call_day$Number_of_calls >= 21, Student10_call_day$Number_of_calls-17,
                                                                  ifelse(Student10_call_day$Number_of_calls >= 15, Student10_call_day$Number_of_calls-7,
                                                                         ifelse(Student10_call_day$Number_of_calls <= 14, Student10_call_day$Number_of_calls, "None"))))))

Student11_call_day$Number_of_calls <- ifelse(Student11_call_day$Number_of_calls >= 51, Student11_call_day$Number_of_calls-47,
                                             ifelse(Student11_call_day$Number_of_calls >= 41, Student11_call_day$Number_of_calls-37,
                                                    ifelse(Student11_call_day$Number_of_calls >= 31, Student11_call_day$Number_of_calls-27,
                                                           ifelse(Student11_call_day$Number_of_calls >= 21, Student11_call_day$Number_of_calls-17,
                                                                  ifelse(Student11_call_day$Number_of_calls >= 15, Student11_call_day$Number_of_calls-7,
                                                                         ifelse(Student11_call_day$Number_of_calls <= 14, Student11_call_day$Number_of_calls, "None"))))))

Student12_call_day$Number_of_calls <- ifelse(Student12_call_day$Number_of_calls >= 70, 7,
                                             ifelse(Student12_call_day$Number_of_calls >= 51, Student12_call_day$Number_of_calls-47,
                                                    ifelse(Student12_call_day$Number_of_calls >= 41, Student12_call_day$Number_of_calls-37,
                                                           ifelse(Student12_call_day$Number_of_calls >= 31, Student12_call_day$Number_of_calls-27,
                                                                  ifelse(Student12_call_day$Number_of_calls >= 21, Student12_call_day$Number_of_calls-17,
                                                                         ifelse(Student12_call_day$Number_of_calls >= 15, Student12_call_day$Number_of_calls-7,
                                                                                ifelse(Student12_call_day$Number_of_calls <= 14, Student12_call_day$Number_of_calls, "None")))))))

Student13_call_day$Number_of_calls <- ifelse(Student13_call_day$Number_of_calls >= 51, Student13_call_day$Number_of_calls-47,
                                             ifelse(Student13_call_day$Number_of_calls >= 41, Student13_call_day$Number_of_calls-37,
                                                    ifelse(Student13_call_day$Number_of_calls >= 31, Student13_call_day$Number_of_calls-27,
                                                           ifelse(Student13_call_day$Number_of_calls >= 21, Student13_call_day$Number_of_calls-17,
                                                                  ifelse(Student13_call_day$Number_of_calls >= 15, Student13_call_day$Number_of_calls-7,
                                                                         ifelse(Student13_call_day$Number_of_calls <= 14, Student13_call_day$Number_of_calls, "None"))))))




#Assigning a number to each person for clarity
Emma_call_day$id <- 1
Claire_call_day$id <- 2
Aparna_call_day$id <- 3
Zach_call_day$id <- 4
Caton_call_day$id <- 5
Student6_call_day$id <- 6
Student7_call_day$id <- 7
Student8_call_day$id <- 8
Student9_call_day$id <- 9
Student10_call_day$id <- 10
Student11_call_day$id <- 11
Student12_call_day$id <- 12
Student13_call_day$id <- 13


# merging each cleaned dataset with ID numbers
calls_ALL_MERGED <- rbind(Emma_call_day,Claire_call_day, Aparna_call_day, Zach_call_day, Caton_call_day, Student6_call_day, Student7_call_day, Student8_call_day, Student9_call_day, Student10_call_day, Student11_call_day, Student12_call_day, Student13_call_day)

#Splitting the dataframe into pre and post spring break data
Emma_call_day_pre <- Emma_call_day[1:16,]
Emma_call_day_post <- Emma_call_day[17:25,]

Claire_call_day_pre <- Claire_call_day[1:34,]
Claire_call_day_post <- Claire_call_day[35:75,]

Aparna_call_day_pre <- Aparna_call_day[1:19,]
Aparna_call_day_post <- Aparna_call_day[20:52,]

Zach_call_day_pre <- Zach_call_day[1:34,]
Zach_call_day_post <- Zach_call_day[35:75,]

Caton_call_day_pre <- Caton_call_day[1:15,]
Caton_call_day_post <- Caton_call_day[16:20,]

Student6_call_day_pre <- Student6_call_day[1:25,]
Student6_call_day_post <- Student6_call_day[26:59,]

Student7_call_day_pre <- Student7_call_day[1:35,]
Student7_call_day_post <- Student7_call_day[36:73,]

Student8_call_day_pre <- Student8_call_day[1:29,]
Student8_call_day_post <- Student8_call_day[30:58,]

Student9_call_day_pre <- Student9_call_day[1:41,]
Student9_call_day_post <- Student9_call_day[42:74,]

Student10_call_day_pre <- Student10_call_day[1:30,]
Student10_call_day_post <- Student10_call_day[31:48,]

Student11_call_day_pre <- Student11_call_day[1:11,]
Student11_call_day_post <- Student11_call_day[12:22,]

Student12_call_day_pre <- Student12_call_day[1:34,]
Student12_call_day_post <- Student12_call_day[35:46,]

Student13_call_day_pre <- Student13_call_day[1:11,]
Student13_call_day_post <- Student13_call_day[12:14,]

# merging each cleaned dataset with ID numbers
calls_ALL_PRE <- rbind(Emma_call_day_pre,Claire_call_day_pre,Aparna_call_day_pre, Zach_call_day_pre, Caton_call_day_pre, Student6_call_day_pre, Student7_call_day_pre, Student8_call_day_pre, Student9_call_day_pre, Student10_call_day_pre, Student11_call_day_pre, Student12_call_day_pre, Student13_call_day_pre)

# merging each cleaned dataset with ID numbers
calls_ALL_POST <- rbind(Emma_call_day_post,Claire_call_day_post,Aparna_call_day_post, Zach_call_day_post, Caton_call_day_post, Student6_call_day_post, Student7_call_day_post, Student8_call_day_post, Student9_call_day_post, Student10_call_day_post, Student11_call_day_post, Student12_call_day_post, Student13_call_day_post)

