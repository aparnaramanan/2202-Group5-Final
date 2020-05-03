#load required libraries
library(readxl)
library(lubridate)
library(tidyverse)
library(dplyr)
library(anytime)
library(data.table)
library(plyr)
library(padr)
library(zoo)

#read in the csv file of screen aware data 1
screen1 <- read.csv(file = '/Users/emmaw/Documents/sys2202/screen1.csv', sep = ",")
#read in the csv file of screen aware data 2
screen2 <- read.csv(file = '/Users/emmaw/Downloads/screen2.csv', sep = ",")
#read in the csv file of screen aware data 3
screen3 <- read.csv(file = '/Users/emmaw/Downloads/screen3.csv', sep = ",")
#read in the csv file of screen aware data 4
screen4 <- read.csv(file = '/Users/emmaw/Downloads/screen4.csv', sep = ",")
#read in the csv file of screen aware data 5
screen5 <- read.csv(file = '/Users/emmaw/Downloads/screen5.csv', sep = ",")
#read in the csv file of screen aware data 6
screen6 <- read.csv(file = '/Users/emmaw/Downloads/screen6.csv', sep = ",")
#read in the csv file of screen aware data 7
screen7 <- read.csv(file = '/Users/emmaw/Downloads/screen7.csv', sep = ",")
#read in the csv file of screen aware data 8
screen8 <- read.csv(file = '/Users/emmaw/Downloads/screen8.csv', sep = ",")
#read in the csv file of screen aware data 9
screen9 <- read.csv(file = '/Users/emmaw/Downloads/screen9.csv', sep = ",")
#read in the csv file of screen aware data 10
screen10 <- read.csv(file = '/Users/emmaw/Downloads/screen10.csv', sep = ",")
#read in the csv file of screen aware data 11
screen11 <- read.csv(file = '/Users/emmaw/Downloads/screen11.csv', sep = ",")
#read in the csv file of screen aware data 12
screen12 <- read.csv(file = '/Users/emmaw/Downloads/screen12.csv', sep = ",")
#read in the csv file of screen aware data 13
screen13 <- read.csv(file = '/Users/emmaw/Downloads/screen13.csv', sep = ",")

########################- Dates -########################

#create a vector of timestamps, transform to matrix
timeVector1 <- as.vector(screen1['timestamp'])
timeVector1 <- data.matrix(timeVector1)
#convert timestamps to POSIXct, add the matrix to original data frame
screen1$DateTime <- as.POSIXct(timeVector1/1000, origin="1970-01-01")
screen1$DateTime <- anydate(screen1$DateTime)

timeVector2 <- as.vector(screen2['timestamp'])
timeVector2 <- data.matrix(timeVector2)
screen2$DateTime <- as.POSIXct(timeVector2/1000, origin="1970-01-01")
screen2$DateTime <- anydate(screen2$DateTime)

timeVector3 <- as.vector(screen3['timestamp'])
timeVector3 <- data.matrix(timeVector3)
screen3$DateTime <- as.POSIXct(timeVector3/1000, origin="1970-01-01")
screen3$DateTime <- anydate(screen3$DateTime)

timeVector4 <- as.vector(screen4['timestamp'])
timeVector4 <- data.matrix(timeVector4)
screen4$DateTime <- as.POSIXct(timeVector4/1000, origin="1970-01-01")
screen4$DateTime <- anydate(screen4$DateTime)

timeVector5 <- as.vector(screen5['timestamp'])
timeVector5 <- data.matrix(timeVector5)
screen5$DateTime <- as.POSIXct(timeVector5/1000, origin="1970-01-01")
screen5$DateTime <- anydate(screen5$DateTime)

timeVector6 <- as.vector(screen6['timestamp'])
timeVector6 <- data.matrix(timeVector6)
screen6$DateTime <- as.POSIXct(timeVector6/1000, origin="1970-01-01")
screen6$DateTime <- anydate(screen6$DateTime)

timeVector7 <- as.vector(screen7['timestamp'])
timeVector7 <- data.matrix(timeVector7)
screen7$DateTime <- as.POSIXct(timeVector7/1000, origin="1970-01-01")
screen7$DateTime <- anydate(screen7$DateTime)

timeVector8 <- as.vector(screen8['timestamp'])
timeVector8 <- data.matrix(timeVector8)
screen8$DateTime <- as.POSIXct(timeVector8/1000, origin="1970-01-01")
screen8$DateTime <- anydate(screen8$DateTime)

timeVector9 <- as.vector(screen9['timestamp'])
timeVector9 <- data.matrix(timeVector9)
screen9$DateTime <- as.POSIXct(timeVector9/1000, origin="1970-01-01")
screen9$DateTime <- anydate(screen9$DateTime)

timeVector10 <- as.vector(screen10['timestamp'])
timeVector10 <- data.matrix(timeVector10)
screen10$DateTime <- as.POSIXct(timeVector10/1000, origin="1970-01-01")
screen10$DateTime <- anydate(screen10$DateTime)

timeVector11 <- as.vector(screen11['timestamp'])
timeVector11 <- data.matrix(timeVector11)
screen11$DateTime <- as.POSIXct(timeVector11/1000, origin="1970-01-01")
screen11$DateTime <- anydate(screen11$DateTime)

timeVector12 <- as.vector(screen12['timestamp'])
timeVector12 <- data.matrix(timeVector12)
screen12$DateTime <- as.POSIXct(timeVector12/1000, origin="1970-01-01")
screen12$DateTime <- anydate(screen12$DateTime)

timeVector13 <- as.vector(screen13['timestamp'])
timeVector13 <- data.matrix(timeVector13)
screen13$DateTime <- as.POSIXct(timeVector13/1000, origin="1970-01-01")
screen13$DateTime <- anydate(screen13$DateTime)

########################- Total Count -########################

#create data frame of total count per day
count1 <- data.frame(table(screen1$DateTime))
count2 <- data.frame(table(screen2$DateTime))
count3 <- data.frame(table(screen3$DateTime))
count4 <- data.frame(table(screen4$DateTime))
count5 <- data.frame(table(screen5$DateTime))
count6 <- data.frame(table(screen6$DateTime))
count7 <- data.frame(table(screen7$DateTime))
count8 <- data.frame(table(screen8$DateTime))
count9 <- data.frame(table(screen9$DateTime))
count10 <- data.frame(table(screen10$DateTime))
count11 <- data.frame(table(screen11$DateTime))
count12 <- data.frame(table(screen12$DateTime))
count13 <- data.frame(table(screen13$DateTime))

count1$id <- '1'
count2$id <- '2'
count3$id <- '3'
count4$id <- '4'
count5$id <- '5'
count6$id <- '6'
count7$id <- '7'
count8$id <- '8'
count9$id <- '9'
count10$id <- '10'
count11$id <- '11'
count12$id <- '12'
count13$id <- '13'

########################- Unlocked Count -########################

#remove all locked rows (when screen status is 2)
screen1 <- screen1[screen1$screen_status == 3, ]
screen2 <- screen2[screen2$screen_status == 3, ]
screen3 <- screen3[screen3$screen_status == 3, ]
screen4 <- screen4[screen4$screen_status == 3, ]
screen5 <- screen5[screen5$screen_status == 3, ]
screen6 <- screen6[screen6$screen_status == 3, ]
screen7 <- screen7[screen7$screen_status == 3, ]
screen8 <- screen8[screen8$screen_status == 3, ]
screen9 <- screen9[screen9$screen_status == 3, ]
screen10 <- screen10[screen10$screen_status == 3, ]
screen11 <- screen11[screen11$screen_status == 3, ]
screen12 <- screen12[screen12$screen_status == 3, ]
screen13 <- screen13[screen13$screen_status == 3, ]

#convert movement column to factor and then to numeric
screen1$screen_status <- as.numeric(as.factor(screen1$screen_status))
screen2$screen_status <- as.numeric(as.factor(screen2$screen_status))
screen3$screen_status <- as.numeric(as.factor(screen3$screen_status))
screen4$screen_status <- as.numeric(as.factor(screen4$screen_status))
screen5$screen_status <- as.numeric(as.factor(screen5$screen_status))
screen6$screen_status <- as.numeric(as.factor(screen6$screen_status))
screen7$screen_status <- as.numeric(as.factor(screen7$screen_status))
screen8$screen_status <- as.numeric(as.factor(screen8$screen_status))
screen9$screen_status <- as.numeric(as.factor(screen9$screen_status))
screen10$screen_status <- as.numeric(as.factor(screen10$screen_status))
screen11$screen_status <- as.numeric(as.factor(screen11$screen_status))
screen12$screen_status <- as.numeric(as.factor(screen12$screen_status))
screen13$screen_status <- as.numeric(as.factor(screen13$screen_status))

#create a dataframe of unlocked count per day
unlocked1 <- aggregate(screen1$screen_status,by=list(screen1$DateTime),FUN=sum)
unlocked2 <- aggregate(screen2$screen_status,by=list(screen2$DateTime),FUN=sum)
unlocked3 <- aggregate(screen3$screen_status,by=list(screen3$DateTime),FUN=sum)
unlocked4 <- aggregate(screen4$screen_status,by=list(screen4$DateTime),FUN=sum)
unlocked5 <- aggregate(screen5$screen_status,by=list(screen5$DateTime),FUN=sum)
unlocked6 <- aggregate(screen6$screen_status,by=list(screen6$DateTime),FUN=sum)
unlocked7 <- aggregate(screen7$screen_status,by=list(screen7$DateTime),FUN=sum)
unlocked8 <- aggregate(screen8$screen_status,by=list(screen8$DateTime),FUN=sum)
unlocked9 <- aggregate(screen9$screen_status,by=list(screen9$DateTime),FUN=sum)
unlocked10 <- aggregate(screen10$screen_status,by=list(screen10$DateTime),FUN=sum)
unlocked11 <- aggregate(screen11$screen_status,by=list(screen11$DateTime),FUN=sum)
unlocked12 <- aggregate(screen12$screen_status,by=list(screen12$DateTime),FUN=sum)
unlocked13 <- aggregate(screen13$screen_status,by=list(screen13$DateTime),FUN=sum)

#adjust for factor of 3, round off counts
unlocked1$x <- round(as.numeric(as.character(unlocked1$x)) / 3)
unlocked2$x <- round(as.numeric(as.character(unlocked2$x)) / 3)
unlocked3$x <- round(as.numeric(as.character(unlocked3$x)) / 3)
unlocked4$x <- round(as.numeric(as.character(unlocked4$x)) / 3)
unlocked5$x <- round(as.numeric(as.character(unlocked5$x)) / 3)
unlocked6$x <- round(as.numeric(as.character(unlocked6$x)) / 3)
unlocked7$x <- round(as.numeric(as.character(unlocked7$x)) / 3)
unlocked8$x <- round(as.numeric(as.character(unlocked8$x)) / 3)
unlocked9$x <- round(as.numeric(as.character(unlocked9$x)) / 3)
unlocked10$x <- round(as.numeric(as.character(unlocked10$x)) / 3)
unlocked11$x <- round(as.numeric(as.character(unlocked11$x)) / 3)
unlocked12$x <- round(as.numeric(as.character(unlocked12$x)) / 3)
unlocked13$x <- round(as.numeric(as.character(unlocked13$x)) / 3)

#removing nonmatching rows
count7 = filter(count7, Freq != "1")
count10 = filter(count10, Freq != "1")
count11 = filter(count11, Freq != "1")

#count7 = count7[-21,]

#added count unlocked per day to count
count1 <- cbind(count1, unlocked = unlocked1$x)
count2 <- cbind(count2, unlocked = unlocked2$x)
count3 <- cbind(count3, unlocked = unlocked3$x)
count4 <- cbind(count4, unlocked = unlocked4$x)
count5 <- cbind(count5, unlocked = unlocked5$x)
count6 <- cbind(count6, unlocked = unlocked6$x)
count7 <- cbind(count7, unlocked = unlocked7$x)
count8 <- cbind(count8, unlocked = unlocked8$x)
count9 <- cbind(count9, unlocked = unlocked9$x)
count10 <- cbind(count10, unlocked = unlocked10$x)
count11 <- cbind(count11, unlocked = unlocked11$x)
count12 <- cbind(count12, unlocked = unlocked12$x)
count13 <- cbind(count13, unlocked = unlocked13$x)


########################- Final Cleaning -########################

#Binning by week
#count1 <- count1 %>% mutate(Date = as.Date(Var1))
#count1 <- count1 %>% mutate(week = ((as.numeric(Date) %/% 7)-2611)) 
#count3 <- count3 %>% mutate(Date = as.Date(Var1))
#count3 <- count3 %>% mutate(week = ((as.numeric(Date) %/% 7)-2611)) 
#count4 <- count4 %>% mutate(Date = as.Date(Var1))
#count4 <- count4 %>% mutate(week = ((as.numeric(Date) %/% 7)-2611)) 
#count5 <- count5 %>% mutate(Date = as.Date(Var1))
#count5 <- count5 %>% mutate(week = ((as.numeric(Date) %/% 7)-2611)) 

#combining files
post_break <- rbind(count1, count2, count3, count4, count5, count6, count7, 
                    count8, count9, count10, count11, count12, count13)

#deleting, renaming, reordering columns for clearer view
post_break <- post_break[,c(3,1,4,2)]
post_break <- rename(post_break, c("Var1" = "date", 
                                   "Freq" = "total_count", 
                                   "unlocked" = "unlock_count"))

#add appropriate rows to pre-break df
pre_break = data.frame()
post_break$date <- as.Date(post_break$date)
pre_break <- rbind(pre_break, post_break[which(post_break$date <= "2020-03-06"),])

#remove appropriate rows from post_break df
post_break <- post_break[-which(post_break$date <= ("2020-03-06")),]

#WEEKLY SCREEN AVGS
#finding mean screen usage for each week
#screen_avg1 <- with(screen_clean, round(mean(screen_clean$Unlocked[Week == 1])))
#screen_avg2 <- with(screen_clean, round(mean(screen_clean$Unlocked[Week == 2])))
#screen_avg3 <- with(screen_clean, round(mean(screen_clean$Unlocked[Week == 3])))
#screen_avg4 <- with(screen_clean, round(mean(screen_clean$Unlocked[Week == 4])))
#assigning mean screen usage to each week
#screen_clean$Week_Avg <- ifelse(screen_clean$Week == 1, screen_avg1,
#                                ifelse(screen_clean$Week == 2, screen_avg2,
#                                      ifelse(screen_clean$Week == 3, screen_avg3,
#                                            ifelse(screen_clean$Week == 4, screen_avg4, 0))))