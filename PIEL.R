# Claire Miller - Cleaning and Binning All PIEL Data

library(tidyverse)
library(dplyr)
library(plyr)

#PRE SPRING BREAK
sample1 <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/1pre.csv",sep=",")
#delete unnecessary columns 
sample1 <- sample1 %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample1 <- rename(sample1, replace = c("X9" = "Mood"))
sample1 <- rename(sample1, replace = c("X10" = "Productivity"))
sample1 <- rename(sample1, replace = c("X11" = "Stress"))
sample1$Wellbeing <- (sample1$Mood + (1-sample1$Stress))/2
sample1 <- cbind(ID = 1, sample1)

sample2 <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/2pre.csv",sep=",")
#delete unnecessary columns 
sample2 <- sample2 %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample2 <- rename(sample2, replace = c("X9" = "Mood"))
sample2 <- rename(sample2, replace = c("X10" = "Productivity"))
sample2 <- rename(sample2, replace = c("X11" = "Stress"))
sample2$Wellbeing <- (sample2$Mood + (1-sample2$Stress))/2
sample2 <- cbind(ID = 2, sample2)

sample3 <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/3pre.csv",sep=",")
#delete unnecessary columns 
sample3 <- sample3 %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample3 <- rename(sample3, replace = c("X9" = "Mood"))
sample3 <- rename(sample3, replace = c("X10" = "Productivity"))
sample3 <- rename(sample3, replace = c("X11" = "Stress"))
sample3$Wellbeing <- (sample3$Mood + (1-sample3$Stress))/2
sample3 <- cbind(ID = 3, sample3)

sample4 <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/4pre.csv",sep=",")
#delete unnecessary columns 
sample4 <- sample4 %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample4 <- rename(sample4, replace = c("X9" = "Mood"))
sample4 <- rename(sample4, replace = c("X10" = "Productivity"))
sample4 <- rename(sample4, replace = c("X11" = "Stress"))
sample4$Wellbeing <- (sample4$Mood + (1-sample4$Stress))/2
sample4 <- cbind(ID = 4, sample4)

sample5 <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/5pre.csv",sep=",")
#delete unnecessary columns 
sample5 <- sample5 %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample5 <- rename(sample5, replace = c("X9" = "Mood"))
sample5 <- rename(sample5, replace = c("X10" = "Productivity"))
sample5 <- rename(sample5, replace = c("X11" = "Stress"))
sample5$Wellbeing <- (sample5$Mood + (1-sample5$Stress))/2
sample5 <- cbind(ID = 5, sample5)

sample6 <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/6pre (1).csv",sep=",")
#delete unnecessary columns 
sample6 <- sample6 %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample6 <- rename(sample6, replace = c("X9" = "Mood"))
sample6 <- rename(sample6, replace = c("X10" = "Productivity"))
sample6 <- rename(sample6, replace = c("X11" = "Stress"))
sample6$Wellbeing <- (sample6$Mood + (1-sample6$Stress))/2
sample6 <- cbind(ID = 6, sample6)

sample7 <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/7pre (1).csv",sep=",")
#delete unnecessary columns 
sample7 <- sample7 %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample7 <- rename(sample7, replace = c("X9" = "Mood"))
sample7 <- rename(sample7, replace = c("X10" = "Productivity"))
sample7 <- rename(sample7, replace = c("X11" = "Stress"))
sample7$Wellbeing <- (sample7$Mood + (1-sample7$Stress))/2
sample7 <- cbind(ID = 7, sample7)

sample8 <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/8pre (1).csv",sep=",")
#delete unnecessary columns 
sample8 <- sample8 %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample8 <- rename(sample8, replace = c("X9" = "Mood"))
sample8 <- rename(sample8, replace = c("X10" = "Productivity"))
sample8 <- rename(sample8, replace = c("X11" = "Stress"))
sample8$Wellbeing <- (sample8$Mood + (1-sample8$Stress))/2
sample8 <- cbind(ID = 8, sample8)

sample9 <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/9pre (1).csv",sep=",")
#delete unnecessary columns 
sample9 <- sample9 %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample9 <- rename(sample9, replace = c("X9" = "Mood"))
sample9 <- rename(sample9, replace = c("X10" = "Productivity"))
sample9 <- rename(sample9, replace = c("X11" = "Stress"))
sample9$Wellbeing <- (sample9$Mood + (1-sample9$Stress))/2
sample9 <- cbind(ID = 9, sample9)

sample10 <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/10pre (1).csv",sep=",")
#delete unnecessary columns 
sample10 <- sample10 %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample10 <- rename(sample10, replace = c("X9" = "Mood"))
sample10 <- rename(sample10, replace = c("X10" = "Productivity"))
sample10 <- rename(sample10, replace = c("X11" = "Stress"))
sample10$Wellbeing <- (sample10$Mood + (1-sample10$Stress))/2
sample10 <- cbind(ID = 10, sample10)

sample11 <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/11pre (1).csv",sep=",")
#delete unnecessary columns 
sample11 <- sample11 %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample11 <- rename(sample11, replace = c("X9" = "Mood"))
sample11 <- rename(sample11, replace = c("X10" = "Productivity"))
sample11 <- rename(sample11, replace = c("X11" = "Stress"))
sample11$Wellbeing <- (sample11$Mood + (1-sample11$Stress))/2
sample11 <- cbind(ID = 11, sample11)

sample12 <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/12pre (1).csv",sep=",")
#delete unnecessary columns 
sample12 <- sample12 %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample12 <- rename(sample12, replace = c("X9" = "Mood"))
sample12 <- rename(sample12, replace = c("X10" = "Productivity"))
sample12 <- rename(sample12, replace = c("X11" = "Stress"))
sample12$Wellbeing <- (sample12$Mood + (1-sample12$Stress))/2
sample12 <- cbind(ID = 12, sample12)

sample13 <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/13pre (1).csv",sep=",")
#delete unnecessary columns 
sample13 <- sample13 %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample13 <- rename(sample13, replace = c("X9" = "Mood"))
sample13 <- rename(sample13, replace = c("X10" = "Productivity"))
sample13 <- rename(sample13, replace = c("X11" = "Stress"))
sample13$Wellbeing <- (sample13$Mood + (1-sample13$Stress))/2
sample13 <- cbind(ID = 13, sample13)



#aggregating all data
piel_pre <- rbind(sample1, sample2, sample3, sample4, sample5, sample6, sample7, sample8, sample9, sample10, sample11, sample12, sample13)

#binning by week
piel_pre$Date <- as.Date(format(as.Date(piel_pre$Date, "%m/%d/%Y"),format="%Y-%m-%d"))
piel_pre <- piel_pre %>% mutate(Week = ((as.numeric(Date) %/% 7))-2613) 





# POST SPRING BREAK
sample1_p <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/1post (2).csv",sep=",")
#delete unnecessary columns 
sample1_p <- sample1_p %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample1_p <- rename(sample1_p, replace = c("X9" = "Mood"))
sample1_p <- rename(sample1_p, replace = c("X10" = "Productivity"))
sample1_p <- rename(sample1_p, replace = c("X11" = "Stress"))
sample1_p$Wellbeing <- (sample1_p$Mood + (1-sample1_p$Stress))/2
sample1_p <- cbind(ID = 1, sample1_p)

sample2_p <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/2post (1).csv",sep=",")
#delete unnecessary columns 
sample2_p <- sample2_p %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample2_p <- rename(sample2_p, replace = c("X9" = "Mood"))
sample2_p <- rename(sample2_p, replace = c("X10" = "Productivity"))
sample2_p <- rename(sample2_p, replace = c("X11" = "Stress"))
sample2_p$Wellbeing <- (sample2_p$Mood + (1-sample2_p$Stress))/2
sample2_p <- cbind(ID = 2, sample2_p)

sample3_p <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/3post (1).csv",sep=",")
#delete unnecessary columns 
sample3_p <- sample3_p %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample3_p <- rename(sample3_p, replace = c("X9" = "Mood"))
sample3_p <- rename(sample3_p, replace = c("X10" = "Productivity"))
sample3_p <- rename(sample3_p, replace = c("X11" = "Stress"))
sample3_p$Wellbeing <- (sample3_p$Mood + (1-sample3_p$Stress))/2
sample3_p <- cbind(ID = 3, sample3_p)

sample4_p <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/4post (1).csv",sep=",")
#delete unnecessary columns 
sample4_p <- sample4_p %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample4_p <- rename(sample4_p, replace = c("X9" = "Mood"))
sample4_p <- rename(sample4_p, replace = c("X10" = "Productivity"))
sample4_p <- rename(sample4_p, replace = c("X11" = "Stress"))
sample4_p$Wellbeing <- (sample4_p$Mood + (1-sample4_p$Stress))/2
sample4_p <- cbind(ID = 4, sample4_p)

sample5_p <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/5post (1).csv",sep=",")
#delete unnecessary columns 
sample5_p <- sample5_p %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample5_p <- rename(sample5_p, replace = c("X9" = "Mood"))
sample5_p <- rename(sample5_p, replace = c("X10" = "Productivity"))
sample5_p <- rename(sample5_p, replace = c("X11" = "Stress"))
sample5_p$Wellbeing <- (sample5_p$Mood + (1-sample5_p$Stress))/2
sample5_p <- cbind(ID = 5, sample5_p)

sample6_p <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/6post (1).csv",sep=",")
#delete unnecessary columns 
sample6_p <- sample6_p %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample6_p <- rename(sample6_p, replace = c("X9" = "Mood"))
sample6_p <- rename(sample6_p, replace = c("X10" = "Productivity"))
sample6_p <- rename(sample6_p, replace = c("X11" = "Stress"))
sample6_p$Wellbeing <- (sample6_p$Mood + (1-sample6_p$Stress))/2
sample6_p <- cbind(ID = 6, sample6_p)

sample7_p <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/7post (1).csv",sep=",")
#delete unnecessary columns 
sample7_p <- sample7_p %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample7_p <- rename(sample7_p, replace = c("X9" = "Mood"))
sample7_p <- rename(sample7_p, replace = c("X10" = "Productivity"))
sample7_p <- rename(sample7_p, replace = c("X11" = "Stress"))
sample7_p$Wellbeing <- (sample7_p$Mood + (1-sample7_p$Stress))/2
sample7_p <- cbind(ID = 7, sample7_p)

sample8_p <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/8post (1).csv",sep=",")
#delete unnecessary columns 
sample8_p <- sample8_p %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample8_p <- rename(sample8_p, replace = c("X9" = "Mood"))
sample8_p <- rename(sample8_p, replace = c("X10" = "Productivity"))
sample8_p <- rename(sample8_p, replace = c("X11" = "Stress"))
sample8_p$Wellbeing <- (sample8_p$Mood + (1-sample8_p$Stress))/2
sample8_p <- cbind(ID = 8, sample8_p)

sample9_p <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/9post (1).csv",sep=",")
#delete unnecessary columns 
sample9_p <- sample9_p %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample9_p <- rename(sample9_p, replace = c("X9" = "Mood"))
sample9_p <- rename(sample9_p, replace = c("X10" = "Productivity"))
sample9_p <- rename(sample9_p, replace = c("X11" = "Stress"))
sample9_p$Wellbeing <- (sample9_p$Mood + (1-sample9_p$Stress))/2
sample9_p <- cbind(ID = 9, sample9_p)

sample10_p <- read.csv(file="//Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/10post (1).csv",sep=",")
#delete unnecessary columns 
sample10_p <- sample10_p %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample10_p <- rename(sample10_p, replace = c("X9" = "Mood"))
sample10_p <- rename(sample10_p, replace = c("X10" = "Productivity"))
sample10_p <- rename(sample10_p, replace = c("X11" = "Stress"))
sample10_p$Wellbeing <- (sample10_p$Mood + (1-sample10_p$Stress))/2
sample10_p <- cbind(ID = 10, sample10_p)

sample11_p <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/11post (1).csv",sep=",")
#delete unnecessary columns 
sample11_p <- sample11_p %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample11_p <- rename(sample11_p, replace = c("X9" = "Mood"))
sample11_p <- rename(sample11_p, replace = c("X10" = "Productivity"))
sample11_p <- rename(sample11_p, replace = c("X11" = "Stress"))
sample11_p$Wellbeing <- (sample11_p$Mood + (1-sample11_p$Stress))/2
sample11_p <- cbind(ID = 11, sample11_p)

sample12_p <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/12post (1).csv",sep=",")
#delete unnecessary columns 
sample12_p <- sample12_p %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample12_p <- rename(sample12_p, replace = c("X9" = "Mood"))
sample12_p <- rename(sample12_p, replace = c("X10" = "Productivity"))
sample12_p <- rename(sample12_p, replace = c("X11" = "Stress"))
sample12_p$Wellbeing <- (sample12_p$Mood + (1-sample12_p$Stress))/2
sample12_p <- cbind(ID = 12, sample12_p)

sample13_p <- read.csv(file="/Users/aparnaramanan/Documents/SYS 2202/FinalDataWrangling/PIEL/13post (1).csv",sep=",")
#delete unnecessary columns 
sample13_p <- sample13_p %>% select(Date,X9,X10,X11) #keeping mood, prod, and stress (last 3 columns w/ decimals)
#rename and add Wellbeing column 
sample13_p <- rename(sample13_p, replace = c("X9" = "Mood"))
sample13_p <- rename(sample13_p, replace = c("X10" = "Productivity"))
sample13_p <- rename(sample13_p, replace = c("X11" = "Stress"))
sample13_p$Wellbeing <- (sample13_p$Mood + (1-sample13_p$Stress))/2
sample13_p <- cbind(ID = 13, sample13_p)

#aggregating all data
piel_post <- rbind(sample1_p, sample2_p, sample3_p, sample4_p, sample5_p, sample6_p, sample7_p, sample8_p, sample9_p, sample10_p, sample11_p, sample12_p, sample13_p)

#binning by week
piel_post$Date <- as.Date(format(as.Date(piel_post$Date, "%m/%d/%Y"),format="%Y-%m-%d"))
piel_post <- piel_post %>% mutate(Week = ((as.numeric(Date) %/% 7))+101740) 




