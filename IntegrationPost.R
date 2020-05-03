# Aparna Ramanan - Integration and Plotting

### Cleaning Dates for Screen Vs. PIEL Post Quarantine
library(dplyr)
names(post_break_screen)[1] <- "ID"
names(post_break_screen)[2] <- "Date"
post_break_screen$ID <- as.numeric(as.character(post_break_screen$ID)) # converts id to number in order to match data sets
sp_post <- left_join(post_break_screen, piel_post, by=c('ID','Date'))
sp_post <- filter(sp_post,is.na(Mood) == FALSE) # gets rid of days where there wasn't enough data

### Cleaning Dates for Activities Vs. PIEL Post Quarantine
library(dplyr)
names(activities_post)[4] <- "ID"
names(activities_post)[1] <- "Date"
ap_post <- left_join(activities_post, piel_post, by=c('ID','Date'))
ap_post <- filter(ap_post,is.na(Mood) == FALSE)

### Cleaning Dates for Calls Vs. PIEL Post Quarantine
library(dplyr)
names(calls_ALL_POST)[3] <- "ID"
names(calls_ALL_POST)[1] <- "Date"
calls_ALL_POST$Date <- as.Date(calls_ALL_POST$Date)
cp_post <- left_join(calls_ALL_POST, piel_post, by=c('ID','Date'))
cp_post <- filter(cp_post,is.na(Mood) == FALSE) 

### Plotting Screen Vs. PIEL Post Quarantine
Times_Unlocked <- sp_post$unlock_count
sp_post$ID <- as.character(sp_post$ID)
screen_productivity_post <- ggplot(sp_post, aes(x = Times_Unlocked, y = Productivity)) + geom_point(size=2, aes(color = ID)) + 
  scale_color_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10","11","12","13")) +
  ggtitle("Times Phone Unlocked Vs. Productivity (Post)") + geom_blank()

Times_Unlocked <- sp_post$unlock_count
sp_post$ID <- as.character(sp_post$ID)
screen_wellbeing_post <- ggplot(sp_post, aes(x = Times_Unlocked, y = Wellbeing)) + geom_point(size=2, aes(color = ID)) + 
  scale_color_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10","11","12","13")) +
  ggtitle("Times Phone Unlocked Vs. Wellbeing (Post)") + geom_blank()

### Plotting Activities Vs. PIEL Post Quarantine
Number_of_Stationaries <- ap_post$still_count
ap_post$ID <- as.character(ap_post$ID)
activities_productivity_post <- ggplot(ap_post, aes(x = Number_of_Stationaries, y = Productivity)) + geom_point(size=2, aes(color = ID)) + 
  scale_color_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10","11","12","13")) +
  ggtitle("Number of Stationaries Vs. Productivity (Post)") + geom_blank()

Number_of_Stationaries <- ap_post$still_count
ap_post$ID <- as.character(ap_post$ID)
activities_wellbeing_post <- ggplot(ap_post, aes(x = Number_of_Stationaries, y = Wellbeing)) + geom_point(size=2, aes(color = ID)) + 
  scale_color_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10","11","12","13")) +
  ggtitle("Number of Stationaries Vs. Wellbeing (Post)") + geom_blank()

### Plotting Calls Vs. PIEL Post Quarantine
Time_on_Calls <- cp_post$Freq
cp_post$ID <- as.character(cp_post$ID)
calls_productivity_post <- ggplot(cp_post, aes(x = Time_on_Calls, y = Productivity)) + geom_point(size=2, aes(color = ID)) + 
  scale_color_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10","11","12","13")) +
  ggtitle("Time on Calls Vs. Productivity (Post)") + geom_blank()

Time_on_Calls <- cp_post$Freq
cp_post$ID <- as.character(cp_post$ID)
calls_wellbeing_post <- ggplot(cp_post, aes(x = Time_on_Calls, y = Wellbeing)) + geom_point(size=2, aes(color = ID)) + 
  scale_color_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10","11","12","13")) +
  ggtitle("Time on Calls Vs. Wellbeing (Post)") + geom_blank()