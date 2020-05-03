## Aparna Ramanan - Integration and Plotting

### Cleaning Dates for Screen Vs. PIEL Pre Quarantine
library(dplyr)
names(pre_break_screen)[1] <- "ID"
names(pre_break_screen)[2] <- "Date"
pre_break_screen$ID <- as.numeric(as.character(pre_break_screen$ID))
sp_pre <- left_join(pre_break_screen, piel_pre, by=c('ID','Date'))
sp_pre <- filter(sp_pre,is.na(Mood) == FALSE) 

### Cleaning Dates for Activities Vs. PIEL Pre Quarantine
library(dplyr)
names(activities_pre)[4] <- "ID"
names(activities_pre)[1] <- "Date"
ap_pre <- left_join(activities_pre, piel_pre, by=c('ID','Date'))
ap_pre <- filter(ap_pre,is.na(Mood) == FALSE)

### Cleaning Dates for Calls Vs. PIEL Pre Quarantine
library(dplyr)
names(calls_ALL_PRE)[3] <- "ID"
names(calls_ALL_PRE)[1] <- "Date"
calls_ALL_PRE$Date <- as.Date(calls_ALL_PRE$Date)
cp_pre <- left_join(calls_ALL_PRE, piel_pre, by=c('ID','Date'))
cp_pre <- filter(cp_pre,is.na(Mood) == FALSE)

### Plotting Screen Vs. PIEL Pre Quarantine
Times_Unlocked <- sp_pre$unlock_count
sp_pre$ID <- as.character(sp_pre$ID)
screen_productivity_pre <- ggplot(sp_pre, aes(x = Times_Unlocked, y = Productivity)) + geom_point(size=2, aes(color = ID)) + 
  scale_color_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10","11","12","13")) +
  ggtitle("Times Phone Unlocked Vs. Productivity (Pre)") + geom_blank()

Times_Unlocked <- sp_pre$unlock_count
sp_pre$ID <- as.character(sp_pre$ID)
screen_wellbeing_pre <- ggplot(sp_pre, aes(x = Times_Unlocked, y = Wellbeing)) + geom_point(size=2, aes(color = ID)) + 
  scale_color_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10","11","12","13")) +
  ggtitle("Times Phone Unlocked Vs. Wellbeing (Pre)") + geom_blank()

### Plotting Activities Vs. PIEL Pre Quarantine
Number_of_Stationaries <- ap_pre$still_count
ap_pre$ID <- as.character(ap_pre$ID)
activities_productivity_pre <- ggplot(ap_pre, aes(x = Number_of_Stationaries, y = Productivity)) + geom_point(size=2, aes(color = ID)) + 
  scale_color_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10","11","12","13")) +
  ggtitle("Number of Stationaries Vs. Productivity (Pre)") + geom_blank()

Number_of_Stationaries <- ap_pre$still_count
ap_pre$ID <- as.character(ap_pre$ID)
activities_wellbeing_pre <- ggplot(ap_pre, aes(x = Number_of_Stationaries, y = Wellbeing)) + geom_point(size=2, aes(color = ID)) + 
  scale_color_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10","11","12","13")) +
  ggtitle("Number of Stationaries Vs. Wellbeing (Pre)") + geom_blank()

### Plotting Calls Vs. PIEL Pre Quarantine
Time_on_Calls <- cp_pre$Freq
cp_pre$ID <- as.character(cp_pre$ID)
calls_productivity_pre <- ggplot(cp_pre, aes(x = Time_on_Calls, y = Productivity)) + geom_point(size=2, aes(color = ID)) + 
  scale_color_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10","11","12","13")) +
  ggtitle("Time on Calls Vs. Productivity (Pre)") + geom_blank()

Time_on_Calls <- cp_pre$Freq
cp_pre$ID <- as.character(cp_pre$ID)
calls_wellbeing_pre <- ggplot(cp_pre, aes(x = Time_on_Calls, y = Wellbeing)) + geom_point(size=2, aes(color = ID)) + 
  scale_color_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10","11","12","13")) +
  ggtitle("Time on Calls Vs. Wellbeing (Pre)") + geom_blank()






