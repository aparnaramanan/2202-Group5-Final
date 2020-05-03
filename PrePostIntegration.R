# Aparna Ramanan - Final Integration for RShiny

### Screen and PIEL Integration of Pre and Post
sp_pre$Time = "Pre"
sp_post$Time = "Post"
sp <- rbind(sp_pre,sp_post)

# Calls and PIEL Integration of Pre and Post
cp_pre$Time = "Pre"
cp_post$Time = "Post"
cp <- rbind(cp_pre,cp_post)

# Activities and PIEL Integration of Pre and Post
ap_pre$Time = "Pre"
ap_post$Time = "Post"
ap <- rbind(ap_pre,ap_post)
ap$Week = NULL

# Graphing Screen and PIEL Integrated TEST
Times_Unlocked <- sp$unlock_count
sp$ID <- as.character(sp$ID)
screen_productivity<- ggplot(sp, aes(x = Times_Unlocked, y = Productivity)) + geom_point(size=2, aes(color = ID)) +
  ggtitle("Times Phone Unlocked Vs. Productivity") + geom_blank() + stat_smooth(method = 'lm',aes(color = ID), se = FALSE)

# Graphing Screen and Productivity Integrated
Times_Unlocked <- sp$unlock_count
sp$ID <- as.character(sp$ID)
screen_productivity<- ggplot(sp, aes(x = Times_Unlocked, y = Productivity)) + geom_point(size=2, aes(color = Time)) +
  ggtitle("Times Phone Unlocked Vs. Productivity") + geom_blank() + stat_smooth(method = 'lm',aes(color = Time), se = FALSE)

# Graphing Calls and Productivity Integrated
Time_on_Calls <- cp$Freq
cp$ID <- as.character(cp_pre$ID)
calls_productivity <- ggplot(cp, aes(x = Time_on_Calls, y = Productivity)) + geom_point(size=2, aes(color = Time)) + 
  ggtitle("Time on Calls Vs. Productivity") + geom_blank() + stat_smooth(method = 'lm',aes(color = Time), se = FALSE)

# Graphing Activities and Productivity Integrated
Number_of_Stationaries <- ap$still_count
ap$ID <- as.character(ap$ID)
activities_productivity <- ggplot(ap, aes(x = Number_of_Stationaries, y = Productivity)) + geom_point(size=2, aes(color = Time)) + 
  ggtitle("Number of Stationaries Vs. Productivity") + geom_blank()+ stat_smooth(method = 'lm',aes(color = Time), se = FALSE)

# Graphing Screen and Wellbeing Integrated
Times_Unlocked <- sp$unlock_count
sp$ID <- as.character(sp$ID)
screen_wellbeing<- ggplot(sp, aes(x = Times_Unlocked, y = Wellbeing)) + geom_point(size=2, aes(color = Time)) +
  ggtitle("Times Phone Unlocked Vs. Wellbeing") + geom_blank() + stat_smooth(method = 'lm',aes(color = Time), se = FALSE)

# Graphing Calls and Wellbeing Integrated
Time_on_Calls <- cp$Freq
cp$ID <- as.character(cp_pre$ID)
calls_wellbeing <- ggplot(cp, aes(x = Time_on_Calls, y = Wellbeing)) + geom_point(size=2, aes(color = Time)) + 
  ggtitle("Time on Calls Vs. Wellbeing") + geom_blank() + stat_smooth(method = 'lm',aes(color = Time), se = FALSE)

# Graphing Activities and Wellbeing Integrated
Number_of_Stationaries <- ap$still_count
ap$ID <- as.character(ap$ID)
activities_wellbeing<- ggplot(ap, aes(x = Number_of_Stationaries, y = Wellbeing)) + geom_point(size=2, aes(color = Time)) + 
  ggtitle("Number of Stationaries Vs. Wellbeing") + stat_smooth(method = 'lm',aes(color = Time), se = FALSE)




