# Aparna Ramanan - Graphing Individual

### Graphing Bar Graph of Pre Call Data in General
ggplot(data=cp_pre, aes(x=Date, y=Freq)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Frequency of Call Values per Day (Pre)") 

### Graphing Line Graph of Pre Call Data in General
ggplot(data=cp_pre, aes(x=Date, y=Freq)) + 
  geom_smooth(stat = "identity", fill = "steelblue") +
  ggtitle("Frequency of Call Values per Day (Pre)") 

### Graphing Bar Graph of Post Call Data in General
ggplot(data=cp_post, aes(x=Date, y=Freq)) + 
  geom_bar(stat = "identity", fill = "red") +
  ggtitle("Frequency of Call Values per Day (Post)")

### Graphing Line Graph of Post Call Data in General
ggplot(data=cp_post, aes(x=Date, y=Freq)) + 
  geom_smooth(stat = "identity", color = "red") +
  ggtitle("Frequency of Call Values per Day (Post)")

### Graphing Bar Graph of Pre Activities Data in General
ggplot(data=ap_pre, aes(x=Date, y=still_count)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Frequency of Stationary Values per Day (Pre)") 

### Graphing Line Graph of Pre Activities Data in General
ggplot(data=ap_pre, aes(x=Date, y=still_count)) + 
  geom_smooth(stat = "identity", fill = "steelblue") +
  ggtitle("Frequency of Stationary Values per Day (Pre)") 

### Graphing Bar Graph of Post Activities Data in General
ggplot(data=ap_post, aes(x=Date, y=still_count)) + 
  geom_bar(stat = "identity", fill = "red") +
  ggtitle("Frequency of Stationary Values per Day (Post)") 

### Graphing Line Graph of Post Activities Data in General
ggplot(data=ap_post, aes(x=Date, y=still_count)) + 
  geom_smooth(stat = "identity", color = "red") +
  ggtitle("Frequency of Stationary Values per Day (Post)") 

### Graphing Bar Graph of Pre Screen Data in General
ggplot(data=sp_pre, aes(x=Date, y=unlock_count)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Frequency of Unlock Values per Day (Pre)")

### Graphing Line Graph of Pre Screen Data in General
ggplot(data=sp_pre, aes(x=Date, y=unlock_count)) + 
  geom_smooth(stat = "identity", fill = "steelblue") +
  ggtitle("Frequency of Unlock Values per Day (Pre)")

### Graphing Bar Graph of Post Screen Data in General
ggplot(data=sp_post, aes(x=Date, y=unlock_count)) + 
  geom_bar(stat = "identity", fill = "red") +
  ggtitle("Frequency of Unlock Values per Day (Post)") 

### Graphing Line Graph of Post Screen Data in General
ggplot(data=sp_post, aes(x=Date, y=unlock_count)) + 
  geom_smooth(stat = "identity", color = "red") +
  ggtitle("Frequency of Unlock Values per Day (Post)") 

### Graphing Bar Graph of Pre Productivity Data in General
ggplot(data=sp_pre, aes(x=Date, y=Productivity)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Productivity per Day (Pre)") 

### Graphing Line Graph of Pre Productivity Data in General
ggplot(data=sp_pre, aes(x=Date, y=Productivity)) + 
  geom_smooth(stat = "identity", fill = "steelblue") +
  ggtitle("Productivity per Day (Pre)") 

### Graphing Bar Graph of Post Productivity Data in General
ggplot(data=sp_post, aes(x=Date, y=Productivity)) + 
  geom_bar(stat = "identity", fill = "red") +
  ggtitle("Productivity per Day (Post)") 

### Graphing Line Graph of Post Productivity Data in General
ggplot(data=sp_post, aes(x=Date, y=Productivity)) + 
  geom_smooth(stat = "identity", color = "red") +
  ggtitle("Productivity per Day (Post)") 

### Graphing Bar Graph of Pre Wellbeing Data in General
ggplot(data=sp_pre, aes(x=Date, y=Wellbeing)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Wellbeing per Day (Pre)") 

### Graphing Line Graph of Pre Wellbeing Data in General
ggplot(data=sp_pre, aes(x=Date, y=Wellbeing)) + 
  geom_smooth(stat = "identity", fill = "steelblue") +
  ggtitle("Wellbeing per Day (Pre)") 

### Graphing Bar Graph of Post Wellbeing Data in General
ggplot(data=sp_post, aes(x=Date, y=Wellbeing)) + 
  geom_bar(stat = "identity", fill = "red") +
  ggtitle("Wellbeing per Day (Post)") 

### Graphing Line Graph of Post Wellbeing Data in General
ggplot(data=sp_post, aes(x=Date, y=Wellbeing)) + 
  geom_smooth(stat = "identity", color = "red") +
  ggtitle("Wellbeing per Day (Post)") 


