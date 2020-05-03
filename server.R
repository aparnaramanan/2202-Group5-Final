# Aparna Ramanan - RShiny Server


# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
#shinyServer(function(input, output) {
 #   output$distPlot <- renderPlot({
  #    if(input$x=='a' && input$y == 'd'){
   #     pre <- calls_productivity_pre
    #    post <- calls_productivity_post
     # }
      #else if(input$x=='a' && input$y == 'e'){
       # pre <- calls_wellbeing_pre
        #post <- calls_wellbeing_post
     # }
      #else if(input$x=='b' && input$y == 'd'){
       # pre <- activities_productivity_pre
        #post <- activities_productivity_post
      #}
      #else if(input$x=='b' && input$y == 'e'){
       # pre <- activities_wellbeing_pre
        #post <- activities_wellbeing_post
      #}
      #else if(input$x=='c' && input$y == 'd'){
       # pre <- screen_productivity_pre
        #post <- screen_productivity_post
      #}
      #else if(input$x=='c' && input$y == 'e'){
       # pre <- screen_wellbeing_pre
        #post <- screen_wellbeing_post
      #}
    #})
    #output$pre = renderPlot(pre)
    #output$post = renderPlot(post)
#})

#library(shiny) 
#shinyServer(function(input, output) {   
  #output$distPlot <- renderPlot({     
    #if(input$x=='a' && input$y == 'd'){       
     # xa <- cp_pre$Freq
      #ya <- cp_pre$Productivity
      #}     
    #if(input$x=='a' && input$y == 'e'){       
     # xa <- cp_pre$Freq
      #ya <- cp_pre$Wellbeing
    #}     
    #if(input$x=='b' && input$y == 'd'){       
     # xa <- ap_pre$still_count
      #ya <- ap_pre$Productivity
    #}
    #if(input$x=='b' && input$y == 'e'){       
     # xa <- ap_pre$still_count
      #ya <- ap_pre$Wellbeing
    #}
    #if(input$x=='c' && input$y == 'd'){       
     # xa <- sp_pre$unlock_count
      #ya <- sp_pre$Productivity
    #}
    #if(input$x=='c' && input$y == 'e'){       
     # xa <- sp_pre$unlock_count
      #ya <- sp_pre$Wellbeing
    #}
    #plot(xa,ya)
     #}) })

library(shiny) 
shinyServer(function(input, output) {
  # If statements that chooses the plot based on the selection of the radio Buttons
  output$Plot <- renderPlot({     
    # Calls and Productivity
    if(input$x=='a' && input$y == 'd'){       
      Time_on_Calls <- cp$Freq
      cp$ID <- as.character(cp$ID)
      calls_productivity <- ggplot(cp, aes(x = Time_on_Calls, y = Productivity)) + geom_point(size=2, aes(color = Time)) + 
        ggtitle("Time on Calls Vs. Productivity") + geom_blank() + stat_smooth(method = 'lm',aes(color = Time), se = FALSE)
      return(calls_productivity)
    }
    # Calls and Wellbeing
    if(input$x=='a' && input$y == 'e'){
      Time_on_Calls <- cp$Freq
      cp$ID <- as.character(cp$ID)
      calls_wellbeing <- ggplot(cp, aes(x = Time_on_Calls, y = Wellbeing)) + geom_point(size=2, aes(color = Time)) + 
        ggtitle("Time on Calls Vs. Wellbeing") + geom_blank() + stat_smooth(method = 'lm',aes(color = Time), se = FALSE)
      return(calls_wellbeing)
    }
    # Activities and Productivity
    if(input$x=='b' && input$y == 'd'){       
      Number_of_Stationaries <- ap$still_count
      ap$ID <- as.character(ap$ID)
      activities_productivity <- ggplot(ap, aes(x = Number_of_Stationaries, y = Productivity)) + geom_point(size=2, aes(color = Time)) + 
        ggtitle("Number of Stationaries Vs. Productivity") + geom_blank()+ stat_smooth(method = 'lm',aes(color = Time), se = FALSE)
      return(activities_productivity)
    }
    # Activities and Wellbeing
    if(input$x=='b' && input$y == 'e'){       
      Number_of_Stationaries <- ap$still_count
      ap$ID <- as.character(ap$ID)
      activities_wellbeing<- ggplot(ap, aes(x = Number_of_Stationaries, y = Wellbeing)) + geom_point(size=2, aes(color = Time)) + 
        ggtitle("Number of Stationaries Vs. Wellbeing") + stat_smooth(method = 'lm',aes(color = Time), se = FALSE)
      return(activities_wellbeing)
    }
    # Screen and Productivity
    if(input$x=='c' && input$y == 'd'){
      Times_Unlocked <- sp$unlock_count
      sp$ID <- as.character(sp$ID)
      screen_productivity<- ggplot(sp, aes(x = Times_Unlocked, y = Productivity)) + geom_point(size=2, aes(color = Time)) +
        ggtitle("Times Phone Unlocked Vs. Productivity") + geom_blank() + stat_smooth(method = 'lm',aes(color = Time), se = FALSE)
      return(screen_productivity)
    # Screen and Wellbeing
    if(input$x=='c' && input$y == 'e'){       
      Times_Unlocked <- sp$unlock_count
      sp$ID <- as.character(sp$ID)
      screen_wellbeing<- ggplot(sp, aes(x = Times_Unlocked, y = Wellbeing)) + geom_point(size=2, aes(color = Time)) +
        ggtitle("Times Phone Unlocked Vs. Wellbeing") + geom_blank() + stat_smooth(method = 'lm',aes(color = Time), se = FALSE)
      return(screen_wellbeing)
    }
  })
})
  
