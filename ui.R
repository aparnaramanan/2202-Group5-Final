# Aparna Ramanan - RShiny UI

# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(

    # Application title
    titlePanel("Student Productivity and Wellbeing"),

    # Making Radio Buttons to Choose Axis
    sidebarLayout(
        sidebarPanel(
            radioButtons("x", "Select an AWARE Behavior Pattern for x-axis:",
                         list("Calls"='a',"Activities"='b',"Screen Usage"='c')),
            radioButtons("y", "Select a PIEL Data for y-axis:",
                         list("Productivity"='d',"WellBeing"='e'))
        ),
  # Plots the Graph According to the Selection of Radio Buttons
        mainPanel(column(12, plotOutput("Plot")))
                   ))
        ) 
    

