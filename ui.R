#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        dashboardHeader(title='FBP Visualizer'),
        dashboardSidebar(
            sidebarMenu(
              menuItem("ROS", tabName="ros")  
            ),
            dateInput("date", label="Date"),
            sliderInput("ffmc", "FFMC", value=c(70, 101), min=0, max=101, step=0.1),
            numericInput("wind", "Wind Speed", value=20, min=0, step=0.1),
            numericInput("dmc", "DMC", value=20, min=0, step=1),
            numericInput("dc", "DC", value=200, min=0, step=1),
            disabled(numericInput("bui", "BUI", value=80, min=0, step=1))
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName="ros",    
                    fluidPage(
                        plotOutput("distPlot"),
                        fluidRow(
                            column(4,
                                   selectInput("fuel",
                                               "Fuel:",
                                               c("All",
                                                 "C-1", "C-2", "C-3", "C-4", "C-5", "C-6", "C-7"))
                            )
                        ),
                        # Create a new row for the table.
                        DT::dataTableOutput("table")
                    )
                )
            )
        )
    )
)
