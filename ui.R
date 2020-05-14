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

ALL_FUELS <- c("C-1", "C-2", "C-3", "C-4", "C-5", "C-6", "C-7")

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    dashboardHeader(title = 'FBP Visualizer'),
    dashboardSidebar(
        shinyjs::useShinyjs(),
        sidebarMenu(
            id = 'sidebarmenu',
            menuItem("Wind Speed", tabName = "windTab"),
            menuItem("FFMC", tabName = "ffmcTab"),
            menuItem("BUI", tabName = "buiTab")
        ),
        splitLayout(
            numericInput("lat", "Lat", 55),
            numericInput("lon", "Lon",-120)
        ),
        dateInput("date", label = "Date"),
        checkboxGroupInput("fuels", "Fuels", choices = ALL_FUELS, selected =
                               ALL_FUELS),
        conditionalPanel(
            "input.sidebarmenu=='buiTab'|| input.sidebarmenu=='windTab'",
            sliderInput(
                "ffmc",
                "FFMC",
                value = 90,
                min = 0,
                max = 101,
                step = 0.1
            )
        ),
        conditionalPanel(
            "input.sidebarmenu=='ffmcTab'",
            sliderInput(
                "ffmcRange",
                "FFMC",
                value = c(70, 101),
                min = 0,
                max = 101,
                step = 0.1
            )
        ),
        conditionalPanel(
            "input.sidebarmenu=='windTab'",
            sliderInput(
                "windRange",
                "Wind Speed",
                value = c(0, 100),
                min = 0,
                max = 100
            )
        ),
        splitLayout(
            conditionalPanel(
                "input.sidebarmenu=='ffmcTab' || input.sidebarmenu=='buiTab'",
                numericInput(
                    "wind",
                    "Wind Speed",
                    value = 20,
                    min = 0,
                    step = 0.1
                )
            ),
            conditionalPanel("input.sidebarmenu=='buiTab'",
                             disabled(
                                 numericInput(
                                     "buiISI",
                                     "ISI",
                                     value = 10,
                                     min = 0,
                                     step = 0.1
                                 )
                             ))
        ),
        conditionalPanel(
            "input.sidebarmenu=='windTab' || input.sidebarmenu=='ffmcTab'",
            splitLayout(
                numericInput(
                    "dmc",
                    "DMC",
                    value = 20,
                    min = 0,
                    step = 1
                ),
                numericInput(
                    "dc",
                    "DC",
                    value = 200,
                    min = 0,
                    step = 1
                )
            ),
            disabled(numericInput(
                "bui",
                "BUI",
                value = 80,
                min = 0,
                step = 1
            )),
        ),
        conditionalPanel(
            "input.sidebarmenu=='buiTab'",
            sliderInput(
                "buiRange",
                "BUI",
                value = c(0, 100),
                min = 0,
                max = 300,
                step = 1
            )
        )
    ),
    dashboardBody(
        tabsetPanel(
            tabPanel("ROS", plotOutput("plotROS")),
            tabPanel("HFI", plotOutput("plotHFI")),
            tabPanel("SFC", plotOutput("plotSFC")),
            tabPanel("TFC", plotOutput("plotTFC"))
        ),
        column(4,
               selectInput("fuel",
                           "Fuel:",
                           c("All",
                             ALL_FUELS))),
        column(4,
               selectInput(
                   "fd",
                   "Fire Type:",
                   c("All",
                     "Surface", "Intermittent Crown", "Crown")
               )),
        # Create a new row for the table.
        DT::dataTableOutput("table")
    )
))
