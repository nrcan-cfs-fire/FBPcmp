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
            ),
            numericInput(
                "windDMC",
                "DMC",
                value = 20,
                min = 0,
                step = 1
            ),
            numericInput(
                "windDC",
                "DC",
                value = 200,
                min = 0,
                step = 1
            ),
            disabled(numericInput(
                "windBUI",
                "BUI",
                value = 80,
                min = 0,
                step = 1
            )),
        ),
        conditionalPanel(
            "input.sidebarmenu=='ffmcTab'",
            numericInput(
                "ffmcWind",
                "Wind Speed",
                value = 20,
                min = 0,
                step = 0.1
            ),
            numericInput(
                "ffmcDMC",
                "DMC",
                value = 20,
                min = 0,
                step = 1
            ),
            numericInput(
                "ffmcDC",
                "DC",
                value = 200,
                min = 0,
                step = 1
            ),
            disabled(numericInput(
                "ffmcBUI",
                "BUI",
                value = 80,
                min = 0,
                step = 1
            )),
        ),
        conditionalPanel(
            "input.sidebarmenu=='buiTab'",
            numericInput(
                "buiWind",
                "Wind Speed",
                value = 20,
                min = 0,
                step = 0.1
            ),
            disabled(numericInput(
                "buiISI",
                "ISI",
                value = 10,
                min = 0,
                step = 0.1
            )),
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
    dashboardBody(tabItems(
        tabItem(
            tabName = "windTab",
            tabsetPanel(
                tabPanel("ROS", plotOutput("windROS")),
                tabPanel("HFI", plotOutput("windHFI")),
                tabPanel("SFC", plotOutput("windSFC")),
                tabPanel("TFC", plotOutput("windTFC"))
            ),
            column(4,
                   selectInput(
                       "windFuel",
                       "Fuel:",
                       c("All",
                         "C-1", "C-2", "C-3", "C-4", "C-5", "C-6", "C-7")
                   )),
            column(4,
                   selectInput(
                       "windFD",
                       "Fire Type:",
                       c("All",
                         "Surface", "Intermittent Crown", "Crown")
                   )),
            # Create a new row for the table.
            DT::dataTableOutput("windTable")
        ),
        tabItem(
            tabName = "ffmcTab",
            tabsetPanel(
                tabPanel("ROS", plotOutput("ffmcROS")),
                tabPanel("HFI", plotOutput("ffmcHFI")),
                tabPanel("SFC", plotOutput("ffmcSFC")),
                tabPanel("TFC", plotOutput("ffmcTFC"))
            ),
            column(4,
                   selectInput(
                       "ffmcFuel",
                       "Fuel:",
                       c("All",
                         "C-1", "C-2", "C-3", "C-4", "C-5", "C-6", "C-7")
                   )),
            column(4,
                   selectInput(
                       "ffmcFD",
                       "Fire Type:",
                       c("All",
                         "Surface", "Intermittent Crown", "Crown")
                   )),
            # Create a new row for the table.
            DT::dataTableOutput("ffmcTable")
        ),
        tabItem(
            tabName = "buiTab",
            tabsetPanel(
                tabPanel("ROS", plotOutput("buiROS")),
                tabPanel("HFI", plotOutput("buiHFI")),
                tabPanel("SFC", plotOutput("buiSFC")),
                tabPanel("TFC", plotOutput("buiTFC"))
            ),
            column(4,
                   selectInput(
                       "buiFuel",
                       "Fuel:",
                       c("All",
                         "C-1", "C-2", "C-3", "C-4", "C-5", "C-6", "C-7")
                   )),
            column(4,
                   selectInput(
                       "buiFD",
                       "Fire Type:",
                       c("All",
                         "Surface", "Intermittent Crown", "Crown")
                   )),
            # Create a new row for the table.
            DT::dataTableOutput("buiTable")
        )
    ))
))
