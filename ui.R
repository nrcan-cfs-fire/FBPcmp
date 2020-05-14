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

CONIFER_FUELS <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7")
DECIDUOUS_FUELS <- c("D1")
SLASH_FUELS <- c('S1', 'S2', 'S3')
MIXED_PERCENT <- c('25', '50', '75')
DEAD_PERCENT <- c('30', '60', '100')
ALL_FUELS <-
    c(CONIFER_FUELS,
      DECIDUOUS_FUELS,
      SLASH_FUELS,
      'M1',
      'M2',
      'M3',
      'M4')

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    dashboardHeader(title = 'FBP Visualizer'),
    dashboardSidebar(
        tags$head(tags$style(
            HTML("
          .form-group {
            margin-bottom: 0 !important;
          }
        ")
        )),
        shinyjs::useShinyjs(),
        sidebarMenu(
            id = 'sidebarmenu',
            menuItem("Wind Speed", tabName = "WS"),
            menuItem("FFMC", tabName = "FFMC"),
            menuItem("BUI", tabName = "BUI")
        ),
        splitLayout(
            numericInput("lat", "Lat", 55),
            numericInput("lon", "Lon",-120)
        ),
        dateInput("date", label = "Date"),
        splitLayout(
            column(
                width = 2,
                checkboxGroupInput("conifer",
                                   "Conifer",
                                   choices = CONIFER_FUELS,
                                   selected = CONIFER_FUELS),
                checkboxGroupInput(
                    "deciduous",
                    "Decid.",
                    choices = DECIDUOUS_FUELS,
                    selected = DECIDUOUS_FUELS
                )
            ),
            column(
                width = 2,
                checkboxGroupInput("slash",
                                   "Slash",
                                   choices = SLASH_FUELS,
                                   selected = SLASH_FUELS),
                checkboxGroupInput("m1",
                                   "M1",
                                   choices = MIXED_PERCENT,
                                   selected = MIXED_PERCENT),
                checkboxGroupInput("m2",
                                   "M2",
                                   choices = MIXED_PERCENT,
                                   selected = MIXED_PERCENT)
            ),
            column(
                width = 2,
                checkboxGroupInput("m3",
                                   "M3",
                                   choices = DEAD_PERCENT,
                                   selected = DEAD_PERCENT),
                checkboxGroupInput("m4",
                                   "M4",
                                   choices = DEAD_PERCENT,
                                   selected = DEAD_PERCENT)
            )
        ),
        conditionalPanel(
            "input.sidebarmenu=='BUI'|| input.sidebarmenu=='WS'",
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
            "input.sidebarmenu=='FFMC'",
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
            "input.sidebarmenu=='WS'",
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
                "input.sidebarmenu=='FFMC' || input.sidebarmenu=='BUI'",
                numericInput(
                    "wind",
                    "Wind Speed",
                    value = 20,
                    min = 0,
                    step = 0.1
                )
            ),
            conditionalPanel("input.sidebarmenu=='BUI'",
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
            "input.sidebarmenu=='WS' || input.sidebarmenu=='FFMC'",
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
            "input.sidebarmenu=='BUI'",
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
