#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(cffdrs)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    observeEvent(input$ffmc,
                 {
                     isi <- cffdrs:::.ISIcalc(input$ffmc, input$wind)
                     fwi <- cffdrs:::.fwiCalc(isi, input$bui)
                     updateNumericInput(session, "isi", value=isi)
                     updateNumericInput(session, "fwi", value=fwi)
                     disable("isi")
                     disable("fwi")
                 })
    observeEvent(input$wind,
                 {
                     isi <- cffdrs:::.ISIcalc(input$ffmc, input$wind)
                     fwi <- cffdrs:::.fwiCalc(isi, input$bui)
                     updateNumericInput(session, "isi", value=isi)
                     updateNumericInput(session, "fwi", value=fwi)
                     disable("isi")
                     disable("fwi")
                 })
    
    observeEvent(input$dmc,
                 {
                     bui <- cffdrs:::.buiCalc(input$dmc, input$dc)
                     fwi <- cffdrs:::.fwiCalc(input$isi, bui)
                     updateNumericInput(session, "bui", value=bui)
                     updateNumericInput(session, "fwi", value=fwi)
                     disable("bui")
                     disable("fwi")
                 })
    
    
    observeEvent(input$dc,
                 {
                     bui <- cffdrs:::.buiCalc(input$dmc, input$dc)
                     fwi <- cffdrs:::.fwiCalc(input$isi, bui)
                     updateNumericInput(session, "bui", value=bui)
                     updateNumericInput(session, "fwi", value=fwi)
                     disable("bui")
                     disable("fwi")
                 })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

})
