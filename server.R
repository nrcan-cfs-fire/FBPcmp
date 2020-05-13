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
library(ggplot2)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    observeEvent(input$dmc,
                 {
                     bui <- cffdrs:::.buiCalc(input$dmc, input$dc)
                     updateNumericInput(session, "bui", value=bui)
                     disable("bui")
                 })
    
    
    observeEvent(input$dc,
                 {
                     bui <- cffdrs:::.buiCalc(input$dmc, input$dc)
                     updateNumericInput(session, "bui", value=bui)
                     disable("bui")
                 })

    makeData <- function(fuel)
    {
        ffmc <- input$ffmc
        bui <- input$bui
        ws <- input$wind
        dj <- as.POSIXlt(input$date)$yday
        rows <- seq(0,(ffmc[2] - ffmc[1]) / 0.1)
        ffmcs <- seq(ffmc[1], ffmc[2], by=0.1)
        input <- data.table(ID=rows, FuelType=fuel, LAT=55, LONG=-120, FFMC=ffmcs, BUI=bui, WS=ws, GS=0, Dj=dj, Aspect=0)
        output <- merge(input, data.table(fbp(input)), by=c('ID'))[,ID:=NULL]
        return (output)
    }
    
    makeFuels <- function()
    {
        r <- makeData("C-1")
        for (fuel in c("C-2", "C-3", "C-4", "C-5", "C-6", "C-7"))
        {
            f <- makeData(fuel)
            r <- rbind(r, f)
        }
        return (r)
    }
    
    output$distPlot <- renderPlot({
        fuels <- makeFuels()
        c1 <- fuels[fuels$FuelType == "C-1",c('FuelType', 'FFMC', 'ROS')]
        maxRos <- max(fuels$ROS)
        plot(c1$ROS ~ c1$FFMC, ylim=c(0, maxRos), type='l', col=1, lty=1, xlab="FFMC", ylab="Rate of Spread (m/min)")
        col = 2
        for (fuel in c("C-2", "C-3", "C-4", "C-5", "C-6", "C-7"))
        {
            f <- fuels[fuels$FuelType == fuel,c('FuelType', 'FFMC', 'ROS')]
            lines(f$ROS ~ f$FFMC, type='l', col=col, lty=col)
            col <- col + 1
        }
        legend(input$ffmc[1], y=maxRos, legend=c("C-1", "C-2", "C-3", "C-4", "C-5", "C-6", "C-7"), col=seq(1, col), lty=seq(1, col))
    })
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        data <- makeFuels()
        if (input$fuel != "All") {
            data <- data[data$FuelType == input$fuel,]
        }
        data
    }))
    

})
