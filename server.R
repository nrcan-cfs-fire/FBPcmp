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
        if (0 == length(input$fuels))
        {
            return(NULL)
        }
        r <- makeData(input$fuels[1])
        if (1 < length(input$fuels))
        {
            for (fuel in seq(2, length(input$fuels)))
            {
                f <- makeData(input$fuels[fuel])
                r <- rbind(r, f)
            }
        }
        return (r)
    }
    
    makePlot <- function(forWhat, ylab, ylim=NULL)
    {
        fuels <- makeFuels()
        if (is.null(ylim))
        {
            ylim <- forWhat
        }
        maxY <- max(fuels[[ylim]])
        cols <- c('FuelType', 'FFMC', forWhat)
        col <- 1
        for (fuel in unique(fuels$FuelType))
        {
            f <- fuels[fuels$FuelType == fuel, ..cols]
            if (1 == col)
            {
                plot(f[[forWhat]] ~ f$FFMC, ylim=c(0, maxY), type='l', col=1, lty=1, xlab="FFMC", ylab=ylab)
            }
            else
            {
                lines(f[[forWhat]] ~ f$FFMC, type='l', col=col, lty=col)
            }
            f <- fuels[fuels$FuelType == fuel & fuels$FD == 'I', ..cols]
            lines(f[[forWhat]] ~ f$FFMC, type='l', col=col, lty=col, lwd=2)
            f <- fuels[fuels$FuelType == fuel & fuels$FD == 'C', ..cols]
            lines(f[[forWhat]] ~ f$FFMC, type='l', col=col, lty=1, lwd=3)
            col <- col + 1
        }
        legend(input$ffmc[1], y=maxY, legend=unique(fuels$FuelType), col=seq(1, col), lty=seq(1, col))
    }
    
    output$rosPlot <- renderPlot({
        makePlot('ROS', "Rate of Spread (m/min)")
    })
    
    output$hfiPlot <- renderPlot({
        makePlot('HFI', "Head Fire Intensity (kW/m)")
    })
    
    output$sfcPlot <- renderPlot({
        makePlot('SFC', "Surface FUel Consumption (kg/m^2)", 'TFC')
    })
    
    output$tfcPlot <- renderPlot({
        makePlot('TFC', "Total FUel Consumption (kg/m^2)")
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
