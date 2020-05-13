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
    
    output$rosPlot <- renderPlot({
        fuels <- makeFuels()
        maxY <- max(fuels$ROS)
        col <- 1
        for (fuel in unique(fuels$FuelType))
        {
            f <- fuels[fuels$FuelType == fuel,c('FuelType', 'FFMC', 'ROS')]
            if (1 == col)
            {
                plot(f$ROS ~ f$FFMC, ylim=c(0, maxY), type='l', col=1, lty=1, xlab="FFMC", ylab="Rate of Spread (m/min)")
            }
            else
            {
                lines(f$ROS ~ f$FFMC, type='l', col=col, lty=col)
            }
            f <- fuels[fuels$FuelType == fuel & fuels$FD == 'I',c('FuelType', 'FFMC', 'ROS')]
            lines(f$ROS ~ f$FFMC, type='l', col=col, lty=col, lwd=2)
            f <- fuels[fuels$FuelType == fuel & fuels$FD == 'C',c('FuelType', 'FFMC', 'ROS')]
            lines(f$ROS ~ f$FFMC, type='l', col=col, lty=1, lwd=3)
            col <- col + 1
        }
        legend(input$ffmc[1], y=maxY, legend=unique(fuels$FuelType), col=seq(1, col), lty=seq(1, col))
    })
    
    output$hfiPlot <- renderPlot({
        fuels <- makeFuels()
        maxY <- max(fuels$HFI)
        col <- 1
        for (fuel in unique(fuels$FuelType))
        {
            f <- fuels[fuels$FuelType == fuel,c('FuelType', 'FFMC', 'HFI')]
            if (1 == col)
            {
                plot(f$HFI ~ f$FFMC, ylim=c(0, maxY), type='l', col=1, lty=1, xlab="FFMC", ylab="Head Fire Intensity (kW/m)")
            }
            else
            {
                lines(f$HFI ~ f$FFMC, type='l', col=col, lty=col)
            }
            f <- fuels[fuels$FuelType == fuel & fuels$FD == 'I',c('FuelType', 'FFMC', 'HFI')]
            lines(f$HFI ~ f$FFMC, type='l', col=col, lty=col, lwd=2)
            f <- fuels[fuels$FuelType == fuel & fuels$FD == 'C',c('FuelType', 'FFMC', 'HFI')]
            lines(f$HFI ~ f$FFMC, type='l', col=col, lty=1, lwd=3)
            col <- col + 1
        }
        legend(input$ffmc[1], y=maxY, legend=unique(fuels$FuelType), col=seq(1, col), lty=seq(1, col))
    })
    
    output$sfcPlot <- renderPlot({
        fuels <- makeFuels()
        # use same scale as TFC so you can flip between them
        maxY <- max(fuels$TFC)
        col <- 1
        for (fuel in unique(fuels$FuelType))
        {
            f <- fuels[fuels$FuelType == fuel,c('FuelType', 'FFMC', 'SFC')]
            if (1 == col)
            {
                plot(f$SFC ~ f$FFMC, ylim=c(0, maxY), type='l', col=1, lty=1, xlab="FFMC", ylab="Surface FUel Consumption (kg/m^2)")
            }
            else
            {
                lines(f$SFC ~ f$FFMC, type='l', col=col, lty=col)
            }
            f <- fuels[fuels$FuelType == fuel & fuels$FD == 'I',c('FuelType', 'FFMC', 'SFC')]
            lines(f$SFC ~ f$FFMC, type='l', col=col, lty=col, lwd=2)
            f <- fuels[fuels$FuelType == fuel & fuels$FD == 'C',c('FuelType', 'FFMC', 'SFC')]
            lines(f$SFC ~ f$FFMC, type='l', col=col, lty=1, lwd=3)
            col <- col + 1
        }
        legend(input$ffmc[1], y=maxY, legend=unique(fuels$FuelType), col=seq(1, col), lty=seq(1, col))
    })
    
    output$tfcPlot <- renderPlot({
        fuels <- makeFuels()
        maxY <- max(fuels$TFC)
        col <- 1
        for (fuel in unique(fuels$FuelType))
        {
            f <- fuels[fuels$FuelType == fuel,c('FuelType', 'FFMC', 'TFC')]
            if (1 == col)
            {
                plot(f$TFC ~ f$FFMC, ylim=c(0, maxY), type='l', col=1, lty=1, xlab="FFMC", ylab="Total FUel Consumption (kg/m^2)")
            }
            else
            {
                lines(f$TFC ~ f$FFMC, type='l', col=col, lty=col)
            }
            f <- fuels[fuels$FuelType == fuel & fuels$FD == 'I',c('FuelType', 'FFMC', 'TFC')]
            lines(f$TFC ~ f$FFMC, type='l', col=col, lty=col, lwd=2)
            f <- fuels[fuels$FuelType == fuel & fuels$FD == 'C',c('FuelType', 'FFMC', 'TFC')]
            lines(f$TFC ~ f$FFMC, type='l', col=col, lty=1, lwd=3)
            col <- col + 1
        }
        legend(input$ffmc[1], y=maxY, legend=unique(fuels$FuelType), col=seq(1, col), lty=seq(1, col))
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
