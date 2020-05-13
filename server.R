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
library(data.table)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    observeEvent(input$ffmcDMC,
                 {
                     bui <- cffdrs:::.buiCalc(input$ffmcDMC, input$ffmcDC)
                     updateNumericInput(session, "ffmcBUI", value = bui)
                 })
    
    
    observeEvent(input$ffmcDC,
                 {
                     bui <- cffdrs:::.buiCalc(input$ffmcDMC, input$ffmcDC)
                     updateNumericInput(session, "ffmcBUI", value = bui)
                 })
    
    observeEvent(input$buiFFMC,
                 {
                     isi <- cffdrs:::.ISIcalc(input$buiFFMC, input$buiWind)
                     updateNumericInput(session, "buiISI", value = isi)
                 })
    
    observeEvent(input$buiWind,
                 {
                     isi <- cffdrs:::.ISIcalc(input$buiFFMC, input$buiWind)
                     updateNumericInput(session, "buiISI", value = isi)
                 })
    
    makeData <- function(vsWhat, fuel)
    {
        dj <- as.POSIXlt(input$date)$yday
        if ('FFMC' == vsWhat)
        {
            ffmc <- input$ffmcRange
            bui <- input$ffmcBUI
            ws <- input$ffmcWind
            rows <- seq(0, (ffmc[2] - ffmc[1]) / 0.1)
            ffmcs <- seq(ffmc[1], ffmc[2], by = 0.1)
            input <-
                data.table(
                    ID = rows,
                    FuelType = fuel,
                    LAT = 55,
                    LONG = -120,
                    FFMC = ffmcs,
                    BUI = bui,
                    WS = ws,
                    GS = 0,
                    Dj = dj,
                    Aspect = 0
                )
            output <-
                merge(input, data.table(fbp(input)), by = c('ID'))[, ID := NULL]
            return (output)
        }
        ffmc <- input$buiFFMC
        bui <- input$buiRange
        ws <- input$buiWind
        rows <- seq(0, (bui[2] - bui[1]))
        buis <- seq(bui[1], bui[2])
        input <-
            data.table(
                ID = rows,
                FuelType = fuel,
                LAT = 55,
                LONG = -120,
                FFMC = ffmc,
                BUI = buis,
                WS = ws,
                GS = 0,
                Dj = dj,
                Aspect = 0
            )
        output <-
            merge(input, data.table(fbp(input)), by = c('ID'))[, ID := NULL]
        return (output)
        
    }
    
    makeFuels <- function(vsWhat)
    {
        if (0 == length(input$fuels))
        {
            return(NULL)
        }
        r <- makeData(vsWhat, input$fuels[1])
        if (1 < length(input$fuels))
        {
            for (fuel in seq(2, length(input$fuels)))
            {
                f <- makeData(vsWhat, input$fuels[fuel])
                r <- rbind(r, f)
            }
        }
        return (r)
    }
    
    makePlot <- function(vsWhat, forWhat, ylab, ylim = NULL)
    {
        fuels <- makeFuels(vsWhat)
        if (is.null(ylim))
        {
            ylim <- forWhat
        }
        minX <- min(fuels[[vsWhat]])
        maxY <- max(fuels[[ylim]])
        cols <- c('FuelType', vsWhat, forWhat)
        col <- 1
        for (fuel in unique(fuels$FuelType))
        {
            f <- fuels[fuels$FuelType == fuel, ..cols]
            if (1 == col)
            {
                plot(
                    f[[forWhat]] ~ f[[vsWhat]],
                    ylim = c(0, maxY),
                    type = 'l',
                    col = 1,
                    lty = 1,
                    xlab = vsWhat,
                    ylab = ylab
                )
            }
            else
            {
                lines(f[[forWhat]] ~ f[[vsWhat]],
                      type = 'l',
                      col = col,
                      lty = col)
            }
            f <-
                fuels[fuels$FuelType == fuel &
                          fuels$FD == 'I', ..cols]
            lines(
                f[[forWhat]] ~ f[[vsWhat]],
                type = 'l',
                col = col,
                lty = col,
                lwd = 2
            )
            f <-
                fuels[fuels$FuelType == fuel &
                          fuels$FD == 'C', ..cols]
            lines(
                f[[forWhat]] ~ f[[vsWhat]],
                type = 'l',
                col = col,
                lty = 1,
                lwd = 3
            )
            col <- col + 1
        }
        legend(
            minX,
            y = maxY,
            legend = unique(fuels$FuelType),
            col = seq(1, col),
            lty = seq(1, col)
        )
    }
    
    output$ffmcROS <- renderPlot({
        makePlot('FFMC', 'ROS', "Rate of Spread (m/min)")
    })
    
    output$ffmcHFI <- renderPlot({
        makePlot('FFMC', 'HFI', "Head Fire Intensity (kW/m)")
    })
    
    output$ffmcSFC <- renderPlot({
        makePlot('FFMC',
                 'SFC',
                 "Surface Fuel Consumption (kg/m^2)",
                 'TFC')
    })
    
    output$ffmcTFC <- renderPlot({
        makePlot('FFMC', 'TFC', "Total Fuel Consumption (kg/m^2)")
    })
    
    # Filter data based on selections
    output$ffmcTable <- DT::renderDataTable(DT::datatable({
        data <- makeFuels('FFMC')
        if (input$ffmcFuel != "All") {
            data <- data[data$FuelType == input$ffmcFuel,]
        }
        if (input$ffmcFD != "All") {
            data <- data[data$FD == substr(input$ffmcFD[1], 1, 1),]
        }
        data
    }))
    
    output$buiROS <- renderPlot({
        makePlot('BUI', 'ROS', "Rate of Spread (m/min)")
    })
    
    output$buiHFI <- renderPlot({
        makePlot('BUI', 'HFI', "Head Fire Intensity (kW/m)")
    })
    
    output$buiSFC <- renderPlot({
        makePlot('BUI',
                 'SFC',
                 "Surface Fuel Consumption (kg/m^2)",
                 'TFC')
    })
    
    output$buiTFC <- renderPlot({
        makePlot('BUI', 'TFC', "Total Fuel Consumption (kg/m^2)")
    })
    
    # Filter data based on selections
    output$buiTable <- DT::renderDataTable(DT::datatable({
        data <- makeFuels('BUI')
        if (input$buiFuel != "All") {
            data <- data[data$FuelType == input$buiFuel,]
        }
        if (input$buiFD != "All") {
            data <- data[data$FD == substr(input$buiFD[1], 1, 1),]
        }
        data
    }))
    
})
