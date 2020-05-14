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
    observeEvent(input$dmc | input$dc,
                 {
                     bui <- cffdrs:::.buiCalc(input$dmc, input$dc)
                     updateNumericInput(session, "bui", value = bui)
                 })
    
    observeEvent(input$ffmc | input$wind,
                 {
                     isi <- cffdrs:::.ISIcalc(input$ffmc, input$wind)
                     updateNumericInput(session, "buiISI", value = isi)
                 })
    
    makeData <- function(vsWhat, fuel)
    {
        dj <- as.POSIXlt(input$date)$yday
        ffmc <- input$ffmc
        bui <- input$bui
        ws <- input$wind
        if ('WS' == vsWhat)
        {
            ws <- input$windRange
            rows <- seq(0, (ws[2] - ws[1]))
            ws <- seq(ws[1], ws[2])
        }
        if ('FFMC' == vsWhat)
        {
            ffmc <- input$ffmcRange
            rows <- seq(0, (ffmc[2] - ffmc[1]) / 0.1)
            ffmc <- seq(ffmc[1], ffmc[2], by = 0.1)
        }
        if ('BUI' == vsWhat)
        {
            bui <- input$buiRange
            rows <- seq(0, (bui[2] - bui[1]))
            bui <- seq(bui[1], bui[2])
        }
        input <-
            data.table(
                ID = rows,
                FuelType = fuel,
                LAT = input$lat,
                LONG = input$lon,
                FFMC = ffmc,
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
    
    makeFuels <- function(vsWhat)
    {
        forFuels = c(input$conifer, input$slash)
        if (0 == length(forFuels))
        {
            return(NULL)
        }
        r <- makeData(vsWhat, forFuels[1])
        if (1 < length(forFuels))
        {
            for (fuel in seq(2, length(forFuels)))
            {
                f <- makeData(vsWhat, forFuels[fuel])
                r <- rbind(r, f)
            }
        }
        return (r)
    }
    
    makePlot <- function(forWhat, ylab, ylim = NULL)
    {
        vsWhat <- getFor()
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
    
    getFor <- function()
    {
        return(input$sidebarmenu)
    }
    
    output$plotROS <- renderPlot({
        makePlot('ROS', "Rate of Spread (m/min)")
    })
    
    output$plotHFI <- renderPlot({
        makePlot('HFI', "Head Fire Intensity (kW/m)")
    })
    
    output$plotSFC <- renderPlot({
        makePlot('SFC', "Surface Fuel Consumption (kg/m^2)", 'TFC')
    })
    
    output$plotTFC <- renderPlot({
        makePlot('TFC', "Total Fuel Consumption (kg/m^2)")
    })
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        data <- makeFuels(getFor())
        if (input$fuel != "All") {
            data <- data[data$FuelType == input$fuel,]
        }
        if (input$fd != "All") {
            data <- data[data$FD == substr(input$fd[1], 1, 1),]
        }
        data
    }))
})
