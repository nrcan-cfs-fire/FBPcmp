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
library(parallel)

CONIFER_FUELS <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7")
DECIDUOUS_FUELS <- c("D1")
SLASH_FUELS <- c('S1', 'S2', 'S3')
MIXED_PERCENT <- c('25', '50', '75')
DEAD_PERCENT <- c('30', '60', '100')
ALL_FUELS <-
    c(
        CONIFER_FUELS,
        DECIDUOUS_FUELS,
        SLASH_FUELS,
        'M125',
        'M150',
        'M175',
        'M225',
        'M250',
        'M275',
        'M330',
        'M360',
        'M3100',
        'M430',
        'M460',
        'M4100'
    )


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
        mainFuel <- substr(fuel, 1, 2)
        pc <- ''
        pdf <- ''
        if ('M1' == mainFuel || 'M2' == mainFuel)
        {
            pc <- substr(fuel, 3, 4)
        }
        if ('M3' == mainFuel || 'M4' == mainFuel)
        {
            pdf <- substr(fuel, 3, length(fuel))
        }
        input <-
            data.table(
                ID = rows,
                FullFuel = fuel,
                FuelType = mainFuel,
                LAT = input$lat,
                LONG = input$lon,
                FFMC = ffmc,
                BUI = bui,
                WS = ws,
                GS = 0,
                Dj = dj,
                PC = pc,
                PDF = pdf,
                Aspect = 0
            )
        output <-
            merge(input, data.table(fbp(input)), by = c('ID'))[, ID := NULL]
        return (output)
        
    }
    
    makeFuels <- function()
    {
        vsWhat <- getFor()
        forFuels = c(input$conifer, input$deciduous, input$slash)
        for (f in input$m1)
        {
            forFuels <- append(forFuels, paste0('M1', f))
        }
        for (f in input$m2)
        {
            forFuels <- append(forFuels, paste0('M2', f))
        }
        for (f in input$m3)
        {
            forFuels <- append(forFuels, paste0('M3', f))
        }
        for (f in input$m4)
        {
            forFuels <- append(forFuels, paste0('M4', f))
        }
        if (0 == length(forFuels))
        {
            return(NULL)
        }
        fct <- function(fuel)
        {
            return (makeData(vsWhat, fuel))
        }
        r <- lapply(forFuels, fct)
        r <- rbindlist(r)
        return (r)
    }
    
    makePlot <- function(forWhat, ylab, ylim = NULL)
    {
        vsWhat <- getFor()
        fuels <- makeFuels()
        if (is.null(ylim))
        {
            ylim <- forWhat
        }
        minX <- min(fuels[[vsWhat]])
        maxY <- max(fuels[[ylim]])
        cols <- c('FullFuel', vsWhat, forWhat)
        col <- 1
        for (fuel in unique(fuels$FullFuel))
        {
            f <- fuels[fuels$FullFuel == fuel, ..cols]
            by_index = match(fuel, ALL_FUELS)
            if (1 == col)
            {
                plot(
                    f[[forWhat]] ~ f[[vsWhat]],
                    ylim = c(0, maxY),
                    type = 'l',
                    col = by_index,
                    lty = by_index,
                    xlab = vsWhat,
                    ylab = ylab
                )
            }
            else
            {
                lines(f[[forWhat]] ~ f[[vsWhat]],
                      type = 'l',
                      col = by_index,
                      lty = by_index)
            }
            f <-
                fuels[fuels$FullFuel == fuel &
                          fuels$FD == 'I', ..cols]
            lines(
                f[[forWhat]] ~ f[[vsWhat]],
                type = 'l',
                col = by_index,
                lty = by_index,
                lwd = 2
            )
            f <-
                fuels[fuels$FullFuel == fuel &
                          fuels$FD == 'C', ..cols]
            lines(
                f[[forWhat]] ~ f[[vsWhat]],
                type = 'l',
                col = by_index,
                lty = 1,
                lwd = 3
            )
            col <- col + 1
        }
        all_index = match(unique(fuels$FullFuel), ALL_FUELS)
        legend(
            minX,
            y = maxY,
            legend = unique(fuels$FullFuel),
            ncol = 2,
            col = all_index,
            lty = all_index
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
        data <- makeFuels()[, FullFuel := NULL]
        if (input$fuel != "All") {
            data <- data[data$FuelType == input$fuel,]
        }
        if (input$fd != "All") {
            data <- data[data$FD == substr(input$fd[1], 1, 1),]
        }
        data
    }))
})
