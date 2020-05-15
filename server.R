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
    
    observeEvent(input$allFuels,
                 {
                     if (input$allFuels)
                     {
                         updateCheckboxGroupInput(
                             session = session,
                             inputId = "conifer",
                             choices = CONIFER_FUELS,
                             selected = CONIFER_FUELS
                         )
                         updateCheckboxGroupInput(
                             session = session,
                             inputId = "deciduous",
                             choices = DECIDUOUS_FUELS,
                             selected = DECIDUOUS_FUELS
                         )
                         updateCheckboxGroupInput(
                             session = session,
                             inputId = "m1",
                             choices = MIXED_PERCENT,
                             selected = MIXED_PERCENT
                         )
                         updateCheckboxGroupInput(
                             session = session,
                             inputId = "m2",
                             choices = MIXED_PERCENT,
                             selected = MIXED_PERCENT
                         )
                         updateCheckboxGroupInput(
                             session = session,
                             inputId = "m3",
                             choices = DEAD_PERCENT,
                             selected = DEAD_PERCENT
                         )
                         updateCheckboxGroupInput(
                             session = session,
                             inputId = "m4",
                             choices = DEAD_PERCENT,
                             selected = DEAD_PERCENT
                         )
                         updateCheckboxGroupInput(
                             session = session,
                             inputId = "slash",
                             choices = SLASH_FUELS,
                             selected = SLASH_FUELS
                         )
                     }
                     else
                     {
                         updateCheckboxGroupInput(
                             session = session,
                             inputId = "conifer",
                             choices = CONIFER_FUELS,
                             selected = NULL
                         )
                         updateCheckboxGroupInput(
                             session = session,
                             inputId = "deciduous",
                             choices = DECIDUOUS_FUELS,
                             selected = NULL
                         )
                         updateCheckboxGroupInput(
                             session = session,
                             inputId = "m1",
                             choices = MIXED_PERCENT,
                             selected = NULL
                         )
                         updateCheckboxGroupInput(
                             session = session,
                             inputId = "m2",
                             choices = MIXED_PERCENT,
                             selected = NULL
                         )
                         updateCheckboxGroupInput(
                             session = session,
                             inputId = "m3",
                             choices = DEAD_PERCENT,
                             selected = NULL
                         )
                         updateCheckboxGroupInput(
                             session = session,
                             inputId = "m4",
                             choices = DEAD_PERCENT,
                             selected = NULL
                         )
                         updateCheckboxGroupInput(
                             session = session,
                             inputId = "slash",
                             choices = SLASH_FUELS,
                             selected = NULL
                         )
                     }
                 })
    
    makeData <- reactive(function(fuel)
    {
        vsWhat <- input$sidebarmenu
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
    })
    
    c1 <- reactive(makeData()('C1'))
    c2 <- reactive(makeData()('C2'))
    c3 <- reactive(makeData()('C3'))
    c4 <- reactive(makeData()('C4'))
    c5 <- reactive(makeData()('C5'))
    c6 <- reactive(makeData()('C6'))
    c7 <- reactive(makeData()('C7'))
    d1 <- reactive(makeData()('D1'))
    s1 <- reactive(makeData()('S1'))
    s2 <- reactive(makeData()('S2'))
    s3 <- reactive(makeData()('S3'))
    m125 <- reactive(makeData()('M125'))
    m150 <- reactive(makeData()('M150'))
    m175 <- reactive(makeData()('M175'))
    m225 <- reactive(makeData()('M225'))
    m250 <- reactive(makeData()('M250'))
    m275 <- reactive(makeData()('M275'))
    m330 <- reactive(makeData()('M330'))
    m360 <- reactive(makeData()('M360'))
    m3100 <- reactive(makeData()('M3100'))
    m430 <- reactive(makeData()('M430'))
    m460 <- reactive(makeData()('M460'))
    m4100 <- reactive(makeData()('M4100'))
    
    makeFuels <- reactive(function()
    {
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
        r <- c1()[0,]
        fct <- function(df, value)
        {
            if (is.element(value, forFuels))
            {
                r <- rbind(r, df())
            }
            return (r)
        }
        r <- fct(c1, 'C1')
        r <- fct(c2, 'C2')
        r <- fct(c3, 'C3')
        r <- fct(c4, 'C4')
        r <- fct(c5, 'C5')
        r <- fct(c6, 'C6')
        r <- fct(c7, 'C7')
        r <- fct(d1, 'D1')
        r <- fct(s1, 'S1')
        r <- fct(s2, 'S2')
        r <- fct(s3, 'S3')
        r <- fct(m125, 'M125')
        r <- fct(m150, 'M150')
        r <- fct(m175, 'M175')
        r <- fct(m225, 'M225')
        r <- fct(m250, 'M250')
        r <- fct(m275, 'M275')
        r <- fct(m330, 'M330')
        r <- fct(m360, 'M360')
        r <- fct(m3100, 'M3100')
        r <- fct(m430, 'M430')
        r <- fct(m460, 'M460')
        r <- fct(m4100, 'M4100')
        return (r)
    })
    
    makePlot <- reactive(function(forWhat, ylab, ylim = NULL)
    {
        vsWhat <- input$sidebarmenu
        fuels <- makeFuels()()
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
        if (!is.null(fuels) && input$legend)
        {
            legend(
                minX,
                y = maxY,
                legend = unique(fuels$FullFuel),
                ncol = 2,
                col = all_index,
                lty = all_index
            )
        }
    })
    
    output$plotROS <- renderPlot({
        makePlot()('ROS', "Rate of Spread (m/min)")
    })
    
    output$plotHFI <- renderPlot({
        makePlot()('HFI', "Head Fire Intensity (kW/m)")
    })
    
    output$plotSFC <- renderPlot({
        makePlot()('SFC', "Surface Fuel Consumption (kg/m^2)", 'TFC')
    })
    
    output$plotTFC <- renderPlot({
        makePlot()('TFC', "Total Fuel Consumption (kg/m^2)")
    })
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        data <- makeFuels()()
        if (!is.null(data))
        {
            if (input$fuel != "All") {
                data <- data[data$FuelType == input$fuel,]
            }
            if (input$fd != "All") {
                data <- data[data$FD == substr(input$fd[1], 1, 1),]
            }
            data[, FullFuel := NULL]
        }
    }))
})
