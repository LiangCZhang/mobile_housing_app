function(input, output, session) {
  
  rV <-
    reactiveValues(
      plotData = data_frame(),
      plotLocations = c(),
      plotAllLoc = FALSE,
      screenDim = c()
    )
  
  observe({
    rV$screenDim <- input$dimension
  })
  
  observeEvent(input$stateChoice, {
    
    cities_df <- 
      HPI_AT_metro %>% 
      filter(State == input$stateChoice) %>% 
      select(City) %>% 
      unique() %>% 
      arrange(City)
    
    updateSelectInput(
      session,
      inputId = 'cityChoice',
      choices = cities_df$City
    )
    
  })
  
  observeEvent(input$indexYear, {
    
    rV$plotData <- HPI_AT_metro %>% 
      Index_HPI_AT_metro(
        Index_Year = input$indexYear
      )
  })
  
  observeEvent(input$addLocation, {
    
    plotLocationsAdded <- paste0(input$cityChoice, '_', input$stateChoice)
    
    plotLocationsCurrent <- rV$plotLocations
    
    rV$plotLocations <-
      c(
        plotLocationsCurrent,
        plotLocationsAdded
      ) %>% 
      unique()
  })
  
  
  observeEvent(input$resetLocations, {
    rV$plotLocations <- c()
  })
  
  output$ggplot2PlotTitle <- renderUI({
    mostRecentQ <- 
      ifelse(
        max(rV$plotData$Year_Quarter) - floor(max(rV$plotData$Year_Quarter)) == 0,
        'Q1 ',
        ifelse(
          max(rV$plotData$Year_Quarter) - floor(max(rV$plotData$Year_Quarter)) == .25,
          'Q2 ',
          ifelse(
            max(rV$plotData$Year_Quarter) - floor(max(rV$plotData$Year_Quarter)) == .5,
            'Q3 ',
            'Q4 '
          )
        )
      )
    
    title <- paste0('Q1 ', 
                    min(rV$plotData$Year_Quarter),
                    ' through ',
                    mostRecentQ,
                    floor(max(rV$plotData$Year_Quarter)),
                    ' (% Change)'
    )
    HTML(paste0("<p style='color:white;font-weight: bold;'>",title, "</p>"))
    
  })
  
  plotSettingsModal <- function(failed = FALSE) {
    modalDialog(
      checkboxInput(
        inputId = "plotAllLoc",
        label = "Plot All Locations",
        value = rV$plotAllLoc
      )
    )
  }
  
  observeEvent(input$plotAllLoc,{
    rV$plotAllLoc <- input$plotAllLoc
  })
  
  observeEvent(input$plotSettings, {
    showModal(plotSettingsModal())
  })
  
  output$ggplot2Plot <- renderPlot({
    
    firstHalfAverage <-
      rV$plotData %>% 
      filter(
        Year_Quarter < median(Year_Quarter)
      ) %>% 
      summarise(
        MeanChange = mean(Change)
      )
    
    if(firstHalfAverage$MeanChange < 0){
      legendJustification <- c(0, 1)
      legendPosition <- c(0, 1)
    } else {
      legendJustification <- c(0, 0)
      legendPosition <- c(0, 0)
    }
    
    ggplot2Plot <- 
      ggplot(
        data = rV$plotData
      ) +
      annotate(
        geom = "text",
        label = "",
        x = Inf,
        y = Inf
      ) +
      geom_abline(
        slope = 0,
        intercept = 0,
        linetype = 'dashed'
      ) +
      scale_x_continuous(
        breaks = range(rV$plotData$Year),
        expand = c(0.01, 0)
      ) +
      scale_y_continuous(
        labels = percent
      ) +
      xlab('Year') +
      ylab(NULL) +
      coord_cartesian(
        xlim = range(rV$plotData$Year_Quarter),
        ylim = 
          c(
            -max(abs(rV$plotData$Change) * 1.5),
            max(abs(rV$plotData$Change) * 1.5)
          )
      ) +
      theme(
        rect = element_rect(fill = 'grey92'),
        legend.text = element_text(size = 12),
        legend.position = legendPosition,
        legend.justification = legendJustification,
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank()
      )
    
    if(rV$plotAllLoc){
      ggplot2Plot <- 
        ggplot2Plot +
        geom_line(
          aes(
            x = Year_Quarter,
            y = Change,
            group = City_State
          ),
          alpha = .1
        ) 
    }
    
    if(length(rV$plotLocations) > 0){
      
      for(location.i in rV$plotLocations){
        
        plotData.i <- 
          filter(rV$plotData, City_State == location.i)
        
        indexChange.i <- filter(rV$plotData, City_State == location.i, Year_Quarter == max(Year_Quarter))
        indexChangeSign.i <- ifelse(sign(indexChange.i$Change) == 1, "+", "-")
        # 320 20
        # 375 30
        # 414 35
        
        maxStringLengthIntercept.i <- -(1460 / 47)
        maxStringLengthSlope.i <- (15 / 94)
        
        maxStringLength.i <- 
          maxStringLengthIntercept.i  + 
          (rV$screenDim[1] * maxStringLengthSlope.i)

        cityLabel.i <-
          substr(
            unique(plotData.i$City),
            start = 1,
            stop = maxStringLength.i
          )
        
        if(nchar(cityLabel.i) > maxStringLength.i - 2){
          cityLabel.i <-
            paste0(
              substr(
                cityLabel.i,
                1,
                maxStringLength.i - 2
              ),
              '...'
            )
        }
        
        plotData.i$legend <- 
          paste0(
            cityLabel.i,
            ', ',
            unique(plotData.i$State),
            ' ',
            indexChangeSign.i,
            abs(indexChange.i$Change) * 100, '%'
          )
        
        ggplot2Plot <- 
          ggplot2Plot +
          geom_line(
            data = plotData.i,
            size = 2,
            aes(
              x = Year_Quarter,
              y = Change,
              color = legend
            )
          )
      }
    }
    ggplot2Plot
  })
  
}
