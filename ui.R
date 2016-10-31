bootstrapPage(
  tags$head(
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "styles.css"),
    tags$script(src = "screenSize.js")
  ),
  mainPanel(
    fluidRow(
      column(
        width = 12,
        HTML("<p style='color:white;font-weight: bold; margin-bottom:0;'>Analyze Location (Press +)</p>"),
        selectInput(
          inputId = 'stateChoice',
          label = "",
          choices = states,
          selectize = FALSE,
          width = "23%"
        ),
        selectInput(
          inputId = 'cityChoice',
          label = '',
          choices = '',
          selectize = FALSE,
          width = "38%"
        ),
        actionButton(
          inputId = 'addLocation',
          label = '',
          icon = icon('plus'),
          width = "11%"
        ),
        actionButton(
          inputId = 'resetLocations',
          label = '',
          icon = icon('refresh'),
          width = "11%"
        ),
        actionButton(
          inputId = 'plotSettings',
          label = '',
          icon = icon('wrench'),
          width = "11%"
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        sliderInput(
          inputId = 'indexYear',
          label = HTML("<p style='color:white;font-weight: bold;'>Select Starting Year on Slider</p>"),
          min = min(years),
          max = max(years),
          value = years[(length(years) - 5)],
          sep = "",
          step = 1,
          width = "100%"
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        uiOutput('ggplot2PlotTitle'),
        plotOutput('ggplot2Plot',
                   height = "300px")
      )
    )
  )
)