
library(shiny)
library(plotly)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
options(scipen=5)

# Data of Zhendong
duration_histogram_data <-
  read.csv(
    "./Data/duration_histogram_data.csv"
  )
duration_boxplot_data <-
  read.csv(
    "./Data/duration_boxplot_data.csv"
  )
duration_injure_data <-
  read.csv(
    "./Data/duration_injure_data.csv"
  )
duration_mean_data <-
  read.csv(
    "./Data/duration_mean_data.csv"
  )
duration_magnitude_data <-
  read.csv(
    "./Data/duration_magnitude_data.csv"
  )
magnitude_histogram_data <-
  read.csv(
    "./Data/magnitude_histogram_data.csv"
  )
magnitude_mean_data <-
  read.csv(
    "./Data/magnitude_mean_data.csv"
  )
Tornado_damage_data <-
  read.csv(
    "./Data/Tornado_damage_data.csv"
  )
Tornado_month_data <-
  read.csv(
    "./Data/Tornado_month_data.csv"
  )
Tornado_state <-
  read.csv(
    "./Data/Tornado_state.csv"
  )
Tornado_timeseries <-
  read.csv(
    "./Data/Tornado_timeseries.csv"
  )
damage_time <-
  read.csv(
    "./Data/damage_time.csv"
  )

# Data of Inhenn

# Data of Xiaowei

# Data of EJ


# Define UI for application that draws a histogram
ui <- navbarPage(
  "US Weather Events",
  id = "uwe",
  tabPanel(
    "Event Type",
    titlePanel("Event Type"),
    
    fixedRow(column(
      5,
      selectInput(
        inputId = "var_inhenn",
        label = "Choose a variable to display",
        choices = c(
          "Average Life Expectancy",
          "Unhealthy Days",
          "Health Status",
          "All Death"
        ),
        selected = "Average Life Expectancy"
      )
    )),
    tags$hr(),
    fixedRow(column(8, offset = 2, plotOutput("plotl1")) ),
    fixedRow(column(8, offset = 2, verbatimTextOutput("info")))
  ),
  
  tabPanel(
    "Duration Analysis",
    titlePanel("Histogram"),
    # Histogram
    selectInput(
      inputId = "var_zhendong01",
      label = "Choose an event type to display",
      choices = unique(duration_histogram_data$EVENT_TYPE),
      selected = "Heavy Snow"
    ),
    
    fixedRow(column(8, offset = 2, plotlyOutput("plotw1"))),
    
    # Boxplot
    titlePanel("Boxplot"),
    selectInput(
      inputId = "var_zhendong02",
      label = "Choose a event class to display",
      choices = c("short duration","short-mid duration", "mid-long duration", "long duration"),
      selected = "short-mid duration"
    ),
    
    fixedRow(column(8, offset = 2, plotlyOutput("plotw2"))),
    
    # ClevelandDotPlot
    titlePanel("ClevelandDotPlot"),
    fixedRow(column(6, offset = 3, plotOutput("plotw3"))),
    
    
    # Duration with injuries
    titlePanel("Duration with Injuries"),
    selectInput(
      inputId = "var_zhendong03",
      label = "Choose an event type to display",
      choices = unique(duration_injure_data$EVENT_TYPE),
      selected = "Strong Wind"
    ),
    
    fixedRow(column(8, offset = 2, plotlyOutput("plotw4"))),
    
    
    fixedRow(column(
      12,
      helpText(
        "Relationship between duration with injuries and deaths of events."
      )
    ))
    
  ),
  
  tabPanel(
    "Magnitude Analysis",
    titlePanel("Histogram"),
    
    selectInput(
      inputId = "var_zhendong04",
      label = "Choose an event type to display",
      choices = unique(magnitude_histogram_data$EVENT_TYPE),
      selected = "Marine Thunderstorm Wind"
    ),
    fixedRow(column(8, offset = 2, plotlyOutput("plotw5"))),
    
    
    # ClevelandDotPlot
    titlePanel("ClevelandDotPlot"),
    fixedRow(column(6, offset = 3, plotOutput("plotw6"))),
    
    titlePanel("Magnitude with Duration"),
    
    fixedRow(column(
      10,
      selectInput(
        inputId = "var_zhendong05",
        label = "Choose an event type to display",
        choices = unique(duration_magnitude_data$EVENT_TYPE),
        selected = "Marine Thunderstorm Wind"
      )),
      fixedRow(column(8, offset = 2, plotlyOutput("plotw7")) )
    )
    
  ),
  
  tabPanel(
    "Injuries and Damages",
    titlePanel("Injuries")
    
  ),
  
  tabPanel(
    "Event Topics",
    titlePanel("Tornado"),
    fixedRow(column(
      8,
      helpText(
        "Histogram"
      )
    )),
    
    fixedRow(column(
      8,
      offset = 2,
      plotlyOutput("plotw8")
    )),
    
    fixedRow(column(
      8,
      helpText(
        "Spatial Distribution"
      )
    )),
    
    fixedRow(column(
      8,offset = 2, plotlyOutput("plotw9")
    )),
    
    fixedRow(column(
      8,
      helpText(
        "Monthly Frequency of Tornado in US (2017)"
      )
    )),
    
    fixedRow(column(
      8,offset = 2, plotOutput("plotw10")
    )
    ),
    
    fixedRow(column(
      8,
      helpText(
        "Year trend of Tornado"
      )
    )),
    
    selectInput(
      inputId = "var_zhendong06",
      label = "Choose a variable to display",
      choices = c("Frequency", "Total Damage", "Mean Damage"),
      selected = "Frequency"
    ),
    
    fixedRow(column(
      8,offset = 2, plotlyOutput("plotw11")
    )
    )
  ),
  
  tabPanel(
    "Year Trend",
    titlePanel("Year Trend")
    
  )
  
  
)




# Define server logic required to draw a histogram
server <- function(input,
                   output,
                   session) {
  # Output of Zhendong
  
  output$plotw1 <- renderPlotly({
    event = input$var_zhendong01
    data <- duration_histogram_data[duration_histogram_data$EVENT_TYPE == event,]
    #plot_ly(x = ~data$DURATION, type = "histogram", marker = list(color = 'orange')) %>%
      #layout(title="Histogram of Duration", xaxis=list(title="Duration"))
    h1 <- ggplot(data, aes(DURATION)) +
      geom_histogram(bins = 60, fill = "lightblue", color = "blue") +
      ggtitle("Histogram of Duration by Event type")+
      theme(plot.title = element_text(hjust = 0.5)) +
      ylab("Frequency")
    ggplotly(h1)
  })
  
  
  
  output$plotw2 <- renderPlotly({
    level = switch(input$var_zhendong02,
                   "short duration" = "d1",
                   "short-mid duration" = "d2",
                   "mid-long duration" = "d3",
                   "long duration" = "d4")
  
    data <- duration_boxplot_data[duration_boxplot_data$dlevel == level,]
    plot_ly(data, x = ~DURATION, color = ~EVENT_TYPE, type = "box") %>% 
      layout(title="Boxplots of Duration",
             xaxis=list(title="Duration"),
             margin = list(l = 100))
    
    
  })
  
  output$plotw3 <- renderPlot({
    theme_dotplot <- theme_bw(16) +
      theme(axis.text.y = element_text(size = rel(.75)),
            axis.ticks.y = element_blank(),
            axis.title.x = element_text(size = rel(.75)),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(size = 0.5),
            panel.grid.minor.x = element_blank())+
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(duration_mean_data) +
      geom_point(aes(mean, fct_reorder(EVENT_TYPE, mean)), size = 2, alpha = 0.8, color = "blue") +
      theme_dotplot +
      ylab("EVENT_TYPE")+
      xlab("Mean of Duration") + 
      ggtitle("Mean of Duration over Event_types")
    
  })
  
  output$plotw4 <- renderPlotly({
    class = input$var_zhendong03
    data <- duration_injure_data[duration_injure_data$EVENT_TYPE == class, ]
    fv <- data %>% lm( INJURE_DEATH ~ DURATION,.) %>% fitted.values()
    plot_ly(
      data, x = ~DURATION, y = ~INJURE_DEATH,
      color = ~INJURE_DEATH, size = ~INJURE_DEATH
    ) %>% add_markers(y = ~INJURE_DEATH) %>%
      add_trace(x = ~DURATION, y = fv, mode = "lines") %>%
      layout(title="Scatter plot",
             xaxis=list(title="Duration"),
             yaxis=list(title="Injuries and Deaths"),
             margin = list(l = 100),
             showlegend = F)
    
  })
  
  output$plotw5 <- renderPlotly({
    event = input$var_zhendong04
    data <- magnitude_histogram_data[magnitude_histogram_data$EVENT_TYPE == event, ]
  
    #plot_ly(x = ~data$DURATION, type = "histogram", marker = list(color = 'orange')) %>%
      #layout(title="Histogram of Duration", xaxis=list(title="Duration"))
    h2 <- ggplot(data, aes(MAGNITUDE)) +
      geom_histogram(bins = 60, fill = "lightblue", color = "blue") +
      ggtitle("Histogram of Magnitude")+
      theme(plot.title = element_text(hjust = 0.5)) +
      ylab("Frequency")
    ggplotly(h2)

  
  })
  
  output$plotw6 <- renderPlot({
    theme_dotplot <- theme_bw(16) +
      theme(axis.text.y = element_text(size = rel(.75)),
            axis.ticks.y = element_blank(),
            axis.title.x = element_text(size = rel(.75)),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(size = 0.5),
            panel.grid.minor.x = element_blank())+
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(magnitude_mean_data) +
      geom_point(aes(mean, fct_reorder(EVENT_TYPE, mean)), size = 2, alpha = 0.8, color = "blue") +
      theme_dotplot +
      ylab("EVENT_TYPE")+
      xlab("Mean of Magnitude") + 
      ggtitle("Mean of Magnitude over Event_types")
  })
  
  output$plotw7 <- renderPlotly({
    class = input$var_zhendong05
    data <- duration_magnitude_data[duration_magnitude_data$EVENT_TYPE == class, ]
    fv <- data %>% lm( MAGNITUDE ~ DURATION,.) %>% fitted.values()
    plot_ly(
      data, x = ~DURATION, y = ~MAGNITUDE,
      color = ~MAGNITUDE, size = ~MAGNITUDE
    ) %>% add_markers(y = ~MAGNITUDE) %>%
      add_trace(x = ~DURATION, y = fv, mode = "lines") %>%
      layout(title="Scatter plot",
             xaxis=list(title="Duration"),
             yaxis=list(title="Magnitude"),
             margin = list(l = 100),
             showlegend = F)
    
  })
  
  output$plotw8 <- renderPlotly({
    
    ggplot(Tornado_state,aes(x=reorder(region,value),y=value)) +
      geom_bar(stat = 'identity',fill='darkslateblue') +
      xlab("State") + 
      ylab("Frequency of Tornado") +
      coord_flip() +
      ggtitle('Tornado distribution over US')+
      theme(axis.title.x = element_text(size = 12, face = "bold"),axis.title.y = element_text(size = 15, face = "bold"),axis.text=element_text(size=6,vjust = 0.5, hjust = 0.5, angle = 0))+
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
  })
  
  output$plotw9 <- renderPlotly({
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa')
      
    )
    
    plot_geo(Tornado_state, locationmode = 'USA-states') %>%
      add_trace(
        z = ~value, locations = ~code,
        color = ~value, colors = colorRampPalette(brewer.pal(11,"RdPu"))(100)
      ) %>%
      colorbar(title = "Frequency") %>%
      layout(
        title = '2017 USA Tornado Frequency by State',
        geo = g
      )
    
  })
  
  output$plotw10 <- renderPlot({
    theme_dotplot <- theme_bw(12) +
      theme(axis.text.y = element_text(size = rel(.75)),
            axis.ticks.y = element_blank(),
            axis.title.x = element_text(size = rel(.75)),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(size = 0.5),
            panel.grid.minor.x = element_blank())+
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(Tornado_month_data) +
      geom_point(aes(value, fct_reorder(MONTH_NAME, value)), size = 2, alpha = 0.8, color = "blue") +
      theme_dotplot +
      ylab("Occuring Month")+
      xlab("Frequency of Month") + 
      ggtitle("2017 Monthly Frequency of Tornado")
  })
  
  output$plotw11 <- renderPlotly({
    type = input$var_zhendong06
    if(type == "Frequency") {
      ggplot(Tornado_timeseries, aes(YEAR, Frequency)) +
        geom_line() +
        geom_point() 
    }else if(type == "Total Damage"){
      ggplot(Tornado_timeseries, aes(YEAR, Total_damage)) +
        geom_line() +
        geom_point() +
        ylab("Total Damage(k)")
    }else{
      ggplot(Tornado_timeseries, aes(YEAR, Mean_damage)) +
        geom_line() +
        geom_point()+
        ylab("Mean Damage(k)")
    }
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
