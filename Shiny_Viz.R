# Load packages
library(dplyr)
library(shiny)
library(shinythemes)
library(ggplot2)

# Load data
setwd("/Users/ytma9/OneDrive/Documents/INFO/Project/Airport/")
top.data <- read.csv("top_data.csv")

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Flight Delay Regression Models"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select airport
                    selectInput(inputId = "airport", label = strong("Airport"),
                                choices = unique(top.data$airport),
                                selected = "ATL"),
                    
                    # Slider for carrier ct
                    sliderInput("carrier_ct", "Carrier Ct:",
                                min = 0, max = as.integer(max(top.data$carrier_ct)),
                                value = 1, step = 1),
                    
                    # Slider for weather ct
                    sliderInput("weather_ct", "Weather Ct:",
                                min = 0, max = as.integer(max(top.data$weather_ct)),
                                value = 1, step = 1),
                    
                    # Slider for nas ct
                    sliderInput("nas_ct", "NAS Ct:",
                                min = 0, max = as.integer(max(top.data$nas_ct)),
                                value = 1, step = 1),
                    
                    # Slider for late aircraft ct
                    sliderInput("late_aircraft_ct", "Late Aircraft Ct:",
                                min = 0, max = as.integer(max(top.data$late_aircraft_ct)),
                                value = 1, step = 1),
                    
                    # Slider for security ct
                    sliderInput("security_ct", "Security Ct:",
                                min = 0, max = as.integer(max(top.data$security_ct)),
                                value = 1, step = 1)
                    ),
                    # Output: lmplot
                    mainPanel(
                      plotOutput(outputId = "lmplot", height = "300px"),
                      textOutput('lmtable')
                    )
                  )
                )

# Define server function
server <- function(input, output) {
  
  output$lmtable <- renderPrint({
    # Subset data
    selected.data <- top.data %>% 
      subset(airport == input$airport)
    
    # Create lm based on given params
    selected.lm <- lm(arr_delay ~ year + month + carrier_ct + weather_ct + nas_ct +
                        late_aircraft_ct + security_ct, data = selected.data)
    
    print(summary(selected.lm))
  })
  
  # Output plot
  output$lmplot <- renderPlot({
    
    # Subset data
    selected.data <- top.data %>% 
        subset(airport == input$airport)
    
    # Create lm based on given params
    selected.lm <- lm(arr_delay ~ year + month + carrier_ct + weather_ct + nas_ct +
                        late_aircraft_ct + security_ct, data = selected.data)
    
    # Create new table
    selected.table <- data.frame("year" = c(0,0,0,0,0,1,1,1,1,1,1,1,1),
                                 "month" = c(8,9,10,11,12,1,2,3,4,5,6,7,8))
    
    selected.table$carrier_ct <- rep(input$carrier_ct, 13)
    
    selected.table$weather_ct <- rep(input$weather_ct, 13)
    
    selected.table$late_aircraft_ct <- rep(input$late_aircraft_ct, 13)
    
    selected.table$security_ct <- rep(input$security_ct, 13)
    
    selected.table$nas_ct <- rep(input$nas_ct, 13)
    
    # Insert predict value
    selected.table$predict <- predict(selected.lm, newdata = selected.table)
    
    # Add date
    selected.table$date <- paste(as.character(selected.table$year + 2017), as.character(selected.table$month), sep = "/")
    
    ggplot(data=selected.table, aes(x=date, y=predict, group=1)) +
      geom_line()+
      geom_point()
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)