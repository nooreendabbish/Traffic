# server.R

source("helpers.R")
counties <- readRDS("data/counties.rds")
library(maps)
library(mapproj)

shinyServer(
  function(input, output) {
    
    output$map <- renderPlot({
      
      data <- switch(input$var, 
                     "Percent White" = counties$white,
                     "Percent Black" = counties$black,
                     "Percent Hispanic" = counties$hispanic,
                     "Percent Asian" = counties$asian)
      
      color <- switch(input$color, 
                      "Green" = "darkgreen",
                      "Red" = "darkred",
                      "Blue" = "darkblue",
                      "Orange" = "darkorange")
      
      percent_map(data, 
                  color, 
                  paste0("% ", input$var), 
                  input$range[1], 
                  input$range[2],
                  x = 0)
    })
      
  }
    )