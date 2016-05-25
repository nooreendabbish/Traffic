# ui.R

# shinyUI(fluidPage(
#   titlePanel("censusVis"),
# 
#   sidebarLayout(
#     sidebarPanel(
#       helpText("Create demographic maps with
#                information from the 2010 US Census."),
# 
#       selectInput("var",
#                   label = "Choose a variable to display",
#                   choices = c("Percent White", "Percent Black",
#                               "Percent Hispanic", "Percent Asian"),
#                   selected = "Percent White"),
# 
#       selectInput("color",
#                   label = "Choose a color to display",
#                   choices = c("Green", "Red",
#                               "Blue", "Orange"),
#                   selected = "Green"),
# 
#       sliderInput("range",
#                   label = "Range of interest:",
#                   min = 0, max = 100, value = c(0, 100))
#       ),
#     
#     hr(),
# 
#     mainPanel(plotOutput("map", height = "200px"))
#   )
# ))

shinyUI(fluidPage(
  
  title = "censusVis",
  
  fluidRow(
    column(12, 
      helpText("Create demographic maps with
               information from the 2010 US Census."),
      
      selectInput("var",
                  label = "Choose a variable to display",
                  choices = c("Percent White", "Percent Black",
                              "Percent Hispanic", "Percent Asian"),
                  selected = "Percent White"),
      
      selectInput("color",
                  label = "Choose a color to display",
                  choices = c("Green", "Red",
                              "Blue", "Orange"),
                  selected = "Green"),
      
      sliderInput("range",
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))
      )
  ),
    
    mainPanel(plotOutput("map", height = "300px"))
  )
)

##Print legend in a text box separate from the main panel instead.