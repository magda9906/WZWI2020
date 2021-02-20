library(shiny)

shapes<- list("Circle" = "circle",
              "Diamond"= "diamond",
              "Pentagon" = "pentagon",
              "Star"="star",
              "Triangle" = "triangle");

# Define UI for displaying current time ----
ui <- fluidPage(
  # Application title
  titlePanel("Word Cloud"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Wybierz ksztalt:",
                  choices = shapes),
      hr(),
      sliderInput("freq",
                  "Czestosc wystepowania:",
                  min = 1,  max = 50, value = 5),
      sliderInput("size",
                  "Rozmiar:",
                  min = 0.1,  max = 1, value = 0.3)
    ),
    
    # Show Word Cloud
    mainPanel(
      wordcloud2Output('cloud')
    )
  )
)