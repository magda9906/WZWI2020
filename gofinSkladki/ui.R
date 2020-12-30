library(shiny)

# Define UI for displaying current time ----
ui <- fluidPage(
  # Application title
  titlePanel("Word Cloud"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Wybierz ksztalt:",
                  choices = shapes),
      actionButton("update", "Generuj"),
      hr(),
      sliderInput("freq",
                  "Czestosc wystepowania:",
                  min = 1,  max = 50, value = 15),
      sliderInput("size",
                  "Rozmiar:",
                  min = 0.1,  max = 1, value = 0.5),
      sliderInput("max",
                  "Maksymalna ilosc slow:",
                  min = 1,  max = 300,  value = 100)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
    )
  )
)

