library(shiny)
library(shinydashboard)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
  sliderInput(inputId = "num",
              label = "How many points?",
              min = 1, max = 1000, value = 500),
  sliderInput(inputId = "centroids",
              label = "How many centroids?",
              min = 1, max = 5, value = 2),
  actionButton(
    inputId = "run", label = "Display new scatter plot"
  ),
  plotOutput("scat")
  )
)

server <- function(input, output) {
  output$scat <- renderPlot({
    input$run
    
    cents <- input$centroids
    points <- input$num
    
    bins <- points %/% cents
    mod <- points %% cents
    last_bin <- bins + mod
    
    x_s <- vector("list", points)
    y_s <- vector("list", points)

    mu_x <- sample(-10:10, cents)
    mu_y <- sample(-10:10, cents)
    sd <- sample(1:2, cents, replace = TRUE)
    
    i <- 0
    for (bin in 1:cents) {
      if (bin != cents) {
        x_data <- rnorm(bins, mu_x[bin], sd[bin])
        y_data <- rnorm(bins, mu_y[bin], sd[bin])
        all_data <- bins
      } else {
        x_data <- rnorm(last_bin, mu_x[bin], sd[bin])
        y_data <- rnorm(last_bin, mu_y[bin], sd[bin])
        all_data <- bins
      }
      for (j in 1:all_data) {
        i <-  i + 1
        x_s[[i]] <- x_data[j]
        y_s[[i]] <- y_data[j]
      }
    }
    x_s <- unlist(x_s)
    y_s <- unlist(y_s)
    
    ggplot() +
      aes(x_s, y_s) +
      geom_point()
  })
}

shinyApp(ui = ui, server = server)
