library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(RColorBrewer)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(
    
  ),
  dashboardSidebar(
    
  ),
  dashboardBody(
    tags$h2("K-means clustering with random data"),
    fluidRow(
      column(5, wellPanel(
        sliderInput(inputId = "num",
                  label = "How many random data points?",
                  min = 100, max = 1000, value = 500),
        sliderInput(inputId = "centroids",
                    label = "How many centroids for the random data?",
                    min = 1, max = 5, value = 2),
        sliderInput(inputId = "km_clusters",
                    label = "How many clusters for k-means?",
                    min = 1, max = 5, value = 2),
        switchInput(
          inputId = "show_clusters",
          label = "Show assigned clusters", 
          labelWidth = "160px"),
        actionButton(
          inputId = "new_data", label = "Generate new data"),
        actionButton(
          inputId = "kmeans", label = "Run k-means clustering")
      )),
      column(7, plotOutput("scat"))
    )
  )
)

color_pal <- c("#6551E8", "#6ED8CC", "#E9C46A", "#490838", "#E76F51")

server <- function(input, output, session) {
    # Generate random data from user input when user presses "run" button
    rand_data <- reactive({
      # Rerun this when user presses "new_data" button
      rerun <- input$new_data
      
      cents <- input$centroids
      points <- input$num
      
      bins <- points %/% cents
      mod <- points %% cents
      last_bin <- bins + mod
      
      x <- vector("list", points)
      y <- vector("list", points)
      cluster <- vector("list", points)
      
      mu_x <- sample(-5:5, cents)
      mu_y <- sample(-5:5, cents)
      sd_x <- sample(seq(0.5, 1.2, 0.1), cents, replace = TRUE)
      sd_y <- sample(seq(0.5, 1.2, 0.1), cents, replace = TRUE)
      
      # Iterate through each point and generate it's random data
      # Use "i" to remember what point
      i <- 0
      for (bin in 1:cents) {
        if (bin != cents) {
          x_data <- rnorm(bins, mu_x[bin], sd_x[bin])
          y_data <- rnorm(bins, mu_y[bin], sd_y[bin])
          counter <- bins
        } else {
          x_data <- rnorm(last_bin, mu_x[bin], sd_x[bin])
          y_data <- rnorm(last_bin, mu_y[bin], sd_y[bin])
          counter <- last_bin
        }
        for (j in 1:counter) {
          i <-  i + 1
          x[[i]] <- x_data[j]
          y[[i]] <- y_data[j]
          cluster[[i]] <- bin
          
        }
      }
      
      x = unlist(x)
      y = unlist(y)
      cluster = unlist(cluster)
      
      # Create a tibble of the data
      df <- tibble(
        x = x,
        y = y,
        clusters = factor(cluster)
      )
    })
    
  # Render the new plot
  output$scat <- renderPlot({
    rand_data() %>% 
    ggplot() +
      aes(x, y) +
      geom_point() +
      labs(x = "", y = "")
  })
  
  toggle_clustering_view <- function() {
    if (input$show_clusters) {
      output$scat <- renderPlot({
        rand_data() %>% 
          ggplot() +
          aes(x, y, color = clusters) +
          geom_point() +
          scale_color_manual(
            values = color_pal
          ) +
          labs(color = "Clusters", x = "", y = "")
      })
    } else {
      output$scat <- renderPlot({
        rand_data() %>% 
          ggplot() +
          aes(x, y) +
          geom_point() +
          labs(x = "", y = "")
      })
    }
  }

  observeEvent(input$new_data, {
    toggle_clustering_view()
  })
  
  observeEvent(input$show_clusters, {
    toggle_clustering_view()
  })
  
  observeEvent(input$kmeans, {
    # Toggle show clusters switch to off if on

    
    df <- rand_data() %>% 
      select(x, y)
    
    km_data <- kmeans(df, centers = input$km_clusters, nstart = 25)

    output$scat <- renderPlot({
      df %>% 
        mutate(km_clusters = km_data$cluster) %>% 
        ggplot() +
        aes(x, y, color = factor(km_clusters)) +
        geom_point() +
        scale_color_manual(
          values = color_pal
        ) +
        labs(color = "K-means Clusters", x = "", y = "")
      })
    })
  

}

shinyApp(ui = ui, server = server)
