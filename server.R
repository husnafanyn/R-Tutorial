library(shiny)
library(DT)
library(dplyr)
library(ggplot2)

iris
server <- function(input, output) {
  output$panel <- renderText({
    paste("Current panel: ", input$tabset)
  })
  filtered_data <- reactive({
    iris %>% filter(Species %in% input$species_filter)
  })
  
  # Tampilkan data
  output$data_tabel <- renderDataTable({
    datatable(filtered_data())
  })
  
  # Tampilkan summary
  output$summary_stat <- renderTable({
    df1 <- filtered_data()[,1:4]
    statistik <- sapply(df1, function(x){
      c(Min = min(x),
        '1st Qu.' = quantile(x, 0.25),
        Median = median(x),
        Mean = mean(x),
        '3rd Qu.' = quantile(x, 0.75),
        Max = max(x))
    })
    t(round(statistik, 2))
  }, rownames = TRUE)
  
  # Tampilkan visualisasi
  output$scatter_plot <- renderPlot({
    df2 <- filtered_data()
    ggplot(df2, aes_string(x = input$xvar, y = input$yvar,
                           color = "Species")) +
      geom_point(size = 3, alpha = 0.7) +
      theme_minimal() +
      labs(title = "Scatterplot Iris Data", 
           x = input$xvar, y = input$yvar)
    })
}
