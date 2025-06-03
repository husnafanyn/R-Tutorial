library(shiny)
library(DT)
library(dplyr)
library(ggplot2)

iris
ui <- fluidPage(
  titlePanel("📊 Dashboard Data Iris"),
  sidebarLayout(
    sidebarPanel(
      textOutput("panel"),
      checkboxGroupInput("species_filter", "Pilih Spesies:",
                         choices = unique(iris$Species),
                         selected = unique(iris$Species))
    ),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Data",
                 dataTableOutput("data_tabel")),
        tabPanel("Statistika Deskriptif",
                 tableOutput("summary_stat")),
        tabPanel("Visualisasi",
                 selectInput("xvar", "Pilih variabel X:",
                             choices = names(iris)[1:4],
                             selected = "Sepal.Length"),
                 selectInput("yvar", "Pilih Variabel Y:",
                             choices = names(iris)[1:4],
                             selected = "Petal.Length"),
                 plotOutput("scatter_plot"))
      )
    )
  )
)

