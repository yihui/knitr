#install.packages("shinydashboard")
#library(shinydashboard)
install.packages("shiny")
install.packages("ggplot2")
install.packages("shinyExt")
library(shiny)
library(ggplot2)

shinyUI(pageWithSidebar(
  headerPanel("Inventory Management Dashboard"),
  sidebarPanel(
    selectInput("graph", "Choose a graph:",
                list("Pie Chart" = "pie",
                "Histogram" = "hist"))
  ),
  mainPanel(
    plotOutput("piePlot"),
    plotOutput("histPlot")
  )))
