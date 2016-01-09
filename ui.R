library(shiny)
library(shinydashboard)
library(data.table)
library(DT)

header  <- dashboardHeader(title = "DashMe")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Fitbit", tabName = "fitbit", icon = icon("motorcycle")),
    menuItem("Interactions", tabName = "interactions")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "fitbit",
            h2("Fitbit"),
            h3("Steps"),
            plotlyOutput("plot_steps"),
            h3("Resting Heart Rate"),
            plotlyOutput("plot_resting_hr")
    ),
    tabItem(tabName = "interactions",
            h2("Interactions"),
            plotOutput("plot_steps_hr"))
  )
)

dashboardPage(header, sidebar, body, "DashMe")


