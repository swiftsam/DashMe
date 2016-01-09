
library(shiny)
library(data.table)
library(magrittr)
library(DT)
library(betterviz)
library(plotly)

steps      <- readRDS("data/fitbit_steps.Rds")
resting.hr <- readRDS("data/fitbit_resting_hr.Rds")

shinyServer(function(input, output) {

  output$plot_steps <- renderPlotly(
    plot_ly(steps,
            x = report_date,
            y = steps,
            mode = "markers",
            showlegend = FALSE,
            name = "actual value") %>%
      add_trace(y = fitted(loess(steps ~ as.numeric(report_date))),
                mode = "lines",
                showlegend = FALSE,
                name = "smoothed value")
  )

  output$plot_resting_hr <- renderPlotly(
      plot_ly(resting.hr,
              x = report_date,
              y = resting_hr,
              mode = "markers",
              showlegend = FALSE,
              name = "actual value") %>%
        add_trace(y = fitted(loess(resting_hr ~ as.numeric(report_date))),
                  mode = "lines",
                  showlegend = FALSE,
                  name = "smoothed value")
  )

  output$plot_steps_hr <- renderPlot({
    merge(steps, resting.hr, by = "report_date") %>%
    ggplot(aes(steps, resting_hr)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(title = "Daily steps vs Resting Hear Rate")
  })
})
