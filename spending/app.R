library(shiny)
library(tidyverse)
library(janitor)
#library(plotly)

# Load and prepare data
transactions <- read_csv(file = "ps-transactions-2026-01-31.csv", show_col_types = FALSE) |>
  clean_names() |>
  filter(date > '2017-12-31') |>
  filter(!(parent_categories %in% c("Transfers", "Income"))) |>
  filter(!(category %in% c("Taxes", "Transfers"))) |>
  select(date, amount, category, parent_categories) |>
  mutate(transaction_year = factor(year(date)),
         transaction_yday = yday(date))

categories <- transactions |>
  select(category, parent_categories) |>
  distinct() |>
  arrange(category)

# Get available years
available_years <- unique(year(transactions$date)) |> sort(decreasing = TRUE)

# UI
ui <- fluidPage(
  titlePanel("Cumulative Spending Tracker"),

  # Row 1: Category selectors and comparison
  fluidRow(
    column(3,
      selectInput("parent_category",
                  "Filter by Parent Category:",
                  choices = c("All" = "", sort(unique(categories$parent_categories))),
                  selected = "")
    ),
    column(4,
      selectInput("categories",
                  "Select Categories:",
                  choices = categories$category,
                  selected = c("Media & Subscriptions"),
                  multiple = TRUE)
    ),
    column(5,
      uiOutput("comparison_text")
    )
  ),

  # Row 2: Chart
  fluidRow(
    column(12,
      plotOutput("spending_plot", height = "700px")
    )
  ),

  # Row 3: Year selectors
  fluidRow(
    column(2,
      selectInput("focal_year",
                  "Focal Year",
                  choices = available_years,
                  selected = 2026)
    ),
    column(2,
      selectInput("reference_year",
                  "Reference Year",
                  choices = available_years,
                  selected = 2025)
    ),
    column(2,
      checkboxInput("show_previous",
                    "Show Previous Years",
                    value = TRUE)
    )
  )
)

# Server
server <- function(input, output, session) {

  # Update categories dropdown based on parent category selection
  observeEvent(input$parent_category, {
    if (input$parent_category == "") {
      # Show all categories when no parent is selected
      available_categories <- categories$category
    } else {
      # Filter categories by selected parent category
      available_categories <- categories |>
        filter(parent_categories == input$parent_category) |>
        pull(category)
    }

    updateSelectInput(session, "categories",
                      choices = available_categories,
                      selected = character(0))
  }, ignoreNULL = FALSE)

  # Reactive data based on selected categories
  filtered_data <- reactive({
    req(input$categories)

    transactions |>
      filter(category %in% input$categories) |>
      group_by(transaction_year) |>
      arrange(date) |>
      mutate(cumulative_amount = -1 * cumsum(amount)) |>
      mutate(focal_year = fct(case_when(
        transaction_year == input$focal_year ~ "focal",
        transaction_year == input$reference_year ~ "reference",
        .default = "previous"
      ), levels = c("focal", "reference", "previous")))
  })

  # Calculate YTD comparison
  output$comparison_text <- renderUI({
    data <- filtered_data()

    current_yday <- yday(Sys.Date())

    # Get focal year YTD
    focal_ytd <- data |>
      filter(focal_year == "focal", transaction_yday <= current_yday) |>
      summarise(ytd_amount = max(cumulative_amount, na.rm = TRUE)) |>
      pull(ytd_amount)

    # Get reference year YTD (same day of year)
    reference_ytd <- data |>
      filter(focal_year == "reference", transaction_yday <= current_yday) |>
      summarise(ytd_amount = max(cumulative_amount, na.rm = TRUE)) |>
      pull(ytd_amount)

    # Get reference year final amount
    reference_final <- data |>
      filter(focal_year == "reference") |>
      summarise(final_amount = max(cumulative_amount, na.rm = TRUE)) |>
      pull(final_amount)

    # Calculate pacing and projection
    if (length(focal_ytd) > 0 && length(reference_ytd) > 0 &&
        !is.na(focal_ytd) && !is.na(reference_ytd) && reference_ytd != 0) {

      pacing_pct <- (focal_ytd / reference_ytd) * 100
      projected_focal_final <- reference_final * (focal_ytd / reference_ytd)
      difference <- projected_focal_final - reference_final

      # Round to nearest $500
      difference_rounded <- round(difference / 500) * 500

      # Format as thousands
      difference_k <- abs(difference_rounded) / 1000
      difference_formatted <- if (difference_k == round(difference_k)) {
        sprintf("$%.0fk", difference_k)
      } else {
        sprintf("$%.1fk", difference_k)
      }

      # Determine text based on sign
      difference_text <- if (difference_rounded < 0) {
        paste0("a savings of ", difference_formatted)
      } else if (difference_rounded > 0) {
        paste0("additional spending of ", difference_formatted)
      } else {
        "no change"
      }

      HTML(sprintf(
        "<div style='padding: 15px; margin-top: 10px;'>
          <p style='margin: 5px 0; font-size: 16px;'><strong>YTD </strong> %s: %s, %s: %s</p>
          <p style='margin: 5px 0; font-size: 16px;'><strong>Pacing at:</strong> %.0f%%, on the full year that would mean %s</p>
        </div>",
        input$reference_year,
        scales::dollar(reference_ytd, accuracy = 1),
        input$focal_year,
        scales::dollar(focal_ytd, accuracy = 1),
        pacing_pct,
        difference_text
      ))
    } else {
      HTML("<div style='padding: 15px; margin-top: 10px;'>
            <p style='margin: 5px 0; font-size: 16px;'>Insufficient data for YTD comparison</p>
           </div>")
    }
  })

  output$spending_plot <- renderPlot({
    data <- filtered_data()

    # Filter out previous years if not selected
    if (!input$show_previous) {
      data <- data |>
        filter(focal_year %in% c("focal", "reference"))
    }

    series_ends_data <- data |>
      group_by(transaction_year) |>
      slice_tail(n = 1)

    focal_year_data <- series_ends_data |>
      filter(transaction_year == focal_year)

    ggplot(data,
           aes(x = transaction_yday,
               y = cumulative_amount,
               group = transaction_year,
               color = focal_year,
               alpha = focal_year)) +
      # geom_segment(
      #   data = focal_year_data,
      #   aes(x = transaction_yday, xend = 365,
      #       y = cumulative_amount, yend = 0),
      #   color = "#5ab4ac"
      # ) +
      geom_step(linewidth = 1) +
      geom_label(data = series_ends_data,
                aes(x = 365, label = transaction_year),
                nudge_x = 14,
                size = 4) +
      scale_color_manual(values = c("#5ab4ac", "#d8b365", "black")) +
      scale_alpha_manual(values = c(1, 1, 0.15)) +
      scale_y_continuous(labels = scales::dollar, n.breaks = 10) +
      scale_x_continuous(
        limits = c(0, 385),
        breaks = c(1, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365),
        labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "")) +
      labs(x = "",
           y = "",
           title = paste(input$categories, collapse = ", ")) +
      theme_bw(base_size = 18) +
      theme(
        legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 18, face = "bold"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
