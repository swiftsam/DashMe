library(shiny)
library(tidyverse)
library(janitor)
library(httr)
library(dotenv)
#library(plotly)

# Load environment variables
load_dot_env(file = "../.env")

# Function to fetch recent transactions from PocketSmith API (14 day lookback)
fetch_pocketsmith_transactions <- function(days_back = 14) {
  api_key <- Sys.getenv("pocketsmith_developer_key")

  if (api_key == "") {
    warning("PocketSmith API key not found in environment variables")
    return(NULL)
  }

  # Get user ID
  me_response <- GET(
    "https://api.pocketsmith.com/v2/me",
    add_headers(`X-Developer-Key` = api_key)
  )

  if (status_code(me_response) != 200) {
    warning("Failed to fetch user info from PocketSmith API")
    return(NULL)
  }

  user_id <- content(me_response)$id

  # Calculate date range (14 days)
  start_date <- Sys.Date() - days_back

  # Fetch transactions
  transactions_response <- GET(
    sprintf("https://api.pocketsmith.com/v2/users/%s/transactions", user_id),
    add_headers(`X-Developer-Key` = api_key),
    query = list(
      start_date = as.character(start_date),
      end_date = as.character(Sys.Date())
    )
  )

  if (status_code(transactions_response) != 200) {
    warning("Failed to fetch transactions from PocketSmith API")
    return(NULL)
  }

  # Parse and transform API response
  api_transactions <- content(transactions_response)

  if (length(api_transactions) == 0) {
    return(NULL)
  }

  # Convert to data frame with transaction ID for de-duplication
  api_df <- map_dfr(api_transactions, function(txn) {
    tibble(
      transaction_id = as.character(txn$id),
      date = as.Date(txn$date),
      amount = as.numeric(txn$amount),
      category = txn$category$title %||% NA_character_,
      parent_categories = NA_character_  # Will be added later
    )
  })

  return(api_df)
}

# Function to save API transactions to a dated CSV file
save_api_transactions <- function(transactions_df) {
  if (is.null(transactions_df) || nrow(transactions_df) == 0) {
    return(FALSE)
  }

  filename <- sprintf("ps-api-snapshot-%s.csv", Sys.Date())
  write_csv(transactions_df, filename)
  return(TRUE)
}

# Load and prepare data
# Find all CSV files (main export + API snapshots)
csv_files <- list.files(pattern = "^ps-.*\\.csv$")

# Load all CSVs and combine
all_transactions <- map_dfr(csv_files, function(file) {
  read_csv(file, show_col_types = FALSE) |>
    clean_names() |>
    mutate(source_file = file)  # Track source for debugging
})

# Standardize ID column: main CSV has "id", API snapshots have "transaction_id"
# Create unified txn_id that uses whichever is available
if ("transaction_id" %in% names(all_transactions) && "id" %in% names(all_transactions)) {
  # Both columns exist - coalesce them (convert both to character first)
  all_transactions <- all_transactions |>
    mutate(txn_id = coalesce(as.character(transaction_id), as.character(id)))
} else if ("transaction_id" %in% names(all_transactions)) {
  # Only transaction_id exists
  all_transactions <- all_transactions |>
    mutate(txn_id = as.character(transaction_id))
} else if ("id" %in% names(all_transactions)) {
  # Only id exists
  all_transactions <- all_transactions |>
    mutate(txn_id = as.character(id))
} else {
  # No ID columns
  all_transactions <- all_transactions |>
    mutate(txn_id = NA_character_)
}

# De-duplicate transactions based on unified ID
if (any(!is.na(all_transactions$txn_id))) {
  # Remove duplicates based on txn_id
  all_transactions <- all_transactions |>
    distinct(txn_id, .keep_all = TRUE)
} else {
  # Fall back to deduping by date + amount + category if no IDs available
  all_transactions <- all_transactions |>
    distinct(date, amount, category, .keep_all = TRUE)
}

# Build category to parent category mapping
category_mapping <- all_transactions |>
  select(category, parent_categories) |>
  filter(!is.na(category), !is.na(parent_categories)) |>
  distinct() |>
  # If same category has multiple parents, keep the most common one
  group_by(category) |>
  slice_head(n = 1) |>
  ungroup()

# Apply parent categories to any transactions missing them
all_transactions <- all_transactions |>
  select(-parent_categories) |>
  left_join(category_mapping, by = "category")

# Apply common filters and transformations
transactions <- all_transactions |>
  filter(date > '2017-12-31') |>
  filter(!(parent_categories %in% c("Transfers", "Income"))) |>
  filter(!(category %in% c("Taxes", "Transfers"))) |>
  select(date, amount, category, parent_categories) |>
  mutate(transaction_year = factor(year(date)),
         transaction_yday = yday(date))

message(sprintf("Loaded %d transactions from %d CSV files (after de-duplication)",
                nrow(all_transactions), length(csv_files)))

categories <- transactions |>
  select(category, parent_categories) |>
  distinct() |>
  arrange(category)

# Get available years
available_years <- unique(year(transactions$date)) |> sort(decreasing = TRUE)

# Build hierarchical category structure
# Create a mapping of parent to children
parent_to_children <- categories |>
  group_by(parent_categories) |>
  summarise(children = list(category), .groups = "drop")

# Build checkbox choices with indentation
hierarchical_choices <- map(sort(unique(categories$parent_categories)), function(parent) {
  children <- categories |>
    filter(parent_categories == parent) |>
    pull(category) |>
    sort()

  # Create list with parent first, then indented children
  c(setNames(parent, parent),
    setNames(children, paste0("  → ", children)))
}) |> unlist()

# Create mapping for parent-child relationships
parent_categories_list <- setNames(
  lapply(sort(unique(categories$parent_categories)), function(parent) {
    categories |>
      filter(parent_categories == parent) |>
      pull(category)
  }),
  sort(unique(categories$parent_categories))
)

# UI
ui <- fluidPage(
  titlePanel("Cumulative Spending Tracker"),

  fluidRow(
    # Left sidebar: Category selection
    column(2,
      wellPanel(
        div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
          tags$label("Categories:"),
          div(
            actionButton("all_categories",
                         "All",
                         size = "xs",
                         style = "padding: 2px 8px; font-size: 11px; margin-right: 5px;"),
            actionButton("clear_categories",
                         "Clear",
                         size = "xs",
                         style = "padding: 2px 8px; font-size: 11px;")
          )
        ),
        tags$style(HTML("
          #categories .checkbox { margin-top: 3px; margin-bottom: 3px; }
          #categories label { font-weight: normal; }
        ")),
        checkboxGroupInput("categories",
                           label = NULL,
                           choices = hierarchical_choices,
                           selected = c(names(parent_categories_list), unlist(parent_categories_list, use.names = FALSE)))
      )
    ),

    # Main content area
    column(9,
      # Comparison text
      fluidRow(
        column(12,
          uiOutput("comparison_text")
        )
      ),

      # Chart
      fluidRow(
        column(12,
          plotOutput("spending_plot", height = "700px")
        )
      ),

      # Relative spend chart
      fluidRow(
        column(12,
          plotOutput("relative_spend_plot", height = "500px")
        )
      ),

      # Year selectors and data refresh
      fluidRow(
        column(3,
          selectInput("focal_year",
                      "Focal Year",
                      choices = available_years,
                      selected = 2026)
        ),
        column(3,
          selectInput("reference_year",
                      "Reference Year",
                      choices = available_years,
                      selected = 2025)
        ),
        column(3,
          checkboxInput("show_previous",
                        "Show Previous Years",
                        value = TRUE)
        ),
        column(3,
          div(style = "padding-top: 5px;",
            uiOutput("latest_transaction_info"),
            actionButton("refresh_api",
                         "Refresh from API",
                         icon = icon("sync"),
                         style = "margin-top: 10px;")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Make transactions reactive so it can be updated by API refresh
  transactions_data <- reactiveVal(transactions)

  # Track previous category selection for parent-child logic
  previous_categories <- reactiveVal(c(names(parent_categories_list), unlist(parent_categories_list, use.names = FALSE)))

  # Reactive categories based on current transactions
  categories_data <- reactive({
    transactions_data() |>
      select(category, parent_categories) |>
      distinct() |>
      arrange(category)
  })

  # Display latest transaction date
  output$latest_transaction_info <- renderUI({
    data <- transactions_data()
    latest_date <- max(data$date)
    HTML(sprintf("<p style='margin: 5px 0; font-size: 14px;'><strong>Latest transaction:</strong> %s</p>",
                 format(latest_date, "%B %d, %Y")))
  })

  # Refresh data from API when button is clicked
  observeEvent(input$refresh_api, {
    showNotification("Fetching last 14 days from PocketSmith API...", type = "message", duration = NULL, id = "api_fetch")

    # Fetch recent transactions from API (14 days)
    api_transactions <- fetch_pocketsmith_transactions(days_back = 14)

    if (!is.null(api_transactions)) {
      # Add parent categories from existing mapping
      api_transactions <- api_transactions |>
        select(-parent_categories) |>
        left_join(category_mapping, by = "category")

      # Save to dated CSV snapshot
      saved <- save_api_transactions(api_transactions)

      if (saved) {
        # Reload all CSVs and de-duplicate
        csv_files <- list.files(pattern = "^ps-.*\\.csv$")

        all_transactions <- map_dfr(csv_files, function(file) {
          read_csv(file, show_col_types = FALSE) |>
            clean_names() |>
            mutate(source_file = file)
        })

        # Standardize ID column: main CSV has "id", API snapshots have "transaction_id"
        if ("transaction_id" %in% names(all_transactions) && "id" %in% names(all_transactions)) {
          all_transactions <- all_transactions |>
            mutate(txn_id = coalesce(as.character(transaction_id), as.character(id)))
        } else if ("transaction_id" %in% names(all_transactions)) {
          all_transactions <- all_transactions |>
            mutate(txn_id = as.character(transaction_id))
        } else if ("id" %in% names(all_transactions)) {
          all_transactions <- all_transactions |>
            mutate(txn_id = as.character(id))
        } else {
          all_transactions <- all_transactions |>
            mutate(txn_id = NA_character_)
        }

        # De-duplicate based on unified ID
        if (any(!is.na(all_transactions$txn_id))) {
          all_transactions <- all_transactions |>
            distinct(txn_id, .keep_all = TRUE)
        } else {
          all_transactions <- all_transactions |>
            distinct(date, amount, category, .keep_all = TRUE)
        }

        # Update category mapping
        new_category_mapping <- all_transactions |>
          select(category, parent_categories) |>
          filter(!is.na(category), !is.na(parent_categories)) |>
          distinct() |>
          group_by(category) |>
          slice_head(n = 1) |>
          ungroup()

        # Apply parent categories
        all_transactions <- all_transactions |>
          select(-parent_categories) |>
          left_join(new_category_mapping, by = "category")

        # Apply filters and update reactive data
        updated_transactions <- all_transactions |>
          filter(date > '2017-12-31') |>
          filter(!(parent_categories %in% c("Transfers", "Income"))) |>
          filter(!(category %in% c("Taxes", "Transfers"))) |>
          select(date, amount, category, parent_categories) |>
          mutate(transaction_year = factor(year(date)),
                 transaction_yday = yday(date))

        transactions_data(updated_transactions)

        # Rebuild hierarchical category structure with new data
        cats <- categories_data()

        new_parent_to_children <- cats |>
          group_by(parent_categories) |>
          summarise(children = list(category), .groups = "drop")

        new_hierarchical_choices <- map(sort(unique(cats$parent_categories)), function(parent) {
          children <- cats |>
            filter(parent_categories == parent) |>
            pull(category) |>
            sort()

          c(setNames(parent, parent),
            setNames(children, paste0("  → ", children)))
        }) |> unlist()

        # Update checkbox choices
        updateCheckboxGroupInput(session, "categories",
                                 choices = new_hierarchical_choices,
                                 selected = c(sort(unique(cats$parent_categories)),
                                            cats$category))

        removeNotification(id = "api_fetch")
        showNotification(
          sprintf("Successfully refreshed! Fetched %d transactions, saved to snapshot. Total: %d transactions from %d files.",
                  nrow(api_transactions), nrow(all_transactions), length(csv_files)),
          type = "message",
          duration = 5
        )
      } else {
        removeNotification(id = "api_fetch")
        showNotification("Failed to save API transactions to CSV", type = "error", duration = 3)
      }
    } else {
      removeNotification(id = "api_fetch")
      showNotification("Failed to fetch transactions from API", type = "error", duration = 3)
    }
  })

  # Select all categories
  observeEvent(input$all_categories, {
    updateCheckboxGroupInput(session, "categories",
                             selected = c(names(parent_categories_list),
                                        unlist(parent_categories_list, use.names = FALSE)))
  })

  # Clear all category selections
  observeEvent(input$clear_categories, {
    updateCheckboxGroupInput(session, "categories",
                             selected = character(0))
  })

  # Handle parent-child checkbox relationships
  observeEvent(input$categories, {
    current_selection <- input$categories
    prev_selection <- previous_categories()

    # Determine what changed
    newly_selected <- setdiff(current_selection, prev_selection)
    newly_deselected <- setdiff(prev_selection, current_selection)

    # Check if we need to update
    needs_update <- FALSE
    new_selection <- current_selection

    # If a parent was selected, select all its children
    for (item in newly_selected) {
      if (item %in% names(parent_categories_list)) {
        children <- parent_categories_list[[item]]
        new_selection <- unique(c(new_selection, children))
        needs_update <- TRUE
      }
    }

    # If a parent was deselected, deselect all its children
    for (item in newly_deselected) {
      if (item %in% names(parent_categories_list)) {
        children <- parent_categories_list[[item]]
        new_selection <- setdiff(new_selection, children)
        needs_update <- TRUE
      }
    }

    # Update if needed
    if (needs_update) {
      previous_categories(new_selection)
      updateCheckboxGroupInput(session, "categories",
                               selected = new_selection)
    } else {
      previous_categories(current_selection)
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Reactive data based on selected categories
  filtered_data <- reactive({
    # If no categories selected, show all
    if (is.null(input$categories) || length(input$categories) == 0) {
      filtered_transactions <- transactions_data()
    } else {
      # Get only actual categories (not parent categories) from selection
      # Parent categories are in the parent_categories_list names
      selected_cats <- input$categories
      actual_categories <- setdiff(selected_cats, names(parent_categories_list))

      # Filter to selected actual categories
      if (length(actual_categories) == 0) {
        # If only parents selected (shouldn't happen with our logic), show all
        filtered_transactions <- transactions_data()
      } else {
        filtered_transactions <- transactions_data() |>
          filter(category %in% actual_categories)
      }
    }

    filtered_transactions |>
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

  output$relative_spend_plot <- renderPlot({
    data <- filtered_data()

    # Filter out previous years if not selected
    if (!input$show_previous) {
      data <- data |>
        filter(focal_year %in% c("focal", "reference"))
    }

    # Aggregate to one row per day per year (take max cumulative amount for each day)
    aggregated_data <- data |>
      group_by(transaction_year, transaction_yday, focal_year) |>
      summarise(cumulative_amount = max(cumulative_amount, na.rm = TRUE),
                .groups = "drop")

    # Get reference year data for comparison
    reference_data <- aggregated_data |>
      filter(focal_year == "reference") |>
      select(transaction_yday, reference_amount = cumulative_amount)

    # Calculate relative spend (difference from reference year)
    relative_data <- aggregated_data |>
      filter(focal_year != "reference") |>
      left_join(reference_data, by = "transaction_yday", relationship = "many-to-one") |>
      mutate(relative_spend = cumulative_amount - reference_amount) |>
      filter(!is.na(relative_spend))

    if (nrow(relative_data) > 0) {
      ggplot(relative_data,
             aes(x = transaction_yday,
                 y = relative_spend,
                 group = transaction_year,
                 color = focal_year,
                 alpha = focal_year)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        geom_step(linewidth = 1) +
        scale_color_manual(values = c("#5ab4ac", "black")) +
        scale_alpha_manual(values = c(1, 0.15)) +
        scale_y_continuous(labels = scales::dollar, n.breaks = 8) +
        scale_x_continuous(
          limits = c(0, 385),
          breaks = c(1, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365),
          labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "")) +
        labs(x = "",
             y = "Difference from Reference Year",
             title = paste0("Relative Spend: ", paste(input$categories, collapse = ", "))) +
        theme_bw(base_size = 18) +
        theme(
          legend.position = "none",
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(size = 18, face = "bold"))
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
