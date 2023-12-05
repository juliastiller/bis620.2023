#' @title Run Shiny App
#' @description Starts our Clinical Trials Query Shiny App
#'
#' @returns No return.
#'
#' @import shiny
#' @importFrom dplyr left_join
#' @importFrom ggplot2 ggplot aes geom_col labs scale_x_discrete theme_minimal element_text theme xlab ylab
#' @importFrom utils data
#' @importFrom tidyr pivot_longer

#' @importFrom scales wrap_format
#' @export

run_shiny_app <- function() {
  # Define the maximum number of studies to display within the app
  max_num_studies = 1000

  # Load Data
  data("studies")
  data("sponsors")
  data("conditions")
  data("countries")
  data("interventions")

  # Functions
  #' @title Query keywords from a database table.
  #' @description Description goes here.
  #' @param d the database table.
  #' @param kwds the keywords to look for.
  #' @param column the column to look for the keywords in.
  #' @param ignore_case should the case be ignored when searching for a keyword?
  #' (default TRUE)
  #' @param match_all should we look for values that match all of the keywords
  #' (intersection) or any of the keywords (union)? (default FALSE; union).
  query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE) {
    kwds = kwds[kwds != ""]
    kwds = paste0("%", kwds, "%") |>
      gsub("'", "''", x = _)
    if (ignore_case) {
      like <- " ilike "
    } else{
      like <- " like "
    }
    query = paste(
      paste0(column, like, "'", kwds, "'"),
      collapse = ifelse(match_all, " AND ", " OR ")
    )
    filter(d, sql(query))
  }

  #' Create a histogram of the phases returned by a brief title keyword search
  #' @param x the database table.
  plot_phase_histogram = function(x) {
    # Define a fixed set of phases
    x$phase[is.na(x$phase)] = "NA"
    # Problem 1: Fix the phase histogram so that the x-axis values are uniform regardless of the query.
    fixed_phases <- c("Early Phase 1", "Phase 1", "Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3",
                      "Phase 3", "Phase 4", "Not Applicable", "NA")  # Include all possible phases

    # Count phase frequencies
    phase_counts <- table(factor(x$phase, levels = fixed_phases))

    # Create a data frame with the fixed phases and their counts
    phase_data <- data.frame(Phase = names(phase_counts), Count = as.numeric(phase_counts))

    # Order phases and create labels
    phase_data$Phase <- factor(phase_data$Phase, levels = fixed_phases)

    # Create the phase histogram
    ggplot(phase_data, aes(x = Phase, y = Count)) +
      geom_col(fill = "skyblue", color = "black") +  # Customize fill and border colors
      xlab("Phase") +
      ylab("Count") +
      labs(title = "Clinical Trial Phase Distribution",  # Add title
           caption = "Source: https://clinicaltrials.gov/") +  # Add caption
      scale_x_discrete(labels = scales::wrap_format(width = 10)) +  # Wrap x-axis labels for better presentation
      theme_minimal() +  # Use a minimal theme
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  }

  #' Get the number of concurrent trials for each date in a set of studies
  #' @param d the studies to get the number of concurrent trials for.
  #' @return A tibble with a `date` column and a `count` of the number of
  #' concurrent trials at that date.
  get_concurrent_trials = function(d) {
    # Get all of the unique dates.
    all_dates = d |>
      pivot_longer(cols = everything()) |>
      select(-name) |>
      distinct() |>
      arrange(value) |>
      na.omit() |>
      rename(date = value)

    within_date = function(date, starts, ends) {
      date >= starts & date <= ends
    }

    # Get the number of concurrent trials at each of the unique dates.
    all_dates$count =
      map_dbl(
        all_dates$date,
        ~ .x |>
          within_date(d$start_date, d$completion_date) |>
          sum(na.rm = TRUE)
      )
    return(all_dates)
  }

  #' Create a plot of the concurrent studies in the query
  #' @param studies the database table.
  plot_concurrent_studies = function(studies) {
    studies |>
      select(start_date, completion_date) |>
      get_concurrent_trials() |>
      ggplot(aes(x = date, y = count)) +
      geom_line(color = "blue") +  # Customize line color
      xlab("Date") +
      ylab("Count") +
      labs(title = "Concurrent Trials Over Time",  # Add title
           caption = "Source: https://clinicaltrials.gov/") +  # Add caption
      theme_minimal() +  # Use a minimal theme
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  }

  #' Create a histogram of the conditions that trials in a query are examining
  #' @param data the database table.
  #' @param num_top_conditions the number conditions the user wants to visualize
  plot_conditions_histogram = function(data, num_top_conditions) {
    # Find the top n most common conditions
    x_grouped <- data |>
      group_by(condition_name) |>
      summarize(n=n()) |>
      arrange(desc(n))

    top_conditions <- x_grouped$condition_name |>
      head(num_top_conditions)

    # Create a new column that determines whether the study is in one of those top n conditions or not
    x_grouped$condition_group <- ifelse(x_grouped$condition_name %in% top_conditions,
                                        x_grouped$condition_name,
                                        "Other")

    # Define a fixed set of conditions
    fixed_conditions <- append(top_conditions, "Other")

    # Count condition frequencies
    condition_counts <- x_grouped |>
      group_by(condition_group)  |>
      summarize(total= sum(n))

    # Create a data frame with the fixed conditions and their counts
    condition_data <- data.frame(Condition = condition_counts$condition_group, Count = as.numeric(condition_counts$total))

    # Order conditions and create labels
    condition_data$Condition <- factor(condition_data$Condition, levels = fixed_conditions)

    # Plot
    ggplot(condition_data, aes(x = Condition, y = Count)) +
      geom_col(fill = "skyblue", color = "black") +
      xlab("Condition") +
      ylab("Count") +
      labs(title = "Clinical Trial Condition Distribution",  # Add title
           caption = "Source: https://clinicaltrials.gov/") +  # Add caption
      scale_x_discrete(labels = scales::wrap_format(width = 15)) +  # Wrap x-axis labels for better presentation
      scale_y_log10() + # Scale y to better see smaller buckets
      theme_minimal() + # Use a minimal theme
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability
  }

  #' Create a histogram of the countries that trials in a query are coming from
  #' @param data the database table.
  #' @param num_top_countries the number countries the user wants to visualize
  plot_countries_frequency = function(data, num_top_countries) {
    # Find the top n most common countries
    country_grouped <- data |>
      group_by(country_name) |>
      summarize(n = n()) |>
      arrange(desc(n))

    top_countries <- country_grouped$country_name |>
      head(num_top_countries)

    # Create a new column that determines whether the study is in one of those top n countries or not
    country_grouped$country_group <- ifelse(country_grouped$country_name %in% top_countries,
                                            country_grouped$country_name,
                                            "Other")

    # Define a fixed set of countries
    fixed_countries <- append(top_countries, "Other")

    # Count country frequencies
    country_counts <- country_grouped |>
      group_by(country_group)  |>
      summarize(total = sum(n))

    # Create a data frame with the fixed countries and their counts
    country_data <- data.frame(Country = country_counts$country_group, Count = as.numeric(country_counts$total))

    # Order countries and create labels
    country_data$Country <- factor(country_data$Country, levels = fixed_countries)

    # Plot
    ggplot(country_data, aes(x = Country, y = Count)) +
      geom_col(fill = "skyblue", color = "black") +
      xlab("Country") +
      ylab("Count") +
      labs(title = "Clinical Trial Country Distribution",  # Add title
           caption = "Source: https://clinicaltrials.gov/") +  # Add caption
      scale_x_discrete(labels = scales::wrap_format(width = 15)) +  # Wrap x-axis labels for better presentation
      theme_minimal() + # Use a minimal theme
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability
  }

  #' Create a histogram of the intervention types that trials in a query are coming from
  #' @param data the database table.
  plot_interventions_histogram = function(data) {
    ggplot(data, aes(x = intervention_type)) +
      geom_bar(fill = "skyblue", color = "black") +
      xlab("Intervention") +
      ylab("Count") +
      labs(title = "Clinical Trial Intervention Distribution",  # Add title
           caption = "Source: https://clinicaltrials.gov/") +  # Add caption
      theme_minimal() + # Use a minimal theme
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability
  }

  # Define the UI layout for the Clinical Trials Query Shiny App
  ui <- fluidPage(

    # Application title
    titlePanel("Clinical Trials Query"),

    # Sidebar with input controls
    sidebarLayout(
      sidebarPanel(

        # 1. Text input for entering keywords related to brief titles
        textInput("brief_title_kw", label = h4("Brief Title Keywords")),

        # 2. Drop down input for selecting sponsor types
        selectizeInput("source_class",
                       label = h4("Sponsor Type"),
                       choices = list(
                         "Federal" = "FED",
                         "Individual" = "INDIV",
                         "Industry" = "INDUSTRY",
                         "Network" = "NETWORK",
                         "NIH" = "NIH",
                         "Other" = "OTHER",
                         "Other gov" = "OTHER_GOV",
                         "Unknown" = "Unknown"
                       ),
                       multiple = TRUE,
                       options = list(
                         placeholder = 'Select sponsor types',
                         style = 'btn-primary',
                         dropdown = TRUE)
        ),

        # 3. Dropdown input for filtering on status
        selectizeInput("overall_status", label = h4("Study Status"),
                       choices = list("Unknown" = "Unknown status",
                                      "Completed" = "Completed",
                                      "Withdrawn" = "Withdrawn",
                                      "Recruiting" = "Recruiting",
                                      "Terminated" = "Terminated",
                                      "Active, not recruiting" = "Active, not recruiting",
                                      "Suspended" = "Suspended",
                                      "Enrolling by invitation" = "Enrolling by invitation",
                                      "Not yet recruiting" = "Not yet recruiting",
                                      "Withheld" = "Withheld",
                                      "No longer available" = "No longer available",
                                      "Approved for marketing" = "Approved for marketing",
                                      "Available" = "Available",
                                      "Temporarily not available" = "Temporarily not available"),
                       multiple = TRUE,
                       options = list(
                         placeholder = 'Select status',
                         style = 'btn-primary',
                         dropdown = TRUE)
        ),

        # 4. Check box input for filtering FDA regulated drugs
        checkboxGroupInput("is_fda_filter",
                           label = h4("FDA Regulated Drug"),
                           choices = c("Yes" = "TRUE", "No" = "FALSE")
        ),

        # 5. Check box input for filtering FDA regulated devices
        checkboxGroupInput("is_fda_device_filter",
                           label = h4("FDA Regulated Device"),
                           choices = c("Yes" = "TRUE", "No" = "FALSE")
        ),

        # 6. Button to download queried data
        downloadButton("download_csv", "Download CSV")
      ),

      # Main panel with tabs for data visualization
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("Phase", plotOutput("phase_plot")),
          tabPanel("Concurrent", plotOutput("concurrent_plot")),
          tabPanel("Conditions", plotOutput("conditions_plot"),
                   div(
                     numericInput("num_top_conditions",
                                  "Number of Top Conditions",
                                  value = 5,
                                  min = 1,
                                  max = 10)
                   )
          ),
          tabPanel("Countries", plotOutput("countries_plot"),
                   div(
                     numericInput("num_top_countries",
                                  "Number of Top Countries",
                                  value = 5,
                                  min = 1,
                                  max = 10)
                   )
          ),
          tabPanel("Interventions", plotOutput("interventions_plot"))
        ),

        # Data table to display query results
        dataTableOutput("trial_table")
      )
    )
  )

  # Define the server logic for the Clinical Trials Query Shiny App
  server <- function(input, output) {

    # Define a reactive function to retrieve and process studies data
    get_studies = reactive({

      # 1. Filter data by Brief Title Keywords
      if (input$brief_title_kw != "") {
        si = input$brief_title_kw |>
          strsplit(",") |>
          unlist() |>
          trimws()
        ret = query_kwds(studies, si, "brief_title", match_all = TRUE)
      } else {
        ret = studies
      }

      # 2. Filter data by Sponsor Type
      if (!is.null(input$source_class)) {
        ret = ret |>
          filter(source_class %in% !!input$source_class)
      }

      # 3. Filter data by Study Status
      if (!is.null(input$overall_status)) {
        ret = ret |>
          filter(overall_status %in% !!input$overall_status)
      }

      # 4. Filter data by FDA Regulated Drug
      if ("TRUE" %in% input$is_fda_filter) {
        ret <- ret |>
          filter(is_fda_regulated_drug == TRUE)
      }
      if ("FALSE" %in% input$is_fda_filter) {
        ret <- ret |>
          filter(is_fda_regulated_drug == FALSE)
      }

      # 5. Filter data by FDA Regulated Device
      if ("TRUE" %in% input$is_fda_device_filter) {
        ret <- ret |>
          filter(is_fda_regulated_device == TRUE)
      }
      if ("FALSE" %in% input$is_fda_device_filter) {
        ret <- ret |>
          filter(is_fda_regulated_device == FALSE)
      }

      # LEFT JOIN conditions data into the studies data based on nct_id
      ret = ret |>
        left_join(conditions |> rename(condition_name = name), by = "nct_id")

      # We will not include countries that have been removed
      filtered_countries <- countries |>
        filter(!removed) |>
        rename(country_name = name)

      # LEFT JOIN filtered countries data into the studies data based on nct_id
      ret = ret |>
        left_join(filtered_countries, by = "nct_id")

      # LEFT JOIN intervention type data into studies data based on nct_id
      ret = ret |>
        left_join(interventions, by="nct_id")

      ret |>
        head(max_num_studies) |>
        collect()
    })

    # 1. Phase histogram
    output$phase_plot = renderPlot({
      get_studies() |>
        plot_phase_histogram()
    })

    # 2. Concurrent studies plot
    output$concurrent_plot = renderPlot({
      get_studies() |>
        plot_concurrent_studies()
    })

    # 3. Conditions histogram
    # Problem 2: Add a new tab that gives a histogram showing the conditions that trials in a query are examining.
    output$conditions_plot = renderPlot({
      get_studies() |>
        plot_conditions_histogram(input$num_top_conditions)
    })

    # 4. Countries histogram
    output$countries_plot = renderPlot({
      get_studies() |>
        plot_countries_frequency(input$num_top_countries)
    })

    # 5. Interventions histogram
    output$interventions_plot = renderPlot({
      get_studies() |>
        plot_interventions_histogram()
    })

    # Output a clean table of results of query
    output$trial_table = renderDataTable({
      get_studies() |>
        head(max_num_studies) |>
        select(nct_id, brief_title, start_date, completion_date) |>
        rename(`NCT ID` = nct_id,
               `Brief Title` = brief_title,
               `Start Date` = start_date,
               `Completion Date` = completion_date)
    })

    # Define the server logic for downloading data as CSV
    output$download_csv <- downloadHandler(
      filename = function() {
        # Generate a dynamic file name with a description of the query
        query_description <- paste("query_results", input$brief_title_kw, ".csv", sep = "_")
        return(query_description)
      },
      content = function(file) {
        data <- get_studies()
        write.csv(data, file, row.names = FALSE)
      }
    )
  }

  # Run the application
  shinyApp(ui = ui, server = server)

}
