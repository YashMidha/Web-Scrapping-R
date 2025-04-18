library(shiny)
library(DT)
library(plotly)

# Loading the dataset and assigning a unique ID to each record
data <- read.csv("~/countries_statistics.csv")
data$ID <- seq_len(nrow(data))

# Defining UI
ui <- fluidPage(
  titlePanel("Global Development Dashboard"),
  
  # Sidebar containing control options
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "sidebar_tabs",
                  # Filter tab to filter records based on country name, life expectancy, population, income group and life expectany group
                  tabPanel("Filters",
                           textInput("search", "Search Country", value = ""),
                           selectInput("filter_income", "Income Group", choices = c("All", unique(data$Income_group))),
                           selectInput("filter_life", "Life Expectancy Group", choices = c("All", unique(data$Life_Expectancy_Above_Average))),
                           sliderInput("filter_population", "Population Range",
                                       min = min(data$Population),
                                       max = max(data$Population),
                                       value = c(min(data$Population), max(data$Population))),
                           sliderInput("filter_life_expectancy", "Life Expectancy (Years)",
                                       min = min(data$Life_Expectancy, na.rm = TRUE),
                                       max = max(data$Life_Expectancy, na.rm = TRUE),
                                       value = c(min(data$Life_Expectancy, na.rm = TRUE), max(data$Life_Expectancy, na.rm = TRUE))),
                           actionButton("clear_filters", "Clear Filters")
                  ),
                  
                  # Add tab used to add records
                  tabPanel("Add",
                           textInput("add_Location", "Country"),
                           numericInput("add_Population", "Population", value = NULL),
                           numericInput("add_Life_Expectancy", "Life Expectancy", value = NULL),
                           selectInput("add_Income_group", "Income Group", choices = unique(data$Income_group)),
                           numericInput("add_Poverty_rate", "Poverty Rate", value = NULL),
                           numericInput("add_GDP", "GDP", value = NULL),
                           numericInput("add_Inflation_Rate", "Inflation Rate", value = NULL),
                           numericInput("add_Human_Capital_Index", "Human Capital Index", value = NULL),
                           actionButton("add_btn", "Add")
                  ),
                  
                  # Update tab used to update records based on ID
                  tabPanel("Update",
                           numericInput("update_ID", "ID", value = NULL),
                           textInput("update_Location", "Country"),
                           numericInput("update_Population", "Population", value = NULL),
                           numericInput("update_Life_Expectancy", "Life Expectancy", value = NULL),
                           selectInput("update_Income_group", "Income Group", choices = unique(data$Income_group)),
                           numericInput("update_Poverty_rate", "Poverty Rate", value = NULL),
                           numericInput("update_GDP", "GDP", value = NULL),
                           numericInput("update_Inflation_Rate", "Inflation Rate", value = NULL),
                           numericInput("update_Human_Capital_Index", "Human Capital Index", value = NULL),
                           actionButton("update_btn", "Update")
                  ),
                  
                  # Delete tab used to delete record based on ID
                  tabPanel("Delete",
                           numericInput("delete_ID", "ID", value = NULL),
                           actionButton("delete_btn", "Delete")
                  )
      )
    ),
    
    # Main panel with visualizations and data table
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DTOutput("data_table")),
        tabPanel("GDP by Country", plotlyOutput("plotly_gdp")),
        tabPanel("Inflation Rate Histogram", plotlyOutput("plotly_inflation")),
        tabPanel("Income Group Distribution", plotlyOutput("plotly_income_group"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive storage of the dataset
  rv <- reactiveValues(data = data)
  
  # Function to reset form inputs when a record has been added
  resetForm <- function() {
    updateTextInput(session, "add_Location", value = "")
    updateNumericInput(session, "add_Population", value=NA)
    updateNumericInput(session, "add_Life_Expectancy", value = NA)
    updateSelectInput(session, "add_Income_group", selected = unique(data$Income_group)[1])
    updateNumericInput(session, "add_Poverty_rate", value = NA)
    updateNumericInput(session, "add_GDP", value = NA)
    updateNumericInput(session, "add_Inflation_Rate", value = NA)
    updateNumericInput(session, "add_Human_Capital_Index", value = NA)
  }
  
  # Add Record Event to add new record
  observeEvent(input$add_btn, {
    new_entry <- data.frame(
      Location = input$add_Location,
      Population = input$add_Population,
      Life_Expectancy = input$add_Life_Expectancy,
      Income_group = input$add_Income_group,
      Poverty_rate = input$add_Poverty_rate,
      GDP = input$add_GDP,
      Inflation_Rate = input$add_Inflation_Rate,
      Human_Capital_Index = input$add_Human_Capital_Index,
      Wealthy = ifelse(input$add_Income_group == "High income", "Yes", "No"),
      GDP_per_capita = input$add_GDP/input$add_Population,
      Life_Expectancy_Above_Average = ifelse(input$add_Life_Expectancy > mean(rv$data$Life_Expectancy, na.rm=TRUE), "Above Average", "Below Average"),
      ID = max(rv$data$ID, na.rm = TRUE) + 1
    )
    rv$data <- rbind(rv$data, new_entry)
    
    showModal(modalDialog(
      title = "Added Record",
      "Record was successfully added",
      easyClose = TRUE,
    ))
    
    resetForm()
  })
  
  # Update record using ID
  observeEvent(input$update_btn, {
    req(input$update_ID)
    
    row_index <- which(rv$data$ID == input$update_ID)
    if (length(row_index) == 1) {
      rv$data[row_index, "Location"] <- input$update_Location
      rv$data[row_index, "Population"] <- input$update_Population
      rv$data[row_index, "Life_Expectancy"] <- input$update_Life_Expectancy
      rv$data[row_index, "Income_group"] <- input$update_Income_group
      rv$data[row_index, "Poverty_rate"] <- input$update_Poverty_rate
      rv$data[row_index, "GDP"] <- input$update_GDP
      rv$data[row_index, "Inflation_Rate"] <- input$update_Inflation_Rate
      rv$data[row_index, "Human_Capital_Index"] <- input$update_Human_Capital_Index
      rv$data[row_index, "Wealthy"] <- ifelse(input$update_Income_group == "High income", "Yes", "No")
      rv$data[row_index, "GDP_per_capita"] <- input$update_GDP/input$update_Population
      rv$data[row_index, "Life_Expectancy_Above_Average"] <- ifelse(input$update_Life_Expectancy > mean(rv$data$Life_Expectancy, na.rm=TRUE), "Above Average", "Below Average")
      
      showModal(modalDialog(
        title = "Updated Record",
        "Record was successfully updated",
        easyClose = TRUE,
      ))
    }
  })
  
  # Delete record using ID
  observeEvent(input$delete_btn, {
    if (!is.na(input$delete_ID)) {
      row_index <- which(rv$data$ID == input$delete_ID)
      if (length(row_index) == 1) {
        rv$data <- rv$data[-row_index, ]
      }
      showModal(modalDialog(
        title = "Deleted Record",
        "Record was successfully deleted",
        easyClose = TRUE,
      ))
    }
  })
  
  # Filter record based on country name, life expectancy, population, income group and life expectany group
  observeEvent(input$clear_filters, {
    updateTextInput(session, "search", value = "")
    updateSelectInput(session, "filter_income", selected = "All")
    updateSelectInput(session, "filter_life", selected = "All")
    updateSliderInput(session, "filter_population", value = c(min(data$Population), max(data$Population)))
    updateSliderInput(session, "filter_life_expectancy", value = c(min(data$Life_Expectancy, na.rm = TRUE), max(data$Life_Expectancy, na.rm = TRUE)))
  })
  
  # Filtered data update
  filtered_data <- reactive({
    df <- rv$data
    if (input$search != "") df <- df[grepl(input$search, df$Location, ignore.case = TRUE), ]
    if (input$filter_income != "All") df <- df[df$Income_group == input$filter_income, ]
    if (input$filter_life != "All") df <- df[df$Life_Expectancy_Above_Average == input$filter_life, ]
    df <- df[df$Population >= input$filter_population[1] & df$Population <= input$filter_population[2], ]
    df <- df[df$Life_Expectancy >= input$filter_life_expectancy[1] & df$Life_Expectancy <= input$filter_life_expectancy[2], ]
    df
  })
  
  # Rendering data table
  output$data_table <- renderDT({
    datatable(filtered_data(), selection = "single", options = list(scrollX = TRUE))
  })
  
  # Rendering GDP plot
  output$plotly_gdp <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~Location, y = ~GDP, type = "bar")
  })
  
  # Rendering inflation rate plot
  output$plotly_inflation <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~Inflation_Rate, type = "histogram")
  })
  
  # Rendering income group plot
  output$plotly_income_group <- renderPlotly({
    plot_ly(data = filtered_data(), labels = ~Income_group, type = "pie")
  })
  
  # Handling click update event
  observeEvent(input$data_table_rows_selected, {
    selected_row <- filtered_data()[input$data_table_rows_selected, ]
    if (nrow(selected_row) == 1) {
      updateNumericInput(session, "update_ID", value = selected_row$ID)
      updateTextInput(session, "update_Location", value = selected_row$Location)
      updateNumericInput(session, "update_Population", value = selected_row$Population)
      updateNumericInput(session, "update_Life_Expectancy", value = selected_row$Life_Expectancy)
      updateSelectInput(session, "update_Income_group", selected = selected_row$Income_group)
      updateNumericInput(session, "update_Poverty_rate", value = selected_row$Poverty_rate)
      updateNumericInput(session, "update_GDP", value = selected_row$GDP)
      updateNumericInput(session, "update_Inflation_Rate", value = selected_row$Inflation_Rate)
      updateNumericInput(session, "update_Human_Capital_Index", value = selected_row$Human_Capital_Index)
      updateNumericInput(session, "delete_ID", value = selected_row$ID)
      updateTabsetPanel(session, "sidebar_tabs", selected = "Update")
    }
  })
  
}

shinyApp(ui, server)
