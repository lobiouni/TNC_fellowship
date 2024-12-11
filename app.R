# Set a CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com"))

# Specify required packages
required_packages <- c("shiny", "shinydashboard", "data.table", "DT", "tidyverse", "arrow")

# Install missing packages
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load required packages
lapply(required_packages, library, character.only = TRUE)

# Prevent scientific notation
options(scipen=999)

# Define UI for application
ui <- fluidPage(
  # Title of the application
  titlePanel("Survey Results Explorer"),
  
  # Dropdown menu to select the state
  selectInput("state", "Select a State:",
              choices = NULL),  # Initially no choices
  
  # Dropdown menu to select the county
  selectInput("county", "Select a County:",
              choices = NULL),  # Initially no choices
  
  # Dropdown menu to select the question
  selectInput("question", "Choose a question to visualize:",
              choices = c("How many times contacted?" = "Question_B1",
                          "Interest in paid restoration programs?" = "Question_B4",
                          "How familiar with restoration programs?" = "Question_B6",
                          "Motivation for restoration programs?" = "Question_B8",
                          "Agreement or disagreement with statements about wetlands" = "Question_C1")),
  
  # Output for the dynamically chosen bar plot
  plotOutput("barplot")
)

# Define server logic
server <- function(input, output, session) {
  
  # Load dataset (reactively loaded for real-time filtering)
  dataset <- reactive({
    #file <- "/Users/loren/OneDrive/TNC/Activity_8_survey_app/TNC_fellowship/data/derived_products/dt_long.feather" # for windows use
    file <- "/Users/lsilva88/Library/CloudStorage/OneDrive-Personal/TNC/Activity_8_survey_app/TNC_fellowship/data/derived_products/dt_long.feather" # for mac use
    read_feather(file)  # Load dataset
  })
  
  # the state dropdown based on the dataset
  observe({
    req(dataset())  # Ensure the dataset is available
    states <- unique(dataset()$State)  # Get unique states
    updateSelectInput(session, "state", choices = states)
  })
  
  # the county dropdown based on the dataset
  observe({
    req(dataset(), input$state)  # Ensure the dataset and state are available
   
    # Get counties for selected state
    counties <- dataset() %>%
      filter(State == input$state) %>%
      pull(County) %>%
      unique()
    
    # Ensure counties are character and add "All Counties"
    counties <- as.character(counties)
    counties <- c("All Counties", counties)
    
    # Update county dropdown with proper names
    updateSelectInput(session, "county", choices = counties)
  })
  
  # Render the selected plot based on dropdown choice
  output$barplot <- renderPlot({
    req(input$state)  # Only state is required to visualize the data
    
    # Filter data
    data_filtered <- dataset() %>%
      filter(State == input$state)  # Filter data by state
    
    if (input$county != "All Counties") {
      data_filtered <- data_filtered %>%
        filter(County == input$county)  # Further filter by county if selected
    }
    
    # Create the plot based on selected question
    if (input$question == "Question_B1") {
      
      # Data summary for question B1
      data_summary <- data_filtered %>%
        select(sid, Question_B1) %>%
        unique() %>%
        group_by(Question_B1) %>%
        summarise(count = n()) %>%
        mutate(percentage = count / sum(count) * 100)
      
      # Update the levels of Question_B4 to include line breaks
      data_summary$Question_B1 <- factor(data_summary$Question_B1, 
                                         levels = unique(data_summary$Question_B1), 
                                         labels = sapply(unique(data_summary$Question_B1), function(x) {
                                           strwrap(x, width = 10, simplify = TRUE) %>%
                                             paste(collapse = "\n")
                                         }))
      
      # Plot for question B1
      ggplot(data_summary, aes(x = Question_B1, y = count)) +
        geom_bar(stat = "identity", fill = "#A8D8B9", color = "#3C8A5A") +
        geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                  vjust = -0.5, size = 5, color = "black") +
        theme_minimal(base_size = 15) +
        theme(axis.text.y = element_text(colour = "black", size = 14),
              axis.text.x = element_text(colour = "black", size = 14, angle = 0),
              axis.title.y = element_text(size=16),
              axis.title.x = element_text(size=16),
              plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
              legend.text = element_text(size=16),
              legend.title = element_text(size=16)) +
        labs(title = "How many times contacted?", 
             y = "Number of Observations", 
             x = NULL)
      
    } else if (input$question == "Question_B4") {
      
      # Data summary for question B4
      data_summary <- data_filtered %>%
        select(sid, Question_B4) %>%
        unique() %>%
        group_by(Question_B4) %>%
        summarise(count = n()) %>%
        mutate(percentage = count / sum(count) * 100)
      
      # Update the levels of Question_B4 to include line breaks
      data_summary$Question_B4 <- factor(data_summary$Question_B4, 
                                         levels = unique(data_summary$Question_B4), 
                                         labels = sapply(unique(data_summary$Question_B4), function(x) {
                                           strwrap(x, width = 10, simplify = TRUE) %>%
                                             paste(collapse = "\n")
                                         }))
      
      # Plot for question B4
      ggplot(data_summary, aes(x = Question_B4, y = count)) +
        geom_bar(stat = "identity", fill = "#A8D8B9", color = "#3C8A5A") +
        geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                  vjust = -0.5, size = 5, color = "black") +
        theme_minimal(base_size = 15) +
        theme(axis.text.y = element_text(colour = "black", size = 14),
              axis.text.x = element_text(colour = "black", size = 14, angle = 0),
              axis.title.y = element_text(size=16),
              axis.title.x = element_text(size=16),
              plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
              legend.text = element_text(size=16),
              legend.title = element_text(size=16)) +
        labs(title = "Interest in paid restoration programs", 
             y = "Number of Observations", 
             x = NULL)
      
    }else if (input$question == "Question_B6") {
      
      # Data summary for question B6
      data_summary <- data_filtered %>%
        select(sid, Question_B6, Answer_B6) %>%
        unique() %>%
        group_by(Question_B6, Answer_B6) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(Question_B6) %>%
        mutate(percentage = count / sum(count) * 100)
      
      # Update the levels of Answer_B6 to include line breaks
      data_summary$Answer_B6 <- factor(data_summary$Answer_B6, 
                                       levels = unique(data_summary$Answer_B6), 
                                       labels = sapply(unique(data_summary$Answer_B6), function(x) {
                                         strwrap(x, width = 20, simplify = TRUE) %>%
                                           paste(collapse = "\n")
                                       }))
      
      # Plot for question B6
      ggplot(data_summary, aes(y = Question_B6, x = count, fill = Answer_B6)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = ifelse(percentage > 15, paste0(round(percentage, 1), "%"), "")), 
                  position = position_stack(vjust = 0.5), size = 5, color = "black") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_fill_manual(values = c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51", "#fb8500")) +
        theme_minimal(base_size = 15) +
        theme(axis.text.y = element_text(colour = "black", size = 14),
              axis.text.x = element_text(colour = "black", size = 14, angle = 0),
              axis.title.y = element_text(size=16),
              axis.title.x = element_text(size=16),
              plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
              legend.text = element_text(size=16),
              legend.title = element_text(size=16)) +
        labs(title = "Familiar with restoration programs that property may be eligible for", 
             x = "Number of Observations", 
             y = "Restoration programs",
             fill = "Response")
      
    }else if (input$question == "Question_B8") {
      
      # Data summary for question B8
      data_summary <- data_filtered %>%
        select(sid, Question_B8, Answer_B8) %>%
        unique() %>%
        group_by(Question_B8, Answer_B8) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(Question_B8) %>%
        mutate(percentage = count / sum(count) * 100)
      
      # Update the levels of Answer_B6 to include line breaks
      data_summary$Answer_B8 <- factor(data_summary$Answer_B8, 
                                       levels = unique(data_summary$Answer_B8), 
                                       labels = sapply(unique(data_summary$Answer_B8), function(x) {
                                         strwrap(x, width = 20, simplify = TRUE) %>%
                                           paste(collapse = "\n")
                                       }))
      
      # Plot for question B8
      ggplot(data_summary, aes(y = Question_B8, x = count, fill = Answer_B8)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = ifelse(percentage > 15, paste0(round(percentage, 1), "%"), "")), 
                  position = position_stack(vjust = 0.5), size = 5, color = "black") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_fill_manual(values = c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51", "#fb8500")) +
        theme_minimal(base_size = 15) +
        theme(axis.text.y = element_text(colour = "black", size = 14),
              axis.text.x = element_text(colour = "black", size = 14, angle = 0),
              axis.title.y = element_text(size=16),
              axis.title.x = element_text(size=16),
              plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
              legend.text = element_text(size=16),
              legend.title = element_text(size=16)) +
        labs(title = "Motivation for restoration programs", 
             x = "Number of Observations", 
             y = "Motivation",
             fill = "Response")
      
    }else if (input$question == "Question_C1") {
      
      # Data summary for question C1
      data_summary <- data_filtered %>%
        select(sid, Question_C1, Answer_C1) %>%
        unique() %>%
        group_by(Question_C1, Answer_C1) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(Question_C1) %>%
        mutate(percentage = count / sum(count) * 100)
      
      # Update the levels of Answer_B6 to include line breaks
      data_summary$Answer_C1 <- factor(data_summary$Answer_C1, 
                                       levels = unique(data_summary$Answer_C1), 
                                       labels = sapply(unique(data_summary$Answer_C1), function(x) {
                                         strwrap(x, width = 20, simplify = TRUE) %>%
                                           paste(collapse = "\n")
                                       }))
      
      # Plot for question C1
      ggplot(data_summary, aes(y = Question_C1, x = count, fill = Answer_C1)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = ifelse(percentage > 15, paste0(round(percentage, 1), "%"), "")), 
                  position = position_stack(vjust = 0.5), size = 5, color = "black") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_fill_manual(values = c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51", "#fb8500", "#9e2a2b")) +
        theme_minimal(base_size = 15) +
        theme(axis.text.y = element_text(colour = "black", size = 14),
              axis.text.x = element_text(colour = "black", size = 14, angle = 0),
              axis.title.y = element_text(size=16),
              axis.title.x = element_text(size=16),
              plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
              legend.text = element_text(size=16),
              legend.title = element_text(size=16)) +
        labs(title = "Level of agreement or disagreement with statements about wetlands", 
             x = "Number of Observations", 
             y = "Statements about wetlands",
             fill = "Response")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
