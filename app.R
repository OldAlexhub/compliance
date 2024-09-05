library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)

# Define the Shiny UI
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),  # Use a modern theme
  
  tags$head(
    tags$style(HTML("
      /* Style for the cards */
      .info-card {
        background-color: #3498db;
        color: white;
        padding: 20px;
        border-radius: 8px;
        text-align: center;
        margin-bottom: 15px;
      }
      .info-card h3 {
        margin: 0;
        font-size: 24px;
      }
      .info-card p {
        margin: 0;
        font-size: 18px;
      }
      .plot-container {
        margin-top: 20px;
      }
    "))
  ),
  
  titlePanel("Driver Compliance Checker"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Upload CSV"),
      
      fileInput("file1", "Choose CSV File", 
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      br(),
      
      # Directions for CSV headers
      h4("CSV Format Instructions:"),
      p("Your CSV file should have the following headers:"),
      tags$ul(
        tags$li("DriverId - Unique identifier for each driver"),
        tags$li("driverName - Full name of the driver"),
        tags$li("DateofBirth - Date in YYYY-MM-DD or MM/DD/YYYY format"),
        tags$li("DLExpirationDate - Driver's License expiration date in YYYY-MM-DD or MM/DD/YYYY format"),
        tags$li("DOTExpirationDate - DOT expiration date in YYYY-MM-DD or MM/DD/YYYY format"),
        tags$li("LastDrugTest - Last drug test date in YYYY-MM-DD or MM/DD/YYYY format"),
        tags$li("BackgroundCheck - Background check date in YYYY-MM-DD or MM/DD/YYYY format"),
        tags$li("PUCFingerPrints - Fingerprints date in YYYY-MM-DD or MM/DD/YYYY format"),
        tags$li("MVRLastRan - Last motor vehicle record check in YYYY-MM-DD or MM/DD/YYYY format"),
        tags$li("LastTrained - Last training date in YYYY-MM-DD or MM/DD/YYYY format")
      ),
      
      br(),
      
      downloadButton("downloadData", "Download Results", class = "btn-custom")
    ),
    
    mainPanel(
      div(class = "info-card", h3(textOutput("totalDrivers")), p("Total Drivers")),
      div(class = "info-card", h3(textOutput("passDrivers")), p("Passed Drivers")),
      div(class = "info-card", h3(textOutput("failDrivers")), p("Failed Drivers")),
      
      div(class = "plot-container", plotOutput("passFailPlot"))
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file1)
    
    # Read the CSV and convert necessary columns to Date format
    dataset <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    
    # Ensure the date columns are correctly converted
    dataset <- dataset %>%
      mutate(
        DateofBirth = as.Date(DateofBirth, format= '%m/%d/%Y'),
        DLExpirationDate = as.Date(DLExpirationDate, format= '%m/%d/%Y'),
        DOTExpirationDate = as.Date(DOTExpirationDate, format= '%m/%d/%Y'),
        LastDrugTest = as.Date(LastDrugTest, format= '%m/%d/%Y'),
        BackgroundCheck = as.Date(BackgroundCheck, format= '%m/%d/%Y'),
        PUCFingerPrints = as.Date(PUCFingerPrints, format= '%m/%d/%Y'),
        MVRLastRan = as.Date(MVRLastRan, format= '%m/%d/%Y'),
        LastTrained = as.Date(LastTrained, format= '%m/%d/%Y')
      )
    
    # Add pass/fail criteria and generate Result column
    dataset <- dataset %>%
      rowwise() %>%
      mutate(
        Is_Pass = case_when(
          (as.numeric(difftime(Sys.Date(), DateofBirth, units = "days")) / 365.25) > 21 &
            DLExpirationDate > Sys.Date() &
            DOTExpirationDate > Sys.Date() &
            LastDrugTest > Sys.Date() - 365.25 &
            BackgroundCheck > Sys.Date() - (365.25 / 2) &
            PUCFingerPrints > Sys.Date() &
            MVRLastRan > Sys.Date() - (365.25 / 2) &
            LastTrained > Sys.Date() - (365.25 / 2) ~ 'Pass',
          TRUE ~ 'Fail'
        ),
        Result = ifelse(Is_Pass == 'Fail', paste(
          ifelse((as.numeric(difftime(Sys.Date(), DateofBirth, units = "days")) / 365.25) <= 21, "Age below 21;", ""),
          ifelse(DLExpirationDate <= Sys.Date(), "Driver's License expired;", ""),
          ifelse(DOTExpirationDate <= Sys.Date(), "DOT expired;", ""),
          ifelse(LastDrugTest <= Sys.Date() - 365.25, "Drug test not within last year;", ""),
          ifelse(BackgroundCheck <= Sys.Date() - (365.25 / 2), "Background check not within last 6 months;", ""),
          ifelse(PUCFingerPrints <= Sys.Date(), "PUC Fingerprints expired;", ""),
          ifelse(MVRLastRan <= Sys.Date() - (365.25 / 2), "MVR not run within last 6 months;", ""),
          ifelse(LastTrained <= Sys.Date() - (365.25 / 2), "Training not done within last 6 months;", "")
        ), "Pass")
      )
    
    dataset
  })
  
  # Output total number of drivers
  output$totalDrivers <- renderText({
    req(data())
    nrow(data())
  })
  
  # Output number of passed drivers
  output$passDrivers <- renderText({
    req(data())
    sum(data()$Is_Pass == "Pass")
  })
  
  # Output number of failed drivers
  output$failDrivers <- renderText({
    req(data())
    sum(data()$Is_Pass == "Fail")
  })
  
  # Plot pass/fail distribution
  output$passFailPlot <- renderPlot({
    req(data())
    ggplot(data(), aes(x = Is_Pass)) +
      geom_bar(aes(fill = Is_Pass), width = 0.5) +
      scale_fill_manual(values = c("Pass" = "#2ecc71", "Fail" = "#e74c3c")) +
      labs(title = "Pass/Fail Distribution", x = "Result", y = "Number of Drivers") +
      theme_minimal()
  })
  
  # Provide a downloadable CSV of the results
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("driver_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server, options = list(port = 8080, host = "0.0.0.0"))

