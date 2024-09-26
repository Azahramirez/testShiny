

library(shiny)
# Function to multiply the first column of a data frame by 100
m1c <- function(df,n) {
  n=as.integer(n)
  # Check if the data frame has at least one column
  if (ncol(df) < 1) {
    stop("The data frame must have at least one column.")
  }

  # Multiply the first column by n
  df[, 1] <- df[, 1] * n

  # Return the modified data frame
  return(df)
}
# Define UI for dataset viewer app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Test for different variables inputs"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # File input for CSV file upload
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      textInput(inputId = "caption",
                label = "Caption:",
                value = "Data Summary"),

      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("rock", "pressure", "cars","multiply")),

      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10),

      selectInput(
        "select",
        "Select options below:",
        list("Choice 1A" = 0, "Choice 1B" = 10, "Choice 1C" = 100),
        multiple = FALSE
      ),


    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),



      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),

      # Output: HTML table with requested number of observations ----
      tableOutput("view"),



      # Table output to display the contents of the uploaded file
      tableOutput("contents")

    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {

  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = m1c(rock,input$select),
           "pressure" = pressure,
           "cars" = cars)

  })

#


  output$file1_contents <- renderPrint({print(input$file1)})

  output$contents <- renderTable({
    file <- input$file1
    req(file)

    ext <- tools::file_ext(file$datapath)
    validate(need(ext == "csv", "Please upload a csv file"))

    read.csv(file$datapath)
  })

  # Create caption ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display
  #
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes
  output$caption <- renderText({
    input$caption
  })

  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)

  })

  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })

}

# Create Shiny app ----
shinyApp(ui, server)


