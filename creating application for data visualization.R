#installing packages
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
library(shiny)
library(ggplot2)
library(DT) 
#starting with the code
ui <- fluidPage(
  titlePanel("Simple Shiny Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("chart_type", "Choose Chart Type:",
                  choices = c("Bar Plot", "Histogram", "Line Chart", "Pie Chart")),
      sliderInput("sample_size", "Sample Size:", min = 10, max = 100, value = 50),
      checkboxInput("show_table", "Show Data Table", value = FALSE)
    ),
    
    mainPanel(
      plotOutput("selected_plot"),
      DTOutput("data_table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Generate random data based on selected chart type
  generate_data <- reactive({
    set.seed(123)
    switch(input$chart_type,
           "Bar Plot" = data.frame(Category = letters[1:input$sample_size], Value = runif(input$sample_size, 10, 100)),
           "Histogram" = data.frame(Values = rnorm(input$sample_size)),
           "Line Chart" = data.frame(Time = 1:input$sample_size, Value = cumsum(runif(input$sample_size, 5, 15))),
           "Pie Chart" = data.frame(Category = letters[1:input$sample_size], Value = runif(input$sample_size, 10, 100))
    )
  })
  
  # Render selected plot
  output$selected_plot <- renderPlot({
    data <- generate_data()
    
    if (input$chart_type == "Bar Plot") {
      ggplot(data, aes_string(x = names(data)[1], y = names(data)[2])) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = input$chart_type, x = names(data)[1], y = names(data)[2])
    } else if (input$chart_type == "Histogram") {
      ggplot(data, aes(x = Values)) +
        geom_histogram(binwidth = 1, fill = "salmon", color = "black") +
        labs(title = input$chart_type, x = names(data)[1], y = names(data)[2])
    } else if (input$chart_type == "Line Chart") {
      ggplot(data, aes(x = Time, y = Value)) +
        geom_line(color = "green") +
        labs(title = input$chart_type, x = names(data)[1], y = names(data)[2])
    } else if (input$chart_type == "Pie Chart" && names(data)[1] == "Category") {
      ggplot(data, aes(x = 1, y = Value, fill = Category)) +
        geom_bar(stat = "identity") +
        coord_polar(theta = "y") +
        labs(title = input$chart_type, x = names(data)[1], y = names(data)[2])
    } else {
      ggplot() + theme_void()  # Blank plot if conditions are not met
    }
  })
  
  # Render data table
  output$data_table <- renderDT({
    if (input$show_table) {
      datatable(generate_data(), options = list(pageLength = 5))
    }
  })
}

# Run the code for application
shinyApp(ui = ui,server=server)
