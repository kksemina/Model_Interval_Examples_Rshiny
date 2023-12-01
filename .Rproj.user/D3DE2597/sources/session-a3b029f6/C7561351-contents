
# Load required libraries
library(shiny)
library(ggplot2)
library(gridExtra)

# Load the mtcars dataset
data(mtcars)

# Split the dataset into training and testing sets
set.seed(123)  # For reproducibility
sample_size <- floor(0.7 * nrow(mtcars))  # 70% for training, 30% for testing
train_indices <- sample(seq_len(nrow(mtcars)), size = sample_size)
train_data <- mtcars[train_indices, ]
test_data <- mtcars[-train_indices, ]

# Define the user interface (UI)
ui <- fluidPage(
  titlePanel("Confidence vs. Prediction Intervals"),
  
  
  # Add a text section at the top
  fluidRow(
    column(
      width = 12,  # Use the full width of the row
      h3("Welcome to the Confidence vs. Prediction Intervals App"),
      p("This app allows you to explore confidence and prediction intervals for a linear regression model based on the mtcars dataset with in R."),
      p("This app shows the difference between confidence and prediction intervals. A prediction interval is less certain than a confidence interval. A prediction interval predicts an individual number, whereas a confidence interval predicts the mean value. A prediction interval focuses on future events, whereas a confidence interval focuses on past or current events."),
      p("Adjust the sliders on the left to change the confidence and prediction levels.")
    )
  ),
  
  
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha_confidence", "Confidence Level:",
                  min = 0.01, max = 0.99, value = 0.90, step = 0.01),
      sliderInput("alpha_prediction", "Prediction Level:",
                  min = 0.01, max = 0.99, value = 0.80, step = 0.01)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Reactive function to calculate intervals based on user input
  intervals <- reactive({
    # Fit linear regression models
    lm_model_confidence <- lm(mpg ~ wt, data = train_data)
    lm_model_prediction <- lm(mpg ~ wt, data = train_data)
    
    # Predict the values and intervals
    predictions_confidence <- predict(lm_model_confidence, newdata = test_data, interval = "confidence", level = input$alpha_confidence)
    predictions_prediction <- predict(lm_model_prediction, newdata = test_data, interval = "prediction", level = input$alpha_prediction)
    
    # Create data frames for the results
    results_confidence <- data.frame(
      x = test_data$wt,
      Predicted = predictions_confidence[, 1],
      Lower_CI = predictions_confidence[, 2],
      Upper_CI = predictions_confidence[, 3]
    )
    
    results_prediction <- data.frame(
      x = test_data$wt,
      Predicted = predictions_prediction[, 1],
      Lower_PI = predictions_prediction[, 2],
      Upper_PI = predictions_prediction[, 3]
    )
    
    list(results_confidence, results_prediction)
  })
  
  # Create the plot based on user input
  output$plot <- renderPlot({
    # Extract intervals based on user input
    intervals_data <- intervals()
    
    # Create a plot for confidence intervals
    plot_confidence <- ggplot(intervals_data[[1]], aes(x = x, y = Predicted)) +
      geom_line() +
      geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "blue", alpha = 0.3) +
      labs(title = "Confidence Intervals", x = "Weight (wt)", y = "Predicted Value")
    
    # Create a plot for prediction intervals
    plot_prediction <- ggplot(intervals_data[[2]], aes(x = x, y = Predicted)) +
      geom_line() +
      geom_ribbon(aes(ymin = Lower_PI, ymax = Upper_PI), fill = "green", alpha = 0.3) +
      labs(title = "Prediction Intervals", x = "Weight (wt)", y = "Predicted Value")
    
    # Display both plots side by side
    grid.arrange(plot_confidence, plot_prediction, ncol = 2)
  })
}

# Run the Shiny application
shinyApp(ui, server)






