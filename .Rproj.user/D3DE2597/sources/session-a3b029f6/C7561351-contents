# Code for R shiny app

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
      width = 12,
      h3("Welcome to the Confidence vs. Prediction Intervals App"),
      p("This app is designed for in-depth exploration of confidence and prediction intervals within the context of a linear regression model, utilizing the mtcars dataset in R."),
      p("The primary focus of this app is to elucidate the distinction between confidence and prediction intervals. Confidence intervals provide insights into the level of certainty surrounding the average value, based on historical or current data. Conversely, prediction intervals extend their scope to predict individual values, offering a glimpse into future events or outcomes."),
      p("To harness the full functionality of this application, simply manipulate the sliders on the left to fine-tune the levels of confidence and prediction to suit your specific needs."),
      style = "margin-bottom: 20px;"  # Increase margin-bottom for spacing
      
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha_confidence", "Confidence Level:",
                  min = 0.01, max = 0.99, value = 0.90, step = 0.01),
      sliderInput("alpha_prediction", "Prediction Level:",
                  min = 0.01, max = 0.99, value = 0.80, step = 0.01),
      style = "margin-top: 50px;"  # Increase margin-top for spacing
      
    ),
    mainPanel(
      plotOutput("plot"),  # Existing plot
      plotOutput("data_plot")  # New plot for training and testing data with prediction intervals
      # Display the calculation of which interval is more correct


    )
  ),
  
  # Add a text section at the bottom left
  div(
    style = "position: absolute; bottom: 100px; left: 10px; padding: 10px; max-width: 500px; word-wrap;",
    uiOutput("correct_interval_text"),
    uiOutput("correct_prediction_text"),
    br(),  # Adds a single line break
    h4("Predicted vs Actual Linear Graph"),
    p("This graph illustrates how much wider the prediction interval is compared to confidence interval."),
    p("We can see that as the Weight is getting hevier the linear model confidence intervals are larger or what is called the `Hourglass` shape."), 
    p("This is because the linear regression model is more precise at predicting when predictors are around the means.")
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
  
  # Correct predictions within the confidence interval
  total_confidence_count <- reactive({
    intervals_data <- intervals()[[1]]
    # Ensure that Lower_CI, Upper_CI, and Predicted exist in intervals_data
    correct_interval <- with(intervals_data, 
                             Predicted >= Lower_CI & Predicted <= Upper_CI & 
                               test_data$mpg >= Lower_CI & test_data$mpg <= Upper_CI)
    sum(correct_interval, na.rm = TRUE)
  })
  
  # Correct predictions within the prediction interval
  total_prediction_count <- reactive({
    intervals_data <- intervals()[[2]]
    # Ensure that Lower_PI, Upper_PI, and Predicted exist in intervals_data
    correct_interval <- with(intervals_data, 
                             Predicted >= Lower_PI & Predicted <= Upper_PI & 
                               test_data$mpg >= Lower_PI & test_data$mpg <= Upper_PI)
    sum(correct_interval, na.rm = TRUE)
  })
  
  
  # Render the text elements with HTML for styling, including larger font size
  output$correct_interval_text <- renderUI({
    count <- total_confidence_count()
    largerCount <- count > total_prediction_count()
    styledCount <- if (largerCount) 
      tags$span(style = "color: red; font-weight: bold; font-size: 20px;", count) 
    else 
      tags$span(style = "font-weight: bold; font-size: 20px;", count)
    
    HTML(paste("Total Correct Predictions for Confidence Interval:", styledCount))
  })
  
  output$correct_prediction_text <- renderUI({
    count <- total_prediction_count()
    largerCount <- count > total_confidence_count()
    styledCount <- if (largerCount) 
      tags$span(style = "color: red; font-weight: bold; font-size: 20px;", count) 
    else 
      tags$span(style = "font-weight: bold; font-size: 20px;", count)
    
    HTML(paste("Total Correct Predictions for Prediction Interval:", styledCount))
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
  
  
  
  # Create the plot for training and testing data with prediction and confidence intervals
  output$data_plot <- renderPlot({
    # Extract intervals based on user input
    intervals_data <- intervals()
    
    # Create a plot for training and testing data
    data_plot <- ggplot() +
      geom_point(data = train_data, aes(x = wt, y = mpg, color = "Training Data"), size = 3) +
      geom_point(data = test_data, aes(x = wt, y = mpg, color = "Testing Data"), size = 3) +
      geom_point(data = intervals_data[[2]], aes(x = x, y = Predicted, color = "Predicted Values"), shape = 3, size = 3) +  # Prediction points as crosses (shape = 3)
      geom_ribbon(data = intervals_data[[2]], aes(x = x, ymin = Lower_PI, ymax = Upper_PI, fill = "Prediction Intervals"), alpha = 0.3) +
      geom_ribbon(data = intervals_data[[1]], aes(x = x, ymin = Lower_CI, ymax = Upper_CI, fill = "Confidence Intervals"), alpha = 0.3) +
      scale_color_manual(values = c("Training Data" = "blue", "Testing Data" = "red", "Predicted Values" = "darkgreen")) +  # Specify colors manually
      scale_fill_manual(values = c("Confidence Intervals" = "blue", "Prediction Intervals" = "green")) +  # Specify fill colors for intervals
      labs(title = "Training and Testing Data with Prediction and Confidence Intervals", x = "Weight (wt)", y = "Miles per Gallon (mpg)") +
      theme_minimal() +
      labs(
        color = "Legend Title for Colors",
        fill = "Legend Title for Fill Colors",
        shape = "Legend Title for Shapes"
      ) +
      guides(
        color = guide_legend(title = "Legend Title for Colors"),
        fill = guide_legend(title = "Legend Title for Fill Colors"),
        shape = guide_legend(title = "Legend Title for Shapes")
      ) +
      theme(legend.key.size = unit(2, "lines"),  # Adjust the key size
            legend.text = element_text(size = 12))  # Adjust the legend text size
    
    print(data_plot)
  })
  # ...
  
  
}

# Run the Shiny application
shinyApp(ui, server)






