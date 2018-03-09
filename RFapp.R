library(shiny)

ui <- fluidPage(
  tags$h1("This is a RandomForest Shiny app for Wine Quality Test"),
  sliderInput(inputId = "num", 
    label = "Number of Trees", 
    value = 500, min = 10, max = 500),
  textInput(inputId = "file",
            label = "Write file name",
            value = "winequality-white.csv"),
  actionButton(inputId = "go",
               label = "Run Model"),
  plotOutput("hist"),
  tags$strong("Sample Data Overview"),
  verbatimTextOutput("stats1"),
  tags$strong("Classification Results"),
  verbatimTextOutput("stats"),
  tags$strong("Prediction Accuracy"),
  verbatimTextOutput("stats2")
)

server <- function(input, output) {
  library(randomForest)
  library(ggplot2)
  #click on run model to calculate results
  data <- eventReactive(input$go, {
    input$num
  })
  
  observe({
  #make sure the file is stored in the current folder
  wine1<-read.csv(input$file)
  wine1$taste <- ifelse(wine1$quality < 6, 'bad', 'good')
  wine1$taste[wine1$quality == 6] <- 'normal'
  wine1$taste <- as.factor(wine1$taste)
  output$hist <- renderPlot({
    ggplot(wine1, aes(quality, fill = cut(quality, 100))) +
      geom_histogram(show.legend = FALSE,binwidth = 0.5)
  })
  set.seed(123)
  samp <- sample(nrow(wine1), 0.6 * nrow(wine1))
  train <- wine1[samp, ]
  test <- wine1[-samp, ]
  output$stats1 <- renderPrint({
  head(wine1)
  })
  model <- reactive({randomForest(taste ~ . - quality, data = train, ntree=data())})
 
  pred <- reactive({predict(model(), newdata = test)})
  t<-reactive({table(pred(), test$taste)})
  output$stats <- renderPrint({
    table(pred(), test$taste)
  })
  
  output$stats2 <- renderPrint({
    (t()[1,1] + t()[2,2] + t()[3,3]) / nrow(test)
  })
  })

}

shinyApp(ui = ui, server = server)