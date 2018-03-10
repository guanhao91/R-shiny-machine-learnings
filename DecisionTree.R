library(shiny)

ui <- fluidPage(
  tags$h1("This is a Shiny app for Decision Tree Analysis"),
  textInput(inputId = "file",
            label = "Input File Name",
            value = "breast-cancer-wisconsin.data"),
  sliderInput(inputId = "num", 
    label = "Percentage of Train Set", 
    value = 0.75, min = 0.1, max = 0.9),
  textInput(inputId = "F1",
            label = "Class Level 2 label",
            value = "benign"),
  textInput(inputId = "F2",
            label = "Class Level 4 label",
            value = "malignant"),
  actionButton(inputId = "go",
               label = "Run Model"),
  plotOutput("dtree"),
  tags$strong("Model Fit Summary"),
  verbatimTextOutput("stats1"),
  tags$strong("Classification Results"),
  verbatimTextOutput("stats")
  #tags$strong("Prediction Accuracy"),
  #verbatimTextOutput("stats2")
)

server <- function(input, output) {
  library(rpart)
  library(rpart.plot)
  library(party)
  #click on run model to calculate results
  percent <- eventReactive(input$go, {
    input$num
  })
  
  observe({
  #make sure the file is stored in the current folder
    url<-input$file
    breast<-read.table(url,sep=",",header=FALSE,na.strings="?")
    names(breast)<-c("ID","clumpThickness","sizeUniformity","shapeUniformity","maginalAdhesion","singleEpithelialCellSize",
                     "bareNuclei","blandChromatin","normalNucleoli","mitosis","class")
    df<-breast[-1]
    df$class<-factor(df$class,levels=c(2,4),labels=c(input$F1,input$F2))
    set.seed(1234)
    train<-sample(nrow(df),percent()*nrow(df))
    df.train<-df[train,]
    df.validate<-df[-train,]
    table(df.train$class)
    table(df.validate$class)
    set.seed(1234)
    dtree<-rpart(class~.,data=df.train,method="class",parms=list(split="information"))
    output$stats1 <- renderPrint({
      dtree$cptable
    })
    output$dtree <- renderPlot({
      plotcp(dtree)
    })
    dtree.pruned<-prune(dtree,cp=0.0125)
    
    prp(dtree.pruned,type=2,extra=104,fallen.leaves=TRUE,main="Decision Tree")
    
    dtree.pred<-predict(dtree.pruned,df.validate,type="class")
    
    dtree.perf<-table(df.validate$class,dtree.pred,dnn=c("Actual","Predicted"))
    output$stats <- renderPrint({
    dtree.perf
  })
  
  })

}

shinyApp(ui = ui, server = server)