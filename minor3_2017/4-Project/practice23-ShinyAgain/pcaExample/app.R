library(shiny)

library(ISLR)
library(e1071)
library(caret)
dataInitial <- College

ui <- fluidPage(
   titlePanel("Пример работы с PCA"),
   sidebarLayout(
      sidebarPanel(
         numericInput("seed",
                     "Установите set.seed",
                     min = 1,
                     value = 1325),
         selectInput("varnames",
                     "Выберите переменные для анализа",
                     choices = names(dataInitial)[-1],  #удаляем Private из списка выбора
                     multiple = TRUE)   # для выбора нескольких элементов списка
         
      ),
      mainPanel(
         tableOutput("metrics")
      )
   )
)

server <- function(input, output) {
  dataSelected <- reactive({
     if (length(input$varnames)<2) dataInitial  # если выбрано меньше 2 предикторов
     else dplyr::select(dataInitial, c(input$varnames, "Private")) # выбираем переменные и добавляем Private к данным
  })
  
  trainTest = reactive({
    data = dataSelected()
    set.seed(input$seed)
    data.test.ind = sample(seq_len(nrow(data)), size = nrow(data)*0.2)
    data.test.ind
  })
  
  transformation <- reactive({
    data = dataSelected()
    data.test = data[trainTest(),]
    data.main = data[-trainTest(),]
    preprocessParams <- preProcess(data.main, method=c("center", "scale", "pca"))
    transformed.main <- predict(preprocessParams, data.main)
    transformed.test <- predict(preprocessParams, data.test)
    svm_model <- svm(Private~., data = transformed.main)
    #Результаты на обучающей 
    svm.PredMain<-predict(svm_model, transformed.main, probability=FALSE)
    cmMain = confusionMatrix(svm.PredMain,transformed.main$Private)
    #и тестовой выборках
    svm.PredTest<-predict(svm_model, transformed.test, probability=FALSE)
    cmTest = confusionMatrix(svm.PredTest,transformed.test$Private)
    rbind(t(cmMain$overall), t(cmTest$overall))
  })

   output$metrics <- renderTable({
     transformation()
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

