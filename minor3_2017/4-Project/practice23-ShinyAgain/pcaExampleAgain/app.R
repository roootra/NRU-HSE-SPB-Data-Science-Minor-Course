library(shiny)

library(ISLR)
library(e1071)
library(caret)


ui <- fluidPage(
   titlePanel("Пример работы с PCA"),
   sidebarLayout(
      sidebarPanel(
         selectInput("method",
                     "Выберите метод классификации",
                     choices = list("svm", "ctree"))  
         
      ),
      mainPanel(
         tableOutput("metrics")
      )
   )
)

server <- function(input, output) {
  
  load("pca.Rdata")

   output$metrics <- renderTable({
     svm_model <- svm(Private~., data = transformed.main)
     #Результаты на обучающей 
     svm.PredMain<-predict(svm_model, transformed.main, probability=FALSE)
     cmMain = confusionMatrix(svm.PredMain,transformed.main$Private)
     #и тестовой выборках
     svm.PredTest<-predict(svm_model, transformed.test, probability=FALSE)
     cmTest = confusionMatrix(svm.PredTest,transformed.test$Private)
     rbind(t(cmMain$overall), t(cmTest$overall))
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

