#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

load("models.RData")

ui <- fluidPage(
  titlePanel("Ансамбли"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num",
                  "Установите число строк",
                  min = 1,
                  max = nrow(boston.test),
                  value = 1),
      selectInput("method",
                  "Выберите переменные для анализа",
                  choices = c("Averaging", "Stacking"))   
      
    ),
    mainPanel(
      tableOutput("metrics")
    )
  )
)

server <- function(input, output) {
  
  selectedData = reactive({
    boston.ind = sample(nrow(boston.test), input$num)
    boston.test[boston.ind,]
  })
  
  output$metrics <- renderTable({
    boston.data = selectedData()
    pred.1.tree = predict(model.1.tree, newdata = boston.data)
    pred.2.lm = predict(model.2.lm, newdata = boston.data)
    pred.3.reg = predict(model.3.reg, newdata = boston.data)
    
     if(input$method == "Averaging"){
       res = pred.final = (pred.1.tree + pred.2.lm + pred.3.reg)/3
     }else{
      dataStack = data.frame(tree = predict(model.1.tree, newdata = boston.train), 
                             lm = predict(model.2.lm, newdata = boston.train), 
                             reg = predict(model.3.reg, newdata = boston.train),
                             medv = boston.train$medv)
      model.tree = train(medv~., data=dataStack, method = "ctree")
      dataStack.test = data.frame(tree = pred.1.tree, lm = pred.2.lm, reg = pred.3.reg)
      res = predict(model.tree, newdata = dataStack.test)
    }
    boston.data = cbind(res,boston.data)
    boston.data
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

