library(shiny)
library(rpart)
library(rpart.plot)

# Laden des trainierten Entscheidungsbaummodells (ersetzen Sie dies durch den tatsächlichen Pfad Ihres Modells)
model <- readRDS("decision_tree_model.rds")

# Definition der UI
ui <- fluidPage(
  titlePanel("Fraud Detection"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("amount", "Transaction Amount", value = 1),
      numericInput("oldbalanceOrg", "Old Balance Sender", value = 0),
      numericInput("newbalanceOrig", "New Balance Receiver", value = 0)
    ),
    mainPanel(
      htmlOutput("prediction")
    )
  ),
  
  # Elemente unter der Sidebar
  fluidRow(
    tags$hr(),  # Horizontale Linie als Trenner
    column(6, tableOutput("featureImportance"), style = "padding: 20px;"),  # Tabelle mit Padding für Abstand
    column(6, plotOutput("treePlot"))  # Plot neben der Tabelle
  )
)

# Definition des Server-Teils
server <- function(input, output) {
  
  output$prediction <- renderUI({
    # Daten vorbereiten
    data <- data.frame(
      type = factor("TRANSFER"),
      amount = log10(input$amount + 1),
      oldbalanceOrg = log10(input$oldbalanceOrg + 1),
      newbalanceOrig = log10(input$newbalanceOrig + 1),
      nameDestType = factor("Individual")
    )
    
    data$change.balanceOrg <- input$oldbalanceOrg - input$newbalanceOrig
    data$flagFraud = factor(ifelse(input$newbalanceOrig == 0 & data$change.balanceOrg == input$amount, 1, 0), levels = c(0, 1), labels = c("No", "Yes"))
    
    # Vorhersage
    prediction <- predict(model, newdata = data, type = "class")
    
    if (prediction == "Yes") {
      tags$h2("Prediction: Fraud", style = "color: red; font-size: 30px; border: 5px solid red; padding: 10px;")
    } else {
      tags$h2("Prediction: Not Fraud", style = "color: green; font-size: 30px; border: 5px solid green; padding: 10px;")
    }
  })
  
  output$featureImportance <- renderTable({
    feature_importance <- model$variable.importance
    feature_importance <- data.frame(feature = names(feature_importance), importance = feature_importance)
    feature_importance <- feature_importance[order(-feature_importance$importance), ]
    feature_importance
  })
  
  output$treePlot <- renderPlot({
    rpart.plot(model, main = "Decision Tree Model")
  })
}

# App starten
shinyApp(ui = ui, server = server)
