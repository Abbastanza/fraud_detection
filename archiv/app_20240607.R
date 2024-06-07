library(shiny)
library(rpart)

# Laden des trainierten Entscheidungsbaummodells (ersetzen Sie dies durch den tatsächlichen Pfad Ihres Modells)
model <- readRDS("decision_tree_model.rds")

# Definition der UI
ui <- fluidPage(
  titlePanel("Fraud Detection"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("amount", "Amount", value = 1),
      numericInput("oldbalanceOrg", "Old Balance Org", value = 0),
      numericInput("newbalanceOrig", "New Balance Orig", value = 0),
      selectInput("type", "Type", choices = c("CASH_IN", "CASH_OUT", "DEBIT", "PAYMENT", "TRANSFER")),
      selectInput("nameDestType", "Destination Type", choices = c("Merchant", "Individual"))
    ),
    
    mainPanel(
      textOutput("prediction")
    )
  )
)

# Definition des Server-Teils
server <- function(input, output) {
  
  output$prediction <- renderText({
    # Daten vorbereiten
    data <- data.frame(
      type = factor(input$type, levels = c("CASH_IN", "CASH_OUT", "DEBIT", "PAYMENT", "TRANSFER")),
      amount = log10(input$amount + 1),
      oldbalanceOrg = log10(input$oldbalanceOrg + 1),
      newbalanceOrig = log10(input$newbalanceOrig + 1),
      nameDestType = factor(input$nameDestType, levels = c("Merchant", "Individual"))
    )
    
    data$change.balanceOrg <- input$oldbalanceOrg - input$newbalanceOrig
    data$flagFraud = factor(ifelse(input$newbalanceOrig == 0 & data$change.balanceOrg == input$amount, 1, 0),
                            levels = c(0, 1), labels = c("No", "Yes"))
    
    # Vorhersage
    prediction <- predict(model, newdata = data, type = "class")
    
    if (prediction == "Yes") {
      "Prediction: Fraud"
    } else {
      "Prediction: Not Fraud"
    }
  })
}

# App starten
shinyApp(ui = ui, server = server)
