library(shiny)


companies <- c("Johnson & Johnson" = "JNJ",
  "IBM" = "IBM",
  "Intel" = "INTC",
  "General Electric" = "GE",
  "Cisco" = "CSCO",
  "Disney" = "DIS",
  "Microsoft" = "MSFT",
  "Nike" = "NKE")

shinyUI(fluidPage(
        titlePanel("Portfolio Risk"),
        
        verbatimTextOutput("help"),

        fluidRow(
                column(3,
                       h4("Portfolio:"),
                       br(),
                       selectInput("symb1", "Stock 1:", c(companies)),
                       selectInput("symb2", "Stock 2:", c(companies)),
                       selectInput("symb3", "Stock 3:", c(companies)),
                       br(),
                       br(),
                       br(),
                       br(),
                       actionButton("risk", "Calculate Risk", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                ),
                column(3,
                       br(),
                       br(),
                       br(),
                       numericInput("amount1", "Amount ($)", 0, min = 0, max = 10000),
                       numericInput("amount2", "Amount ($)", 0, min = 0, max = 10000),
                       numericInput("amount3", "Amount ($)", 0, min = 0, max = 10000),
                       h4('Total:'),
                       verbatimTextOutput("total")
                ),
                column(6,
                       plotOutput("plot")
                       
                )
        ),
        br(),
        fluidRow(
                
                column(6,
                       htmlOutput("riskText"))
        ),
        br(),
        fluidRow(       
                column(3,
                       verbatimTextOutput("valueAtRisk"))
        )
))