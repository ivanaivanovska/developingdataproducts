# server.R
library(ggplot2)

getYahooUrl <- function(symbol) {
        url <- paste0("http://real-chart.finance.yahoo.com/table.csv?s=", symbol, 
                      "&a=", 07, "&b=", 01, "&c=", 2011,
                      "&d=", 06, "&e=", 01, "&f=", 2016,
                      "&g=d&ignore=.csv")
        url
}

readFromYahoo <- function(url){
        dat <- read.table(url,header=TRUE,sep=",")
        df <- dat[,c(1,5)]
        df$Date <- as.Date(as.character(df$Date))
        df
}

 calculateRisk <- function(M, A) {
         
         valueAtRisk <- 0
         if(sum(A) > 0) {
                 # Relative differences of the Close prices
                 M <- diff(M)/M[1:nrow(M)-1, ]
                 
                 # Covariance Matrix of the price differences
                 C <- cov(M) 
                 
                 # Weights
                 W <- A / sum(A) 
                 
                 # Daily variance of the Portfolio
                 varP <- W %*% C %*% t(W)  
                 
                 # Daily std of Portfolio
                 stdP <- sqrt(varP) 
                 
                 # Monthly std of Portfolio: avg 21 trading days
                 stdP <- stdP * sqrt(21)
                 
                 valueAtRisk <- - 2.33 * stdP * sum(A)
         }
         valueAtRisk
 }

shinyServer(

   function(input, output) {
        
        # Text Output
        output$help <- renderText({""
                paste("Build your investment portfolio:",
                      "   - Select 3 stocks in which you would like to invest.",
                      "   - Enter the amount (in dollars) that you would like to invest.",
                      "",
                      "Based on information for the stock price in the past 5 years, the risk of the investment will be calculated.", sep='\n')
        })
        
        output$riskText <- renderText({ paste("The ",
                                           "<font color=\"blue\"><b>", "maximum loss", "</b></font>",
                                           "expected (in dollars), over the ", 
                                           "<font color=\"blue\"><b>", "next month", "</b></font>",
                                           ", with ", 
                                           "<font color=\"blue\"><b>", 99, "%</b></font>",
                                           " confidence:")
        })
        
        # Read Data From Yahoo on new Stock selected
        x1 <- reactive({
                url1 <- getYahooUrl(input$symb1)
                df1 <- readFromYahoo(url1)
                df1
        })
        
        x2 <- reactive({
                url2 <- getYahooUrl(input$symb2)
                df2 <- readFromYahoo(url2)
                df2
        })
        
        x3 <- reactive({
                url3 <- getYahooUrl(input$symb3)
                df3 <- readFromYahoo(url3)
                df3
        })

        # Calculate Value at Risk, only when the action button is pressed
        riskVal <- eventReactive(input$risk, {

                        M <- cbind(x1()$Close, x2()$Close, x3()$Close)
                        A <- cbind(input$amount1, input$amount2, input$amount3)
                        
                        # Calculate value at risk
                        risk <- calculateRisk(M, A)

                        risk
        })
        output$valueAtRisk <- renderText({riskVal()})

        # Calculate Total amount
        output$total <- renderText(input$amount1 + input$amount2 + input$amount3)        
       
        # Plot Close Price for the selected stocks
        output$plot <- renderPlot({
                                
                   breaksVals <- c(input$symb1, input$symb2, input$symb3 )
                   colorVals <- c("blue", "green", "red")
                 
                   p <- ggplot(x1(), aes(Date,Close)) +
                         geom_line(aes(color=input$symb1))
                   p <- p + geom_line(data=x2(),aes(color=input$symb2))                           
                   p <- p + geom_line(data=x3(), aes(color=input$symb3))
                   p <- p + labs(color="Legend")
                   p <- p + scale_colour_manual("", breaks = breaksVals, values = colorVals)
                   p <- p + ggtitle("Closing Stock Prices")
                   p <- p + theme(plot.title = element_text(lineheight=.7, face="bold"))
                   p
        })
})