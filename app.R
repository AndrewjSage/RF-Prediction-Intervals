#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
library(shiny)
library(tidyverse)
library(randomForestSRC)
library(gridExtra)



# Define UI ----
ui <- fluidPage(
  titlePanel("Basic widgets"),
  
  fluidRow(
    column(3, 
           sliderInput("a", h3("a"),
                       min = -5, max = 5, value = 0)),
    column(3,
           sliderInput("b", h3("b"),
                       min = -5, max = 5, value = 0)),
    column(3,
          sliderInput("c", h3("c"),
                      min = -5, max = 5, value = 0)),
    column(3,
          sliderInput("d", h3("d"),
                      min = -5, max = 5, value = 0))
),
 
fluidRow(
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("plot"),
    tableOutput("table")
  )  
)

)

# Define server logic required to draw a histogram
server <- function(input, output){

  Simulation <- function(a,b,c,d){
   
   ntrain <- 1000
   ntest <- 1000
   N <- ntrain + ntest
   
   
   #x1 <- rnorm(ntrain + ntest, 0, 1)
   x1 <- seq(from=-5, to=5, by=10/(ntrain+ntest))
   x1 <- x1[sample(1:length(x1))]
   x1 <- x1[1:(ntrain+ntest)]
   #x2 <- rnorm(ntrain + ntest, 0, 1)
   #x3 <- rnorm(ntrain + ntest, 0, 1)
   #x4 <- rnorm(ntrain + ntest, 0, 1)
   #x5 <- rnorm(ntrain + ntest, 0, 1)
   e <- rnorm(ntrain + ntest, 0, 1)
   #e <- rexp(ntrain + ntest, rate=1/5) - 1
   y <- a*x1 + b*x1^2 + c*(x1>0) + d*(x1<1) + e
   
   Data <- data.frame(x1, y)
   Train <- Data[1:ntrain, ]
   Test <- Data[(ntrain+1):(ntrain+ntest), ]
   
   
   # Intervals from Linear Model
   # Assume linearity, normality, constant variance
   LM <- lm(data=Train, y~.)
   LMpred <- predict(LM, newdata=Test) 
   MSPE_LM <- mean((LMpred-Test$y)^2)
   LM_PI <- predict(LM, newdata=Test, interval="prediction") 
   Cov_LM <- mean(Test$y >= LM_PI[,2] & Test$y <= LM_PI[,3])
   Width_LM <- mean(LM_PI[,3]-LM_PI[,2])
   
   RF <- quantreg(data=Train, formula = y~., nodesize=5, method="forest", prob=c(0.025, 0.975))
   RFpredinfo <- quantreg(object=RF, newdata=Test, prob=c(0.025, 0.975))
   RFpred <- RFpredinfo$predicted
   MSPE_RF <- mean((RFpred - Test$y)^2)
   # Intervals from Random Forest - Sym. OOB method
   # Assumes constant variance, error distribution symmetric
   OOB_resid <- Train$y - RF$predicted.oob
   MOE <- quantile(abs(OOB_resid), .95)
   RF_PI2 <- data.frame(RFpred, RFpred-MOE, RFpred+MOE)
   Coverage_RFOOB_Sym <- mean(Test$y >= RF_PI2[,2] & Test$y <= RF_PI2[,3])
   Width_RFOOB_Sym <- mean(RF_PI2[,3]-RF_PI2[,2])
   # Intervals from Random Forest - Non. Sym. OOB method
   # Assumes constant variance
   Lower <- quantile(OOB_resid, .025)
   Upper <- quantile(OOB_resid, .975)
   RF_PI <- data.frame(RFpred, RFpred+Lower, RFpred+Upper)
   Coverage_RFOOB_NonSym <- mean(Test$y >= RF_PI[,2] & Test$y <= RF_PI[,3])
   Width_RFOOB_NonSym <- mean(RF_PI[,3]-RF_PI[,2])
   # Intervals from Quantile Random Forest 
   # Does not assume constant variance
   QRFLower <- RFpredinfo$quantreg$quantiles[,1]
   QRFUpper <- RFpredinfo$quantreg$quantiles[,2]
   QRF_PI <- data.frame(RFpred, QRFLower, QRFUpper)
   Coverage_QRF <- mean(Test$y >= QRF_PI[,2] & Test$y <= QRF_PI[,3])
   Width_QRF <- mean(QRF_PI[,3]-QRF_PI[,2])
   
   
   Res_LM <- c(MSPE_LM, Cov_LM, Width_LM)
   Res_RFOOBSym <- c(MSPE_RF, Coverage_RFOOB_Sym, Width_RFOOB_Sym)
   Res_RFOOBNonSym <- c(MSPE_RF, Coverage_RFOOB_NonSym, Width_RFOOB_NonSym)
   Res_QRF <- c(MSPE_RF, Coverage_QRF, Width_QRF)
   
   Res_Table <- t(data.frame(Res_LM, Res_RFOOBSym, Res_RFOOBNonSym, Res_QRF))
   colnames(Res_Table) <- c("MSPE", "Coverage", "Width")
   rownames(Res_Table) <- c("Linear Model", "Random Forest with Symmetry Assumption", "Random Forest - No Symmetry Assumption", "Random Forest - No Constant Variance Assumption")

   #Plot <- qplot(Test$x1, Test$y)
   p1<-ggplot(data=Test, aes(x=x1, y=y)) + geom_point() + geom_line(aes(x=x1, y=LM_PI[,1]), color="red")
   p1<-p1+geom_ribbon(aes(ymin=LM_PI[,2], ymax=LM_PI[,3]), linetype=2, alpha=0.5)
   
   p2<-ggplot(data=Test, aes(x=x1, y=y)) + geom_point() + geom_line(aes(x=x1, y=RF_PI[,1]), color="red")
   p2<-p2+geom_ribbon(aes(ymin=RF_PI[,2], ymax=RF_PI[,3]), linetype=2, alpha=0.5)
   
   p3<-ggplot(data=Test, aes(x=x1, y=y)) + geom_point() + geom_line(aes(x=x1, y=RF_PI2[,1]), color="red")
   p3<-p3+geom_ribbon(aes(ymin=RF_PI2[,2], ymax=RF_PI2[,3]), linetype=2, alpha=0.5)
   
   p4<-ggplot(data=Test, aes(x=x1, y=y)) + geom_point() + geom_line(aes(x=x1, y=QRF_PI[,1]), color="red")
   p4<-p4+geom_ribbon(aes(ymin=QRF_PI[,2], ymax=QRF_PI[,3]), linetype=2, alpha=0.5)
   
   Plots <- grid.arrange(p1, p2, p3, p4, ncol=2)
   return(list(Plots, Res_Table))
   }
  
  SimulationRes <- reactive({Simulation(input$a,input$b,input$c,input$d)})
  output$plot <- renderPlot(SimulationRes()[[1]])
  output$table <- renderTable(SimulationRes()[[2]])
}



# Run the application 
shinyApp(ui = ui, server = server)
