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

# Boston Housing Data
x <- read.delim("https://raw.githubusercontent.com/haozhestat/RFIntervals/master/DataAnalysis/data/nipsdata/Boston/x.txt", header=FALSE, sep=" ")
y <- read.delim("https://raw.githubusercontent.com/haozhestat/RFIntervals/master/DataAnalysis/data/nipsdata/Boston/y.txt", header=FALSE, sep=" ")
names(y) <- "y"
Data <- cbind(x,y)

# Define UI ----
ui <- fluidPage(
  titlePanel("Random Forest Prediction Intervals"),
  
  fluidRow(
    column(3, 
           sliderInput("a", h3("Linearity"),
                       min = 0, max = 5, value = 0)),
    column(3,
           sliderInput("b", h3("Normality/Symmetry"),
                       min = 0, max = 1, value = 0)),
    column(3,
          sliderInput("c", h3("Constant Variance"),
                      min = 0, max = 5, value = 0))
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

  Simulation <- function(a,b,c,ns){
   
    samp <- sample(1:nrow(Data), floor(nrow(Data)/2), replace=FALSE)
    Train <- Data[samp, ]
    Test <- Data[-samp, ]
    
    alpha=0.05
    ns <- 10
    
    
   
   # Intervals from Linear Model
   # Assume linearity, normality, constant variance
   LM <- lm(data=Train, y~.)
   LMpred <- predict(LM, newdata=Test) 
   MSPE_LM <- mean((LMpred-Test$y)^2)
   LM_PI <- predict(LM, newdata=Test, interval="prediction", level=1-alpha) 
   LM_Contains <- (Test$y >= LM_PI[,2]) & (Test$y <= LM_PI[,3])
   Cov_LM <- mean(LM_Contains)
   Width_LM <- mean(LM_PI[,3]-LM_PI[,2])
   
   RF <- quantreg(data=Train, formula = y~., method="forest", prob=c(alpha/2, 1-alpha/2), ntree=100, nodesize=ns)
   RFpredinfo <- quantreg(object=RF, newdata=Test, prob=c(alpha/2, 1-alpha/2))
   RFpred <- RFpredinfo$predicted
   MSPE_RF <- mean((RFpred - Test$y)^2)
   # Intervals from Random Forest - Sym. OOB method
   # Assumes constant variance, error distribution symmetric
   OOB_resid <- Train$y - RF$predicted.oob
   MOE <- quantile(abs(OOB_resid), 1-alpha)
   RF_PI2 <- data.frame(RFpred, RFpred-MOE, RFpred+MOE)
   RF2_Contains <- (Test$y >= RF_PI2[,2]) & (Test$y <= RF_PI2[,3])
   Coverage_RFOOB_Sym <- mean(RF2_Contains)
   Width_RFOOB_Sym <- mean(RF_PI2[,3]-RF_PI2[,2])
   # Intervals from Random Forest - Non. Sym. OOB method
   # Assumes constant variance
   Lower <- quantile(OOB_resid, alpha/2)
   Upper <- quantile(OOB_resid, 1-alpha/2)
   RF_PI <- data.frame(RFpred, RFpred+Lower, RFpred+Upper)
   RF_Contains <- (Test$y >= RF_PI[,2]) & (Test$y <= RF_PI[,3])
   Coverage_RFOOB_NonSym <- mean(RF_Contains)
   Width_RFOOB_NonSym <- mean(RF_PI[,3]-RF_PI[,2])
   # Intervals from Quantile Random Forest 
   # Does not assume constant variance
   # QRFLower <- RFpredinfo$quantreg$quantiles[,1]
   # QRFUpper <- RFpredinfo$quantreg$quantiles[,2]
   # QRF_PI <- data.frame(RFpred, QRFLower, QRFUpper)
   # QRF_Contains <- (Test$y >= QRF_PI[,2]) & (Test$y <= QRF_PI[,3])
   # Coverage_QRF <- mean(QRF_Contains)
   # Width_QRF <- mean(QRF_PI[,3]-QRF_PI[,2])   
   library(quantregForest)
   Trainx <- Train[,1:10]
   y <- Train$y
   Testx <- Test[,1:10]
   y <- Test$y
   #Trainx <- data.frame(Train$V1)
   #names(Trainx) <- "V1"
   #Testx <- data.frame(Test$V1)
   #names(Testx) <- "V1"
   QRF <- quantregForest(x=Trainx, y=Train$y, keep.inbag = TRUE, ntree=100, nodesize=ns)
   QRFpred <- predict(QRF, newdata = Testx, what=mean)
   #mean((QRFpred - Test$y)^2)
   QRFLower <- predict(QRF, newdata=Testx, what=alpha/2)
   QRFUpper <- predict(QRF, newdata=Testx, what=1-alpha/2)
   QRF_PI <- data.frame(QRFpred, QRFLower, QRFUpper)
   QRF_Contains <- (Test$y >= QRF_PI[,2]) & (Test$y <= QRF_PI[,3])
   Coverage_QRF <- mean(QRF_Contains)
   Width_QRF <- mean(QRF_PI[,3]-QRF_PI[,2])   
   
   
   Res_LM <- c(MSPE_LM, Cov_LM, Width_LM)
   Res_RFOOBSym <- c(MSPE_RF, Coverage_RFOOB_Sym, Width_RFOOB_Sym)
   Res_RFOOBNonSym <- c(MSPE_RF, Coverage_RFOOB_NonSym, Width_RFOOB_NonSym)
   Res_QRF <- c(MSPE_RF, Coverage_QRF, Width_QRF)
   
   Res_Table <- t(data.frame(Res_LM, Res_RFOOBSym, Res_RFOOBNonSym, Res_QRF))
   colnames(Res_Table) <- c("MSPE", "Coverage", "Width")
   rownames(Res_Table) <- c("Linear Model", "Random Forest with Symmetry Assumption", "Random Forest - No Symmetry Assumption", "Random Forest - No Constant Variance Assumption")

   #Plot <- qplot(Test$V1, Test$y)
   p1<-ggplot(data=Test, aes(x=V1, y=y)) + geom_point(aes(color=LM_Contains)) + geom_line(aes(x=V1, y=LM_PI[,1]), color="red") + ylim(2*min(Test$y),2*max(Test$y) )
   p1<-p1+geom_ribbon(aes(ymin=LM_PI[,2], ymax=LM_PI[,3]), linetype=2, alpha=0.5) + ggtitle("Assumes Lin., Norm., CV") + theme(legend.position = "none")
   
   p2<-ggplot(data=Test, aes(x=V1, y=y)) + geom_point(aes(color=RF2_Contains)) + geom_line(aes(x=V1, y=RF_PI2[,1]), color="red")+ ylim(2*min(Test$y),2*max(Test$y) )
   p2<-p2+geom_ribbon(aes(ymin=RF_PI2[,2], ymax=RF_PI2[,3]), linetype=2, alpha=0.5) + ggtitle("Assumes Norm., CV") + theme(legend.position = "none")
   
   p3<-ggplot(data=Test, aes(x=V1, y=y)) + geom_point(aes(color=RF_Contains)) + geom_line(aes(x=V1, y=RF_PI[,1]), color="red")+ ylim(2*min(Test$y),2*max(Test$y) )
   p3<-p3+geom_ribbon(aes(ymin=RF_PI[,2], ymax=RF_PI[,3]), linetype=2, alpha=0.5)  + ggtitle("Assumes CV") + theme(legend.position = "none")
   
   p4<-ggplot(data=Test, aes(x=V1, y=y)) + geom_point(aes(color=QRF_Contains)) + geom_line(aes(x=V1, y=QRF_PI[,1]), color="red")+ ylim(2*min(Test$y),2*max(Test$y) )
   p4<-p4+geom_ribbon(aes(ymin=QRF_PI[,2], ymax=QRF_PI[,3]), linetype=2, alpha=0.5)  + ggtitle("Assumes None of 3") + theme(legend.position = "none")
   
   Plots <- grid.arrange(p1, p2, p3, p4, ncol=2)
   return(list(Plots, Res_Table))
   }
  
  SimulationRes <- reactive({Simulation(input$a,input$b,input$c, input$ns)})
  output$plot <- renderPlot(SimulationRes()[[1]])
  output$table <- renderTable(SimulationRes()[[2]])
}



# Run the application 
shinyApp(ui = ui, server = server)
