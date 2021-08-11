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
library(forestError)


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

  Simulation <- function(a,b,c){
   
   ntrain <- 10000
   ntest <- 10000
   alpha <- 0.1
   N <- ntrain + ntest
   ns <- 2
   
   
   #x1 <- rnorm(ntrain + ntest, 0, 1)
   ep1 <- -1
   ep2 <- 1
   x1 <- seq(from=ep1, to=ep2, by=(ep2-ep1)/(ntrain+ntest))
   x1 <- x1[sample(1:length(x1))]
   x1 <- x1[1:(ntrain+ntest)]
   x2 <- rnorm(ntrain + ntest, 0, 1)
   x3 <- rnorm(ntrain + ntest, 0, 1)
   x4 <- rnorm(ntrain + ntest, 0, 1)
   x5 <- rnorm(ntrain + ntest, 0, 1)
   x6 <- rnorm(ntrain + ntest, 0, 1)
   x7 <- rnorm(ntrain + ntest, 0, 1)
   x8 <- rnorm(ntrain + ntest, 0, 1)
   x9 <- rnorm(ntrain + ntest, 0, 1)
   x10 <- rnorm(ntrain + ntest, 0, 1) 
   mx <- x1 + a*x1^2 + a*(x1^3)*(a>1.5)
   mx <- scale(mx) + a*(x1>0)*(a>3) 
   e1 <- rnorm(ntrain + ntest, 0, 1) +  c*rnorm(ntrain + ntest, 0, abs(x1))
   e2 <- rexp(ntrain + ntest, rate=1/5) - 5
   e <- (1-b)*e1 + b*e2 # convex combination of errors
   y <- mx + e  #e + e*(c*(abs(x1))) 

   Data <- data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, y)
   Train <- Data[1:ntrain, ]
   Test <- Data[(ntrain+1):(ntrain+ntest), ]

   Trainx <- Train[,1:10]
   y <- Train$y
   Testx <- Test[,1:10]
   y <- Test$y   
   
   # Intervals from Linear Model
   # Assume linearity, normality, constant variance
   LM <- lm(data=Train, y~x1)
   LMpred <- predict(LM, newdata=Test) 
   MSPE_LM <- mean((LMpred-Test$y)^2)
   LM_PI <- predict(LM, newdata=Test, interval="prediction", level=1-alpha) 
   LM_Contains <- (Test$y >= LM_PI[,2]) & (Test$y <= LM_PI[,3])
   Cov_LM <- mean(LM_Contains)
   Width_LM <- mean(LM_PI[,3]-LM_PI[,2])
   
   RF <- randomForest(data=Train, x=Trainx, y=Train$y, nodesize=ns, method="forest", keep.inbag = TRUE)
   RFpred <- predict(object=RF, newdata=Testx)
   MSPE_RF <- mean((RFpred - Test$y)^2)
   # Intervals from Random Forest - Sym. OOB method
   # Assumes constant variance, error distribution symmetric
   OOB_resid <- Train$y - RF$predicted
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
   library(forestError)
   QFE <- quantForestError(forest=RF, X.train=Trainx, X.test=Testx, Y.train = Train$y,  alpha = alpha)
   QFEPred <- QFE$estimates[,1]
   QFELower <- QFE$estimates[,4]
   QFEUpper <- QFE$estimates[,5]
   QFE_PI <- data.frame(QFEPred, QFELower, QFEUpper)
   QFE_Contains <- (Test$y >= QFE_PI[,2]) & (Test$y <= QFE_PI[,3])
   Coverage_QFE <- mean(QFE_Contains)
   Width_QFE <- mean(QFE_PI[,3]-QFE_PI[,2])
   
   
   Res_LM <- c(MSPE_LM, Cov_LM, Width_LM)
   Res_RFOOBSym <- c(MSPE_RF, Coverage_RFOOB_Sym, Width_RFOOB_Sym)
   Res_RFOOBNonSym <- c(MSPE_RF, Coverage_RFOOB_NonSym, Width_RFOOB_NonSym)
   Res_QFE <- c(MSPE_RF, Coverage_QFE, Width_QFE)
   
   Res_Table <- t(data.frame(Res_LM, Res_RFOOBSym, Res_RFOOBNonSym, Res_QFE))
   colnames(Res_Table) <- c("MSPE", "Coverage", "Width")
   rownames(Res_Table) <- c("Linear Model", "Random Forest with Symmetry Assumption", "Random Forest - No Symmetry Assumption", "Random Forest - No Constant Variance Assumption")

   #Plot <- qplot(Test$x1, Test$y)
   p1<-ggplot(data=Test, aes(x=x1, y=y)) + geom_point(aes(color=LM_Contains)) + geom_line(aes(x=x1, y=LM_PI[,1]), color="red") + ylim(2*min(Test$y),2*max(Test$y) )
   p1<-p1+geom_ribbon(aes(ymin=LM_PI[,2], ymax=LM_PI[,3]), linetype=2, alpha=0.5) + ggtitle("Assumes Lin., Norm., CV") + theme(legend.position = "none")
   
   p2<-ggplot(data=Test, aes(x=x1, y=y)) + geom_point(aes(color=RF2_Contains)) + geom_line(aes(x=x1, y=RF_PI2[,1]), color="red")+ ylim(2*min(Test$y),2*max(Test$y) )
   p2<-p2+geom_ribbon(aes(ymin=RF_PI2[,2], ymax=RF_PI2[,3]), linetype=2, alpha=0.5) + ggtitle("Assumes Norm., CV") + theme(legend.position = "none")
   
   p3<-ggplot(data=Test, aes(x=x1, y=y)) + geom_point(aes(color=RF_Contains)) + geom_line(aes(x=x1, y=RF_PI[,1]), color="red")+ ylim(2*min(Test$y),2*max(Test$y) )
   p3<-p3+geom_ribbon(aes(ymin=RF_PI[,2], ymax=RF_PI[,3]), linetype=2, alpha=0.5)  + ggtitle("Assumes CV") + theme(legend.position = "none")
   
   p4<-ggplot(data=Test, aes(x=x1, y=y)) + geom_point(aes(color=QFE_Contains)) + geom_line(aes(x=x1, y=QFE_PI[,1]), color="red")+ ylim(2*min(Test$y),2*max(Test$y) )
   p4<-p4+geom_ribbon(aes(ymin=QFE_PI[,2], ymax=QFE_PI[,3]), linetype=2, alpha=0.5)  + ggtitle("Assumes None of 3") + theme(legend.position = "none")
   
   Plots <- grid.arrange(p1, p2, p3, p4, ncol=2)
   return(list(Plots, Res_Table))
   }
  
  SimulationRes <- reactive({Simulation(input$a,input$b,input$c)})
  output$plot <- renderPlot(SimulationRes()[[1]])
  output$table <- renderTable(SimulationRes()[[2]])
}



# Run the application 
shinyApp(ui = ui, server = server)
