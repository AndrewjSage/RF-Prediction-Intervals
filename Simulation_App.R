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
library(randomForest)
library(gridExtra)
library(forestError)
library(shinyWidgets)


# Define UI ----
ui <- fluidPage(
  titlePanel("Random Forest Prediction Intervals"),
  
  fluidRow(
    column(6, 
           #sliderInput("a", h5("Linearity"),
           #            min = 0, max = 5, value = 0, step=0.1, ticks=FALSE),
           sliderTextInput(
             inputId = "L",
             label = "Violation of Linearity Assumption",
             grid = TRUE,
             force_edges = TRUE,
             choices = c("None",
                         "Slight", "Moderate",
                         "Large", "Very Large")
           ),
           sliderTextInput(
             inputId = "N",
             label = "Violation of Normality Assumption",
             grid = TRUE,
             force_edges = TRUE,
             choices = c("None",
                         "Slight", "Moderate",
                         "Large", "Very Large")
           ),
           sliderTextInput(
             inputId = "C",
             label = "Violation of Constant Variance Assumption",
             grid = TRUE,
             force_edges = TRUE,
             choices = c("None",
                         "Slight", "Moderate",
                         "Large", "Very Large")
           )
    ),
          # sliderInput("b", h5("Normality/Symmetry"),
          #             min = 0, max = 1, value = 0, ticks=FALSE),

          #sliderInput("c", h5("Constant Variance"),
          #            min = 0, max = 5, value = 0, ticks=FALSE)),
    column(6, 
           selectInput("data", h3("Display"), 
                       choices = list("Training Data and Fitted Response" = "Train", 
                                      "Test Data and Prediction Intervals " = "Test"),
                       selected = "Train"),
             checkboxGroupInput("Assumptions", h3("Prediction Interval Assumptions"), 
             choices = list("Linearity, Normality, and Constant Variance, " = "All", 
                            "Symmetry and Constant Variance" = "Some", 
                            "None" = "None"),
             selected = 1),
           sliderInput("range", "Range:",
                       min = -1, max = 1, step=0.05,
                       value = c(-1,1))
    )
    

  ),
  
fluidRow(
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("plot"),
    tableOutput("table")
   # textOutput("text")
  )  
)

)

# Define server logic required to draw a histogram
server <- function(input, output){

  Simulation <- function(L,N,C){
   
  a <- ifelse(L=="None",0, ifelse(L=="Slight", 2, 
                               ifelse(L=="Moderate", 4, ifelse(L=="Large", 6, 8))))
  b <-  ifelse(N=="None",0, ifelse(N=="Slight", 0.25, 
                                 ifelse(N=="Moderate", 0.5, ifelse(N=="Large",0.75,1))))
  c <-  ifelse(C=="None",0, ifelse(C=="Slight", 2, 
                                 ifelse(C=="Moderate", 4, ifelse(C=="Large", 6, 8))))
    
   ntrain <- 100
   ntest <- 100
   alpha <- 0.1
   N <- ntrain + ntest
   ns <- min(ntrain/10, 10)
   
   
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
   meanfunc <- function(x1){
     mx <- x1 + a*x1^2 + a*(x1^3)*(a>1.5)
     mx <- scale(mx) + a*(x1>0)*(a>3) 
     return(mx)
   }
   mx <- meanfunc(x1)
   e1 <- rnorm(ntrain + ntest, 0, 1) +  c*rnorm(ntrain + ntest, 0, abs(x1))
   e2 <- rexp(ntrain + ntest, rate=1/5) - 5
   e <- (1-b)*e1 + b*e2 # convex combination of errors
    y <- mx + e  #e + e*(c*(abs(x1))) 

     
   Data <- data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, y)
   Data$mx <- mx
   Train <- Data[1:ntrain, ]
   Test <- Data[(ntrain+1):(ntrain+ntest), ]
   Train$Datatype <- "Train"
   Test$Datatype <- "Test"
   Test$EY <- meanfunc(Test$x1)
   Train$EY <- meanfunc(Train$x1)

   Trainx <- Train[,1:10]
   y <- Train$y
   Testx <- Test[,1:10]
   y <- Test$y   
   
   
   # Intervals from Linear Model
   # Assume linearity, normality, constant variance
   LM <- lm(data=Train, y~x1)
   LMPred <- predict(LM, newdata=Test) 
   MSPE_LM <- mean((LMPred-Test$y)^2)
   LM_PI_Test <- predict(LM, newdata=Test, interval="prediction", level=1-alpha) 
   LM_PI_Test <- data.frame(LM_PI_Test)
   LM_Contains <- (Test$y >= LM_PI_Test[,2]) & (Test$y <= LM_PI_Test[,3])
   Cov_LM <- mean(LM_Contains)
   Width_LM <- mean(LM_PI_Test[,3]-LM_PI_Test[,2])
   LM_PI_Test$x1 <- Test$x1
   Test$LMPred <- LMPred
   Test$LMLwr <- LM_PI_Test[,2] 
   Test$LMUpr <- LM_PI_Test[,3]
   Train$LMPred <- predict(LM, newdata=Train) 
   LM_PI_Train <- predict(LM, newdata=Train, interval="prediction", level=1-alpha) 
   Train$LMLwr <- LM_PI_Train[,2] 
   Train$LMUpr <- LM_PI_Train[,3]
  
   Trainx <- data.frame(Trainx$x1)
   names(Trainx) <- "x1"
   Testx <- data.frame(Testx$x1)
   names(Testx) <- "x1"
   
   
   RF <- randomForest(data=Train, x=Trainx, y=Train$y, nodesize=ns, method="forest", keep.inbag = TRUE)
   RFPred <- predict(object=RF, newdata=Testx)
   MSPE_RF <- mean((RFPred - Test$y)^2)
   # Intervals from Random Forest - Sym. OOB method
   # Assumes constant variance, error distribution symmetric
   OOB_resid <- Train$y - RF$predicted
   MOE <- quantile(abs(OOB_resid), 1-alpha)
   RF_PI2_Test <- data.frame(RFPred, RFPred-MOE, RFPred+MOE)
   RF2_Contains <- (Test$y >= RF_PI2_Test[,2]) & (Test$y <= RF_PI2_Test[,3])
   Coverage_RFOOB_Sym <- mean(RF2_Contains)
   Width_RFOOB_Sym <- mean(RF_PI2_Test[,3]-RF_PI2_Test[,2])
   RF_PI2_Test$x1 <- Test$x1
   Test$RFPred <- RFPred
   Test$RF2Lwr <- RF_PI2_Test[,2] 
   Test$RF2Upr <- RF_PI2_Test[,3]
   Train$RFPred <- predict(RF, newdata=Train) 
   Train$RF2Lwr <- Train$RFPred - MOE
   Train$RF2Upr <- Train$RFPred + MOE
   # Intervals from Random Forest - Non. Sym. OOB method
   # Assumes constant variance
   Lower <- quantile(OOB_resid, alpha/2)
   Upper <- quantile(OOB_resid, 1-alpha/2)
   RF_PI <- data.frame(RFPred, RFPred+Lower, RFPred+Upper)
   RF_Contains <- (Test$y >= RF_PI[,2]) & (Test$y <= RF_PI[,3])
   Coverage_RFOOB_NonSym <- mean(RF_Contains)
   Width_RFOOB_NonSym <- mean(RF_PI[,3]-RF_PI[,2])
   RF_PI$x1 <- Test$x1
   Test$RFLwr <- RFPred+Lower
   Test$RFUpr <- RFPred+Upper
   Train$RFLwr <- Train$RFPred + Lower
   Train$RFUpr <- Train$RFPred + Upper
   library(forestError)
   QFE <- quantForestError(forest=RF, X.train=Trainx, X.test=Testx, Y.train = Train$y,  alpha = alpha)
   QFE2 <- quantForestError(forest=RF, X.train=Trainx, X.test=Trainx, Y.train = Train$y,  alpha = alpha)
   Test$QFEPred <- QFE$estimates[,1]
   Test$QFELwr <- QFE$estimates[,4]
   Test$QFEUpr <- QFE$estimates[,5]
   Train$QFEPred <- QFE2$estimates[,1]
   Train$QFELwr <- QFE2$estimates[,4]
   Train$QFEUpr <- QFE2$estimates[,5]
   QFE_PI <- data.frame(Test$QFEPred, Test$QFELwr, Test$QFEUpr)
   QFE_Contains <- (Test$y >= QFE_PI[,2]) & (Test$y <= QFE_PI[,3])
   Coverage_QFE <- mean(QFE_Contains)
   Width_QFE <- mean(QFE_PI[,3]-QFE_PI[,2])
   QFE_PI$x1 <- Test$x1
   
   #Res_LM <- c(MSPE_LM, Cov_LM, Width_LM)
   #Res_RFOOBSym <- c(MSPE_RF, Coverage_RFOOB_Sym, Width_RFOOB_Sym)
   #Res_RFOOBNonSym <- c(MSPE_RF, Coverage_RFOOB_NonSym, Width_RFOOB_NonSym)
   #Res_QFE <- c(MSPE_RF, Coverage_QFE, Width_QFE)
   
   #Res_Table <- t(data.frame(Res_LM, Res_RFOOBSym, Res_RFOOBNonSym, Res_QFE))
   #colnames(Res_Table) <- c("MSPE", "Coverage", "Width")
   #rownames(Res_Table) <- c("Linear Model", "Random Forest with Symmetry Assumption", "Random Forest - No Symmetry Assumption", "Random Forest - No Constant Variance Assumption")

   #Plot <- qplot(Test$x1, Test$y)
  # p1<-ggplot(data=Test, aes(x=x1, y=y)) + geom_point(aes(color=LM_Contains)) + geom_line(aes(x=x1, y=LM_PI[,1]), color="red") + ylim(2*min(Test$y),2*max(Test$y) )
   #p1<-p1+geom_ribbon(aes(ymin=LM_PI[,2], ymax=LM_PI[,3]), linetype=2, alpha=0.5, color="red", fill="lightblue") + ggtitle("Assumes Lin., Norm., CV") + theme(legend.position = "none")
   
   #p2<-ggplot(data=Test, aes(x=x1, y=y)) + geom_point(aes(color=RF2_Contains)) + geom_line(aes(x=x1, y=RF_PI2[,1]), color="red")+ ylim(2*min(Test$y),2*max(Test$y) )
   #p2<-p2+geom_ribbon(aes(ymin=RF_PI2[,2], ymax=RF_PI2[,3]), linetype=2, alpha=0.5) + ggtitle("Assumes Norm., CV") + theme(legend.position = "none")
   
   #p3<-ggplot(data=Test, aes(x=x1, y=y)) + geom_point(aes(color=RF_Contains)) + geom_line(aes(x=x1, y=RF_PI[,1]), color="red")+ ylim(2*min(Test$y),2*max(Test$y) )
   #p3<-p3+geom_ribbon(aes(ymin=RF_PI[,2], ymax=RF_PI[,3]), linetype=2, alpha=0.5)  + ggtitle("Assumes CV") + theme(legend.position = "none")
   
   #p4<-ggplot(data=Test, aes(x=x1, y=y)) + geom_point(aes(color=QFE_Contains)) + geom_line(aes(x=x1, y=QFE_PI[,1]), color="red")+ ylim(2*min(Test$y),2*max(Test$y) )
   #p4<-p4+geom_ribbon(aes(ymin=QFE_PI[,2], ymax=QFE_PI[,3]), linetype=2, alpha=0.5)  + ggtitle("Assumes None of 3") + theme(legend.position = "none")
   
  #p <- p1 + geom_ribbon(aes(ymin=LM_PI[,2], ymax=LM_PI[,3]), linetype=2, alpha=0.5, color="grey", fill="lightgrey") + theme(legend.position = "none") + 
  #          geom_ribbon(aes(ymin=RF_PI2[,2], ymax=RF_PI2[,3]), linetype=2, alpha=0.3, color="blue", fill="lightblue") + theme(legend.position = "none") + 
  #          geom_ribbon(aes(ymin=QFE_PI[,2], ymax=QFE_PI[,3]), linetype=2, alpha=0.1, color="green", fill="lightgreen")  + theme(legend.position = "none")

   #text <- "Norm" %in% input$Assumptions
  # text <- input$data
   Dataset <- rbind(Train, Test)
   #return(list(Dataset, Res_Table))
   return(Dataset)
  }
   
CreatePlot <- function(Dataset, data, Assumptions, range){  
   if("Test"%in%data){Dataset <- Dataset %>% filter(Datatype=="Test")}
   if("Train"%in%data){Dataset <- Dataset %>% filter(Datatype=="Train")}
#   if(length(data)>1){Dataset <- rbind(Train, Test)}
   #if(length(input$data)>1){LM_PI <- rbind(LM_PI, LM_PI); RF_PI2 <- rbind(RF_PI2, RF_PI2); QFE_PI <- rbind(QFE_PI, QFE_PI)}
  Dataset <- Dataset %>% filter(x1 >= range[1] & x1 <= range[2]) 
  p <- ggplot(data=Dataset, aes(x=x1, y=y)) + geom_point(aes(color=Datatype)) + geom_line(aes(x=x1, y=LMPred), color="red") + geom_line(aes(x=x1, y=RFPred), color="blue") +
     geom_line(aes(x=x1, y=mx), color="green") + ylim(2*min(Dataset$y),2*max(Dataset$y)) + theme_bw()
   p <- if("All" %in% Assumptions){p + geom_ribbon(aes(ymin=LMLwr, ymax=LMUpr), linetype=2, alpha=0.5, color="grey", fill="grey")}else{p} 
   p <- if("Some" %in% Assumptions){p + geom_ribbon(aes(ymin=RF2Lwr, ymax=RF2Upr), linetype=2, alpha=0.3, color="blue", fill="blue")}else{p}
   p <- if("None" %in% Assumptions){p + geom_ribbon(aes(ymin=QFELwr, ymax=QFEUpr), linetype=2, alpha=0.5,  color="purple", fill="purple")}else{p}
   p <- p + theme(legend.position = "blank")
  return(p)
   }   
  
CalculateMSPE <- function(Dataset, range){
  Dataset <- Dataset %>% filter(Datatype=="Test")
  Dataset <- Dataset %>% filter(x1 >= range[1] & x1 <= range[2]) 
  MSPE_LM <- mean((Dataset$LMPred-Dataset$y)^2)
  MSPE_RF <- mean((Dataset$RFPred-Dataset$y)^2)
  Method <- c("Linear Model", "Random Forest")
  MSPE <- c(MSPE_LM, MSPE_RF)
  MSPE_Table <- t(data.frame(Method, MSPE))
  return(c(MSPE_Table))
} 

  SimulationRes <- reactive({Simulation(L=input$L,N=input$N,C=input$C)})
  Plot <- reactive({CreatePlot(Dataset=SimulationRes(), data=input$data, Assumptions=input$Assumptions, range=input$range)})
  output$plot <- renderPlot(Plot())
  #MSPE <- reactive(CalculateMSPE(Dataset=SimulationRes(), range=input$range))
  output$table <- renderTable(CalculateMSPE(Dataset=SimulationRes(), range=input$range))
  #output$text <- renderText(paste("Linear Model MSPE=",MSPE()[[1]], "Random Forest MSPE=", MSPE()[[2]]))
  #output$table <- renderTable(SimulationRes()[[2]])
  
  }



# Run the application 
shinyApp(ui = ui, server = server)
