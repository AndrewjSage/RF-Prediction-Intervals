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

# Boston Housing Data
x <- read.delim("https://raw.githubusercontent.com/haozhestat/RFIntervals/master/DataAnalysis/data/nipsdata/Boston/x.txt", header=FALSE, sep=" ")
y <- read.delim("https://raw.githubusercontent.com/haozhestat/RFIntervals/master/DataAnalysis/data/nipsdata/Boston/y.txt", header=FALSE, sep=" ")
#Insurance
#x <- read.delim("https://raw.githubusercontent.com/haozhestat/RFIntervals/master/DataAnalysis/data/nipsdata/Insur/x.txt", header=FALSE, sep=" ")
#y <- read.delim("https://raw.githubusercontent.com/haozhestat/RFIntervals/master/DataAnalysis/data/nipsdata/Insur/y.txt", header=FALSE, sep=" ")
#Attend
#x <- read.delim("https://raw.githubusercontent.com/haozhestat/RFIntervals/master/DataAnalysis/data/nipsdata/Attend/x.txt", header=FALSE, sep=" ")
#y <- read.delim("https://raw.githubusercontent.com/haozhestat/RFIntervals/master/DataAnalysis/data/nipsdata/Attend/y.txt", header=FALSE, sep=" ")
#Baseball
#x <- read.delim("https://raw.githubusercontent.com/haozhestat/RFIntervals/master/DataAnalysis/data/nipsdata/Baseball/x.txt", header=FALSE, sep=" ")
#y <- read.delim("https://raw.githubusercontent.com/haozhestat/RFIntervals/master/DataAnalysis/data/nipsdata/Baseball/y.txt", header=FALSE, sep=" ")
#Baseball
#x <- read.delim("https://raw.githubusercontent.com/haozhestat/RFIntervals/master/DataAnalysis/data/nipsdata/Budget/x.txt", header=FALSE, sep=" ")
#y <- read.delim("https://raw.githubusercontent.com/haozhestat/RFIntervals/master/DataAnalysis/data/nipsdata/Budget/y.txt", header=FALSE, sep=" ")



names(y) <- "y"
Data <- cbind(x,y)

# Define UI ----
ui <- fluidPage(
  titlePanel("Random Forest Prediction Intervals"),
  
  fluidRow(
    column(3, 
           selectInput(inputId="Disp", label="Display", choices = c("Linear Model Diagnostics", "Prediction Intervals", "Coverage Rates"))),
    column(3,
           varSelectInput("var", "Variable to Display", Data))
),
 
fluidRow(
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("plot")
    #tableOutput("table")
  )  
)

)

# Define server logic required to draw a histogram
server <- function(input, output){
  #var <- input$var
  
  Simulation <- function(var, Disp){
   
    varnum <- which(names(Data)==var)
    Means <- data.frame(t(apply(Data, 2, mean, na.rm=TRUE)))
    New <- Means %>% slice(rep(1:1000, each = 1000))
    New[,varnum] <- seq(min(Data[,varnum]), max(Data[,varnum]), by=(max(Data[,varnum])-min(Data[,varnum]))/(1000-1))
    
    #samp <- sample(1:nrow(Data), floor(nrow(Data)/2), replace=FALSE)
    #Train <- Data[samp, ]
    #Test <- Data[-samp, ]
    
    Train <- Data
    Test <- New
    
    alpha=0.05
    ns <- 10
    
    
   # Intervals from Linear Model
   # Assume linearity, normality, constant variance
   LM <- lm(data=Train, y~.)
   LMpred <- predict(LM, newdata=Test) 
   LM_PI <- predict(LM, newdata=Test, interval="prediction", level=1-alpha) 
   LM_PI <- data.frame(LM_PI)
   names(LM_PI) <- c("LM_fit", "LM_lwr", "LM_upr")
   
   
   Trainx <- Train %>% select(-c(y))
   Testx <- Test %>% select(-c(y))

   
   # Intervals from Random Forest - Sym. OOB method
   # Assumes constant variance, error distribution symmetric
   RF <- randomForest(x=Trainx, y=Train$y, nodesize=ns, method="forest", keep.inbag = TRUE)
   RFPred <- predict(object=RF, newdata=Testx)
   
   OOB_resid <- Train$y - RF$predicted
   MOE <- quantile(abs(OOB_resid), 1-alpha)
   RF_PI <- data.frame(RFPred, RFPred-MOE, RFPred+MOE)
   names(RF_PI) <- c("RF_fit", "RF_lwr", "RF_upr")
   


   QFE <- quantForestError(forest=RF, X.train=Trainx, X.test=Testx, Y.train = Test$y,  alpha = alpha)
   QFE_PI <- data.frame(QFE$estimates[,1], QFE$estimates[,4], QFE$estimates[,5])
   names(QFE_PI) <- c("QFE_fit", "QFE_lwr", "QFE_upr")
   
   
LM_PI <- data.frame(LM_PI)   
LM_PI$x <- Test[,varnum]      
RF_PI$x <- Test[,varnum]      
QFE_PI$x <- Test[,varnum]      
#names(RF_PI) <- names(QFE_PI) <- names(LM_PI)

Prediction_Dataset <- cbind(LM_PI[,c(4,1:3)], RF_PI[,-4], QFE_PI[,-4])
  

CreatePlot <- function(Prediction_Dataset, data, Estimate, Assumptions, range){  
  #Dataset <- Dataset %>% filter(x1 >= range[1] & x1 <= range[2]) 
  p <- ggplot(data=Prediction_Dataset, aes(x=x, y=LM_fit)) + xlab("Explanatory Variable (x)") + ylab("Response Variable (y)") + theme_bw() 
  p <- if("RFest"%in% Estimate){p+geom_line(aes(x=x, y=RF_fit, color="blue"), size=1)}else{p}
  p <- if("LMest"%in% Estimate){p+geom_line(aes(x=x, y=LM_fit, color="green"), size=1)}else{p}
  p <- if("All" %in% Assumptions){p + geom_ribbon(aes(ymin=LM_lwr, ymax=LM_upr), linetype=2, alpha=0.5, color="grey", fill="grey")}else{p} 
  p <- if("Some" %in% Assumptions){p + geom_ribbon(aes(ymin=RF_lwr, ymax=RF_upr), linetype=2, alpha=0.3, color="blue", fill="blue")}else{p}
  p <- if("None" %in% Assumptions){p + geom_ribbon(aes(ymin=QFE_lwr, ymax=QFE_upr), linetype=2, alpha=0.5,  color="purple", fill="purple")}else{p}
  p <- p + scale_color_identity(name = "Estimate",  breaks = c("green", "blue"), labels = c("Linear Model Estimate", "Random Forest Estimate"),  guide = "legend") #+ 
  #     scale_color_identity(name = "Interval Method",  breaks = c("grey", "blue", "purple"), labels = c("Linear Model", "Random Forest with Assumptions", "Random Forest without Assumtions"),  guide = "legend")
  
  return(p)
}   


   LM_Res <- data.frame(Train$y)
   names(LM_Res) <- c("y")
   LM_Res$Predicted <- LM$fitted.values
   LM_Res$Residual <- LM_Res$y - LM_Res$Predicted
   LM_Res$x <- Train[,varnum]
   ResidPlot <- ggplot(data=LM_Res, aes(x=Predicted, y=Residual)) + geom_point()
   ResidHist <- ggplot(data=LM_Res, aes(x=Residual)) + geom_histogram()
   QQPlot <- ggplot(data=LM_Res, aes(sample = Residual)) + stat_qq() + stat_qq_line() + xlab("Normal Quantiles") + ylab("Residual Quantiles") + ggtitle("QQ Plot")
  ResidvsPred <- ggplot(data=LM_Res, aes(x=x, y=y)) + geom_point() + stat_smooth()
  
   #DiagnosticPlots <-  grid.arrange(ResidPlot, ResidHist, QQPlot, ncol=3)
   #PIPlots <- grid.arrange(p1, p2, p3, p4, ncol=2)
   #Plot <- if(Disp == "Linear Model Diagnostics"){DiagnosticPlots}else{PIPlots}
   #Text <- Disp
  
   Plot <- if(Disp == "Linear Model Diagnostics")
   {grid.arrange(ResidPlot, ResidHist, QQPlot, ResidvsPred, ncol=4)}else{p}
#   Table <- Res_Table
   
   
   return(list(Plot))
   }

  
  SimulationRes <- reactive({Simulation(input$var, input$Disp)})
  #SimulationPlt <- reactive({Plottest(input$Disp)})
  output$plot <- renderPlot(SimulationRes()[[1]])
  output$table <- renderTable(SimulationRes()[[2]])
}



# Run the application 
shinyApp(ui = ui, server = server)
