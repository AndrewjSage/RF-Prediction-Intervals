#
# simulation app
# Shiny App Created by Andrew Sage

# Load packages
library(shiny)
library(tidyverse)
library(randomForest)
library(gridExtra)
library(forestError)
library(shinyWidgets)


# Define UI ----
ui <- fluidPage(
  titlePanel("Prediction Intervals via Linear Model and Random Forest"),
  
  fluidRow(
    column(3, 
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
           
           # column(4,
           sliderTextInput(
             inputId = "N",
             label = "Violation of Normality Assumption",
             grid = TRUE,
             force_edges = TRUE,
             choices = c("None",
                         "Slight", "Moderate",
                         "Large", "Very Large")
           ),
           
           #column(4,
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
    column(2, 
           
           checkboxGroupInput("LMSettings", h5("Linear Model Settings"), 
                              choices = list("Include Quadratic Term" = "Quad",
                                             "Include Cubic Term" = "Cubic", 
                                             "Use Log(Y) Transformation (negative Y values will be ignored)" = "Log"
                              ),
           ),           
           
           
           sliderInput("level", "Desired Coverage Level:",
                       min = 0.7, max = 0.95, step=0.05,
                       value = 0.9),
           sliderInput("range", "Range:",
                       min = -1, max = 1, step=0.05,
                       value = c(-1,1))
    ), 
    column(3,
           checkboxGroupInput("Estimate", h5("Display"), 
                              choices = list("True Expected Response" = "True",
                                             "Linear Model (LM) Estimate" = "LMest", 
                                             "Random Forest (RF) Estimate" = "RFest" 
                              ),
                              selected = "1"),
           checkboxGroupInput("Assumptions", h5("Prediction Interval Assumptions"), 
                              choices = list("LM - least flexible -assumes lin., norm., C.V." = "All", 
                                             "RF - moderately flexible - assumes sym., C.V." = "Some", 
                                             "RF - most flexible -assumes none of these" = "None"),
                              selected = 1)
    ),
    column(3,
           #checkboxGroupInput("LinViol", h5("Type of Linearity Violation"), 
          #                    choices = list("Exponential Trend" = "Exp",
          #                                   "Polynomial Trend" = "Poly", 
          #                                   "Discontinuity" = "Disc", 
          #                                   "Unknown" = "Unknown"
          #                    ),
          # ),
           radioButtons(inputId="LinViol", label="Type of Linearity Violation", 
                        list("Exponential Trend" = "Exp",
                             "Polynomial Trend" = "Poly", 
                             "Discontinuity" = "Disc", 
                             "Unknown" = "Unknown"
                        ),
           )
           
           ),
    
    column(2,
           actionButton("Regenerate", "Regenerate Data"),
           downloadButton("downloadData", "Download Data"),
           selectInput("data", h5("Display"), 
                       choices = list("Training Data" = "Train", 
                                      "Test Data" = "Test"),
                       selected = "Train"),
           
    )
  ),
  fluidRow(
    # Show a plot of the generated distribution
    #  mainPanel(
    column(8,
           plotOutput("plot"),
           textOutput("References")
    ),
    column(4,
           tableOutput("RMSPEtable"),
           tableOutput("PItable")
    )
    
  )
)
# Define server logic required to draw a histogram
server <- function(input, output){
  
  
  
  Simulation <- function(L,N,C, action, LinViol){
    
    s1 <-1  
    s2 <-1  
    s3 <-1  
    
    a <- s1*ifelse(L=="None",0, ifelse(L=="Slight", 1, 
                                       ifelse(L=="Moderate", 2, ifelse(L=="Large", 3, 4))))
    b <-  s2*ifelse(N=="None",0, ifelse(N=="Slight", 0.25, 
                                        ifelse(N=="Moderate", 0.5, ifelse(N=="Large",0.75,1))))
    c <-  s3*ifelse(C=="None",0, ifelse(C=="Slight", 1, 
                                        ifelse(C=="Moderate", 2, ifelse(C=="Large", 3, 4))))
    
    a1 <- ifelse("Exp" %in% LinViol, 1, 0)
    a2 <- ifelse("Poly" %in% LinViol, 1, 0)
    a3 <- ifelse("Disc" %in% LinViol, 1, 0)
    if("Unknown" %in% LinViol) {a1 <- a2 <- a3 <- 1}
    
    ntrain <- 1000
    ntest <- 1000
    N <- ntrain + ntest
    ns <- 30
    
    
    ep1 <- -1
    ep2 <- 1
    x1 <- seq(from=ep1, to=ep2, by=(ep2-ep1)/(ntrain+ntest))
    x1 <- x1[sample(1:length(x1))]
    x1 <- x1[1:(ntrain+ntest)]
    meanfunc <- function(x1){
      #mx <- a1*5*x1 + s2*a*x1^2 + a2*a*(x1-0.3)^5*(s1*a>1.5) + a3*a*(x1>0)*(s1*a>2.5) - a3*a*(abs(x1)<0.5)*(s1*a>3.5) + 20*a*(a1>0)*exp(1.1*x1)
      
      mx <- 10*(a1>0)*exp(0.2*a*x1) + a2*a*5*x1 + s2*a2*a*x1^2 + a2*a*(x1-0.3)^5*(s1*a>1.5) + 50*(a2>0 | a3>0 | a1 !=0) + a3*a*(x1>0)*(s1*a>2.5) - a3*a*(abs(x1)<0.5)*(s1*a>3.5)
      return(mx)
    }
    mx <- meanfunc(x1)
    e1 <- rnorm(ntrain + ntest, 0, 1) +  c*rnorm(ntrain + ntest, 0, abs(x1))
    e2 <- (c*abs(x1)/4+1)*(rexp(ntrain + ntest, rate=1) - 1)
    e <- (1-b)*e1 + b*e2 # convex combination of errors
    y <- mx + e  #e + e*(c*(abs(x1)))
    
    
    Data <- data.frame(x1, y)
    Data$mx <- mx
    Train <- Data[1:ntrain, ]
    Test <- Data[(ntrain+1):(ntrain+ntest), ]
    Train$Datatype <- "Train"
    Test$Datatype <- "Test"
    Test$EY <- meanfunc(Test$x1)
    Train$EY <- meanfunc(Train$x1)
    
    Dataset <- rbind(Train, Test)
    Trainx <- data.frame(Train$x1)
    names(Trainx) <- "x1"
    Testx <- data.frame(Test$x1)
    names(Testx) <- "x1"
    RF <- randomForest(x=Trainx, y=Train$y, nodesize=ns, method="forest", keep.inbag = TRUE)
    
    return(list(Dataset, RF, action))
  }
  
  
  CalcPreds <- function(Dataset, RF, level, LMSettings){
    # Intervals from Linear Model
    # Assume linearity, normality, constant variance
    alpha = 1-level
    Train <- Dataset %>% filter(Datatype=="Train")
    Test <- Dataset %>% filter(Datatype=="Test")
    LM <- lm(data=Train, y~x1)
    if("Quad"%in%LMSettings){
      LM <- lm(data=Train, y~x1 + I(x1^2))
    }
    if("Cubic"%in%LMSettings){
      LM <- lm(data=Train, y~x1 + I(x1^2) + I(x1^3))
    }
    LMPred <- predict(LM, newdata=Test)
    RMSPE_LM <- sqrt(mean((LMPred-Test$y)^2))
    LM_PI_Test <- predict(LM, newdata=Test, interval="prediction", level=1-alpha) 
    if("Log"%in%LMSettings){
      LM <- lm(data=Train, log(y)~x1)
      if("Quad"%in%LMSettings){
        LM <- lm(data=Train, log(y)~x1 + I(x1^2))
      }
      if("Cubic"%in%LMSettings){
        LM <- lm(data=Train, log(y)~x1 + I(x1^2) + I(x1^3))
      }
      LMPred <- exp(predict(LM, newdata=Test))
      RMSPE_LM <- sqrt(mean(LMPred-Test$y)^2)
      LM_PI_Test <- exp(predict(LM, newdata=Test, interval="prediction", level=1-alpha)) 
    }
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
    if("Log"%in%LMSettings){
      Train$LMPred <- exp(predict(LM, newdata=Train)) 
      LM_PI_Train <- exp(predict(LM, newdata=Train, interval="prediction", level=1-alpha)) 
      Train$LMLwr <- LM_PI_Train[,2] 
      Train$LMUpr <- LM_PI_Train[,3]}
    
    Trainx <- data.frame(Train$x1)
    names(Trainx) <- "x1"
    Testx <- data.frame(Test$x1)
    names(Testx) <- "x1"
    
    
    RFPred <- predict(object=RF, newdata=Testx)
    RMSPE_RF <- sqrt(mean((RFPred - Test$y)^2))
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
    
    Dataset <- rbind(Train, Test)
    #return(list(Dataset, Res_Table))
    return(Dataset)
  }
  
  CreatePlot <- function(Dataset, data, Estimate, Assumptions, range){  
    if("Test"%in%data){Dataset <- Dataset %>% filter(Datatype=="Test")}
    if("Train"%in%data){Dataset <- Dataset %>% filter(Datatype=="Train")}
    # colors <- c("True" = "black", "LMest" = "green", "RFest" = "blue")
    Dataset <- Dataset %>% filter(x1 >= range[1] & x1 <= range[2]) 
    p <- ggplot(data=Dataset, aes(x=x1, y=y)) + geom_point(color="orange") + xlab("Predictor Variable (x)") + ylab("Response Variable (y)") + theme_bw() #+ theme(legend.position = "none") #+ ylim(2*min(Dataset$y),2*max(Dataset$y)) + theme_bw()
    p <- if("True"%in% Estimate){p+geom_line(aes(x=x1, y=mx,color="red"), size=4)}else{p}
    p <- if("RFest"%in% Estimate){p+geom_line(aes(x=x1, y=RFPred, color="blue"), size=1)}else{p}
    p <- if("LMest"%in% Estimate){p+geom_line(aes(x=x1, y=LMPred, color="green"), size=1)}else{p}
    p <- if("All" %in% Assumptions){p + geom_ribbon(aes(ymin=LMLwr, ymax=LMUpr), linetype=2, alpha=0.5, color="grey", fill="grey")}else{p} 
    p <- if("Some" %in% Assumptions){p + geom_ribbon(aes(ymin=RF2Lwr, ymax=RF2Upr), linetype=2, alpha=0.3, color="blue", fill="blue")}else{p}
    p <- if("None" %in% Assumptions){p + geom_ribbon(aes(ymin=QFELwr, ymax=QFEUpr), linetype=2, alpha=0.5,  color="purple", fill="purple")}else{p}
    p <- p + scale_color_identity(name = "Estimate",  breaks = c("red", "green", "blue"), labels = c("True Expected Response", "Linear Model Estimate", "Random Forest Estimate"),  guide = "legend") #+ 
    #     scale_color_identity(name = "Interval Method",  breaks = c("grey", "blue", "purple"), labels = c("Linear Model", "Random Forest with Assumptions", "Random Forest without Assumtions"),  guide = "legend")
    
    return(p)
  }   
  
  
  
  
  
  CalculateRMSPE <- function(Dataset, range){
    Dataset <- Dataset %>% filter(Datatype=="Test")
    Dataset <- Dataset %>% filter(x1 >= range[1] & x1 <= range[2]) 
    RMSPE_LM <- sqrt(mean((Dataset$LMPred-Dataset$y)^2))
    RMSPE_RF <- sqrt(mean((Dataset$RFPred-Dataset$y)^2))
    Method <- c("Linear Model", "Random Forest")
    RMSPE <- c(RMSPE_LM, RMSPE_RF)
    RMSPEdf <- data.frame(Method, RMSPE)
    names(RMSPEdf)[2] <- "Test_Data_RMSPE"
    RMSPE_Table <- (RMSPEdf)
    return(c(RMSPE_Table))
  } 
  
  
  EvaluatePI <- function(Dataset, range){
    Dataset <- Dataset %>% filter(Datatype=="Test")
    Dataset <- Dataset %>% filter(x1 >= range[1] & x1 <= range[2]) 
    LM_Coverage <- mean((Dataset$y >= Dataset$LMLwr) & (Dataset$y <= Dataset$LMUpr))
    RF_Coverage <- mean((Dataset$y >= Dataset$RF2Lwr) & (Dataset$y <= Dataset$RF2Upr))
    QFE_Coverage <- mean((Dataset$y >= Dataset$QFELwr) & (Dataset$y <= Dataset$QFEUpr))
    LM_Width <- mean(Dataset$LMUpr-Dataset$LMLwr)
    RF_Width <- mean(Dataset$RFUpr-Dataset$RFLwr)
    QFE_Width <- mean(Dataset$QFEUpr-Dataset$QFELwr)
    Coverage <- c(LM_Coverage, RF_Coverage, QFE_Coverage)
    Mean_Width <- c(LM_Width, RF_Width, QFE_Width)
    Interval_Method <- c("LM - least flexible - assumes lin., norm., C.V., ", "RF - moderately flexible- assumes sym., and C.V. ", "RF - most flexible - assumes none of these")
    PI_Table <- (data.frame(Interval_Method, Coverage, Mean_Width))
    return(c(PI_Table))
  }
  
  SimulationRes <- reactive({Simulation(L=input$L,N=input$N,C=input$C, action=input$Regenerate, LinViol=input$LinViol)})
  Preds <- reactive({CalcPreds(Dataset=SimulationRes()[[1]], RF=SimulationRes()[[2]], level=input$level, LMSettings = input$LMSettings)})
  Plot <- reactive({CreatePlot(Dataset=Preds(), data=input$data, Estimate=input$Estimate, Assumptions=input$Assumptions, range=input$range)})
  output$plot <- renderPlot(Plot())
  #MSPE <- reactive(CalculateMSPE(Dataset=SimulationRes(), range=input$range))
  output$RMSPEtable <- renderTable(CalculateRMSPE(Dataset=Preds(), range=input$range))
  output$PItable <- renderTable(EvaluatePI(Dataset=Preds(), range=input$range))
  #output$text <- renderText(paste("Linear Model MSPE=",MSPE()[[1]], "Random Forest MSPE=", MSPE()[[2]]))
  #output$table <- renderTable(SimulationRes()[[2]])
  
  
  output$References <- renderText({"For more information on the random forest prediction interval methods, see: \n Zhang, H., Zimmerman, J., Nettleton, D., & Nordman, D. J. (2019)., 'Random Forest Prediction Intervals'. The American Statistician, and \n Lu, B., & Hardin, J. (2021). 'A Unified Framework for Random Forest Prediction Error Estimation.' J. Mach. Learn. Res., 22, 8-1."})
  
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function(){"Dataset.csv"}, 
    content = function(file) {
      write.csv(SimulationRes()[[1]][,c(1,2,4,5)], file, row.names = FALSE)
    }
  )
  
  
  
  
  
  
  
  
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)