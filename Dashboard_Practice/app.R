## app.R ##
library(shinydashboard)

x <- read.delim("https://raw.githubusercontent.com/haozhestat/RFIntervals/master/DataAnalysis/data/nipsdata/Boston/x.txt", header=FALSE, sep=" ")
y <- read.delim("https://raw.githubusercontent.com/haozhestat/RFIntervals/master/DataAnalysis/data/nipsdata/Boston/y.txt", header=FALSE, sep=" ")
names(y) <- "y"
Data <- cbind(x,y)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "Data", icon = icon("dashboard")),
      menuItem("Residual Plots", tabName = "ResPlots", icon = icon("th")),
      menuItem("Prediction Intervals", tabName = "PredIntervals", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Data",
              fluidRow(
                box(plotOutput("plot", height = 250)),
                
                box(
                  title = "Controls",
                  checkboxGroupInput("Estimate", h5("Display"), 
                                     choices = list("Linear Model (LM) Estimate" = "LMest", 
                                                    "Random Forest (RF) Estimate" = "RFest" 
                                     ),
                                     selected = "1"),
                  checkboxGroupInput("Assumptions", h5("Prediction Interval Assumptions"), 
                                     choices = list("LM - assumes lin., norm., C.V." = "All", 
                                                    "RF - assumes sym., C.V." = "Some", 
                                                    "RF - assumes none of these" = "None"),
                                     selected = 1), 
                  sliderInput("range", "Range:",
                              min = min(Test$x1), max = max(Test$x1), step=0.05,
                              value = c(0,5)), 
                  varSelectInput("var", "Variable to Display", Trainx), 
                  sliderInput("level", "Desired Coverage Level:",
                              min = 0.7, max = 0.95, step=0.05,
                              value = 0.9)
                  
                )
                
              )
      ),
      
      # Second tab content
      tabItem(tabName = "ResPlots",
              h2("Residual Plots tab content")
      ),
      tabItem(tabName = "PredIntervals",
              h2("Intervals tab content")
      )

    )
  )
)

server <- function(input, output) {
  
  Train <- Data
  Trainx <-x
  ns <- 10
  RF <- randomForest(x=Trainx, y=Train$y, nodesize=ns, method="forest", keep.inbag = TRUE)
  

  
  MakePreds <- function(var, level){
  
    varnum <- which(names(Train)==var)
    Means <- data.frame(t(apply(Data, 2, mean, na.rm=TRUE)))
    New <- Means %>% slice(rep(1:1000, each = 1000))
    New[,varnum] <- seq(min(Data[,varnum]), max(Data[,varnum]), by=(max(Data[,varnum])-min(Data[,varnum]))/(1000-1))
    Train <- Data
    Test <- New
    Testx <- New %>% select(-y)
    
      
  alpha = 1-level
  LM <- lm(data=Train, y~.)
  LMPred <- predict(LM, newdata=Test) 
  LM_PI_Test <- predict(LM, newdata=Test, interval="prediction", level=1-alpha) 
  LM_PI_Test <- data.frame(LM_PI_Test)
#  Width_LM <- mean(LM_PI_Test[,3]-LM_PI_Test[,2])
  LM_PI_Test$x1 <- Test$x1
  Test$LMPred <- LMPred
  Test$LMLwr <- LM_PI_Test[,2] 
  Test$LMUpr <- LM_PI_Test[,3]
 
  RFPred <- predict(object=RF, newdata=Testx)
  # Intervals from Random Forest - Sym. OOB method
  # Assumes constant variance, error distribution symmetric
  OOB_resid <- Train$y - RF$predicted
  MOE <- quantile(abs(OOB_resid), 1-alpha)
  RF_PI2_Test <- data.frame(RFPred, RFPred-MOE, RFPred+MOE)
  Width_RFOOB_Sym <- mean(RF_PI2_Test[,3]-RF_PI2_Test[,2])
  RF_PI2_Test$x1 <- Test$x1
  Test$RFPred <- RFPred
  Test$RF2Lwr <- RF_PI2_Test[,2] 
  Test$RF2Upr <- RF_PI2_Test[,3]
  QFE <- quantForestError(forest=RF, X.train=Trainx, X.test=Testx, Y.train = Train$y,  alpha = alpha)
  Test$QFEPred <- QFE$estimates[,1]
  Test$QFELwr <- QFE$estimates[,4]
  Test$QFEUpr <- QFE$estimates[,5]
  QFE_PI <- data.frame(Test$QFEPred, Test$QFELwr, Test$QFEUpr)
  Width_QFE <- mean(QFE_PI[,3]-QFE_PI[,2])
  QFE_PI$x1 <- Test$x1
  
  Test$x1 <- Test[,varnum]   
  return(Test)
  }

  CreatePlot <- function(Test, Estimate, Assumptions, range){  
    Dataset <- Test %>% filter(x1 >= range[1] & x1 <= range[2]) 
    p <- ggplot(data=Dataset, aes(x=x1, y=y)) + xlab("Explanatory Variable (x)") + ylab("Response Variable (y)") + theme_bw() #+ theme(legend.position = "none") #+ ylim(2*min(Dataset$y),2*max(Dataset$y)) + theme_bw()
    p <- if("RFest"%in% Estimate){p+geom_line(aes(x=x1, y=RFPred, color="blue"), size=1)}else{p}
    p <- if("LMest"%in% Estimate){p+geom_line(aes(x=x1, y=LMPred, color="green"), size=1)}else{p}
    p <- if("All" %in% Assumptions){p + geom_ribbon(aes(ymin=LMLwr, ymax=LMUpr), linetype=2, alpha=0.5, color="grey", fill="grey")}else{p} 
    p <- if("Some" %in% Assumptions){p + geom_ribbon(aes(ymin=RF2Lwr, ymax=RF2Upr), linetype=2, alpha=0.3, color="blue", fill="blue")}else{p}
    p <- if("None" %in% Assumptions){p + geom_ribbon(aes(ymin=QFELwr, ymax=QFEUpr), linetype=2, alpha=0.5,  color="purple", fill="purple")}else{p}
    p <- p + scale_color_identity(name = "Estimate",  breaks = c("green", "blue"), labels = c("Linear Model Estimate", "Random Forest Estimate"),  guide = "legend") #+ 
    #     scale_color_identity(name = "Interval Method",  breaks = c("grey", "blue", "purple"), labels = c("Linear Model", "Random Forest with Assumptions", "Random Forest without Assumtions"),  guide = "legend")
    return(p)
  }   
  
  Preds <- reactive({MakePreds(var=input$var, level=input$level)})
  Plot <- reactive({CreatePlot(Test=Preds(), Estimate=input$Estimate, Assumptions=input$Assumptions, range=input$range)})
  output$plot <- renderPlot(Plot())
    
}

shinyApp(ui, server)