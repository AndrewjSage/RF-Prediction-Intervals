# Real Data App

## app.R ##
library(shinydashboard)
library(randomForest)
library(forestError)
library(datasets)
library(tidyverse)
library(gridExtra)
library(mlbench)
library(broom)
library(stats)


library(AmesHousing)
data("ames_raw")
ames_raw <- ames_raw %>% mutate(Age = `Yr Sold` - `Year Built`, `Bathrooms` = `Full Bath` + 0.5*`Half Bath`, Bedrooms = `Bedroom AbvGr`)
Ames_Housing <- ames_raw %>% select(`Lot Area`, `Overall Qual`, `Age`, `Bathrooms` , Bedrooms , Fireplaces, `Garage Area`, `Wood Deck SF`, 'Mo Sold', `SalePrice`)
Ames_Housing <- Ames_Housing[complete.cases(Ames_Housing),]
#library(mlbench)
#data("BostonHousing")
#Boston_Housing <- BostonHousing %>% select_if(is.numeric)
library(ISLR)
data("Auto")
Auto <- Auto %>% select_if(is.numeric)
data("Carseats")
Carseats <- Carseats %>% select_if(is.numeric)
data("College")
College <- College %>% select_if(is.numeric)
library(Lock5Data)
data("AllCountries")
Countries <- AllCountries %>% select_if(is.numeric) 
Countries <- Countries[complete.cases(Countries),]
data("BodyFat")
Bodyfat <- BodyFat %>% select_if(is.numeric)
data(Cars2020)
Cars_2020 <- Cars2020 %>% select_if(is.numeric)
data("CollegeScores")
College <- CollegeScores %>% select_if(is.numeric) %>% select(AdmitRate, MidACT, AvgSAT, Enrollment, White,
                                                              Black, Hispanic, Asian, Other, Cost, 
                                                              FacSalary, Pell, CompRate, Debt, FirstGen, MedIncome)
College <- College[complete.cases(College),]
data("HollywoodMovies")
Hollywood_Movies <- HollywoodMovies %>% select_if(is.numeric)
Hollywood_Movies <- Hollywood_Movies[complete.cases(Hollywood_Movies), ]
library(AppliedPredictiveModeling)
data(abalone)
Abalone <- abalone %>% select_if(is.numeric)
data(concrete)
Concrete <- concrete %>% select_if(is.numeric)


listOfDataframes <- list(Abalone, Ames_Housing, BodyFat , Cars_2020, Carseats, College, Concrete, Countries,  Hollywood_Movies)
names(listOfDataframes) = c("Abalone",  "Ames_Housing", "Bodyfat" , "Cars_2020" ,"Carseats", "College", "Concrete",  "Countries", "Hollywood_Movies")
packages <- c("AppliedPredictiveModeling", "AmesHousing", "Lock5Data", "Lock5Data", "ISLR", "Lock5Data", "AppliedPredictiveModeling", "Lock5Data", "Lock5Data")
datasets <- c("Abalone",
              "Ames_Housing", 
              "Bodyfat", 
              "Cars_2020",
              "Carseats", 
              "College",
              "Concrete",
              "Countries", 
              "Hollywood_Movies")

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "Data", icon = icon("dashboard")),
      menuItem("Prediction Intervals", tabName = "PredIntervals", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Data",
              fluidRow(
                column(4,
                box( selectInput("dataset", label = "Dataset", choices = c("Abalone" = "Abalone",
                                                                          "Ames Housing" = "Ames_Housing", 
                                                                          "Bodyfat"="Bodyfat", 
                                                                          "Cars_2020"="Cars_2020",
                                                                          "Carseats" = "Carseats", 
                                                                          "College" = "College",
                                                                          "Concrete" = "Concrete",
                                                                          "Countries"="Countries", 
                                                                          "Hollywood_Movies"="Hollywood_Movies")),
                     uiOutput("Resp_Var_Choice"),
                     uiOutput("Exp_Vars_Choice")
                     #selectInput("Resp_Var", "Response Variable", character(0)),
                     #checkboxGroupInput("Exp_Vars", "Explanatory Variables Used in Model:",character(0)), width=10)
              )
              ), 
              column(8,
                  box(title="Summary of Variables in Dataset",
                      selectInput(inputId="displaytype", label = "Display:", choices = c("Raw Data" = "rawdata", 
                                                                            "Summary Table" = "summarytable"),
                                                                              selected="summarytable"),
                      uiOutput("Table_or_Summary"), width=30
                      ), 
                         box(title="Coefficients Table", tableOutput("model_summary"), width=30), 
                  textOutput("Data_Source")
                       )
                )
              ),


      # Fourth tab content
      tabItem(tabName = "PredIntervals",   
              fluidRow(
                column(width=12, 
                       box(title = "Linear Model Residual and Diagnostic Plots", plotOutput("residplot",  width="100%", height="200px"), height=250, width=12)
                )
              ),

              fluidRow(
                box(width=10, title = "Prediction Interval Plot",
                  column(width=12, 
                  fluidRow(plotOutput("predplot", width="100%", height="450px")), 
                  #fluidRow(tableOutput("Trainx")), 
                  #fluidRow(tableOutput("printvarvalues")),
                  #fluidRow(tableOutput("Testx")),
                  
                  
                  fluidRow(
                           column(3,
                                  checkboxGroupInput(inputId = "Estimate", label = "Estimates to Display:", 
                                                     choices = list("Linear Model (LM) Estimate" = "LMest", 
                                                                    "Random Forest (RF) Estimate" = "RFest" 
                                                     ),
                                                     selected = c("LMest", "RFest")),
                                  
                           ),
                           column(3,
                                  checkboxGroupInput(inputId = "Assumptions", label = "Intervals to Display:", 
                                                     choices = list("LM - assumes lin., norm., C.V." = "All", 
                                                                    "RF - assumes sym., C.V." = "Some", 
                                                                    "RF - assumes none of these" = "None"),
                                                     selected = 1) 
                           ),
                           column(3,
                                  uiOutput("varSelection") 
                                 # selectInput("method", h5("method"), 
                                #              choices = list("Linear Model" = "LM", 
                                #                             "Random Forest" = "RF"))
                           ),
                           column(3,
                                  uiOutput("range_slider"),
                                  sliderInput("level", "Desired Coverage Level:",
                                              min = 0.7, max = 0.95, step=0.05,
                                              value = 0.9)
                           ),
                           textOutput("References")
                       )
                  )),

                       box(width=2,
                   #    actionButton("goButton", "Display",icon("cloud-upload")),
                       uiOutput("set_variable_values"), title="Explanatory Variable Values")
                        )
              
                )
      )
)
)




server <- function(input, output, session) {
  
  
  options(scipen = 100)
  
  
  #output$varSelection <- renderUI({
  updatevarlist <- reactive({
    req(input$dataset)
    choices = names(data.frame(get(input$dataset, listOfDataframes))%>%select(c(input$Exp_Vars))) 
    selectInput("var", "Variable to Display", choices = choices)
  })
  
  output$varSelection <- renderUI(updatevarlist())
  
  
  
  var <- reactive({
      return(input$var)
  })
  
  dataset <- reactive({
    req(input$dataset)
    return(data.frame(get(input$dataset, listOfDataframes)))
  })
  
  observeEvent(dataset(), {
    choices <- names(dataset())
    updateCheckboxGroupInput(session, inputId = "Exp_Vars", choices = choices) 
    updateSelectInput(session, inputId = "Resp_Var", choices = choices)
  })
  
  
output$Resp_Var_Choice <- renderUI({
    req(input$dataset)
    dataset <- data.frame(get(input$dataset, listOfDataframes))
    choices <- names(dataset)
    selectInput("Resp_Var", "Response Variable", choices)
  })
  
output$Exp_Vars_Choice <- renderUI({
    req(input$dataset)
    req(input$Resp_Var)
    dataset <- data.frame(get(input$dataset, listOfDataframes))
    choices <- names(dataset %>% select(-c(Resp_Var())))
    checkboxGroupInput(inputId = "Exp_Vars", label="Explanatory Variable(s)", choices = choices) 
  })
  

  Resp_Var <- reactive({
    req(input$dataset)
    req(input$Resp_Var)
    return(input$Resp_Var)
  })
  
Exp_Vars <- reactive({
    #req(input$dataset)
    #req(input$Resp_Var)
    return(input$Exp_Vars)
 })
  
  
  

  output$range_slider <- renderUI({
    req(input$dataset)
    req(input$var)
    var <- input$var
    dataset <- data.frame(get(input$dataset, listOfDataframes))
    varnum <- which(names(dataset)==var)
    Variable <- dataset[,varnum]
      sliderInput("range", "range", min=min(Variable), max=max(Variable), 
                  step=(max(Variable)-min(Variable))/50,
                  #value=c(min(Variable), max(Variable))
                  value=c(quantile(Variable, .25), quantile(Variable, .75))
                  )})
  

  method <- reactive({
    return(input$method)
  })
  
  
  
  level <- reactive({
    return(input$level)
  })
  
  output$method <- renderPrint({print(method())})
  output$summary <- renderPrint({
    # Use a reactive expression by calling it like a function
    summary(dataset())
  })
  
  output$table <- renderDataTable(
    dataset(), options = list("pageLength" = 10)
  )


output$Table_or_Summary <- renderUI({
  if (input$displaytype=="summarytable") {
    verbatimTextOutput("summary") 
  } else {                      
    dataTableOutput("table") 
  }
})




DataDisplay <- function(displaytype){
  dataset <- data.frame(get(input$dataset, listOfDataframes))
  if(displaytype=="table"){dataset()}else{
    summary(dataset())
  }
}

  
  
  FitModels <- function(Dataset, RespVar, ExpVars){
    Train <- Dataset %>% select(c(ExpVars, RespVar))
    #   Train <- Dataset
    Respvarnum <- which(names(Train)==RespVar)  
    #  names(Train)[Respvarnum] <- "y"
    Trainx <- Dataset %>% select(c(ExpVars))
    y <- Train[, Respvarnum]
    ns <- 10
    RF <- randomForest(x=Trainx, y=y, nodesize=ns, method="forest", keep.inbag = TRUE)
    LM <- lm(data=Trainx, y~.)
    return(list(RF, LM, Train, Trainx))
  }
  
  
  ModelResults <- reactive({FitModels(Dataset = dataset(), RespVar = Resp_Var(), ExpVars = Exp_Vars())})
  RF <- reactive({ModelResults()[[1]]})
  LM <- reactive({ModelResults()[[2]]})
  Train <- reactive({ModelResults()[[3]]})
  Trainx <- reactive({ModelResults()[[4]]})
  
  
  output$Trainx <- renderTable(head(Trainx()))
  
  
  
  output$model_summary <- renderTable({
    req(Exp_Vars())
    # Use a reactive expression by calling it like a function
    tidy(summary(LM()))
  })
  
  
  ResidPlots <- function(LM, RF, Dataset, RespVar, method, xvar){
    req(Exp_Vars())
    req(input$var)
    Respvarnum <- which(names(Dataset)==RespVar)
    y <- Dataset[, Respvarnum]
    xvarnum <- which(names(Dataset)==xvar)
    x <- Dataset[, xvarnum]
    if(method=="LM"){
      residplot1 <- ggplot(data=data.frame(LM()$fitted.values,LM()$residuals), aes(x=LM()$fitted.values, y=LM()$residuals)) +
        geom_point() + xlab("Predicted Value") + ylab("Residual") + ggtitle("Residual by Predicted Plot") + theme_bw()  
      residhist1 <- ggplot(data=data.frame(LM()$fitted.values,LM()$residuals), aes(x=LM()$residuals)) + geom_histogram() + ggtitle("Histogram of Residuals") + xlab("Residual") + theme_bw() 
      QQPlot1 <- ggplot(data=data.frame(LM()$fitted.values,LM()$residuals), aes(sample = scale(LM()$residuals))) + stat_qq() + stat_qq_line() + 
        xlab("Normal Quantiles") + ylab("Residual Quantiles") + ggtitle("Normal Quantile-Quantile Plot") + theme_bw() 
      residbypredplot1 <- ggplot(data=data.frame(LM()$fitted.values,LM()$residuals), aes(x=x, y=LM()$residuals)) +
        geom_point() + xlab(paste(xvar)) + ylab("Residual") + ggtitle("Residual by Predictor Variable Plot") + theme_bw() 
      p <- grid.arrange(residplot1,residhist1, QQPlot1, residbypredplot1, ncol=4 )
    } else{
      residplot2 <- ggplot(data=data.frame(RF()$predicted), aes(x=RF()$predicted, y=y-RF()$predicted))+
        geom_point() + xlab("Predicted Value") + ylab("Residual") + ggtitle("Residual by Predicted Plot") + theme_bw() 
      residhist2 <- ggplot(data=data.frame(RF()$predicted), aes(x=y-RF()$predicted)) + geom_histogram() + theme_bw() + ggtitle("Histogram of Residuals") + xlab("Residual") + theme_bw() 
      QQPlot2 <- ggplot(data=data.frame(RF()$predicted), aes(sample = scale(y-RF()$predicted))) + stat_qq() + stat_qq_line() + 
        xlab("Normal Quantiles") + ylab("Residual Quantiles") + + ggtitle("Normal Quantile-Quantile Plot") + theme_bw() 
      residbypredplot2 <- ggplot(data=data.frame(RF()$predicted), aes(x=x, y=y-RF()$predicted))+
        geom_point() + xlab(paste(xvar)) + ylab("Residual") + ggtitle("Residual by Predictor Variable Plot") + theme_bw() 
      p <- grid.arrange(residplot2,residhist2, QQPlot2, residbypredplot2,ncol=4 )}
    return(p)
  }   
  
  
varvals <- reactive({
      req(input$dataset)
    dataset <- data.frame(get(input$dataset, listOfDataframes))
    
    var <- input$var
    my_cols <- input$Exp_Vars 
    my_cols <- my_cols[my_cols!=var]
    dataset <- dataset %>% select(c(my_cols, input$Resp_Var))
    
    ui_elems <- purrr::map(my_cols, ~{
      if (class(dataset[[.x]]) %in% c("factor", "character")){
        output <- textInput(
          inputId = paste("input", .x, sep = "_"),
          label = .x,
          value = NULL
        )
      } else if (class(dataset[[.x]]) %in% c("integer", "numeric")){
        output <- numericInput(
          inputId = paste("input", .x, sep = "_"),
          label = .x,
          value = median(dataset[[.x]])
        )
      } else output <- NULL
      
      return(output)
    })
    
    TagList <- tagList(ui_elems)
  return(TagList)
      })  
  
  
  
output$set_variable_values <- renderUI({varvals()})  

  
#variablevaluedf <- eventReactive(input$goButton, {
variablevaluedf <- reactive({
var.vals <- varvals()
varvalsunlisted <- unlist(var.vals)
variablenames <- varvalsunlisted[which(names(unlist(var.vals))=="children.children")]
variablevalues <- varvalsunlisted[which(names(unlist(var.vals))=="children.attribs.value")]

req(input$dataset)
dataset <- data.frame(get(input$dataset, listOfDataframes))
#dataset <- dataset %>% select(c(input$Exp_Vars, input$Resp_Var))
#dataset <- dataset %>% select(c(input$Exp_Vars))
#my_cols <- names(dataset)
var <- input$var
my_cols <- input$Exp_Vars 
my_cols <- my_cols[my_cols!=var]
#dataset <- dataset %>% select(c(my_cols))


for ( i in 1:length(my_cols)) {
var <- my_cols[i]
inputname <- paste("input", var, sep="_")
variablevalues[i] <- input[[inputname]]
}
#return(list(variablevalues, my_cols))
variablevalues <- as.numeric(as.character(variablevalues))
return(data.frame(variablenames, variablevalues))
}
)


output$printvarvalues <- renderTable(variablevaluedf())  
  


MakeTest <- function(Trainx, LM, RF, var, level, Variables){
  Variables <- data.frame(Variables)
  varvalues <- Variables[,2]
  varnames <- Variables[,1]
  #    Means <- data.frame(t(apply(Trainx, 2, mean, na.rm=TRUE)))
  #    New <- Means %>% slice(rep(1:1000, each = 1000))
  Entries <- data.frame(t(varvalues))
  names(Entries) <- varnames
  New <- Entries %>% slice(rep(1:1000, each = 1000))
  New$x <- 1
  names(New)[names(New)=="x"] <- as.character(var)
  varnum <- which(names(Trainx)==var)
  varnumNew <- which(names(New)==var)
  New[,varnumNew] <- seq(min(Trainx[,varnum]), max(Trainx[,varnum]), by=(max(Trainx[,varnum])-min(Trainx[,varnum]))/(1000-1))
  Test <- New
  Testx <- New
return(Testx)
}

Testxset <- reactive({MakeTest(Trainx = dataset(), LM=LM(), RF=RF(), var=var(), level=level(), Variables = variablevaluedf())})

output$Testx <- renderTable({
  req(variablevaluedf)
  return(head(Testxset()))})  
  
Get_Package <- reactive({
  datasetname <- input$dataset
  Package <- packages[which(datasets==datasetname)]
  return(Package)
})

output$Data_Source <- renderText({paste("Data come from the", Get_Package() , "R Package")})
output$References <- renderText({"For more information on the random forest prediction interval methods, see: \n Zhang, H., Zimmerman, J., Nettleton, D., & Nordman, D. J. (2019)., 'Random Forest Prediction Intervals'. The American Statistician, and \n Lu, B., & Hardin, J. (2021). 'A Unified Framework for Random Forest Prediction Error Estimation.' J. Mach. Learn. Res., 22, 8-1."})
#output$References <- renderText({"For more information"})


  
  
MakePreds <- function(Trainx, LM, RF, var, level, Variables){
  req(Variables)
  req(variablevaluedf())
    Variables <- data.frame(Variables)
    varvalues <- Variables[,2]
    varnames <- Variables[,1]
#    Means <- data.frame(t(apply(Trainx, 2, mean, na.rm=TRUE)))
#    New <- Means %>% slice(rep(1:1000, each = 1000))
    Entries <- data.frame(t(varvalues))
    names(Entries) <- varnames
    New <- Entries %>% slice(rep(1:1000, each = 1000))
    New$x <- 1
    names(New)[names(New)=="x"] <- as.character(var)
    varnum <- which(names(Trainx)==var)
    varnumNew <- which(names(New)==var)
    New[,varnumNew] <- seq(min(Trainx[,varnum]), max(Trainx[,varnum]), by=(max(Trainx[,varnum])-min(Trainx[,varnum]))/(1000-1))
  #  names(Entries) <- varnames
  #  New <- Entries %>% slice(rep(1:1000, each = 1000))
  #  varnum <- which(names(Trainx)==var)
  #  New[,varnum] <- seq(min(Trainx[,varnum]), max(Trainx[,varnum]), by=(max(Trainx[,varnum])-min(Trainx[,varnum]))/(1000-1))
    Test <- New
    Testx <- New
    varnum <- which(names(Test)==var)
    alpha = 1-level
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
    OOB_resid <- RF$y - RF$predicted
    MOE <- quantile(abs(OOB_resid), 1-alpha)
    RF_PI2_Test <- data.frame(RFPred, RFPred-MOE, RFPred+MOE)
    Test$RFPred <- RFPred
    Test$RF2Lwr <- RF_PI2_Test[,2] 
    Test$RF2Upr <- RF_PI2_Test[,3]
    QFE <- quantForestError(forest=RF, X.train=Trainx, X.test=Testx, Y.train = Train$y,  alpha = alpha)
    Test$QFEPred <- QFE$estimates[,1]
    Test$QFELwr <- QFE$estimates[,4]
    Test$QFEUpr <- QFE$estimates[,5]
    Test$x1 <- Test[,varnum]   
    return(Test)
  }

  
  CreatePlot <- function(Test, Estimate, Assumptions, range, xvar, yvar){  
    req(input$Exp_Vars)
    req(variablevaluedf())
    req(range)
    req(Test)
    req(Estimate)
    req(input$var)
    req(Trainx)
    Dataset <- Test %>% filter(x1 >= range[1] & x1 <= range[2]) 
    p <- ggplot(data=Dataset) + xlab(paste(xvar)) + ylab(paste(yvar)) + theme_bw() + 
      ylim(c(min(Dataset$QFELwr,Dataset$RF2Lwr,Dataset$LMLwr, na.rm=TRUE), max(Dataset$QFEUpr,Dataset$RF2Upr,Dataset$LMUpr, na.rm=TRUE))) #+ theme(legend.position = "none") #+ ylim(2*min(Dataset$y),2*max(Dataset$y)) + theme_bw()
    p <- if("RFest"%in% Estimate){p+geom_line(aes(x=x1, y=RFPred, color="blue"), size=1)}else{p}
    p <- if("LMest"%in% Estimate){p+geom_line(aes(x=x1, y=LMPred, color="green"), size=1)}else{p}
    p <- if("All" %in% Assumptions){p + geom_ribbon(aes( x=x1, y=LMPred, ymin=LMLwr, ymax=LMUpr), linetype=2, alpha=0.5, color="grey", fill="grey")}else{p} 
    p <- if("Some" %in% Assumptions){p + geom_ribbon(aes(x=x1, y=RFPred, ymin=RF2Lwr, ymax=RF2Upr), linetype=2, alpha=0.3, color="blue", fill="blue")}else{p}
    p <- if("None" %in% Assumptions){p + geom_ribbon(aes(x=x1, y=RFPred, ymin=QFELwr, ymax=QFEUpr), linetype=2, alpha=0.5,  color="purple", fill="purple")}else{p}
    p <- p + scale_color_identity(name = "Estimate",  breaks = c("green", "blue"), labels = c("Linear Model Estimate", "Random Forest Estimate"),  guide = "legend") #+ 
    #     scale_color_identity(name = "Interval Method",  breaks = c("grey", "blue", "purple"), labels = c("Linear Model", "Random Forest with Assumptions", "Random Forest without Assumtions"),  guide = "legend")
    return(p)
  }   
  
  residplots <- reactive({ResidPlots(LM=LM(), RF=RF(), Dataset=Train(), RespVar = Resp_Var(), method="LM", xvar=input$var)})  
  output$residplot <- renderPlot(residplots())
  Preds <- reactive({MakePreds(Trainx = Trainx(), LM=LM(), RF=RF(), var=var(), level=level(), Variables = variablevaluedf())})
  output$ShowTestData <- renderTable(Preds())    
  PredPlot <- reactive({CreatePlot(Test=Preds(), Estimate=input$Estimate, Assumptions=input$Assumptions, range=input$range, xvar=input$var, yvar=input$Resp_Var)})
  output$predplot <- renderPlot(PredPlot())
  
  
  
}




shinyApp(ui, server)