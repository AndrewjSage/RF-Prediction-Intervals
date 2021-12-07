## app.R ##
library(shinydashboard)
library(randomForest)
library(forestError)
library(datasets)
library(tidyverse)
library(gridExtra)
library(mlbench)


library(mlbench)
data("BostonHousing")
Boston_Housing <- BostonHousing %>% select_if(is.numeric)
library(ISLR)
data("Auto")
Auto <- Auto %>% select_if(is.numeric)
data("Carseats")
Carseats <- Carseats %>% select_if(is.numeric)

listOfDataframes <- list(Boston_Housing, Auto, Carseats)
names(listOfDataframes) = c("Boston_Housing","Auto","Carseats")


# Boston Housing Data
x <- read.delim("https://raw.githubusercontent.com/haozhestat/RFIntervals/master/DataAnalysis/data/nipsdata/Boston/x.txt", header=FALSE, sep=" ")
y <- read.delim("https://raw.githubusercontent.com/haozhestat/RFIntervals/master/DataAnalysis/data/nipsdata/Boston/y.txt", header=FALSE, sep=" ")

names(y) <- "y"
Data <- cbind(x,y)
Train <- Data
Trainx <-x
ns <- 10
RF <- randomForest(x=Trainx, y=Train$y, nodesize=ns, method="forest", keep.inbag = TRUE)
LM <- lm(data=Train, y~.)


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "Data", icon = icon("dashboard")),
      menuItem("Model", tabName = "Model", icon = icon("th")),
      menuItem("Residual Plots", tabName = "ResPlots", icon = icon("th")),
      menuItem("Prediction Intervals", tabName = "PredIntervals", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Data",
              fluidRow(
                box(plotOutput("plot", height = 10),
                    selectInput("dataset", label = "Dataset", choices = c("Boston_Housing" = "Boston_Housing", 
                                                                          "Auto"="Auto", 
                                                                          "Carseats" = "Carseats")))
              ),
              fluidRow(
                box( dataTableOutput("table"),width=500)),
              fluidRow(
                box(   verbatimTextOutput("summary"),width=500))
      ),
      # Second tab content
      tabItem(tabName = "Model",
              fluidRow(box(
                selectInput("Resp_Var", "Response Variable", character(0))
              ), 
              checkboxGroupInput("Exp_Vars", "Explanatory Variables:",character(0))), 
              box(   verbatimTextOutput("model_summary"),width=500)),
      # Third tab content
      tabItem(tabName = "ResPlots",
              fluidRow(
                box(
                  title = "Controls",
                  selectInput("method", h5("method"), 
                              choices = list("Linear Model" = "LM", 
                                             "Random Forest" = "RF")))),
              fluidRow(
                box(plotOutput("residplot"), height = 500, width=500), 
                box(verbatimTextOutput("method"),width=500))
      ),
      
      # Fourth tab content
      tabItem(tabName = "PredIntervals",              
              fluidRow(
                box(plotOutput("predplot", height = 250), width=6),
                
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
                  uiOutput("range_slider"),
                  #      sliderInput("range", "Range:",
                  #                  min = 0, max = 50, step=1,
                  #                  value = c(0,5)), 
                  #selectInput("var", "Variable to Display", character(0)), 
                  uiOutput("varSelection"),
                  sliderInput("level", "Desired Coverage Level:",
                              min = 0.7, max = 0.95, step=0.05,
                              value = 0.9),
                width=3
                ), 
                box(
                uiOutput("set_variable_values"), 
                verbatimTextOutput("printvarvalues"),
                actionButton("goButton", "Update Table",icon("cloud-upload")), 
                width=3
                #textOutput(("printvarvalues"))
                )
                )
      )
      
    )
  )
)






server <- function(input, output, session) {
  
  
  #output$varSelection <- renderUI({
  updatevarlist <- reactive({
    req(input$dataset)
    choices = names(data.frame(get(input$dataset, listOfDataframes)))
    selectInput("var", "Variable to Display", choices = choices)
  })
  
  output$varSelection <- renderUI(updatevarlist())
  
  
  #  observeEvent(dataset(), {
  #    choices = names(data.frame(get(input$dataset, listOfDataframes)))
  #    updateSelectInput(session, inputId = "var", choices = choices)
  #  })  
  
  
  
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
    #    updateSelectInput(session, inputId = "var", choices = choices)
  })
  
  #  observe({
  #    req(input$var)
  #    var <- input$var
  #    dataset <- data.frame(get(input$dataset, listOfDataframes))
  #    Variable <- dataset %>% select(var)
  #   #Variable <- Boston_Housing$crim
  #    updateSliderInput(session, inputId = "range", min=min(Variable), 
  #                      max=max(Variable),
  #                      step=(max(Variable)-min(Variable))/50, 
  #                      value=c(min(Variable), max(Variable)))})
  
  
  #update_range_slider <- reactive({
  #    req(input$dataset)
  #    req(input$var)
  #    #var <- input$var
  #    #dataset <- data.frame(get(input$dataset, listOfDataframes))
  #    var <- var()
  #    dataset <-
  #    choices <- names(data.frame(get(input$dataset, listOfDataframes)))
  #    Variable <- dataset %>% select(var)
  #    return(sliderInput(session, inputId = "range", min=min(Variable), 
  #               max=max(Variable),
  #               step=(max(Variable)-min(Variable))/50, 
  #               value=c(min(Variable), max(Variable))))})
  
  #  output$range_slider <- renderUI(update_range_slider())
  
  
  #output$range_slider <- renderUI({
  #  tagList(
  #    sliderInput("range", "range", min=0, max=10, step=1,value=c(2,8))
  #  )
  #})
  
  
  output$range_slider <- renderUI({
    req(input$dataset)
    req(input$var)
    var <- input$var
    dataset <- data.frame(get(input$dataset, listOfDataframes))
    Variable <- dataset %>% select(var)
    tagList(
      sliderInput("range", "range", min=min(Variable), max=max(Variable), 
                  step=(max(Variable)-min(Variable))/50,
                  value=c(min(Variable), max(Variable))))})
  
  #output$range_slider <- renderUI(update_range_slider)
  
  
  #output$range_slider <- renderUI({
  #    req(input$dataset)
  #    req(input$var)
  #    var <- input$var
  #    dataset <- data.frame(get(input$dataset, listOfDataframes))
  #    choices <- names(data.frame(get(input$dataset, listOfDataframes)))
  #    Variable <- dataset %>% select(var)
  #    taglist(
  #    sliderInput("range", "range", 
  #              min=min(Variable), 
  #               max=max(Variable),
  #               step=(max(Variable)-min(Variable))/50, 
  #               value=c(min(Variable), max(Variable))))})
  
  
  
  
  
  
  
  
  
  Resp_Var <- reactive({
    return(input$Resp_Var)
  })
  
  Exp_Vars <- reactive({
    return(input$Exp_Vars)
  })
  
  method <- reactive({
    return(input$method)
  })
  
  
  
  # observeEvent(var(), {
  #    Dataset <- data.frame(get(input$dataset, listOfDataframes))
  #    varnum <- which(names(Dataset)==input$var)
  #    updateSliderInput(session, inputId = "range", min=min(Dataset[,varnum]), 
  #                                                    max=max(Dataset[,varnum]),
  #                                                    step=(max(Dataset[,varnum])-min(Dataset[,varnum]))/50, 
  #                                                    value=c(min(Dataset[,varnum]), max(Dataset[,varnum])))})
  
  
  
  level <- reactive({
    return(input$level)
  })
  
  output$method <- renderPrint({print(method())})
  output$summary <- renderPrint({
    # Use a reactive expression by calling it like a function
    summary(dataset())
  })
  
  output$table <- renderDataTable({
    dataset()
  })
  
  
  
  FitModels <- function(Dataset, RespVar, ExpVars){
    Train <- Dataset %>% select(c(ExpVars, RespVar))
    #   Train <- Dataset
    Respvarnum <- which(names(Train)==RespVar)  
    #  names(Train)[Respvarnum] <- "y"
    Trainx <- Train %>% select(-c(RespVar))
    y <- Train[, Respvarnum]
    ns <- 10
    RF <- randomForest(x=Trainx, y=y, nodesize=ns, method="forest", keep.inbag = TRUE)
    LM <- lm(data=Trainx, y~.)
    return(list(RF, LM, Train))
  }
  
  
  ModelResults <- reactive({FitModels(Dataset = dataset(), RespVar = Resp_Var(), ExpVars = Exp_Vars())})
  RF <- reactive({ModelResults()[[1]]})
  LM <- reactive({ModelResults()[[2]]})
  Train <- reactive({ModelResults()[[3]]})
  
  
  #  observeEvent(dataset(), {
  #    choices <- names(dataset())
  #    updateSelectInput(session, inputId = "var", choices = choices)
  #  })
  
  output$model_summary <- renderPrint({
    # Use a reactive expression by calling it like a function
    summary(LM())
  })
  
  
  ResidPlots <- function(LM, RF, Dataset, RespVar, method){
    Respvarnum <- which(names(Dataset)==RespVar)
    y <- Dataset[, Respvarnum]
    if(method=="LM"){
      residplot1 <- ggplot(data=data.frame(LM()$fitted.values,LM()$residuals), aes(x=LM()$fitted.values, y=LM()$residuals)) +
        geom_point() + xlab("Predicted") + ylab("Residual") + theme_bw() 
      residhist1 <- ggplot(data=data.frame(LM()$fitted.values,LM()$residuals), aes(x=LM()$residuals)) + geom_histogram() + theme_bw()
      QQPlot1 <- ggplot(data=data.frame(LM()$fitted.values,LM()$residuals), aes(sample = scale(LM()$residuals))) + stat_qq() + stat_qq_line() + 
        xlab("Normal Quantiles") + ylab("Residual Quantiles") + ggtitle("QQ Plot") + theme_bw() 
      p <- grid.arrange(residplot1,residhist1, QQPlot1, ncol=3 )
    } else{
      residplot2 <- ggplot(data=data.frame(RF()$predicted), aes(x=RF()$predicted, y=y-RF()$predicted))+
        geom_point() + xlab("Predicted") + ylab("Residual") + theme_bw() 
      residhist2 <- ggplot(data=data.frame(RF()$predicted), aes(x=y-RF()$predicted)) + geom_histogram() + theme_bw() 
      QQPlot2 <- ggplot(data=data.frame(RF()$predicted), aes(sample = scale(y-RF()$predicted))) + stat_qq() + stat_qq_line() + 
        xlab("Normal Quantiles") + ylab("Residual Quantiles") + ggtitle("QQ Plot") + theme_bw() 
      p <- grid.arrange(residplot2,residhist2, QQPlot2, ncol=3 )}
    return(p)
  }   
  
  
  
  
#  output$set_variable_values <- renderUI({

varvals <- reactive({
      req(input$dataset)
    dataset <- data.frame(get(input$dataset, listOfDataframes))
    
    my_cols <- names(dataset)
    
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
    #  varvalsunlisted <- unlist(TagList)
  #  variablenames <- varvalsunlisted[seq(from=6, to=length(unlist(varvals())), by=11)]
  #  variablevalues <- varvalsunlisted[seq(from=11, to=length(unlist(varvals())), by=11)]
   # return(c(TagList, data.frame(variablenames, variablevalues)))
      })  
  
  
  
output$set_variable_values <- renderUI({varvals()})  
#output$printvarvalues <- renderPrint(varvals())  







variablevaluedf <- eventReactive(input$goButton, {
#variablevaluedf <-reactive({
var.vals <- varvals()
varvalsunlisted <- unlist(var.vals)
variablenames <- varvalsunlisted[seq(from=6, to=length(unlist(varvals())), by=11)]
variablevalues <- varvalsunlisted[seq(from=11, to=length(unlist(varvals())), by=11)]


req(input$dataset)
dataset <- data.frame(get(input$dataset, listOfDataframes))
my_cols <- names(dataset)

for ( i in 1:length(my_cols)) {
var <- my_cols[i]
inputname <- paste("input", var, sep="_")
variablevalues[i] <- input[[inputname]]
}
return(data.frame(variablenames, variablevalues))
}
)



  
#output$printvarvalues <- renderPrint(unlist(varvals())[seq(from=11, to=length(unlist(varvals())), by=11)])  

output$printvarvalues <- renderPrint(variablevaluedf())  
  
  
  
  
  
  MakePreds <- function(Trainx, LM, RF, var, level){
    varnum <- which(names(Trainx)==var)
    Means <- data.frame(t(apply(Trainx, 2, mean, na.rm=TRUE)))
    New <- Means %>% slice(rep(1:1000, each = 1000))
    New[,varnum] <- seq(min(Trainx[,varnum]), max(Trainx[,varnum]), by=(max(Trainx[,varnum])-min(Trainx[,varnum]))/(1000-1))
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
  
  CreatePlot <- function(Test, Estimate, Assumptions, range){  
    Dataset <- Test %>% filter(x1 >= range[1] & x1 <= range[2]) 
    p <- ggplot(data=Dataset) + xlab("Explanatory Variable (x)") + ylab("Response Variable (y)") + theme_bw() #+ theme(legend.position = "none") #+ ylim(2*min(Dataset$y),2*max(Dataset$y)) + theme_bw()
    p <- if("RFest"%in% Estimate){p+geom_line(aes(x=x1, y=RFPred, color="blue"), size=1)}else{p}
    p <- if("LMest"%in% Estimate){p+geom_line(aes(x=x1, y=LMPred, color="green"), size=1)}else{p}
    p <- if("All" %in% Assumptions){p + geom_ribbon(aes( x=x1, y=LMPred, ymin=LMLwr, ymax=LMUpr), linetype=2, alpha=0.5, color="grey", fill="grey")}else{p} 
    p <- if("Some" %in% Assumptions){p + geom_ribbon(aes(x=x1, y=RFPred, ymin=RF2Lwr, ymax=RF2Upr), linetype=2, alpha=0.3, color="blue", fill="blue")}else{p}
    p <- if("None" %in% Assumptions){p + geom_ribbon(aes(x=x1, y=RFPred, ymin=QFELwr, ymax=QFEUpr), linetype=2, alpha=0.5,  color="purple", fill="purple")}else{p}
    p <- p + scale_color_identity(name = "Estimate",  breaks = c("green", "blue"), labels = c("Linear Model Estimate", "Random Forest Estimate"),  guide = "legend") #+ 
    #     scale_color_identity(name = "Interval Method",  breaks = c("grey", "blue", "purple"), labels = c("Linear Model", "Random Forest with Assumptions", "Random Forest without Assumtions"),  guide = "legend")
    return(p)
  }   
  
  residplots <- reactive({ResidPlots(LM=LM(), RF=RF(), Dataset=Train(), RespVar = Resp_Var(), method=method())})  
  output$residplot <- renderPlot(residplots())
  Preds <- reactive({MakePreds(Trainx = dataset(), LM=LM(), RF=RF(), var=var(), level=level())})
  PredPlot <- reactive({CreatePlot(Test=Preds(), Estimate=input$Estimate, Assumptions=input$Assumptions, range=input$range)})
  output$predplot <- renderPlot(PredPlot())
  
  
  
}




shinyApp(ui, server)