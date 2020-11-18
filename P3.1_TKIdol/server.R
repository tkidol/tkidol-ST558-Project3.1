# 
# Todd Idol
# 11/03/2020
# ST558
# Backend program to present graphs / table on mssleep DS 
#
#
# Required packages
library(dplyr)
library(readr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(caret)


ds <- read.csv("https://corgis-edu.github.io/corgis/datasets/csv/county_demographics/county_demographics.csv")
  
    ds <- ds %>% filter(State == "NC") %>% 
    
    mutate(Pop_Rank = ifelse(Population.2014.Population < 50000, "< 50k",
                             ifelse(Population.2014.Population < 200000, "50k - 199k", ">= 200k")),
           CollegeGrad_Rank = ifelse(Education.Bachelor.s.Degree.or.Higher >= 30, ">= 30%", "< 30%"),
           Gender_Proportion = ifelse(Miscellaneous.Percent.Female >= 50, "more female", "more male"),
           HS_Grads = (Education.High.School.or.Higher - Education.Bachelor.s.Degree.or.Higher),
           Pop_Expanding = ifelse(Population.Population.Percent.Change > 0, "yes", "no")) %>%
    
    select(County, ends_with("2014.Population"), Pop_Rank, starts_with("Age"), ends_with("Percent.Change"), Pop_Expanding,
           starts_with("Education"), CollegeGrad_Rank, HS_Grads, starts_with("Ethniciities"), ends_with("Female"), Gender_Proportion,
           ends_with("Occupied.Units"), starts_with("Income")) %>% 
    
    rename("Population" = Population.2014.Population,"Pop_Change" = Population.Population.Percent.Change, "65_Up" =
             Age.Percent.65.and.Older, "18_Down" = Age.Percent.Under.18.Years, "College_Grads" = Education.Bachelor.s.Degree.or.Higher,
           "Percent_Female" = Miscellaneous.Percent.Female, "Home_Value" = Housing.Median.Value.of.Owner.Occupied.Units,
           "Household_Income" = Income.Median.Houseold.Income,"Percent_Poverty" = Income.Persons.Below.Poverty.Level) %>%
    
    select(-c(Income.Per.Capita.Income, Age.Percent.Under.5.Years, Education.High.School.or.Higher,
              Employment.Private.Non.farm.Employment.Percent.Change)) %>% 
    
    arrange(desc(Population))
    
# RF Classification Train / Test set 
ds$Pop_Expanding <- as.factor(ds$Pop_Expanding)

classIndex <- createDataPartition(ds$Pop_Expanding, p = .8, list = FALSE)
classTrain <- ds[classIndex, ]
classTest <- ds[-classIndex, ]

 # RF Regression Train / Test set
regIndex <- createDataPartition(ds$Home_Value, p = .8, list = FALSE)
regTrain <- ds[regIndex, ]
regTest <- ds[-regIndex, ]
    
   
shinyServer(function(input, output, session){
  
  # RF Classification Train / Test set 
  ds$Pop_Expanding <- as.factor(ds$Pop_Expanding)
  classIndex <- createDataPartition(ds$Pop_Expanding, p = .8, list = FALSE)
  classTrain <- ds[classIndex, ]
  classTest <- ds[-classIndex, ]
  
  # RF Regression Train / Test set
  regIndex <- createDataPartition(ds$Home_Value, p = .8, list = FALSE)
  regTrain <- ds[regIndex, ]
  regTest <- ds[-regIndex, ]
  
  
  
    tab <- reactive({
        ds1 <- ds %>% filter(Pop_Rank == input$si)
        ds1
    })
    
    tab1 <- reactive({
        ds2 <- ds %>% select(input$summs)
        ds2
    })
    
    PCs <- reactive({
        ds3 <- ds %>% select(input$pc)
        pc <- prcomp(ds3, scale=TRUE)
        pc
    })
    
    cTreeFit <- reactive({
      cFit <- train(Pop_Expanding ~ ., data = classTrain,
                    method = "rf", preProcess = c("center", "scale"),
                    trControl = trainControl(method = "cv",
                    number = as.numeric(input$folds)),
                    tuneGrid = data.frame(mtry = as.numeric(input$mtry)), 
                    ntree=500, importance = TRUE)
                                         
     cFit
    })
    
    cTreePred <- reactive({
      cPred <- predict(cTreeFit(), newdata = classTest)
      cResultz <- as_tibble(postResample(cPred, classTest$Pop_Expanding))
      accuracy <- round(cResultz[1,1], digits = 3)
      accuracy
    })
    
    rTreeFit <- reactive({
      rFit <- train(Home_Value ~ ., data = regTrain,
                    method = "rf", preProcess = c("center", "scale"),
                    trControl = trainControl(method = "cv",
                    number = as.numeric(input$folds)),
                    tuneGrid = data.frame(mtry = as.numeric(input$mtry)), 
                    ntree=500, importance = TRUE)
                                          
      rFit
    })
    
    rTreePred <- reactive({
      rPred <- predict(rTreeFit(), newdata = regTest)
      rResultz <- as_tibble(postResample(rPred, regTest$Home_Value))
      r_mse <- round(rResultz[1,1], digits = 3)
      r_mse
    })
    
    tab2 <- reactive({
      ds4 <- ds %>% select(input$select)
      ds4
    })
    
    
    
    

    # Observe college rank checkbox and adjust slider settings if true
    observe({
        if(input$cgr){
            updateSliderInput(session, "size", min = 5)
        }else{
            updateSliderInput(session, "size", min = 3)
        }
    })
    
    observe({
        if(input$plot == "% Female by Population"){
            updateSelectizeInput(session, "si", label="% Female by Population")
        }else if(input$plot == "% Poverty by Population"){
            updateSelectizeInput(session, "si", label="% Poverty by Population")
        }else{
            updateSelectInput(session, "si", label="Income by Population")
        }
    })
    
    # Output barplot to ui.R
    output$barPlot <- renderPlotly({  
        
        # Plot for 1st radio button option
        if(input$rb == "Population Rank Only"){
            g <- ggplot(data = ds, aes(x = Pop_Rank)) + geom_bar()
            g <- ggplotly(g)
            
            # Plot for 2nd radio button option
        }else if(input$rb == "Population Rank and College Grad Rank"){
            g <- ggplot(data = ds, aes(x = Pop_Rank)) + geom_bar(aes(fill = CollegeGrad_Rank), position = "dodge") +
                scale_fill_discrete(name = "College Grad Rank", labels = c(">= 30%", "< 30%"))
            g <- ggplotly(g)
            
            # Plot for 3rd radio button option
        }else if(input$rb == "Population Rank and Population Expanding"){
            g <- ggplot(data = ds, aes(x = Pop_Rank)) + geom_bar(aes(fill = Pop_Expanding), position = "dodge") +
                scale_fill_discrete(name = "Population Expanding", labels = c("No", "Yes"))
            g <- ggplotly(g)
            
        }else{
            g <- ggplot(data = ds, aes(x = Pop_Rank)) + geom_bar(aes(fill = Gender_Proportion), position = "dodge") +
                scale_fill_discrete(name = "Gender Proportion", labels = c("More Female", "More Male"))
            g <- ggplotly(g)
        }
    })
    
    
    output$incomePlot <- renderPlotly({
        
      if(input$plot == "Income by Population"){
            
            # Create base aesthetic
            g <- ggplot(tab(), aes(x = Population, y = Household_Income))
            
            # Scatter plots by vore type based on checkbox for conservation (color)
            # and transparency (rem)
            if(input$pop & !input$cgr){
                g <- g + geom_point(size = input$size, aes(col = Pop_Expanding, label = County))
                g <-ggplotly(g)
            } else if(input$pop & input$cgr){
                g <- g + geom_point(size = input$size, aes(col = Pop_Expanding, alpha = College_Grads,
                                                           label = County))
                g <- ggplotly(g)
            } else {
                g <- g + geom_point(size = input$size, aes(label = County))
                g <- ggplotly(g)
            }
            
        }else if(input$plot == "% Female by Population"){
            # Create base aesthetic
            g <- ggplot(tab(), aes(x = Population, y = Percent_Female))
            
            # Scatter plots by vore type based on checkbox for conservation (color)
            # and transparency (rem)
            if(input$pop & !input$cgr){
                g <- g + geom_point(size = input$size, aes(col = Pop_Expanding, label = County))
                g <-ggplotly(g)
            }else if(input$pop & input$cgr){
                g <- g + geom_point(size = input$size, aes(col = Pop_Expanding, alpha = College_Grads,
                                                           label = County))
                g <- ggplotly(g)
            }else{
                g <- g + geom_point(size = input$size, aes(label = County))
                g <- ggplotly(g)
            }
            
        }else{
            # Create base aesthetic
            g <- ggplot(tab(), aes(x = Population, y = Percent_Poverty))
            
            # Scatter plots by vore type based on checkbox for conservation (color)
            # and transparency (rem)
            if(input$pop & !input$cgr){
                g <- g + geom_point(size = input$size, aes(col = Pop_Expanding, label = County))
                g <-ggplotly(g)
            }else if(input$pop & input$cgr){
                g <- g + geom_point(size = input$size, aes(col = Pop_Expanding, alpha = College_Grads,
                                                           label = County))
                g <- ggplotly(g)
            }else{
                g <- g + geom_point(size = input$size, aes(label = County))
                g <- ggplotly(g)
            }
        }
        
    })
    
    
    # 5 num summary + mean for quant vars
    output$summ <- renderPrint(summary(tab1()))
    
    output$table <- renderDataTable(ds)
    
    output$biPlot <- renderPlot({
      biplot(PCs(), choices=c(1,2))
    })
    
    output$biPlot1 <- renderPlot({
      biplot(PCs(), choices=c(1,3))
    })
    
    output$biPlot2 <- renderPlot({
      biplot(PCs(), choices=c(2,3))
    })
    
    output$cTree <- renderTable({
     cTreeFit()$results[, 1:2]
   })
    
    output$cPred <- renderPrint({
      paste("The tested Accuracy is:", cTreePred())
    })
    
    output$cModel <- renderPrint({
      cTreeFit()$finalModel
    })
    
    output$rTree <- renderTable({
      rTreeFit()$results[, 1:2]
    })
    
    output$rPred <- renderPrint({
      paste("The tested RMSE is:", rTreePred())
    })
    
    output$subset <- renderDataTable(tab2())
    

})
