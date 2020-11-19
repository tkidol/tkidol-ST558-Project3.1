# 
# Todd Idol
# 11/03/2020
# ST558
# Project 3
# Backend program for 2014 NC County Census Data Analysis
#
#
# Required packages
library(dplyr)
library(readr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(caret)


# Link to CSV file
ds_raw <- read.csv("https://corgis-edu.github.io/corgis/datasets/csv/county_demographics/county_demographics.csv")
  
    # Filter data for NC
    ds <- ds_raw %>% filter(State == "NC") %>% 
    
    # Create categorical variables
    mutate(Pop_Rank = ifelse(Population.2014.Population < 50000, "< 50k",
                             ifelse(Population.2014.Population < 200000, "50k - 199k", ">= 200k")),
           CollegeGrad_Rank = ifelse(Education.Bachelor.s.Degree.or.Higher >= 30, ">= 30%", "< 30%"),
           Gender_Proportion = ifelse(Miscellaneous.Percent.Female >= 50, "more female", "more male"),
           HS_Grads = (Education.High.School.or.Higher - Education.Bachelor.s.Degree.or.Higher),
           Pop_Expanding = ifelse(Population.Population.Percent.Change > 0, "yes", "no")) %>%
    
    # Reduce Vars to population, age, gender, race and income sub cateogories
    select(County, ends_with("2014.Population"), Pop_Rank, starts_with("Age"), ends_with("Percent.Change"), Pop_Expanding,
           starts_with("Education"), CollegeGrad_Rank, HS_Grads, starts_with("Ethniciities"), ends_with("Female"), Gender_Proportion,
           ends_with("Occupied.Units"), starts_with("Income")) %>% 
    
    # Simplify variable naming conventions
    rename("Population" = Population.2014.Population,"Pop_Change" = Population.Population.Percent.Change, "65_Up" =
             Age.Percent.65.and.Older, "18_Down" = Age.Percent.Under.18.Years, "College_Grads" = Education.Bachelor.s.Degree.or.Higher,
           "Percent_Female" = Miscellaneous.Percent.Female, "Home_Value" = Housing.Median.Value.of.Owner.Occupied.Units,
           "Household_Income" = Income.Median.Houseold.Income,"Percent_Poverty" = Income.Persons.Below.Poverty.Level) %>%
    
    # Further reduction of variables
    select(-c(Income.Per.Capita.Income, Age.Percent.Under.5.Years, Education.High.School.or.Higher,
              Employment.Private.Non.farm.Employment.Percent.Change)) %>% 
    
    # Arrange by population descending
    arrange(desc(Population))
    
# Train / Test set to predict if a county population is growing or shrinking (Random Forest will be used)
ds$Pop_Expanding <- as.factor(ds$Pop_Expanding)

# Splitting data 80:20 Train to Test ratio
classIndex <- createDataPartition(ds$Pop_Expanding, p = .8, list = FALSE)
classTrain <- ds[classIndex, ]
classTest <- ds[-classIndex, ]

# Rand Regression Train / Test set to predict a county's median home value (Random Forest will be used)
# Splitting data 80:20 Train to Test ratio
regIndex <- createDataPartition(ds$Home_Value, p = .8, list = FALSE)
regTrain <- ds[regIndex, ]
regTest <- ds[-regIndex, ]
    
# Backend server application 
shinyServer(function(input, output, session){
  
    # Filtered data by Population Rank for ploting
    tab <- reactive({
        ds1 <- ds %>% filter(Pop_Rank == input$si)
        ds1
    })
    
    # Selected data for Summary() function evaluation
    tab1 <- reactive({
        ds2 <- ds %>% select(input$summs)
        ds2
    })
    
    # Selected data for Principal Component Analysis
    PCs <- reactive({
        ds3 <- ds %>% select(input$pc)
        pc <- prcomp(ds3, scale=TRUE)
        pc
    })
    
    # Training RF classification tree model
    cTreeFit <- reactive({
      cFit <- train(Pop_Expanding ~ ., data = classTrain,
                    method = "rf", preProcess = c("center", "scale"),
                    trControl = trainControl(method = "cv",
                    number = as.numeric(input$folds)),
                    tuneGrid = data.frame(mtry = as.numeric(input$mtry)), 
                    ntree=500, importance = TRUE)
                                         
     cFit
    })
    
    # Using model to predict County growing or shringking classification on test set
    cTreePred <- reactive({
      cPred <- predict(cTreeFit(), newdata = classTest)
      cResultz <- as_tibble(postResample(cPred, classTest$Pop_Expanding))
      accuracy <- round(cResultz[1,1], digits = 3)
      accuracy
    })
    
    # Training RF Regression tree model 
    rTreeFit <- reactive({
      rFit <- train(Home_Value ~ ., data = regTrain,
                    method = "rf", preProcess = c("center", "scale"),
                    trControl = trainControl(method = "cv",
                    number = as.numeric(input$folds)),
                    tuneGrid = data.frame(mtry = as.numeric(input$mtry)), 
                    ntree=500, importance = TRUE)
                                          
      rFit
    })

    # Using model to predict home values on test data
    rTreePred <- reactive({
      rPred <- predict(rTreeFit(), newdata = regTest)
      rResultz <- as_tibble(postResample(rPred, regTest$Home_Value))
      r_mse <- round(rResultz[1,1], digits = 3)
      r_mse
    })
    
    # Subsetted data for export
    tab2 <- reactive({
      ds4 <- ds %>% filter(Pop_Rank == input$cond1, Pop_Expanding == input$cond2, CollegeGrad_Rank == input$cond3,
                           Gender_Proportion == input$cond4) %>% select(input$select)
      ds4
    })
    
  
    # Observe College_Rank checkbox and adjust slider settings if chosen
    observe({
        if(input$cgr){
            updateSliderInput(session, "size", min = 5)
        }else{
            updateSliderInput(session, "size", min = 3)
        }
    })
    
    # Observe selection input for plot type filtered by category
    observe({
        if(input$plot == "% Female by Population"){
            updateSelectizeInput(session, "si", label="% Female by Population")
        }else if(input$plot == "% Poverty by Population"){
            updateSelectizeInput(session, "si", label="% Poverty by Population")
        }else{
            updateSelectInput(session, "si", label="Income by Population")
        }
    })
    

    # Output selected bar plots to ui.R
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
            
        }else{ # Plot for 4th radio button option
            g <- ggplot(data = ds, aes(x = Pop_Rank)) + geom_bar(aes(fill = Gender_Proportion), position = "dodge") +
                scale_fill_discrete(name = "Gender Proportion", labels = c("More Female", "More Male"))
            g <- ggplotly(g)
        }
    })
    
    # Download handling to save bar plots to png
    output$dlbar <- downloadHandler(
      filename = function(){
        paste("county-barplot", "png", sep = ".")
      },
      content = function(file){
        png(file)
        barPlot()
        dev.off()
     }
    )
    
    # Dot plots for population by household income
    output$incomePlot <- renderPlotly({
        
      if(input$plot == "Income by Population"){
            
            # Base dot plot aesthetic
            g <- ggplot(tab(), aes(x = Population, y = Household_Income))
            
            # Logic to change aesthetics based on population changes
            if(input$pop & !input$cgr){
                g <- g + geom_point(size = input$size, aes(col = Pop_Expanding, label = County))
                g <-ggplotly(g)
            
            # Logic to change aesthetics based on population change and college grad rank
            } else if(input$pop & input$cgr){
                g <- g + geom_point(size = input$size, aes(col = Pop_Expanding, alpha = College_Grads,
                                                           label = County))
                g <- ggplotly(g)
            
            # Basic plot
            } else {
                g <- g + geom_point(size = input$size, aes(label = County))
                g <- ggplotly(g)
            }
        
        # Same logic as prior section Gender Ratio by Population
        }else if(input$plot == "% Female by Population"){
            # Create base aesthetic
            g <- ggplot(tab(), aes(x = Population, y = Percent_Female))
            
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
          
            # Same logic as original section for Population by Poverty Percentage
            g <- ggplot(tab(), aes(x = Population, y = Percent_Poverty))
            
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
    
    # Download handling for dot plot export to png
    output$dldot <- downloadHandler(
      filename = function(){
        paste("county-dotplot", "png", sep = ".")
      },
      content = function(file){
        png(file)
        incomePlot()
        dev.off()
      }
    )
    

    # 5 num summary + mean for quant vars
    output$summ <- renderPrint(summary(tab1()))
    
    # Pre subsetted data table output
    output$table <- renderDataTable(ds)
    
    # Biplot for PCs 1 & 2 output
    output$biPlot <- renderPlot({
      biplot(PCs(), choices=c(1,2))
    })
    
    # Biplot for PCs 1 & 3 output
    output$biPlot1 <- renderPlot({
      biplot(PCs(), choices=c(1,3))
    })
    
    # Biplot for Pcs 2 & 3 output
    output$biPlot2 <- renderPlot({
      biplot(PCs(), choices=c(2,3))
    })
    
    # Scree plot output
    output$scree <- renderPlot({
      screeplot(PCs(), type = "lines")
    })
    
    # Trained RF  C-Tree output
    output$cTree <- renderTable({
     cTreeFit()$results[, 1:2]
   })
    
    # RF C-Tree prediction output
    output$cPred <- renderPrint({
      paste("The tested Accuracy is:", cTreePred())
    })
    
    # RF C-Tree final model output
    output$cModel <- renderPrint({
      cTreeFit()$finalModel
    })
    
    # RF R-Tree trained model output
    output$rTree <- renderTable({
      rTreeFit()$results[, 1:2]
    })
    
    # RF R-Tree prediction output
    output$rPred <- renderPrint({
      paste("The tested RMSE is:", rTreePred())
    })
   
    # Subsetted DT ouput for "data" tab
    output$subset <- renderDataTable(tab2())
    
    # Download handling for subsetted data export to csv
    output$dldata <- downloadHandler(
      filename = function(){
        paste("county-data", "csv", sep = ".")
      },
      content = function(file){
        write.csv(tab2(), file)
      }
    )

})
