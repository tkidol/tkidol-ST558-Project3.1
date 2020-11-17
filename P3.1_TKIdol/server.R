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

ds <- read.csv("county_demographics.csv")
  
  ds <- ds %>% filter(State == "NC") %>% 
    
    mutate(Pop_Rank = ifelse(Population.2014.Population < 50000, "< 50k",
                             ifelse(Population.2014.Population < 200000, "51k - 199k", "> 200k")),
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
  


shinyServer(function(input, output, session){
  
      
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
            g <- ggplot(data = f(), aes(x = Pop_Rank)) + geom_bar(aes(fill = Pop_Expanding), position = "dodge") +
                scale_fill_discrete(name = "Population Expanding", labels = c("No", "Yes"))
            g <- ggplotly(g)
            
        }else{
            g <- ggplot(data = ds, aes(x = Pop_Rank)) + geom_bar(aes(fill = Gender_Proportion), position = "dodge") +
                scale_fill_discrete(name = "Gender Proportion", labels = c("More Female", "More Male"))
            g <- ggplotly(g)
        }
    })
    
    
    output$incomePlot <- renderPlotly({
        
        # Get filtered data
        newData <- tab()
        
        if(input$plot == "Income by Population"){
            
            # Create base aesthetic
            g <- ggplot(newData, aes(x = Population, y = Household_Income))
            
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
            g <- ggplot(newData, aes(x = Population, y = Percent_Female))
            
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
            g <- ggplot(newData, aes(x = Population, y = Percent_Poverty))
            
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
    
})
