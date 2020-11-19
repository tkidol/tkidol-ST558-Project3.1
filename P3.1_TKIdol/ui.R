# 
# Todd Idol
# 11/18/2020
# ST558
# Project 3
#
# User interface program to interact Server.R

# Required package
library(dplyr)
library(readr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(caret)



# Define dashboard tabs for user interaction
dashboardPage(skin = "blue",
              dashboardHeader(title = "North Carolina County Demographic Data for 2014", titleWidth = 1000),
              dashboardSidebar(
                sidebarMenu(menuItem("About", tabName = "about", icon = icon("archive")),
                            menuItem("Explore", tabName = "explore", icon = icon("hiking")),
                            menuItem("PCA", tabName = "pca", icon = icon("tools")),
                            menuItem("Model", tabName = "model", icon = icon("tree")),
                            menuItem("Data", tabName = "data", icon = icon("folder-open"))
                )
              ),
              dashboardBody(
                tabItems(
                  # First tab content
                  tabItem(tabName = "about",
                          # Boxes need to be put in a row (or column)
                          fluidRow(
                            #add in latex functionality if needed
                            withMathJax(),
                            
                            # Two columns for each of the two items
                            column(6,
                                   #Description of App
                                   h1("What does this app do?"),
                                   #box to contain description
                                   box(background="blue", width=12,
                                       h3("This application allows a user to analyze 2014 demographic data from the CORGIS project filtered for North
                                          Carolina countie. Per the CORGIS site", span("Information described in the data includes the age
                                          distributions, the education levels, employment statistics, ethnicity percents,houseold information, income,
                                          and other miscellneous statistics.",style ="font-style:italic")),
                                        h3("The data is numeric, but some data has been tiered to create categorical variables: Gender_Propoortion
                                          (binary), Pop_Rank (3 tiers), Population Expanding (binary), CollegeGrad_Rank (binary).")
                                   ),
                            ),
                            
                            column(6,
                                   #How to use the app
                                   h1("How to use the app?"),
                                   # Box to contain description
                                   box(background="blue", width=12,
                                       h3("The application is arranged in 4 tabs (excluding About tab) with controls on the left and grpahs / tables
                                          on the right."),
                                       h3("The tabs are:"),
                                       h3("Explore: Allows the user to select graphs and perform basic summary analysis."),
                                       h3("PCA: Allows the user to selct up to 4 variables for principal components analysis and outputs the biplot
                                          combination of PCs 1-3 and a Scree plot."),
                                       h3("Model: A classification and regression model using the Random Forest ensemble supervised learning method.
                                       The user can choose the number of random variables and tree branches for each model"),
                                       h3("Data:  Allows the user to select, filter & arrange the data for exporting to CSV.")
                                          
                                   )
                            )
                          )
                  ),
                  
                  # Second tab content
                  tabItem(tabName = "explore",
                          fluidRow(
                            column(width=3,
                                   
                                   # Description
                                   h4("Choose Bar Plot Graph Type"),
                                   
                                   # Box containing radio button for bar plot selection
                                   box(width=12,background="blue", radioButtons("rb", "Population Rank and other Vars",
                                                                                choices = c("Population Rank Only", 
                                                                                            "Population Rank and Population Expanding",
                                                                                            "Population Rank and College Grad Rank",
                                                                                            "Population Rank and Gender_Proportion"))),
                                
                                   br(),
                                   
                                   # Description
                                   h4("Choose Dot Plot Graph Type"),
                                   
                                   # Box containing radio button for dot plot selection
                                   box(width=12, background="blue", radioButtons("plot", "Dot Plot Variables", 
                                                                                 choices = c("Income by Population",
                                                                                             "% Female by Population",
                                                                                             "% Poverty by Population"))),
                                   # Filtering selection based on Population Rank
                                   box(width=12, background="blue", selectizeInput("si","Population Rank",
                                                                                   choices = c("< 50k", "50k - 199k", ">= 200k"))),
                                   br(),
                                   
                                   # Description
                                   h4("Change dot size"),
                                   
                                   # Slider input to change dot size
                                   box(width=12, background="blue", sliderInput("size", "Size of Points on Graph",
                                                                                min = 1, max = 8, value = 3, step = 1)),
                                   br(),
                                   
                                   # Description
                                   h4("Color Code Option"),
                                   
                                   # Input to color code by Ppulation Expanding class
                                   box(width=12, background="blue", checkboxInput("pop", "Color code by Population Expanding?")),
                                   
                                   br(),
                                   
                                   # Conditional input if Population Expanding is checked
                                   box(width=12, background="blue", conditionalPanel(condition = "input.pop",
                                                                    checkboxInput("cgr","Also change transparency by % College Grads?"))),
                                   
                                   br(),
                                   
                                   h4("Variable Selection for Summariziation"),
                                   
                                   # Input choices for Summary() function analysis
                                   box(width=12,background="blue", selectizeInput("summs","Variables for 5 Number Summary", 
                                                                                  choices = c("Population", "65_Up", "18_Down", "College_Grads",
                                                                                              "HS_Grads", "Pop_Change", "Percent_Female",
                                                                                              "Home_Value", "Household_Income",
                                                                                              "Percent_Poverty"))),
                                   br(),
                            ),
                            
                            # Bar plot layout
                            column(width=9,
                                   fluidRow(
                                     box(width=6,
                                         plotlyOutput("barPlot"),
                                         br(),
                                         h4("Population Rank")
                                     ),
                                     
                                     # Download button for bar plot
                                     downloadButton("dlbar", "Download Bar"),
                                   
                                     # Dot plot layout
                                     box(width=6, 
                                         plotlyOutput("incomePlot"),
                                         br(),
                                         h4("Household Income by Population")
                                     ),
                                   
                                    br(),
                                    
                                    # Download button for dot plot
                                    downloadButton("dldot", "Download Scatter"),
                            
                                     # 5 num summ + mean for chosen var's
                                     box(width=6, 
                                         verbatimTextOutput("summ"),
                                         br(),
                                         h4("Five Number Summary + Mean")
                                     ),
                                   ),
                            ),
                            
                            # Layout for main data table
                            column(width=6,
                                   fluidRow(
                                     box(position="below", width=12, 
                                         dataTableOutput("table"),
                                         br(),
                                         h4("NC County Demographic and Income Table")
                                     )
                                   )
                            )
                          )
                  ),
                  
                  # 3rd tab content and layout
                  tabItem(tabName = "pca",
                          fluidRow(
                            column(width=3,
                                   h4("This tab produces a principal component analysis (PCA) which is dimension reduction technique
                                       exploring relationships between variables in our data set.  PCA examines linear combinations of
                                       variables accounting for the most variability in the DS given by mathjax formula."),
                                   
                                  h4("Higher variability PC's can be good candidated for uncorrelated predictors in a linear regression model, i.e.
                                      Principal Component Regression (PCR)"),
                                  
                                  h4("Each PC has an associated Eigenvalue, denoted by \\(\\Phi\\), representing the amount of variance attributable
                                     to that PC. The first PC has the largest \\(\\Phi\\) value."),
                                   
                                   br(),
                                   
                                   # Variable selection for the PCA 
                                   box(width=12,background="blue", selectizeInput("pc","Choose up to 5 variables for PCA", 
                                                                                  choices = c("Population", "65_Up", "18_Down", "College_Grads",
                                                                                              "HS_Grads", "Pop_Change", "Percent_Female",
                                                                                              "Home_Value", "Household_Income","Percent_Poverty"),
                                                                                  multiple=TRUE, options = list(maxItems=5)))
                                  
                              ),
                            
                            # Biplot and scree plot content & layout
                            column(width=9,
                                   fluidRow(
                                     box(width=6,
                                         plotOutput("biPlot"),
                                         br(),
                                         h4("Biplot of PC1 & PC2 for Chosen Vars")
                                     ),
                                     box(width=6,
                                         plotOutput("biPlot1"),
                                         br(),
                                         h4("Biplot of PC1 & PC3 for Chosen Vars")
                                     ),
                                     box(width=6, 
                                         plotOutput("biPlot2"),
                                         br(),
                                         h4("Biplot of PC2 & PC3 for Chosen Vars")
                                     ),
                                     box(width=6, 
                                         plotOutput("scree"),
                                         br(),
                                         h4("Scree Plot")
                                     )
                                     
                                   )
                            ),
                            
                          )
                  ),
                  
                  # 4th tab content & layout
                  tabItem(tabName = "model",
                          fluidRow(
                            column(width=3,
                                   h4("This tab uses the Caret Package for a Random Forest Classification and Regression Tree analysis using
                                   k fold cross validation method for training and prediction.  The classification Tree predicts whether a
                                   a county's population is expanding or shrinking based on randomly chosen predictors."),
                                   
                                   h4("The Regression Tree technique predicts median home values based on the randomly chosen predictors.
                                   Accuracy (classification) and RMSE (regression) will be be used as model success metrics."),
                                   
                                   h4("The user will be able to specify the following tuning parameters: mtry - number of randomly 
                                      chosen prectors between 1 & 10, folds - 10 or 20 to split training/test data for CV"),
                                      
                                   br(),
                                   
                                   # Input selections for mtry parameter
                                   box(width=12,background="blue", selectizeInput("mtry","Choose number of random variables", 
                                                                                  choices = c(1:10), selected = 4)),
                                   br(), 
                                   
                                   # Input selections for k-folds
                                   box(width=12,background="blue", selectizeInput("folds","Choose number of CV folds", 
                                                                                  choices = c(10, 20), selected = 10)),
                                   br(),
                                   
                                   box(width=12,background="blue", checkboxInput("modrb", "Include classification final model info?"))
                                   
                          ),
                           
                          # Content and layout for C/R Tree training and test outputs 
                          column(width=9,
                                  fluidRow(
                                     box(width=6,
                                         tableOutput("cTree"),
                                         br(),
                                         h4("Expanding Population RF Classification Accuracy (Train)")
                                     ),
                                     box(width=6,
                                         textOutput("cPred"),
                                         br(),
                                         h4("Expanding Population RF Classificatin Accuracy (Prediction)"))
                                     ),
                                 
                                     box(width=6, conditionalPanel(condition = "input.modrb",
                                       textOutput("cModel")),
                                       br(),
                                       h4("Classification Final Model")
                                     ),
                          
                                     box(width=6, 
                                         tableOutput("rTree"),
                                         br(),
                                         h4("Home Value RF Regression Training RMSE")
                                     ),
                                     
                                     box(width=6, 
                                         textOutput("rPred"),
                                         br(),
                                         h4("Home Value RF Regression Prediction RMSE")
                                   )
                              )
                       )
                  ),
                  
                  # Content and layout for 5th tab
                  tabItem(tabName = "data",
                          fluidRow(
                            column(width=3,
                                   h4("This tab enables subsetting and downloading of the NC county demographic data set."),
                                   
                                   h4("Available actionns are Select for all variables, Filter for categorical variables."),

                                   br(),
                                   
                                   # Input selections for variables
                                   box(width=12,background="blue", selectizeInput("select","Select Variables", 
                                                                                  choices = names(ds), multiple = TRUE,
                                                                                  selected = "County")),
                           ),
                            
                            # Subsetted data outpout
                            column(width=12,
                                   fluidRow(
                                     box(width=12,
                                         dataTableOutput("subset")),
                                     
                                     br(),
                                     
                                     # Download button for export to CSV
                                     downloadButton("dldata", "Download Data"),
                                     
                                     
                                        
                        )
                    )
                )
                  
                 )
              )
           )
  )
  


