# 
# Todd Idol
# 11/03/2020
# ST558
# User interface program to interact with msleep DS

# Required package
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(DT)
library(caret)

# Define dashboard for user interaction
dashboardPage(skin = "blue",
              dashboardHeader(title = "Post-Secondary Employment Outcomes", titleWidth = 1000),
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
                            
                            #two columns for each of the two items
                            column(6,
                                   #Description of App
                                   h1("What does this app do?"),
                                   #box to contain description
                                   box(background="blue", width=12,
                                       h4("This application shows the relationship between the prior distribution and the posterior distribution for a simple                                 Bayesian model."),
                                       h4("This application corresponds to an example in ", span("Mathematical Statistics and Data Analysis",style = "font                                   :italic"), "section 3.5, example E, by John Rice."),
                                   )
                            ),
                            
                            column(6,
                                   #How to use the app
                                   h1("How to use the app?"),
                                   #box to contain description
                                   box(background="blue", width=12,
                                       h4("The controls for the app are located to the left and the visualizations are available on the right."),
                                       h4("To change the number of successes observed (for example the number of coins landing head side up), the slider on the                                top left can be used."),
                                   )
                            )
                          )
                  ),
                  
                  #actual app layout      
                  tabItem(tabName = "explore",
                          fluidRow(
                            column(width=3,
                                   box(width=12,background="blue", radioButtons("rb", "Population Rank and other Vars",
                                                                                choices = c("Population Rank Only", 
                                                                                            "Population Rank and Population Expanding",
                                                                                            "Population Rank and College Grad Rank",
                                                                                            "Population Rank and Gender_Proportion"))),
                                   br(),
                                   
                                   box(width=12, background="blue", radioButtons("plot", "Dot Plot Variables", 
                                                                                 choices = c("Income by Population",
                                                                                             "% Female by Population",
                                                                                             "% Poverty by Population"))),
                                   
                                   box(width=12, background="blue", selectizeInput("si","Population Rank",
                                                                                   choices = levels(as.factor(ds$Pop_Rank)))),
                                   br(),
                                   
                                   box(width=12, background="blue", sliderInput("size", "Size of Points on Graph",
                                                                                min = 1, max = 8, value = 3, step = 1)),
                                   br(),
                                   
                                   # Input to color code by expandig pop
                                   box(width=12, background="blue", checkboxInput("pop", "Color code by Population Expanding?")),
                                   
                                   br(),
                                   
                                   # Conditional input if pop expanding is checked
                                   box(width=12, background="blue", condition = "input.pop", 
                                       checkboxInput("cgr","Also change transparency by % College Grads?")),
                                   
                                   br(),
                                   
                                   box(width=12,background="blue", selectizeInput("summs","Variables for 5 Number Summary", 
                                                                                  choices = c("Population", "65_Up", "18_Down", "College_Grads",
                                                                                              "HS_Grads", "Pop_Change", "Percent_Female",
                                                                                              "Home_Value", "Household_Income",
                                                                                              "Percent_Poverty"))),
                                   
                            ),
                            
                            column(width=9,
                                   fluidRow(
                                     box(width=6,
                                         plotlyOutput("barPlot"),
                                         br(),
                                         h4("Population Rank")
                                     ),
                                     box(width=6, 
                                         plotlyOutput("incomePlot"),
                                         br(),
                                         h4("Household Income by Population")
                                     ),
                                     # 5 num summ + mean for chosen var's
                                     box(width=6, 
                                         verbatimTextOutput("summ"),
                                         br(),
                                         h4("Five Number Summary + Mean")
                                     ),
                                   ),
                            ),
                            column(width=6,
                                   fluidRow(
                                     # 5 num summ + mean for chosen var's
                                     box(position="below", width=12, 
                                         dataTableOutput("table"),
                                         br(),
                                         h4("NC County Demographic and Income Table")
                                     )
                                   )
                            )
                          )
                  ),
                  
                  
                  tabItem(tabName = "pca",
                          fluidRow(
                            column(width=3,
                                   h3("This tab will produce a principal component analysis (PCA) which is dimension reduction technique
                                          exploring relationships between variables in our data set.  PCA examines linear combinations of
                                          variables accounting for the most variability in the DS given by mathjax formula.  The high variability
                                          PC's can be good candidated for uncorrelated predictors in a linear regression model, i.e. Principal
                                          Component Regression (PCR)"),
                                   
                                   br(),
                                   
                                   box(width=12,background="blue", selectizeInput("pc","Choose 4 variables for PCA", 
                                                                                  choices = c("Population", "65_Up", "18_Down", "College_Grads",
                                                                                              "HS_Grads", "Pop_Change", "Percent_Female",
                                                                                              "Home_Value", "Household_Income","Percent_Poverty"),
                                                                                  options = list(maxItems=4), multiple=TRUE))
                                   
                                   
                            ),
                            
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
                                     )
                                     
                                   )
                            ),
                            
                          )
                  ),
                  
                  
                  tabItem(tabName = "model",
                          fluidRow(
                            column(width=3,
                                   h3("On this tab we will use the [Caret Package](http://topepo.github.io/caret/index.html) for a Random Forest
                                          Classification and Regression Tree analysis using a bootstrap sampling method on a training and
                                          test dataset.  The classification Tree technique will predict whether a county's population is expanding or
                                          shrinking based on the randomly chosen predictors.  The Regression Tree technique will predict  median
                                          home values based on the randomly chosen predictors. Accuracy (classification) and RMSE(regression)
                                          will be be used as model success metrics."),
                                   
                                   h3("The user will be able to specify the following tuning parameters: *mtry* - number of randomly 
                                           chosen prectors between 1 & 10, *ntree* - number of tree branches from 300, 500 or 700."
                                      
                                   ),
                                   
                                   br(),
                                   
                                   box(width=12,background="blue", selectizeInput("mtry","Choose number of random variables", 
                                                                                  choices = c(1:10))),
                                   br(),
                                   
                                   box(width=12,background="blue", selectizeInput("ntree","Choose number of tree branches", 
                                                                                  choices = c(300, 500, 700)))
                                   
                            ),
                            
                            column(width=9,
                                   fluidRow(
                                     box(width=6,
                                         plotOutput("cTreeFit"),
                                         br(),
                                         h4("Random Forest Classification Tree Trainng Results")
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
                                     )
                                     
                                   )
                            ),
                            
                          )
                  )
                )
              )
)

