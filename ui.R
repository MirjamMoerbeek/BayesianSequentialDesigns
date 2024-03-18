library(shinythemes)
library(DT)
ui <- fluidPage(
  
  # App title ----
  navbarPage(theme = shinytheme("cerulean"),"Bayesian sequential designs in studies with multilevel data                             ",
  tabPanel("Results for a selected scenario", 
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      h3("Select scenario"),
      br(),

      radioButtons("HypSet", label = "Type of hypothesis",inline=FALSE,
                   choices = list("Null hypothesis" = 1, "Inequality constrained hypothesis" = 2), selected = 1),
      br(),
      
      conditionalPanel(
        condition = "input.HypSet == 1",
      radioButtons("ES1", label = "Effect size",inline=FALSE,
                   choices = list("ES = 0" = 1, "ES = 0.35" = 2), selected = 1),
      br()),
      
      conditionalPanel(
        condition = "input.HypSet == 2",
        radioButtons("ES2", label = "Effect size",inline=FALSE,
                     choices = list( "ES = 0.35" = 2), selected = 2),
        br()),
      
      
      
      
      radioButtons("BFtarget", label = "Target BF",inline=FALSE,
                   choices = list("Target BF = 3" = 1, "Target BF = 5" = 2, "Target BF = 10" = 3, "Target BF = 20" = 4), selected = 1),
      br(),



      conditionalPanel(
        condition = "input.HypSet == 1",
        radioButtons("fraction1", label = "Fraction",inline=FALSE,
                     choices = list("Fraction = 1b" = 1, "Fraction = 2b" = 2, " Fraction  = 3b" = 3), selected = 1),
        br()),
      
      
      conditionalPanel(
        condition = "input.HypSet == 2",
        radioButtons("fraction2", label = "Fraction",inline=FALSE,
                     choices = list("Fraction = 1b" = 1), selected = 1),
        br()),
      
      

      
      radioButtons("n2.min", label = "Minimum number of clusters per group",inline=FALSE,
                   choices = list("n2.min = 15" = 1, "n2.min = 25" = 2), selected = 1),
      br(),
      
      radioButtons("n2.max", label = "Maximum number of clusters per group",inline=FALSE,
                   choices = list("n2.max = 30" = 1, "n2.max = 50" = 2, "n2.max = 100" = 3, "n2.max = 200" = 4, "n2.max = 50000" = 5), selected = 1),
      br()
      
      
            
  #    submitButton("Submit")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(   h3("Distribution of number of clusters per group"),
                 plotOutput(outputId = "hist.n2"),
                 br(),
                 textOutput("mean.n2"),
                 textOutput("median.n2"),
                 textOutput("max.n2"),
                 br(),
                 br(),
                 br(),
                 h3("Distribution of Bayes factor"),
                 plotOutput(outputId = "hist.BF"),
                 br(),
                 textOutput("percnonerror"),
                 textOutput("percerror"),
                 textOutput("percinconclusive")
                 
              
    ))),
  tabPanel("Table with summary of results",
           
           DT::dataTableOutput(outputId = "ResultsTable")  
  
)))