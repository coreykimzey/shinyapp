
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
install.packages("shiny", "shinythemes")
library(shiny)
library(shinythemes)
shinyUI(fluidPage(
  #gives it a special title and fonts
  theme = shinytheme("cosmo"),
  # Application title
  titlePanel("Unique Presidential Contributions by Indivduals Under $1000"), 
  #adds subtitle
  h4("According to beta.fec.gov"),
  
  sidebarLayout(
    sidebarPanel(
      #canidate selection
      selectInput(inputId = "canidate", label = "Canidates:", c("Hillary Clinton", "Donald Trump", "Clinton minus Trump"), selected = "Hillary Clinton", multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      #slider for least amount to be included is subsetting
      sliderInput("least",
                  "Mininumum amount included",
                min = 0,
                max = 1000,
                step = 100,
                value = 0
                  ),
      #slider for least amount to be included is subsetting
            sliderInput("most",
                  "Max amount included",
                  min = 0,
                  max = 1000,
                  value = 1000,
                  step = 100
      ),
      #date inputs for subsetting
      dateInput("starting", "Starting Date", value = "2016-01-01", min = "2016-01-01",
                max = "2016-10-06"),
      dateInput("ending", "Ending Date", value = "2016-10-06", min = "2016-01-01",
                max = "2016-10-06"),
      #selecting the variable
      selectInput("variable", "What variable do you want to look at?",c("number of contributors", "sum of contributions", "amount per contribution"), selected = "number of contributions", width = NULL)
    )
    ,
    
    # has a choise between a cholorpeth or micromap
 
       mainPanel(
            tabsetPanel(
              tabPanel("Cholorpeth",
               plotOutput("statePlot",
                          # This allows us to click on this map
                          click = clickOpts(id = "plot1_click")
                          ),
               fluidRow(
                 column(width = 12,
                        h4("State Information:"),
                        #prints info on states
                        verbatimTextOutput("click_info")
                 )
               )
            ),
            tabPanel("Micromap", plotOutput("micromap", height ="800px"))
            )
       )
        )
      )
    )


