#####

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
install.packages("shiny", "maps", "utils", "rgdal", "rgeos", "sp", "micromapST", "RColorBrewer")
library(shiny)
library(maps)
library(utils)
library(rgdal)
library(rgeos)
library(sp)
library(micromapST)
library(RColorBrewer)
#This is the data recieved from beta.fec.gov
#I also had to convert the data into a proper data format
#I got rid of all the variables other than contibutors name, date, amount, and state
load("subsetted.rdata")
#this are polygons of the united states' states
#website that those are found: https://www.arcgis.com/home/item.html?id=f7f805eb65eb4ab787a0a3e1116ca7e5
us_states <- readOGR(".", "states")
#this is projection needed
crsneeded <- proj4string(us_states)
#This are some of the values that are needed later on
statehood <- NULL
state.name <- reactiveValues()
printed.info <- reactiveValues(name = NULL, num_contributors = NULL, sum_contributors = NULL )
place <- reactiveValues(lat = 34, long = -100)
shinyServer(function(input, output) {
  #cholorpeth plot
  output$statePlot <- renderPlot({
    #getting the variable's I need
    if(input$canidate == "Hillary Clinton" | input$canidate == "Donald Trump") {
      if(input$canidate == "Hillary Clinton") dataset_used <- clinton
      if(input$canidate == "Donald Trump") dataset_used <- trump
      dataset_used_subsetted <- dataset_used[dataset_used$contribution_receipt_amount >= input$least & dataset_used$contribution_receipt_amount <= input$most
                                             & dataset_used$date >= input$starting & dataset_used$date <= input$ending,]
      num_contributors <- tapply(dataset_used_subsetted$contributor_name, dataset_used_subsetted$contributor_state,
                                 length)[-12]
      
      num_contributors <- num_contributors[order(names(num_contributors))]
      sum_contributors <- tapply(dataset_used_subsetted$contribution_receipt_amount, dataset_used_subsetted$contributor_state,
                                 sum)
      sum_contributors <- sum_contributors[order(names(sum_contributors))][-12]
      amount_per_contributor <- sum_contributors/num_contributors
      state_name <- us_states@data$STATE_NAME[order(us_states@data$STATE_ABBR)]
      
      #combining variable
      state_data <- cbind.data.frame(names(num_contributors), num_contributors, sum_contributors, amount_per_contributor, state_name)
      #matching variables to map
      state.to.map <- match.map("state", state_data$state_name)
      #picking which variable of interest
      if(input$variable == "number of contributors") variable_interest <- 2
      if(input$variable == "sum of contributions") variable_interest <- 3
      if(input$variable == "amount per contribution") variable_interest <- 4
      x <- state_data[state.to.map, variable_interest]
      #finding quartile breaks
      min_value <- floor(min(x, na.rm = TRUE) -1)
      max_value <- floor(max(x, na.rm = TRUE) +1)
      q1 <- floor(summary(x, na.rm = TRUE)[2])
      med <- floor(summary(x, na.rm = TRUE)[3])
      q3 <- floor(summary(x, na.rm = TRUE)[5])
      breaks <- c(min_value, q1, med, q3, max_value)
      contrib_class <- cut(x, breaks)

      #colorschemes
      color_scheme_clinton <- brewer.pal(4, "Blues")[contrib_class]
      color_scheme_trump <- brewer.pal(4, "Reds")[contrib_class]
      #plotting clinton's map or trump's map
      if(input$canidate == "Hillary Clinton") {map("state",fill = TRUE, col = color_scheme_clinton)
        legend(-120, 31, legend = c(paste(min_value,"-", q1 ),paste(q1 + 1,"-", med ),
                                   paste(med + 1,"-", q3 ),paste(q3+1,"-", max_value )), 
               fill =  brewer.pal(4,"Blues"))
      }
      if(input$canidate == "Donald Trump") {map("state",fill = TRUE, col = color_scheme_trump)
        legend(-120, 31, legend = c(paste(min_value,"-", q1 ),paste(q1 + 1,"-", med ),
                                   paste(med + 1,"-", q3 ),paste(q3+1,"-", max_value )), 
               fill =  brewer.pal(4,"Reds"))
      }
      #this uses the click data to figure out which state it is in
      if(!is.null(input$plot1_click$x)) place$long <- input$plot1_click$x
      if(!is.null(input$plot1_click$y)) place$lat <- input$plot1_click$y
      
      
      for (i in 1:51) statehood[i] <- gContains(us_states[i,], SpatialPoints(cbind(place$long, place$lat), proj4string = CRS(crsneeded)))
      
      name_of_state <- as.character(us_states$STATE_NAME[statehood])
      #Then we pick out the row of that state with all of its variable
      info = state_data[ifelse(name_of_state== state_data$`state_name`, TRUE, FALSE),]
      printed.info$state.name = info$`state_name`
      printed.info$num_contributors <- info$num_contributors
      printed.info$sum_contributors <- info$sum_contributors
      printed.info$amount_per_contributors <- round(info$amount_per_contributor,2)
      printed.info$name = input$canidate
      output$click_info <- renderPrint({
        #then we print it
        paste(printed.info$state.name, "had", printed.info$num_contributors, "contributors that gave $", printed.info$sum_contributors, "to the", printed.info$name, "Campaign.", "(The average contribution was $", printed.info$amount_per_contributors, ")")
        
        
      })
    } else{
      #We follow a similar process here but we do it for both hillary and donald
      #so we find the variables of interest using the subsetted data(parameters come from ui)
      clinton_subsetted <- clinton[clinton$contribution_receipt_amount >= input$least & clinton$contribution_receipt_amount <= input$most
                                   & clinton$date >= input$starting & clinton$date <= input$ending,]
      trump_subsetted <- trump[trump$contribution_receipt_amount >= input$least & trump$contribution_receipt_amount <= input$most
                               & trump$date >= input$starting & trump$date <= input$ending,]
      
      num_contributors_clinton <- tapply(clinton_subsetted$contributor_name, clinton_subsetted$contributor_state,
                                         length)[-12]
      
      num_contributors_clinton <- num_contributors_clinton[order(names(num_contributors_clinton))]
      num_contributors_trump <- tapply(trump_subsetted$contributor_name, trump_subsetted$contributor_state,
                                       length)[-12]
      
      num_contributors_trump <- num_contributors_trump[order(names(num_contributors_trump))]
      sum_contributors_clinton <- tapply(clinton_subsetted$contribution_receipt_amount,
                                         clinton_subsetted$contributor_state, sum)[-12]
      
      sum_contributors_clinton <- sum_contributors_clinton[order(names(sum_contributors_clinton))]
      sum_contributors_trump <- tapply(trump_subsetted$contribution_receipt_amount,
                                       trump_subsetted$contributor_state, sum)[-12]
      
      sum_contributors_trump <- sum_contributors_trump[order(names(sum_contributors_trump))]
      num_diff <- num_contributors_clinton - num_contributors_trump
      sum_diff <- sum_contributors_clinton - sum_contributors_trump
      amount_per_contributor_diff <- sum_diff/num_diff
      state_name <- us_states@data$STATE_NAME[order(us_states@data$STATE_ABBR)]
      
      #new dataframe of all of the variables and we match it to the map
      state_data <- cbind.data.frame(names(num_diff), num_diff, sum_diff, amount_per_contributor_diff, state_name)
      state.to.map <- match.map("state", state_data$state_name)
      #pick variable of interest
      if(input$variable == "number of contributors") variable_interest <- 2
      if(input$variable == "sum of contributions") variable_interest <- 3
      if(input$variable == "amount per contribution") variable_interest <- 4
      x <- state_data[state.to.map, variable_interest]
      #quartile breaks
      min_value <- floor(min(x, na.rm = TRUE) -1)
      max_value <- floor(max(x, na.rm = TRUE) +1)
      q1 <- floor(summary(x, na.rm = TRUE)[2])
      med <- floor(summary(x, na.rm = TRUE)[3])
      q3 <- floor(summary(x, na.rm = TRUE)[5])
      breaks <- floor(c(min_value, q1, med, q3, max_value))
      contrib_class <- cut(x, breaks)
      color_scheme_diff <- brewer.pal(4, "Blues")[contrib_class]
      #map the difference of the two
      map("state",fill = TRUE, col = color_scheme_diff)
      legend(-120, 31, legend = c(paste(min_value,"-", q1 ),paste(q1 + 1,"-", med ),
                                 paste(med + 1,"-", q3 ),paste(q3+1,"-", max_value )), 
             fill =  brewer.pal(4,"Blues"))
      #This is again finding the state info from the click 
      if(!is.null(input$plot1_click$x)) place$long <- input$plot1_click$x
      if(!is.null(input$plot1_click$y)) place$lat <- input$plot1_click$y
      
      
      for (i in 1:51) statehood[i] <- gContains(us_states[i,], SpatialPoints(cbind(place$long, place$lat), proj4string = CRS(crsneeded)))
      
      name_of_state <- as.character(us_states$STATE_NAME[statehood])
      
      info = state_data[ifelse(name_of_state== state_data$state_name, TRUE, FALSE),]
      printed.info$state.name = info$state_name
      printed.info$num_contributors <- info$num_diff
      printed.info$sum_contributors <- info$sum_diff
      printed.info$amount_per_contribution <- round(info$amount_per_contributor_diff,2)
      printed.info$name = input$canidate
      output$click_info <- renderPrint({
        
        paste(printed.info$state.name, "had", printed.info$num_contributors, " more contributors to Hillary Clinton's Campaign who gave a total of $", printed.info$sum_contributors, "more to her campaign. (Average amount per contribution more is $", printed.info$amount_per_contribution,")")
        
        
      })
    }
  })
  
  #micromap
  output$micromap <- renderPlot({
    if(input$canidate == "Hillary Clinton" | input$canidate == "Donald Trump") {
    if(input$canidate == "Hillary Clinton") dataset_used <- clinton
    if(input$canidate == "Donald Trump") dataset_used <- trump
    #again we find the variables of interest using the subsetting parameters(see code above for more info)
    dataset_used_subsetted <- dataset_used[dataset_used$contribution_receipt_amount >= input$least & dataset_used$contribution_receipt_amount <= input$most
                                           & dataset_used$date >= input$starting & dataset_used$date <= input$ending,]
    num_contributors <- tapply(dataset_used_subsetted$contributor_name, dataset_used_subsetted$contributor_state,
                               length)[-12]
    
    num_contributors <- num_contributors[order(names(num_contributors))]
    sum_contributors <- tapply(dataset_used_subsetted$contribution_receipt_amount, dataset_used_subsetted$contributor_state,
                               sum)
    sum_contributors <- sum_contributors[order(names(sum_contributors))][-12]
    amount_per_contributor <- sum_contributors/num_contributors
    state_name <- us_states@data$STATE_NAME[order(us_states@data$STATE_ABBR)]
    #putting all of those data together
    state_data <- cbind.data.frame(names(num_contributors), num_contributors, sum_contributors, amount_per_contributor, state_name)
    #picking variable to plot
    if(input$variable == "number of contributors") variable_interest <- 2
    if(input$variable == "sum of contributions") variable_interest <- 3
    if(input$variable == "amount per contribution") variable_interest <- 4
    #setting up special dataframe for the variable we are going to plot
    state_data2 <- state_data[,c(variable_interest, 5)]
    colnames(state_data2) <- c("variable_of_interest","state.name")
    #row names must be state for micromap to work
    rownames(state_data2) <- state_data2$state.name
    #attribute table
    panelDesc2 <- data.frame(
      type=c("mapcum", "id","dot"),
      col1 = c(NA, NA, 1),
      panelData = c("","","variable_interest")
      
      
    )
    micromapST(stateFrame = state_data2, panelDesc = panelDesc2,
               rowNames = "full", sortVar = 1, ascend = FALSE)
    
    }else {
     #this is for clinton and trumps difference micromap
      #this are the variables we need
       clinton_subsetted <- clinton[clinton$contribution_receipt_amount >= input$least & clinton$contribution_receipt_amount <= input$most
                                   & clinton$date >= input$starting & clinton$date <= input$ending,]
      trump_subsetted <- trump[trump$contribution_receipt_amount >= input$least & trump$contribution_receipt_amount <= input$most
                               & trump$date >= input$starting & trump$date <= input$ending,]
      
      num_contributors_clinton <- tapply(clinton_subsetted$contributor_name, clinton_subsetted$contributor_state,
                                         length)[-12]
      
      num_contributors_clinton <- num_contributors_clinton[order(names(num_contributors_clinton))]
      num_contributors_trump <- tapply(trump_subsetted$contributor_name, trump_subsetted$contributor_state,
                                       length)[-12]
      
      num_contributors_trump <- num_contributors_trump[order(names(num_contributors_trump))]
      sum_contributors_clinton <- tapply(clinton_subsetted$contribution_receipt_amount, clinton_subsetted$contributor_state,
                                         sum)[-12]
      
      sum_contributors_clinton <- sum_contributors_clinton[order(names(num_contributors_clinton))]
      sum_contributors_trump <- tapply(trump_subsetted$contribution_receipt_amount, trump_subsetted$contributor_state,
                                       sum)[-12]
      
      sum_contributors_trump <- sum_contributors_trump[order(names(num_contributors_trump))]
      num_diff <- num_contributors_clinton - num_contributors_trump
      sum_diff <- sum_contributors_clinton - sum_contributors_trump
      amount_per_contributor_diff <- sum_diff/num_diff
      state_name <- us_states@data$STATE_NAME[order(us_states@data$STATE_ABBR)]
      
      #new tdata frame for this
      state_data <- cbind.data.frame(names(num_diff), num_diff, sum_diff, amount_per_contributor_diff, state_name)
      #we pick the variable we want to plot
      if(input$variable == "number of contributors") variable_interest <- 2
      if(input$variable == "sum of contributions") variable_interest <- 3
      if(input$variable == "amount per contribution") variable_interest <- 4
      #special dataframe of variable of interest for micromaps
      state_data2 <- state_data[,c(variable_interest, 5)]
      colnames(state_data2) <- c("variable_of_interest","state.name")
      rownames(state_data2) <- state_data2$state.name
      #dataframe of attributes of micromaps
      panelDesc2 <- data.frame(
        type=c("mapcum", "id","dot"),
        col1 = c(NA, NA, 1),
        panelData = c("","","variable_interest")
        
        
      )
      micromapST(stateFrame = state_data2, panelDesc = panelDesc2,
                 rowNames = "full", sortVar = 1, ascend = FALSE)
    } 
    
  })
  
  
  
})
#Thanks to Rstudio for their help pages and gallery
#Thanks to Adele Culter for introducing teach me about this.


