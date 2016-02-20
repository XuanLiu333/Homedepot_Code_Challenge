#install.packages("shiny")
library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
#install.packages("ggplot2")
library(ggplot2)
PRO <- read.csv(file="C:/Users/Jing/Documents/HomeDepotCoding/PRO.csv")
Skills <- read.csv(file="C:/Users/Jing/Documents/HomeDepotCoding/SkillSet.csv")
Schedule <- read.csv(file="C:/Users/Jing/Documents/HomeDepotCoding/Schedule.csv")
Date <- sample(Schedule$Date,10000,replace=T)
Query <- data.frame(Skills,Date)


# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- data.frame(PRO, Date)
    if (input$man != "All") {
      data <- data[data$Pro.name == input$man,]
    }
    if (input$cyl != "All") {
      data <- data[data$zip == input$cyl,]
    }
    if (input$trans != "All") {
      data <- data[data$WorkYears == input$trans,]
    }
    data
  }))
  
})