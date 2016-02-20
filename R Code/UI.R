library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("Basic DataTable"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(4,
             selectInput("man",
                         "Working Range:",
                         c("All",
                           unique(as.character(PRO$WorkingRange))))
      ),
      column(4,
             selectInput("trans",
                         "Store:",
                         c("All",
                           unique(as.character(PRO$StoreID))))
      ),
      column(4,
             selectInput("cyl",
                         "Zip:",
                         c("All",
                           unique(as.character(PRO$zip))))
      )
    ),
    # Create a new row for the table.
    fluidRow(
      DT::dataTableOutput("table")
    )
  )
)