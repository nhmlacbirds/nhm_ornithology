library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Specimen trends"),
    
    textInput("sp", "Species")

)

server <- function(input, output, session) {
}

# Run the application 
shinyApp(ui = ui, server = server)
