library(shiny)
library(ggplot2)

data <- read.csv("Birds_Collection.csv")

data <- data %>% 
  select(Catalog.No, Field.No, Sex, LAF.No, Age, Spec.Nat, Measurements, Gonads, Weight, Collector, Date.Coll, Family, Genus, Species, Subspecies, Continent, Country, State, County, Township, Nearest.Named.Place) %>% 
  mutate(lacm = Catalog.No,
         field = Field.No,
         sex = Sex,
         laf = LAF.No,
         age = Age, 
         specnat = Spec.Nat,
         measure = Measurements,
         gonads = Gonads,
         wt = as.numeric(Weight),
         coll = Collector,
         datecoll = Date.Coll,
         species = paste(Genus, Species, sep = " "),
         spp = Subspecies,
         genus = Genus,
         family = Family,
         locality = paste(Country, State, County, Township, Nearest.Named.Place, sep = " "),
         state = State,
         county = County
  ) %>% 
  select(lacm, field, sex, laf, age, specnat, measure, gonads, wt, coll, datecoll, family, species, genus, spp, locality, state, county)


# transform date into an actual date category
data$date <- as.Date(data$datecoll, format="%d %B %Y")

data$year <- as.numeric(format(data$date, "%Y"))
data$month <- as.numeric(format(data$date, "%m"))

# remove odd dates
data2 <- data %>% filter(date > "1800-01-01")

# filter out species
example <- data2 %>% filter(species == sp)

table(example$year, example$specnat)
hist(example$date, breaks=20)

example2 <- example %>% 
  filter(specnat == "SS" | specnat == "SN")
min(example2$year)
max(example2$year)

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Specimen trends and maps"),
    
    textInput("sp", "Species")

)

server <- function(input, output, session) {
  
  selected <- reactive(data2 %>% filter(species == input$sp))
  
  output$summary <- renderTable(
    selected() %>% count(year, specnat)
  )

}

# Run the application 
shinyApp(ui = ui, server = server)
