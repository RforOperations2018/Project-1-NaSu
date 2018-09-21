# Class 7
# In Class Examples - Tabset

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
library(DT)
library(wordcloud2)

movies <- unique(unlist(starwars$films))

starwars.load <- starwars %>%
  mutate(films = as.character(films),
         vehicles = as.character(vehicles),
         starships = as.character(starships),
         name = as.factor(name))

pdf(NULL)

# Define UI for application
ui <- navbarPage("Star Wars NavBar", 
                 # Theme
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # Favicon
                              # Homeworld select
                              selectInput("worldSelect",
                                          "Homeworld:",
                                          choices = sort(unique(starwars.load$homeworld)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("Naboo", "Tatooine")),
                              # Birth Selection
                              sliderInput("birthSelect",
                                          "Birth Year:",
                                          min = min(starwars.load$birth_year, na.rm = T),
                                          max = max(starwars.load$birth_year, na.rm = T),
                                          value = c(min(starwars.load$birth_year, na.rm = T), max(starwars.load$birth_year, na.rm = T)),
                                          step = 1),
                              actionButton("reset", "Reset Filters", icon = icon("refresh"))
                            ),
                            # Output plot
                            mainPanel(
                              plotlyOutput("plot")
                            )
                          )
                 ),
                 # Data Table
                 tabPanel("Table",
                          fluidPage(
                            # Background Color
                            fluidRow(style = "padding-bottom: 20px;",
                              downloadButton("downloadData","Download Star Wars Data")
                            ),
                            fluidRow(
                              DT::dataTableOutput("table")
                            )
                          )
                 ),
                 tabPanel("Absolute",
                          fluidPage(
                              absolutePanel(top = 55, left = 90, width = "80%", height = "90%", draggable = TRUE,
                                            wellPanel(
                                              wordcloud2Output("characterCloud")
                                            )
                                          )
                          )
                 ),
                 # Conditional Panel example
                 tabPanel("Conditional",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("showPlot",
                                           "Show Plot",
                                           choices = c("Off", "On"))
                            ),
                            mainPanel(
                              # Hide plot when not toggeled
                              conditionalPanel("input.showPlot == 'On'",
                                               plotlyOutput("hiddenPlot")
                                               )
                            )
                          )
                        )
)

# Define server logic
server <- function(input, output, session = session) {
  # Filtered Starwars data
  swInput <- reactive({
    starwars <- starwars.load %>%
      # Slider Filter
      filter(birth_year >= input$birthSelect[1] & birth_year <= input$birthSelect[2])
    # Homeworld Filter
    if (length(input$worldSelect) > 0 ) {
      starwars <- subset(starwars, homeworld %in% input$worldSelect)
    }
    
    return(starwars)
  })
  # Reactive melted data
  mwInput <- reactive({
    swInput() %>%
      melt(id = "name")
  })
  # Point plot showing Mass, Height and Species
  output$plot <- renderPlotly({
    dat <- swInput()
    ggplotly(
      ggplot(data = dat, aes(x = mass, y = height, color = species, text = paste0("<b>", name, ":</b> ",
                                                                                  "<br>Homeworld: ", homeworld,
                                                                                  "<br>Mass: ", mass,
                                                                                  "<br>Height: ", height))) + 
        geom_point() +
        guides(color = FALSE) +
        theme_dark()
      , tooltip = "text")
  })
  # Data Table
  output$table <- DT::renderDataTable({
    starwars <- swInput() %>%
      select(c(name, height, mass, birth_year, homeworld, species, films))
    
    DT::datatable(starwars, rownames = FALSE, class = "cell-border") %>%
      formatStyle(columns = names(starwars), target = "cell", backgroundColor = c("#3a3f44"))
  })
  output$characterCloud <- renderWordcloud2({
    df <- data.frame("word" = movies, "freq" = 0)
    starwars <- swInput()
    for (i in movies) {
      df$freq[df$word == i] <- nrow(subset(starwars, grepl(i , films)))
    }
    wordcloud2(df, backgroundColor = "grey", size = .2)
  })
  output$hiddenPlot <- renderPlotly({
    starwars <- swInput()
    dat <- starwars %>%
      group_by(hair_color) %>%
      summarise(count = n())
    ggplotly(
      ggplot(data = dat, aes(x = hair_color, y = count, fill = hair_color, text = paste0("<br>Hair Color: ", hair_color,
                                                                           "<br>Count: ", count))) + 
        geom_bar(stat = "identity") +
        guides(color = FALSE) +
        theme_dark()
      , tooltip = "text")
  })
  # Updating the URL Bar
  observe({
    print(reactiveValuesToList(input))
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("star-wars-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(swInput(), file)
    }
  )
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "worldSelect", selected = c("Naboo", "Tatooine"))
    updateSliderInput(session, "birthSelect", value = c(min(starwars.load$birth_year, na.rm = T), max(starwars.load$birth_year, na.rm = T)))
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")