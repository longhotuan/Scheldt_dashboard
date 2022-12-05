#### Global R ####

library(knitr)
library(rmarkdown)
library(tidyverse)
library(reshape)
library(plotly)
library(flexdashboard)
library(shiny)
library(data.table)
library(lubridate)
library(shinythemes)
library(shinydashboard)
library(tweenr)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(rsconnect)
library(DT)
library(rworldmap)
library(feather)

scheldt_ghg <- read_csv("Scheldt_data.xlsx")
first_column <- which(colnames(scheldt_ghg) == "Water temperature (oC)")
last_column <- which(colnames(scheldt_ghg) == "N2O dissolved gas (ug/L)")

#### ui ####

ui <- dashboardPage(skin = "green",
                    # Dashboard header ####
                    dashboardHeader(title="Greenhouse gas emissions from Scheldt estuary"),
                    # Dashboard sidebar #### 
                    dashboardSidebar(
                        sidebarMenu(id="tabs",
                                    menuItem("About", 
                                             tabName = "about",
                                             icon = icon("info")),
                                    menuItem("Site description", 
                                             tabName = "info",
                                             icon = icon("microscope")),
                                    conditionalPanel(condition = "input.tabs == 'info'",
                                                     selectInput(inputId = "water", label = "Select a variable", 
                                                                 choices = c(colnames(scheldt_ghg)[first_column:last_column])))
                                    )
                        
                    ),
                    # Dashboard body #### 
                    dashboardBody(
                        tabItems(
                            # About tab content ####
                            tabItem(tabName = "about",
                                    fluidRow(
                                        box(width = 12, 
                                            h2("Abstract"),
                                            br(),
                                            h4("..."),
                                          
                                        )
                                    ),
                                    fluidRow(
                                        box(width = 12, 
                                            h2("Acknowledgment"),
                                            br(),
                                            h4("..."),
                                            
                                        )
                                    ),
                                    fluidRow(
                                        box(width = 12, 
                                            h2("Information"),
                                            br(),
                                            h4("If you find this tool useful, please cite the reference of our paper:  and help spread the word. If you have questions related to the dataset, please  send us an email to ",
                                               a("Long.TuanHo@UGent.com",
                                                 href = "mailto: Long.TuanHo@UGent.com"))
                                        )
                                    ),
                                    fluidRow(
                                        box(width = 12, 
                                            h2("Funded by"),
                                            column(4, 
                                                   img(align = "left|bottom",
                                                       style = "max-width:80%",
                                                       src = "Logo2.jpg") 
                                            ),
                                            column(4, 
                                                   img(align = "left|bottom",
                                                       style = "max-width:50%",
                                                       src = "Logo3.jpg") 
                                            ),
                                            column(4, 
                                                   img(align = "left|bottom",
                                                       style = "max-width:30%",
                                                       src = "Logo.png") 
                                            )
                                        )
                                    )
                            ), # end of About tabItem
                            # Info tab content ####
                            tabItem(tabName = "info",
                                    fluidRow(
                                        box(title = "Value of the variable at each sampling site", width = 12, height = 500, 
                                            leafletOutput("map", width = "100%", height = 500) # Can be changed
                                        )),
                                    fluidRow(
                                        box(title = "Summary", width = 12, heigh = 200,
                                            verbatimTextOutput("table"))
                                        
                                    )
                            ) # end of Info tab 
                        ) # end tabItems
                    ) # end dashboardbody
) # end dashboardpage

#### server ####

server <- function(input, output,session) {
    # Setting reactivities ####
    df <- reactive({scheldt_ghg})
    variablename <- reactive({
        colnames(df())[first_column:last_column]
    })
    observe({
        updateSelectInput(session, inputId = "water", label = "Select a variable", choices = c(variablename()))
    })
    df_variable <- reactive({
        input$water
    })

    # Output map in Info tab ####
    output$map <- renderLeaflet({
        selecteddf2 <- df()
        colors <- brewer.pal(n = 7, name = "Dark2")
        tilesURL <- 'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png'
        leaflet() %>%
            addTiles(tilesURL) %>%
            fitBounds(lng1 = -79.10, lat1 =  -2.83, lng2 = -78.85, lat2 = -2.963) %>%
            addMinicharts(selecteddf2$Longitude, selecteddf2$Latitude,
                          type = "pie",
                          chartdata = selecteddf2[,which(colnames(selecteddf2) == df_variable())],
                          colorPalette = colors,
                          opacity = 0.6,
                          showLabels = TRUE,
                          transitionTime = 0)
    })
    
    # Output table in Info tab ####
    
    output$table <- renderPrint(summary(df()[[input$water]]))
}

#### Run the application ####
shinyApp(ui = ui, server = server)