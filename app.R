#########################################################
# Octobooks 1.1.3
# Eliot Forcadell
# 2022/12/30
#########################################################

# Environment ----
rm(list = ls())
invisible(gc())
options(encoding = "UTF-8", scipen = 999)

# Packages ----
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(yaml)
library(htmltools)
library(rvest)
library(httr)
library(curl)
library(data.table)
library(stringr)
library(lubridate)
library(tools)
library(ggplot2)
library(gt)

theme_set(theme_minimal())


# Initialisation ----
source("init.R", local = T)


# UI ----
ui <- fluidPage(
    useShinyjs(),
    tags$script(src = "appscript.js"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "appstyle.css")),
    tags$head(tags$style(HTML(sprintf(":root { --theme_colour: %s; }", config$settings$themeColour)))),
    
    # Application title
    titlePanel("Octobooks"),
    
    ## Ajouter ----
    tabsetPanel(
        type = "tabs", 
        id = "tabs",
        
        ## Ajouter
        source(file = "ui/add_tab_ui.R", local = T)$value,
        
        ## Tableau de données ----
        source(file = "ui/table_tab_ui.R", local = T)$value,
        
        ## Statistiques ----
        source(file = "ui/stats_tab_ui.R", local = T)$value,
        
        ## Préférences ----
        source(file = "ui/pref_tab_ui.R", local = T)$value,
    )
)


# Server ----
server <- function(input, output, session) {
    
    shinyjs::runjs("localStorage.clear();")
    
    # Base, choix proposés et choix par défaut
    values <- reactiveValues(books_df = books, 
                             selected_cols = config$selected_cols,
                             choices = config$choices,
                             default_choices = config$default_choices,
                             settings = config$settings)
    
    
    ## Ajouter ----
    source(file = "server/add_tab_server.R", local = T)$value
    
    ## Tableau de données ----
    source(file = "server/table_tab_server.R", local = T)$value
    
    ## Statistiques ----
    source(file = "server/stats_tab_server.R", local = T)$value

    ## Préférences ----
    source(file = "server/pref_tab_server.R", local = T)$value

    
    session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)
