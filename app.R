#########################################################
# Octobooks 1.2
# Eliot Forcadell
# 2024/07/29
#########################################################

# Environment ----
rm(list = ls())
invisible(gc())
options(encoding = "UTF-8", scipen = 999)

# Packages ----
suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(shinyWidgets)
    library(waiter)
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
    library(ggpattern)
    library(ggrepel)
    library(gt)
    library(colorspace)

    library(RPostgres)
    library(DBI)
    library(digest)
})

theme_set(theme_minimal())

serv_type <- "local"

# Initialisation ----
# source("fonctions.R", local = T)
source("init_app.R", local = T)

# Initialisation utilisateur·ice local ----
if (serv_type == "local" && file.exists("users/ego/config/settings.yml")) {
    theme_colour <- read_yaml("users/ego/config/settings.yml")$settings$themeColour
} else {
    theme_colour <- config$settings$themeColour
}

# UI ----
ui <- fluidPage(
    useShinyjs(),
    autoWaiter(
        id = c(
            "books_tbl", "cat_table",
            "plot_count_tot", "plot_count_year", "cat_plot"
        ),
        color = "white",
        html = spin_loaders(4, color = "var(--theme_colour)")
    ),
    tags$script(src = "appscript.js"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "appstyle.css")),
    tags$head(tags$style(HTML(sprintf(":root { --theme_colour: %s; }", theme_colour)))),

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
    values <- reactiveValues(
        books_df = NULL,
        selected_cols = config$selected_cols,
        choices = config$choices,
        default_choices = config$default_choices,
        settings = config$settings
    )

    ## Connexion et inscription ----
    # source(file = "server/signin_server.R", local = T)$value

    ## Inscription ----
    # source(file = "server/signup_server.R", local = T)$value

    # Initialisation utilisateur·ice ----
    if (serv_type == "local") {
        active_user <- "ego"
    } else {
        active_user <- reactiveValues(username = NULL)
    }
    source(file = "init_user.R", local = T)$value

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
