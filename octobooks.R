#########################################################
# Octobooks 1.0
# Eliot Forcadell
# 2022/09/13
#########################################################
install.packages("pacman")
pacman::p_load(shiny, shinyjs, shinyWidgets, DT, yaml,
               htmltools, rvest, httr, RSelenium, curl,
               data.table, stringr, lubridate, tools,
               ggplot2, gt)

theme_set(theme_minimal())


# Initialisation ----

### Fonctions usuelles ----
str_isnum <- function(x) {!grepl("\\D", x)}
fmt_semicol <- function(v) {
    return(gsub("^;|;$", "", 
                gsub("(;){2,}", replacement = ";", paste(v, collapse = ";"))))
}

### Constantes ----
wc_path <- "https://www.worldcat.org"

this_year <- as.POSIXct(paste0(format(Sys.Date(), "%Y"), 
                               c("-01-01", "-12-31")), 
                        tz = "UTC")


code_genders <- c("F" = "Femme(s)",
                  "H" = "Homme(s)",
                  "N" = "Non-binaire(s)",
                  "I" = "Je ne sais pas")

code_langue <- c("fre" = "Français",
                 "eng" = "Anglais",
                 "spa" = "Espagnol")

code_lu <- c("non" = "Non", 
             "oui" = "Oui", 
             "dnf" = "Pas fini")

stat_cats <- c("genre", "langue_vo", "genders", "langue", "format", "owner")

### Base ----

# Vérification de l'existence de la base
if (!file.exists("data")) dir.create("data")
if (!file.exists("data/octobooks.csv")) {
    fwrite(data.table(isbn = character(),
                      title = character(),
                      authors = character(),
                      translators = character(),
                      interpreters = character(),
                      genders = character(),
                      genre = character(),
                      pub_date = integer(),
                      edition_date = integer(),
                      langue_vo = character(),
                      pays_vo = character(),
                      langue = character(),
                      format = character(),
                      pages = integer(),
                      duree_h = integer(),
                      duree_min = integer(),
                      owner = character(),
                      read = character(),
                      read_deb_date = POSIXct(),
                      read_fin_date = POSIXct(),
                      keywords = character(), 
                      cover = logical()),
           "data/octobooks.csv")
} 

# Sauvegarde de la base si nécessaire
if (!file.exists("data/backups")) dir.create("data/backups")
lastsave <- paste0("data/backups/", sort(list.files("data/backups/"), decreasing = T)[1])
if (lastsave == "data/backups/NA" || md5sum("data/octobooks.csv") != md5sum(lastsave)) {
    cat("New backup")
    file.copy(from = "data/octobooks.csv",
              to = sprintf("data/backups/octobooks_%i.csv", as.integer(Sys.time())))
}

# Pour retrouver la date et l'heure de création de la sauvegarde :
# as.POSIXct(n, origin = "1970-01-01")


# Suppression des fichiers images temporaires si nécessaires
if (length(grep("temp_cover", list.files(path = "www/covers/")))) {
    file.remove(paste0("www/covers/", 
                       grep("temp_cover", list.files(path = "www/covers/"), value = T)))
}

# Backup des images de couverture
if (!file.exists("data/covers")) dir.create("data/covers")
sapply(setdiff(list.files("www/covers/"), list.files("data/covers/")),
       function(f) file.copy(sprintf("www/covers/%s", f), sprintf("data/covers/%s", f))) %>% 
    invisible

# Importation de la base
books <- fread("data/octobooks.csv", integer64 = "character",
               colClasses = list(character=c("title", "authors", "translators", "interpreters",
                                             "genders", "genre", "langue_vo", 
                                             "pays_vo", "langue", "format",
                                             "owner", "read", "keywords"),
                                 integer=c("pub_date", "edition_date", "pages", 
                                           "duree_h", "duree_min"),
                                 POSIXct=c("read_deb_date", "read_fin_date"),
                                 logical=c("cover")))



# setcolorder(books, neworder = c("isbn", "title", "authors", "translators", "interpreters",
#                                 "genders", "genre", "pub_date", "edition_date", "langue_vo", 
#                                 "pays_vo", "langue", "format", "pages", "duree_h", "duree_min", 
#                                 "owner", "read", "read_deb_date", "read_fin_date", "keywords", "cover"))

### Config ----

# Création des fichiers de config si nécessaire
config_files <- c("selected_cols", "default_choices", "choices", "settings")
config <- sapply(config_files, function(f) {
    fpath <- sprintf("config/%s.yml", f)
    if (file.exists(fpath)) {
        read_yaml(fpath)
    } else {
        file.copy(sprintf("config/init/%s.yml", f), fpath)
        read_yaml(fpath)
    }
})

labcols <- c(isbn = config$settings$isbnCase, 
             title = "Titre", 
             authors = "Auteurices",
             translators = "Traducteurices",
             interpreters = "Interprètes",
             genders = "Genres",
             genre = "Genre littéraire",
             pub_date = "Parution",
             edition_date = "Édition",
             langue_vo = "Langue VO",
             langue = "Langue",
             format = "Format",
             pages = "Pages",
             duree = "Durée",
             owner = "Propriétaire",
             read = "Lu",
             read_deb_date = "Date début", 
             read_fin_date = "Date fin", 
             keywords = "Mots-clés",
             cover = "Couverture")


# UI ----
ui <- fluidPage(
    useShinyjs(),
    tags$script(src = "appscript.js"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "appstyle.css")),
    
    # Application title
    titlePanel("Octobooks"),
    
    ## Ajouter ----
    tabsetPanel(
        type = "tabs", 
        id = "tabs",
        tabPanel("Ajouter",
                 value = "ajouter",
                 fluidPage(
                     
                     column(2,
                            id = "side-panel",
                            
                            # Panel ISBN
                            wellPanel(
                                textInput("isbn", config$settings$isbnCase, 
                                          placeholder = ""),
                                fluidRow(
                                    column(4,
                                           actionButton(inputId = "isbnButton", 
                                                        label = "Valider")
                                    ),
                                    column(8,
                                           div(id = "loadmessage", 
                                               "",
                                           )
                                    ),
                                ),
                                # width = 2,
                                div(
                                    id = "smallerloadmessage",
                                    ""
                                )
                            ),
                            awesomeRadio("read", "Lu", 
                                         c("Non" = "non",
                                           "Oui" = "oui", 
                                           "Pas fini" = "dnf"),
                                         selected = config$default_choices$read,
                                         inline = T,
                                         status = "info"),
                            tags$div(id = "read_date_div"),
                     ),
                     
                     # Panel central
                     column(8,
                            id = "add-panel",
                            
                            fluidRow(
                                
                                column(8,
                                       # Titre
                                       textInput("titre", h3("Titre"),
                                                 placeholder = "",
                                                 width = "100%"),
                                       
                                       # Auteurices
                                       tags$div(id = "auteuricesMainDiv",
                                                strong("Auteurices"),
                                                actionButton('insertAutBtn', '+', 
                                                             class = "orange-btn autandtrad"), 
                                                actionButton('removeAutBtn', '-', 
                                                             class = "orange-btn autandtrad"),
                                                tags$div(id = "auteuricesSubDiv",
                                                         fluidRow(id = "auteuricesRow",
                                                                  column(4, 
                                                                         textInput(inputId = "aut1", label = NULL)
                                                                  ),
                                                         )
                                                )
                                       ),
                                       
                                       # Traducteurices
                                       tags$div(id = "traducteuricesMainDiv",
                                                strong("Traducteurices"),
                                                actionButton('insertTradBtn', '+', 
                                                             class = "orange-btn autandtrad"), 
                                                actionButton('removeTradBtn', '-', 
                                                             class = "orange-btn autandtrad"),
                                                tags$div(id = "traducteuricesSubDiv",
                                                         fluidRow(id = "traducteuricesRow"))),
                                       
                                       tags$div(id = "interpretesMainDiv"),
                                       
                                       awesomeCheckboxGroup("genders",
                                                            NULL,
                                                            choices = setNames(names(code_genders),
                                                                               unname(code_genders)),
                                                            inline = T, 
                                                            status = "info")
                                ),
                                
                                column(4, 
                                       div(
                                           id = "imageDiv",
                                           div(
                                               id = "imageSubDiv",
                                               img(id = "coverImage",
                                                   src = "covers/dummy_cover.jpg")
                                           ),
                                           splitLayout(
                                               class = "cover-layout",
                                               div(
                                                   id = "coverImageInputDiv",
                                                   fileInput("coverImageInput",
                                                             NULL,
                                                             accept = "image/*",
                                                             buttonLabel = img(src = "camera.webp"),
                                                             placeholder = NULL)
                                               ),
                                               disabled(actionButton(inputId = "resetupload_button", 
                                                                     label = "x")),
                                               cellWidths = c("80%", "15%")
                                           ))
                                ),
                            ),
                            
                            # Genre littéraire, langue originale originale
                            fluidRow(
                                column(4, 
                                       selectInput("genre", "Genre littéraire",
                                                   choices = config$choices$genre,
                                                   selected = config$default_choices$genre)
                                ),
                                column(4,
                                       textInput("pub_date",
                                                 "Date de première parution",
                                                 placeholder = "")
                                ),
                                column(4,
                                       splitLayout(
                                           selectInput("langue_vo",
                                                       "Langue originale",
                                                       choices = config$choices$langue_vo,
                                                       selected = config$default_choices$langue_vo),
                                           selectInput("pays_vo",
                                                       "(pays)",
                                                       choices = config$choices$pays_vo,
                                                       selected = "")
                                       )
                                )
                            ),
                            
                            fluidRow(
                                column(4,
                                       selectInput("format",
                                                   "Format",
                                                   choices = config$choices$format,
                                                   selected = config$default_choices$format)
                                ),
                                column(4,
                                       textInput("edition_date",
                                                 "Date de l'édition",
                                                 placeholder = "")),
                                column(4,
                                       selectInput("langue",
                                                   "Langue de l'édition",
                                                   choices = config$choices$langue,
                                                   selected = config$default_choices$langue)
                                ),
                            ),
                            
                            # Nombre de pages, date de lecture
                            fluidRow(
                                column(4, id = "pages_div",
                                       tags$div(
                                           id = "nbpages_div",
                                           textInput("nbpages", strong("Pages"),
                                                     placeholder = "",
                                                     width = "125px"))
                                ),
                                column(4,
                                       selectInput("owner",
                                                   "Propriétaire",
                                                   choices = config$choices$owner,
                                                   selected = config$default_choices$owner)
                                ),
                                column(4,
                                       selectInput("keywords",
                                                   "Mots-clés",
                                                   multiple = T,
                                                   choices = config$choices$keywords)
                                )
                            ),  
                            
                            # Messages d'erreur et bouton ajouter
                            fluidRow(
                                id = "bottom-row",
                                column(8,
                                       div(id = "addMessage")
                                ),
                                column(4,
                                       actionButton(inputId = "reinit_button", 
                                                    label = "Réinitialiser"),
                                       actionButton(inputId = "add_button", 
                                                    label = "Ajouter",
                                                    class = "orange-btn")
                                ),
                            )
                     ),
                 )
        ),
        
        ## Tableau de données ----
        tabPanel("Table", 
                 value = "table",
                 fluidRow(
                     id = "selcols-row",
                     column(1),
                     uiOutput("selcols"),
                 ),
                 tags$div(id = "table_div",
                          fluidRow(
                              DT::DTOutput("books_tbl")
                          )
                 )
        ),
        
        ## Statistiques ----
        tabPanel("Stats",
                 value = "stats",
                 navlistPanel(
                     id = "stats_nav",
                     tabPanel("Bilan global",
                              h3("Nombre de livres dans la base"),
                              h5(em("L'année correspond à la date de fin de lecture"), style = "color: silver;"),
                              fluidRow(
                                  column(10,
                                         plotOutput("plot_count",
                                                    height = "300px")
                                  )
                              )
                     ),
                     tabPanel("Bilan par catégorie",
                              value = "stats_bycat",
                              fluidRow(
                                  id = "select_cat_row",
                                  column(5,
                                         selectInput("stat_cat", 
                                                     "Catégorie",
                                                     choices = setNames(stat_cats,
                                                                        labcols[stat_cats])),
                                         gt_output("cat_table")
                                  ),
                                  column(7,
                                         plotOutput("cat_plot",
                                                    height = "400px")
                                  )
                              ),
                              fluidRow(
                                  id = "set_cat_row",
                                  column(9,
                                         awesomeRadio("cat_onlyread",
                                                      label = NULL,
                                                      choices = c("Tous les livres" = FALSE,
                                                                  "Livres lus" = TRUE),
                                                      inline = T,
                                                      status = "info"),
                                         awesomeCheckbox("cat_readbydate",
                                                         "Par date de fin de lecture",
                                                         status = "info"),
                                         splitLayout(id = "cat_read_date",
                                                     div("Fini entre", style = "padding-top : 5px"),
                                                     airDatepickerInput("cat_deb_read",
                                                                        label = NULL,
                                                                        value = this_year[1]),
                                                     div("et", style = "padding-top : 5px"),
                                                     airDatepickerInput("cat_fin_read",
                                                                        label = NULL,
                                                                        value = this_year[2]),
                                                     cellWidths = c("11%", "30%", "4%", "30%", "25%"),
                                                     cellArgs = list(style = "text-align : center; vertical"))
                                  ),
                              ),
                              
                     ),
                     widths = c(2,9)
                 ),
        ),
        
        ## Préférences ----
        tabPanel("Préférences",
                 value = "pref",
                 navlistPanel(
                     id = "pref_nav",
                     tabPanel("Tableau de données",
                              h3("Colonnes à afficher par défaut"),
                              uiOutput("select_newdefcols"),
                              
                              actionButton(inputId = "change_defcols_button", 
                                           label = "Modifier",
                                           class = "orange-btn"),
                              div(id = "newdefcolsMessage")
                     ),
                     
                     ### Choix par défaut ----
                     tabPanel("Choix par défaut",
                              value = "choices_def",
                              
                              h3("Modifier les valeurs sélectionnées par défaut"),
                              fluidRow(
                                  column(3,
                                         selectInput("coltochange",
                                                     "Colonne :",
                                                     choices = setNames(names(config$default_choices),
                                                                        labcols[names(config$default_choices)]))
                                  ),
                                  column(5,
                                         splitLayout(
                                             uiOutput("defvalue"),
                                             actionButton(inputId = "change_default_button", 
                                                          label = "Modifier",
                                                          class = "orange-btn"),
                                             cellWidths = c("60%", "40%"))
                                  ),
                              ),
                              fluidRow(
                                  div(id = "newdefvalueMessage")
                              )
                     ),
                     
                     ### Choix proposés ----
                     tabPanel("Choix proposés",
                              value = "choices",
                              h3(id = "addvalues", "Ajouter des valeurs"),
                              fluidRow(
                                  column(3,
                                         uiOutput("select_coltoaddto")
                                  ),
                                  column(5,
                                         splitLayout(
                                             textInput("newchoice",
                                                       "Nouvelle valeur :"),
                                             actionButton(inputId = "add_choice_button", 
                                                          label = "Ajouter",
                                                          class = "orange-btn"),
                                             cellWidths = c("60%", "40%")),
                                  ),
                              ),
                              fluidRow(
                                  column(3,
                                         h5("Choix déjà disponibles :"),
                                         div(
                                             id = "availChoices"
                                         )
                                  ),
                                  column(5,
                                         div(id = "newchoiceError")
                                  ),
                              ),
                              
                     ),
                     
                     ### Réglages ----
                     tabPanel("Réglages",
                              value = "settings",
                              h3("Réglages"),
                              fluidRow(
                                  column(10,
                                         awesomeRadio("set_reset",
                                                      "Réinitialiser le formulaire après l'ajout d'un livre :",
                                                      choices = c("Oui", "Non"),
                                                      selected = config$settings$reset,
                                                      inline = T,
                                                      status = "info")
                                  ),
                              ),
                              fluidRow(
                                  column(10,
                                         awesomeRadio("set_worldcat",
                                                      HTML("Optimiser la recherche d'image de courverture, notamment pour les éditions non-françaises :<br>
                                                           <span style='font-weight:400; color:silver'>Attention, cette option peut rallonger significativement le temps de recherche</span>"),
                                                      choices = c("Oui", "Non"),
                                                      selected = config$settings$worldcat,
                                                      inline = T,
                                                      status = "info")
                                  ),
                              ),
                              h4("Attention, les réglages suivants ne s'appliqueront qu'à la réouverture de l'application"),
                              fluidRow(
                                  column(8,
                                         uiOutput("set_pageLength")
                                  ),
                              ),
                              fluidRow(
                                  column(8,
                                         uiOutput("set_isbnCase")
                                  ),
                              )
                     ),
                     widths = c(2,9)
                 )
        )
    )
)


# Server ----
server <- function(input, output, session) {
    
    # Base, choix proposés et choix par défaut
    values <- reactiveValues(books_df = books, 
                             selected_cols = config$selected_cols,
                             choices = config$choices,
                             default_choices = config$default_choices,
                             settings = config$settings)
    
    
    ## Ajouter ----
    
    ### Listes dynamiques ----
    
    observe({
        updateAwesomeRadio(session,
                           "read",
                           selected = values$default_choices$read)
        
        updateSelectInput(session,
                          "genre",
                          choices = values$choices$genre,
                          selected = values$default_choices$genre)
        
        updateSelectInput(session,
                          "langue_vo",
                          choices = values$choices$langue_vo,
                          selected = values$default_choices$langue_vo)
        updateSelectInput(session, 
                          "pays_vo",
                          choices = values$choices$pays_vo,
                          selected = "")
        
        updateSelectInput(session,
                          "format",
                          choices = values$choices$format,
                          selected = values$default_choices$format)
        
        updateSelectInput(session,
                          "langue",
                          choices = values$choices$langue,
                          selected = values$default_choices$langue)
        
        updateSelectInput(session,
                          "owner",
                          choices = values$choices$owner,
                          selected = values$default_choices$owner)
        
        updateSelectInput(session,
                          "keywords",
                          choices = values$choices$keywords)
        
    })
    
    
    ### Affichage conditionnel ----
    
    shinyjs::runjs("$('#pub_date, #edition_date, #edit_pub_date').attr('maxlength', 4)")
    
    #### Date de lecture ----
    
    date_shown <- reactiveVal(value = F)
    observeEvent(input$read, {
        if (input$read == "non") {
            updateAirDateInput(session, inputId = "read_deb_date", value = NULL, clear = T)
            updateAirDateInput(session, inputId = "read_fin_date", value = NULL, clear = T)
            removeUI(selector = "#read_date_subdiv")
            date_shown(F)
        } else {
            if (!date_shown()) {
                insertUI(
                    selector = "#read_date_div",
                    where = "beforeEnd",
                    ui = tags$div(id = "read_date_subdiv",
                                  tags$div(id = "read_deb_date_div",
                                           airDatepickerInput("read_deb_date",
                                                              label = "Début de lecture",
                                                              language = "fr",
                                                              todayButton = T,
                                                              autoClose = TRUE)),
                                  # checkboxInput("read_date_na",
                                  #               "Je ne sais plus",
                                  #               value = FALSE)
                                  # HTML('<div class="rating">
                                  #        <i class="rating__star far fa-star"></i>
                                  #        <i class="rating__star far fa-star"></i>
                                  #        <i class="rating__star far fa-star"></i>
                                  #        <i class="rating__star far fa-star"></i>
                                  #        <i class="rating__star far fa-star"></i>
                                  #       </div>')
                    )
                ) 
            } else {
                updateCheckboxInput(session, "read_date_na", value = FALSE)
            }
            date_shown(T)
            
            if (input$read == "oui") {
                insertUI(
                    selector = "#read_deb_date_div",
                    where = "afterEnd",
                    ui = tags$div(
                        id = "read_fin_date_div",
                        airDatepickerInput("read_fin_date",
                                           label = "Fin de lecture",
                                           value = input$read_deb_date,
                                           language = "fr",
                                           todayButton = T,
                                           autoClose = TRUE))
                )
            } else {
                removeUI(selector = "#read_fin_date_div")
            }
            
        }
    })
    
    # observeEvent(input$read_date_na, {
    #     if (input$read_date_na) {
    #         updateAirDateInput(session, inputId = "read_deb_date", value = NULL, clear = T)
    #         updateAirDateInput(session, inputId = "read_fin_date", value = NULL, clear = T)
    #         disable(id = "read_deb_date") 
    #         disable(id = "read_fin_date") 
    #     } else {
    #         enable(id = "read_deb_date") 
    #         enable(id = "read_fin_date") 
    #     }
    # })
    
    # Adaptation de la date de fin à partir de la date de début
    observeEvent(input$read_deb_date, {
        if (input$read == "oui") {
            updateAirDateInput(session, "read_fin_date",
                               value = input$read_deb_date,
                               options = list(minDate = input$read_deb_date))
        }
    })
    
    #### Format : nombre de pages ou durée ----
    
    int_inserted <- c("int1")
    intbtn_count <- reactiveVal(value = 1)
    
    nbpages_shown <- reactiveVal(value = T)
    observeEvent(input$format, {
        if (input$format != "Audio") {
            
            # Nombre de pages
            if (!nbpages_shown()) {
                removeUI(selector = "#duree_div")
                updateTextInput(session, "duree_h", value = NULL)
                updateTextInput(session, "duree_min", value = NULL)
                nbpages_shown(T)
                insertUI(
                    selector = "#pages_div",
                    ui = tags$div(
                        id = "nbpages_div",
                        textInput("nbpages", strong("Pages"),
                                  placeholder = "",
                                  width = "125px"),
                        
                    )
                )
            }
            
            # Disparition des interprètes
            removeUI(selector = "#interpretesMainDiv")
            int_inserted <<- c("int1")
            intbtn_count(1)
            
        } else {
            
            # Durée
            updateTextInput(session, "nbpages", value = NULL)
            removeUI(selector = "#nbpages_div")
            nbpages_shown(F)
            insertUI(
                selector = "#pages_div",
                ui = tags$div(
                    id = "duree_div",
                    splitLayout(
                        textInput(inputId = "duree_h",
                                  label = "Durée (h:min)"),
                        tagList(tags$label(" "),
                                tags$p(":", style = "padding-top: 5px;")),
                        tagList(tags$label(id = "duree_min-label", " "),
                                tags$input(id = "duree_min", type = "text",
                                           class = "form-control shiny-bound-input shinyjs-resettable",
                                           maxlength="2")
                        ),
                        cellWidths = c("20%","2%", "20%", "58%")
                    )
                )
            )
            
            # Interprètes
            insertUI(
                selector = "#traducteuricesMainDiv",
                where = "afterEnd",
                ui = div(id = "interpretesMainDiv",
                         strong("Interprètes"),
                         actionButton('insertIntBtn', '+', 
                                      class = "orange-btn autandtrad"), 
                         actionButton('removeIntBtn', '-', 
                                      class = "orange-btn autandtrad"),
                         fluidRow(id = "interpretesRow",
                                  column(4, 
                                         textInput(inputId = "int1", label = NULL)
                                  ),
                         )
                )
            )
        }
    })
    
    
    observeEvent(input$insertIntBtn, {
        intbtn_count(intbtn_count() + 1)
        id <- paste0('int', intbtn_count())
        insertUI(
            selector = '#interpretesRow',
            ui = column(4, textInput(inputId = id, label = NULL))
        )
        int_inserted <<- c(int_inserted, id)
    })
    
    observeEvent(input$removeIntBtn, {
        if (intbtn_count() > 0) {
            id = int_inserted[length(int_inserted)]
            removeUI(selector = sprintf('.col-sm-4:has(#%s)', id))
            int_inserted <<- int_inserted[-length(int_inserted)]
            intbtn_count(intbtn_count() - 1)
        }
    })
    
    
    #### Auteurices et traducteurices ----
    
    aut_inserted <- c("aut1")
    autbtn_count <- reactiveVal(value = 1)
    
    trad_inserted <- c()
    tradbtn_count <- reactiveVal(value = 0)
    
    obsTotrad <- list()
    
    observeEvent(input$insertAutBtn, {
        autbtn_count(autbtn_count() + 1)
        id <- paste0('aut', autbtn_count())
        insertUI(
            selector = '#auteuricesRow',
            ui = column(4, 
                        textInput(inputId = id, label = NULL))
        )
        aut_inserted <<- c(aut_inserted, id)
    })
    
    observeEvent(input$removeAutBtn, {
        if (autbtn_count() > 0) {
            id = aut_inserted[length(aut_inserted)]
            removeUI(selector = sprintf('.col-sm-4:has(#%s)', id))
            aut_inserted <<- aut_inserted[-length(aut_inserted)]
            autbtn_count(autbtn_count() - 1)
        }
    })
    
    insertTrad <- function(val = NULL) {
        tradbtn_count(tradbtn_count() + 1)
        id <- paste0('trad', tradbtn_count())
        insertUI(
            selector = '#traducteuricesRow',
            ui = column(4, textInput(inputId = id, label = NULL,
                                     value = val))
        )
        trad_inserted <<- c(trad_inserted, id)
    }
    
    observeEvent(input$insertTradBtn, {
        insertTrad()
    })
    
    observeEvent(input$removeTradBtn, {
        if (tradbtn_count() > 0) {
            id = trad_inserted[length(trad_inserted)]
            removeUI(selector = sprintf('.col-sm-4:has(#%s)', id))
            trad_inserted <<- trad_inserted[-length(trad_inserted)]
            tradbtn_count(tradbtn_count() - 1)
        }
    })
    
    #### Image ----
    
    coverImg <- reactiveVal(value = "www/covers/dummy_cover.jpg")
    
    update_coverImage <- function() {
        shinyjs::runjs(
            sprintf("
                    var cover = document.getElementById('coverImage');
                    cover.setAttribute('src', '%s');
                    ", 
                    sprintf("%s?%i", gsub("www/", "", coverImg()), as.integer(Sys.time())))
        )
    }
    
    reset_coverInput <- function() {
        removeUI("#coverImageInputDiv")
        insertUI(selector = "#imageDiv .cover-layout div:first",
                 where = "afterBegin",
                 div(
                     id = "coverImageInputDiv",
                     fileInput("coverImageInput",
                               NULL,
                               accept = "image/*",
                               buttonLabel = img(src = "camera.webp"),
                               placeholder = NULL)
                 )
        )
    }
    
    
    observeEvent(input$coverImageInput, {
        
        urlImg <- paste0("www/covers/temp_cover.",
                         file_ext(input$coverImageInput$datapath))
        file.copy(input$coverImageInput$datapath, urlImg, overwrite = T)
        coverImg(urlImg)
        update_coverImage()
        
    })
    
    observe({
        toggleState("resetupload_button", 
                    condition = coverImg() != "www/covers/dummy_cover.jpg")
    })
    
    observeEvent(input$resetupload_button, {
        reset_coverInput()
        if (coverImg() != "www/covers/dummy_cover.jpg") {
            file.remove(coverImg())
            coverImg("www/covers/dummy_cover.jpg")
            update_coverImage()
        }
    })
    
    
    ### Réinitialisation des inputs ----
    
    reset_add <- function() {
        
        if (autbtn_count() == 0) {
            insertUI(
                selector = '#auteuricesRow',
                ui = column(4, textInput(inputId = "aut1", label = NULL))
            )
        } else {
            updateTextInput(session, "aut1", value = "") 
            if (autbtn_count() > 1) {
                for (i in 2:autbtn_count()) {
                    removeUI(selector = sprintf('.col-sm-4:has(#aut%i)', i))
                }
            }
        }
        aut_inserted <<- c("aut1")
        autbtn_count(1)
        
        for (i in 1:tradbtn_count()) {
            removeUI(selector = sprintf('.col-sm-4:has(#trad%i)', i))
        }
        trad_inserted <<- c()
        tradbtn_count(0)
        
        if (intbtn_count() == 0) {
            insertUI(
                selector = '#interpretersRow',
                ui = column(4, textInput(inputId = "int1", label = NULL))
            )
        } else {
            updateTextInput(session, "int1", value = "") 
            if (intbtn_count() > 1) {
                for (i in 2:intbtn_count()) {
                    removeUI(selector = sprintf('.col-sm-4:has(#int%i)', i))
                }
            }
        }
        int_inserted <<- c("int1")
        intbtn_count(1)
        
        updateTextInput(session, "isbn", value = "")
        updateTextInput(session, "titre", value = "")
        updateTextInput(session, "nbpages", value = "")
        updateTextInput(session, "duree_h", value = "")
        updateTextInput(session, "duree_min", value = "")
        updateAwesomeRadio(session, "read", selected = values$default_choices$read)
        updateSelectInput(session, "genre", selected = values$default_choices$genre)
        updateTextInput(session, "pub_date", value = "")
        updateTextInput(session, "edition_date", value = "")
        updateSelectInput(session, "langue_vo", selected = values$default_choices$langue_vo)
        updateSelectInput(session, "pays_vo", selected = "")
        updateSelectInput(session, "langue", selected = values$default_choices$langue)
        updateSelectInput(session, "format", selected = values$default_choices$format)
        updateSelectInput(session, "owner", selected = values$default_choices$owner)
        updateAwesomeCheckboxGroup(session, "genders",
                                   choices = setNames(names(code_genders),
                                                      unname(code_genders)),
                                   selected = NULL, inline = T,
                                   status = "info")
        updateSelectInput(session, "keywords", 
                          choices = values$choices$keywords, selected = NULL)
        
        coverImg("www/covers/dummy_cover.jpg")
        update_coverImage()
        reset_coverInput()
    }
    
    observeEvent(input$reinit_button, {
        shinyjs::html("")
        reset_add()
    })
    
    observeEvent(input$tabs, {
        shinyjs::html("addMessage", "")
        shinyjs::html("newdefcolsMessage", "")
        shinyjs::html("newdefvalueMessage", "")
        shinyjs::html("newchoiceError", "")
    })
    
    
    ### Requête ISBN ----
    
    update_add <- function(res_data) {
        
        print(res_data)
        
        updateTextInput(inputId = "titre", 
                        value = res_data$title)
        
        # Auteurices et traducteurices
        
        for(i in 1:length(res_data$auteurices)) {
            id <- paste0("aut", autbtn_count() + i)
            if (id == "aut1") {
                insertUI(selector = "#auteuricesRow",
                         ui = column(4, 
                                     textInput(inputId = id, label = NULL, 
                                               value = res_data$auteurices[i])
                         )
                )
            } else {
                totradid <- paste0("totrad", autbtn_count() + i)
                insertUI(selector = "#auteuricesRow",
                         ui = column(4, 
                                     splitLayout(
                                         textInput(inputId = id, label = NULL, 
                                                   value = res_data$auteurices[i]),
                                         actionButton(inputId = totradid, "T",
                                                      class = "totrad"),
                                         cellWidths = c("100%", "0%"))))
            }
            aut_inserted <<- c(aut_inserted, id)
        }
        autbtn_count(autbtn_count() + length(res_data$auteurices))
        
        if (length(res_data$auteurices) > 1) {
            lapply(2:length(res_data$auteurices), function(i) {
                autid <- paste0("aut", i)
                totradid <- paste0("totrad", i)
                if (is.null(obsTotrad[[totradid]])) {
                    obsTotrad[[totradid]] <<- observeEvent(input[[totradid]], {
                        insertTrad(input[[autid]])
                        updateTextInput(session, autid, value = "")
                    })
                }
            })  
        }
        
        
        # Langue et date d'édition
        updateSelectInput(inputId = "langue",
                          selected = res_data$language)
        updateTextInput(inputId = "edition_date",
                        value = res_data$date)
        
        # Nombre de pages
        updateTextInput(inputId = "nbpages",
                        value = res_data$numPages)
    }
    
    observeEvent(input$isbnButton, {
        
        shinyjs::html(id = "loadmessage", "En cours...")
        
        shinyjs::html("")
        updateTextInput(inputId = "nbpages", value = "")
        updateTextInput(inputId = "edition_date", value = "")
        updateSelectInput(inputId = "langue", 
                          selected = values$default_choices$langue)
        
        removeUI(selector = "#auteuricesRow")
        autbtn_count(0)
        aut_inserted <<- c()
        
        removeUI(selector = "#traducteuricesRow")
        tradbtn_count(0)
        trad_inserted <<- c()
        
        insertUI(selector = "#auteuricesSubDiv",
                 ui = fluidRow(id = "auteuricesRow"))
        insertUI(selector = "#traducteuricesSubDiv",
                 ui = fluidRow(id = "traducteuricesRow"))
        
        coverImg("www/covers/dummy_cover.jpg")
        reset_coverInput()  
        
        isbn <- gsub("-", "", str_trim(input$isbn))
        wc_info <- FALSE
        wc_tried_img <- FALSE
        
        
        #### Info ----
        
        if (!grepl(pattern = "^[0-9]+$", isbn)) {
            updateTextInput(inputId = "titre", value = "isbn non conforme")
        } else if (!has_internet()) {
            shinyjs::html("addMessage", HTML(sprintf('<p class="error">%s</p>', "Pas de connexion internet !")))
        } else {
            updateTextInput(inputId = "isbn", value = isbn)
            
            print("Trying Zotero for info")
            shinyjs::html(id = "smallerloadmessage", "Searching for book info...")
            
            res <- POST("https://t0guvf0w17.execute-api.us-east-1.amazonaws.com/Prod//search",
                        add_headers(Referer = "https://www.zotero.org/",
                                    Origin = "https://www.zotero.org",
                                    DNT = 1,
                                    Accept = "*/*"),
                        content_type("text/plain"),
                        body = isbn)
            
            if (status_code(res) == 200 && length(content(res))) {
                
                print(content(res)[[1]]$title)
                print(content(res)[[1]]$libraryCatalog)
                
                wc_info <- content(res)[[1]]$libraryCatalog %in% 
                    c("Library of Congress ISBN",
                      "Open WorldCat")
                
                # Auteurices et autres
                auteurices <- sapply(content(res)[[1]]$creators,
                                     function(x) {
                                         if ("name" %in% names(x)) {
                                             x$name
                                         } else {
                                             paste(x$firstName, x$lastName)
                                         }
                                     })
                
                update_add(list(title = content(res)[[1]]$title,
                                auteurices = auteurices,
                                language = code_langue[content(res)[[1]]$language],
                                date = content(res)[[1]]$date,
                                numPages = content(res)[[1]]$numPages))
            } else {
                updateTextInput(inputId = "titre", value = "Pas de résultat, désolé")
            }
            
            
            #### Téléchargement de l'image de couverture ----
            
            if (coverImg() == "www/covers/dummy_cover.jpg") {
                
                print("Trying Decitre for cover")
                shinyjs::html(id = "smallerloadmessage", "Trying Decitre for cover...")
                res_decitre <- GET(sprintf("https://www.decitre.fr/livres/%s.html",
                                           isbn))
                if (status_code(res_decitre) == 200 && 
                    content(res_decitre) %>%
                    html_elements("img") %>%
                    html_attr("src") %>%
                    grep(isbn, .) %>% length()) {
                    
                    imgsrc_init <- content(res_decitre) %>%
                        html_elements("img") %>%
                        html_attr("src") %>%
                        grep(isbn, ., value = T) %>%
                        `[[`(1) %>%
                        str_extract(sprintf("(.*)(?=%s)", isbn)) 
                    
                    imgsrc <- sprintf("%s%s-475x500-2.webp", imgsrc_init, isbn)
                    
                    urlImg <- "www/covers/temp_cover.webp"
                    
                    tryCatch(
                        expr = {
                            download.file(imgsrc, destfile = urlImg)
                        },
                        error = function(e) {
                            cat(sprintf("There was an error when downloading %s\n", imgsrc))
                            imgsrc <- sprintf("%s%s-475x500-1.webp", imgsrc_init, isbn)
                            download.file(imgsrc, destfile = urlImg)
                        }
                    )
                    
                    coverImg(urlImg)
                    update_coverImage()
                    reset_coverInput()
                    
                } else if (values$settings$worldcat == "Oui") {
                    print("Trying Worldcat for cover")
                    shinyjs::html(id = "smallerloadmessage", "Trying Worldcat for cover...")
                    
                    print("Setting up server for image")
                    rD <- rsDriver(browser="chrome", chromever = "105.0.5195.52",
                                   extraCapabilities = list("chromeOptions" = list(args = list('--headless'))),
                                   port=4550L, verbose=F
                    )
                    remDr <- rD$client
                    
                    print("Charging web page")
                    shinyjs::html(id = "smallerloadmessage", "Charging web page...")
                    remDr$navigate(paste0("https://www.worldcat.org/fr/search?q=", isbn))
                    Sys.sleep(5)
                    
                    print("Searching for cover image")
                    shinyjs::html(id = "smallerloadmessage", "Searching for cover image...")
                    
                    imgs <- remDr$findElements(using = "css", "img")
                    imgsrcs <- sapply(1:length(imgs), function(i) imgs[[i]]$getElementAttribute("src")[[1]]) 
                    
                    if (length(grep("coverart.oclc", imgsrcs))) {
                        
                        imgsrc <- grep("coverart.oclc", imgsrcs, value = T) %>%
                            `[[`(1)
                        
                        print(imgsrc)
                        urlImg <- "www/covers/temp_cover.jpg"
                        
                        tryCatch(
                            expr = {
                                download.file(imgsrc, urlImg)
                                coverImg(urlImg)
                                update_coverImage()
                                reset_coverInput()
                                
                                # output$resetupload <- renderUI({
                                #     actionButton(inputId = "resetupload_button",
                                #                  label = "x")
                                # })
                                # enable(id = "resetupload_button")
                                
                            },
                            error = function(e) {
                                cat(sprintf("There was an error when downloading %s\n", imgsrc))
                            }
                        )
                    }
                    
                    remDr$close()
                    rD$server$stop()
                    rD$server$process
                    rm(rD)
                    gc()
                    
                }
            }
            
        }
        
        shinyjs::html(id = "loadmessage", "")
        shinyjs::html(id = "smallerloadmessage", "")
        
        
    })
    
    
    ## Tableau de données ----
    
    ### Ajout d'un livre à la base ----
    
    # Mise à jour de la base locale
    update_db <- function() {
        fwrite(values$books_df, "data/octobooks.csv")
    }
    
    addbooks_df <- reactive({
        
        read_deb_date <- NA_POSIXct_
        if (!is.null(input$read_deb_date)) { 
            read_deb_date <- input$read_deb_date
        }
        read_fin_date <- NA_POSIXct_
        if (!is.null(input$read_fin_date)) { 
            read_fin_date <- input$read_fin_date
        }
        
        nbpages <- NA
        duree_h <- NA; duree_min <- NA
        interpreters <- NA_character_
        if(input$format == "Audio") {
            duree_h <- input$duree_h
            duree_min <- input$duree_min
            interpreters <- fifelse(length(int_inserted) == 0, NA_character_,
                                    fmt_semicol(sapply(int_inserted, function(x) input[[x]])))
        } else {
            nbpages <- input$nbpages
        }
        
        urlImg <- sprintf("www/covers/cover_%s.%s", input$isbn, file_ext(coverImg()))
        cover <- coverImg() != "www/covers/dummy_cover.jpg"
        
        if (cover) {
            file.copy(coverImg(), urlImg, overwrite = T)
        }
        
        addbooks_df <- data.frame(
            isbn = input$isbn,
            title = input$titre,
            authors = fifelse(length(aut_inserted) == 0, NA_character_,
                              fmt_semicol(sapply(aut_inserted, function(x) input[[x]]))),
            translators = fifelse(length(trad_inserted) == 0, NA_character_,
                                  fmt_semicol(sapply(trad_inserted, function(x) input[[x]]))),
            interpreters = interpreters,
            genders = paste(input$genders, collapse = ";"),
            genre = input$genre,
            pub_date = as.integer(input$pub_date),
            edition_date = as.integer(input$edition_date),
            langue_vo = input$langue_vo,
            pays_vo = input$pays_vo,
            langue = input$langue,
            format = input$format,
            pages = as.integer(nbpages),
            duree_h = as.integer(duree_h),
            duree_min = as.integer(duree_min),
            owner = input$owner,
            read = input$read,
            read_deb_date = as.POSIXct(read_deb_date, tz = "GMT"),
            read_fin_date = as.POSIXct(read_fin_date, tz = "GMT"),
            keywords = paste(input$keywords, collapse = ";"),
            cover = cover
        )
        
        return(addbooks_df)
    })
    
    
    # Ajout d'un livre
    observeEvent(input$add_button, {
        
        if (str_trim(input$isbn) == "") {
            shinyjs::html("addMessage", HTML(sprintf('<p class="error">%s</p>', "ISBN manquant")))
        } else if (input$format != "Audio" && !str_isnum(input$nbpages)) {
            shinyjs::html("addMessage", HTML(sprintf('<p class="error">%s</p>', "Nombre de pages non numérique")))
        } else if (!is.null(input$duree_h) && !str_isnum(input$duree_h) | !is.null(input$duree_min) && !str_isnum(input$duree_min)) {
            shinyjs::html("addMessage", HTML(sprintf('<p class="error">%s</p>', "Durée non numérique")))
        } else if (!is.null(input$duree_min) && input$duree_min != "" && as.integer(input$duree_min) >= 60) {
            shinyjs::html("addMessage", HTML(sprintf('<p class="error">%s</p>', "Le nombre de minutes dépasse 60")))
        } else if (!str_isnum(input$pub_date)) {
            shinyjs::html("addMessage", HTML(sprintf('<p class="error">%s</p>', "Année de première parution non numérique")))
        } else if (!str_isnum(input$edition_date)) {
            shinyjs::html("addMessage", HTML(sprintf('<p class="error">%s</p>', "Année d'édition non numérique")))
        } else if (input$isbn %in% values$books_df$isbn) {
            shinyjs::html("addMessage", HTML(sprintf('<p class="error">%s</p>', "ISBN déjà présent dans la base")))
        } else {
            values$books_df <- rbindlist(list(values$books_df, addbooks_df()))
            update_db()
            
            mess <- sprintf('<p class="success"><i>%s</i> (%s) a bien été ajouté à la base !</p>', 
                            input$titre, input$isbn)
            shinyjs::html("addMessage", HTML(mess))
            
            if (values$settings$reset == "Oui") {
                print("je reset")
                reset_add()
            } else {
                print("Je reset pas tu as cru quoi")
            }
        }
    })
    
    ### Affichage du tableau ----
    
    output$selcols <- renderUI({
        lapply(1:5, function(i) {
            column(2,
                   awesomeCheckboxGroup(paste0("selcols", i),
                                        label = NULL,
                                        choices = setNames(names(labcols)[(4*i-3):(4*i)], 
                                                           labcols[(4*i-3):(4*i)]),
                                        selected = config$selected_cols,
                                        inline = F, status = "info")
            )
        })
    })
    
    
    fmt_tbl <- function(book_table, selcols = config$selected_cols) {
        book_table$read <- code_lu[book_table$read]
        
        book_table$authors <- gsub(";", ", ", book_table$authors)
        book_table$translators <- gsub(";", ", ", book_table$translators)
        
        book_table$genders <- sapply(
            book_table$genders, 
            function(s) paste(code_genders[strsplit(s, ";")[[1]]], collapse = ", "))
        
        book_table$duree <- paste(book_table$duree_h, ":", 
                                  str_pad(book_table$duree_min, 2, "left", 0)) %>%
            gsub(x = ., "NA : NA", "")
        book_table$duree_h <- NULL
        book_table$duree_min <- NULL
        
        book_table$langue_vo <- fifelse(book_table$pays != "", 
                                        sprintf("%s (%s)", book_table$langue_vo, book_table$pays_vo),
                                        book_table$langue_vo)
        book_table$pays_vo <- NULL
        
        book_table$keywords <- gsub(";", ", ", book_table$keywords)
        
        book_table$read_deb_date <- as.Date(book_table$read_deb_date, tz = "GMT")
        book_table$read_fin_date <- as.Date(book_table$read_fin_date, tz = "GMT")
        
        book_table$cover <- fifelse(book_table$cover, "Oui", "Non")
        
        setnames(book_table, old = names(labcols), new = labcols)
        
        return(book_table[, .SD, .SDcols = labcols[selcols]])
    }
    
    output$books_tbl <- DT::renderDataTable(expr = {
        selcols <- c(input$selcols1, input$selcols2, input$selcols3,
                     input$selcols4, input$selcols5)
        
        isolate(input$books_tbl_state$start)
        
        if (is.null(isolate(input$books_tbl_state))) {
            dtable <- DT::datatable(
                fmt_tbl(values$books_df, selcols),
                selection = 'single',
                style = "bootstrap",
                rownames = FALSE,
                extensions = c("SearchBuilder", "Buttons"),
                callback = JS("$.fn.dataTable.ext.errMode = 'alert';"),
                options = list(
                    stateSave = TRUE,
                    dom = paste0("<'row'<'col-sm-12'Q>>",
                                 "<'row edit_row'<'col-sm-6'B><'col-sm-6'f>>",
                                 "<'row'<'col-sm-12'tr>>",
                                 "<'row'<'col-sm-12'i>>",
                                 "<'row'<'col-sm-12'p>>"),
                    language = list(url = "fr-FR.json"),
                    searchBuilder = list(
                        greyscale = TRUE
                    ),
                    buttons = list(
                        list(
                            extended = "collection",
                            text = '<i class="fa fa-edit"></i> Modifier',
                            action = DT::JS("function ( e, dt, node, config ) {
                                                        Shiny.setInputValue('edit_button', true, {priority: 'event'});
                                                    }")
                        ),
                        list(
                            extended = "collection",
                            text = '<i class="fa fa-trash-alt"></i> Supprimer',
                            action = DT::JS("function ( e, dt, node, config ) {
                                                        Shiny.setInputValue('delete_button', true, {priority: 'event'});
                                                    }")
                        )
                    )
                )
            )
        } else {
            dtable <- DT::datatable(
                fmt_tbl(values$books_df, selcols),
                selection = 'single',
                style = "bootstrap",
                rownames = FALSE,
                extensions = c("SearchBuilder", "Buttons"),
                callback = JS("$.fn.dataTable.ext.errMode = 'alert';"),
                options = list(
                    stateSave = TRUE,
                    order = isolate(input$books_tbl_state$order),
                    # paging = TRUE,
                    # pageLength = isolate(input$books_tbl_state$length),
                    dom = paste0("<'row'<'col-sm-12'Q>>",
                                 "<'row edit_row'<'col-sm-6'B><'col-sm-6'f>>",
                                 "<'row'<'col-sm-12'tr>>",
                                 "<'row'<'col-sm-12'i>>",
                                 "<'row'<'col-sm-12'p>>"),
                    language = list(url = "fr-FR.json"),
                    searchBuilder = list(
                        greyscale = TRUE,
                        preDefined = isolate(input$books_tbl_state$searchBuilder)
                    ),
                    displayStart = isolate(input$books_tbl_state$start),
                    buttons = list(
                        list(
                            extended = "collection",
                            text = '<i class="fa fa-edit"></i> Modifier',
                            action = DT::JS("function ( e, dt, node, config ) {
                                                        Shiny.setInputValue('edit_button', true, {priority: 'event'});
                                                    }")
                        ),
                        list(
                            extended = "collection",
                            text = '<i class="fa fa-trash-alt"></i> Supprimer',
                            action = DT::JS("function ( e, dt, node, config ) {
                                                        Shiny.setInputValue('delete_button', true, {priority: 'event'});
                                                    }")
                        )
                    )
                )
            )
        }
        dep <- htmlDependency(
            name = "DateTime",
            version = "1.1.2",
            src = "www/",
            script = "dataTables.dateTime.min.js",
            stylesheet = "dataTables.dateTime.min.css",
            all_files = FALSE
        )
        
        dtable$dependencies <- c(dtable$dependencies, list(dep))
        
        dtable
    }, server = FALSE)
    
    proxy <- dataTableProxy('books_tbl')
    
    
    ### Modification de la base ----
    
    #### Affichage du formulaire ----
    entry_form <- function(button_id) {
        
        showModal(
            modalDialog(
                id = "form-modal",
                size = "l",
                fluidPage(
                    disabled(textInput("edit_isbn", config$settings$isbnCase, width = "33%")),
                    fluidRow(
                        column(8,
                               textInput("edit_title", "Titre", width = "100%"),
                               textInput("edit_authors", "Auteurices", width = "100%"),
                               textInput("edit_translators", "Traducteurices", width = "100%"),
                               textInput("edit_interpreters", "Interprètes", width = "100%"),
                               awesomeCheckboxGroup("edit_genders",
                                                    NULL,
                                                    choices = setNames(names(code_genders), code_genders),
                                                    inline = T, status = "info")),
                        column(4,
                               div(
                                   id = "edit_imageDiv",
                                   div(
                                       id = "edit_imageSubDiv",
                                       img(id = "edit_coverImage",
                                           src = "covers/dummy_cover.jpg")
                                   ),
                                   # imageOutput("edit_coverImage",
                                   #             height = "200px"),
                                   splitLayout(
                                       class = "cover-layout",
                                       div(
                                           id = "edit_coverImageInputDiv",
                                           fileInput("edit_coverImageInput",
                                                     NULL,
                                                     accept = "image/*",
                                                     buttonLabel = img(src = "camera.webp"),
                                                     placeholder = NULL)
                                       ),
                                       # uiOutput("edit_coverImageInput"),
                                       actionButton(inputId = "edit_resetupload_button",
                                                    label = "x"),
                                       # uiOutput("edit_resetupload"),
                                       cellWidths = c("76.5%", "15%")
                                   ))
                        ),
                    ),
                    
                    fluidRow(
                        column(4,
                               awesomeRadio("edit_read", "Lu", 
                                            c("Non" = "non",
                                              "Oui" = "oui", 
                                              "Pas fini" = "dnf"),
                                            selected = "oui",
                                            inline = T,
                                            status = "info")
                        ),
                        column(4,
                               airDatepickerInput("edit_read_deb_date",
                                                  label = "Début de lecture",
                                                  language = "fr",
                                                  todayButton = T,
                                                  clearButton = T,
                                                  autoClose = TRUE)
                        ),
                        column(4,
                               airDatepickerInput("edit_read_fin_date",
                                                  label = "Fin de lecture",
                                                  language = "fr",
                                                  todayButton = T,
                                                  clearButton = T,
                                                  autoClose = TRUE)
                        ),
                    ),
                    
                    fluidRow(
                        column(4,
                               selectInput("edit_genre", "Genre littéraire",
                                           choices = values$choices$genre)
                        ),
                        column(4,
                               textInput("edit_pub_date",
                                         "Première parution")
                        ),
                        column(4,
                               splitLayout(
                                   selectInput("edit_langue_vo",
                                               "Langue VO",
                                               choices = values$choices$langue_vo),
                                   selectInput("edit_pays_vo",
                                               "(pays)",
                                               choices = values$choices$pays_vo))
                        )
                    ),
                    
                    fluidRow(
                        column(4,
                               selectInput("edit_owner",
                                           "Propriétaire",
                                           choices = values$choices$owner)
                        ),
                        column(4,
                               textInput("edit_edition_date",
                                         "Date d'édition")
                        ),
                        column(4,
                               selectInput("edit_langue",
                                           "Langue",
                                           choices = values$choices$langue)
                        ),
                    ),
                    
                    fluidRow(
                        column(4,
                               selectInput("edit_keywords",
                                           "Mots-clés",
                                           multiple = T,
                                           choices = values$choices$keywords)
                        ), 
                        column(4,
                               selectInput("edit_format",
                                           "Format",
                                           choices = values$choices$format)
                        ),
                        column(4, 
                               splitLayout(
                                   textInput("edit_nbpages", "Pages",
                                             width = "80%"),  
                                   splitLayout(
                                       textInput(inputId = "edit_duree_h",
                                                 label = "Durée (h:min)"),
                                       tagList(tags$label(" "),
                                               tags$p(":", style = "padding-top: 5px;")),
                                       tagList(tags$label(id = "edit_duree_min-label", " "),
                                               tags$input(id = "edit_duree_min", type = "text",
                                                          class = "form-control shiny-bound-input shinyjs-resettable",
                                                          maxlength="2")),
                                       cellWidths = c("45%","2%", "40%", "13%")),
                                   cellWidths = c("55%", "45%")
                               )
                               
                        ),
                        
                    ),
                ),
                easyClose = TRUE,
                footer = tagList(modalButton("Annuler"),
                                 actionButton(button_id, "Valider", 
                                              class = "orange-btn"))
            )
        )
    }
    
    #### Format : nombre de pages et durée ----
    observeEvent(input$edit_format, {
        if (input$edit_format == "Audio") {
            enable(selector = "#edit_duree_h")
            enable(selector = "#edit_duree_min")
            updateTextInput(session, inputId = "edit_nbpages", value = NA)
            disable(selector = "#edit_nbpages")
            enable(selector = "#edit_interpreters")
        } else {
            disable(selector = "#edit_duree_h")
            disable(selector = "#edit_duree_min")
            updateTextInput(session, inputId = "edit_duree_h", value = NA)
            updateTextInput(session, inputId = "edit_duree_min", value = NA)
            enable(selector = "#edit_nbpages")
            disable(selector = "#edit_interpreters")
            updateTextInput(session, inputId = "edit_interpreters", value = NA)
        }
    })
    
    #### Dates de lecture ----
    observeEvent(input$edit_read, {
        if (input$edit_read == "non") {
            updateAirDateInput(session, inputId = "edit_read_deb_date", value = NULL, clear = T)
            updateAirDateInput(session, inputId = "edit_read_fin_date", value = NULL, clear = T)
            disable(id = "edit_read_deb_date")
            disable(id = "edit_read_fin_date")
        } else {
            enable(id = "edit_read_deb_date")
            if (input$edit_read == "oui") {
                enable(id = "edit_read_fin_date")
                updateAirDateInput(session, "edit_read_fin_date",
                                   value = input$edit_read_deb_date,
                                   options = list(minDate = input$edit_read_deb_date))
            } else {
                disable(id = "edit_read_fin_date")
                updateAirDateInput(session, inputId = "edit_read_fin_date", value = NULL, clear = T)
            }
        }
    })
    
    # Adaptation de la date de fin à partir de la date de début
    observeEvent(input$edit_read_deb_date, {
        if (input$edit_read == "oui") {
            updateAirDateInput(session, "edit_read_fin_date",
                               value = input$edit_read_deb_date,
                               options = list(minDate = input$edit_read_deb_date))
        }
    })
    
    
    #### Image ----
    
    edit_coverImg <- reactiveVal(value = "www/covers/dummy_cover.jpg")
    
    update_edit_coverImage <- function() {
        shinyjs::runjs(
            sprintf("
                    var edit_cover = document.getElementById('edit_coverImage');
                    edit_cover.setAttribute('src', '%s');
                    ", 
                    sprintf("%s?%i", gsub("www/", "", edit_coverImg()), as.integer(Sys.time())))
        )
    }
    
    observeEvent(input$edit_coverImageInput, {
        urlImg <- paste0("www/covers/temp_cover.",
                         file_ext(input$edit_coverImageInput$datapath))
        file.copy(input$edit_coverImageInput$datapath, urlImg, overwrite = T)
        edit_coverImg(urlImg)
        update_edit_coverImage()
        
        enable("edit_resetupload_button")
    })
    
    observeEvent(input$edit_resetupload_button, {
        
        disable(id = "edit_resetupload_button")
        edit_coverImg("www/covers/dummy_cover.jpg")
        update_edit_coverImage()
        
        removeUI("#edit_coverImageInputDiv")
        insertUI(selector = "#edit_imageDiv .cover-layout div:first",
                 where = "afterBegin",
                 div(
                     id = "edit_coverImageInputDiv",
                     fileInput("edit_coverImageInput",
                               NULL,
                               accept = "image/*",
                               buttonLabel = img(src = "camera.webp"),
                               placeholder = NULL)
                 )
        )
    })
    
    #### Récupération des valeurs du formulaire ----
    editForm <- reactive({
        
        edit_read_deb_date <- NA_POSIXct_
        if (!is.null(input$edit_read_deb_date)) { 
            edit_read_deb_date <- input$edit_read_deb_date
        }
        edit_read_fin_date <- NA_POSIXct_
        if (!is.null(input$edit_read_fin_date)) { 
            edit_read_fin_date <- input$edit_read_fin_date
        }
        
        urlImg <- sprintf("www/covers/cover_%s.%s", input$edit_isbn, 
                          file_ext(edit_coverImg()))
        print(urlImg)
        if (edit_coverImg() != urlImg) {
            imgfiles <- grep(input$edit_isbn, list.files(path = "www/covers/"), value = T)
            if (length(imgfiles)) {
                file.remove(paste0("www/covers/", imgfiles))
            }
            file.copy(edit_coverImg(), urlImg, overwrite = T)
            
        }
        cover <- md5sum("www/covers/dummy_cover.jpg") != md5sum(urlImg)
        
        editForm <- data.frame(
            isbn = input$edit_isbn,
            title = input$edit_title,
            authors = fmt_semicol(str_trim(strsplit(input$edit_authors, ",")[[1]])),
            translators = fmt_semicol(str_trim(strsplit(input$edit_translators, ",")[[1]])),
            interpreters = fmt_semicol(str_trim(strsplit(input$edit_interpreters, ",")[[1]])),
            genders = paste(input$edit_genders, collapse = ";"),
            pages = as.integer(input$edit_nbpages),
            duree_h = as.integer(input$edit_duree_h),
            duree_min = as.integer(input$edit_duree_min),
            genre = input$edit_genre,
            pub_date = as.integer(input$edit_pub_date),
            edition_date = as.integer(input$edit_edition_date),
            langue_vo = input$edit_langue_vo,
            pays_vo = input$edit_pays_vo,
            langue = input$edit_langue,
            format = input$edit_format,
            owner = input$edit_owner,
            read = input$edit_read,
            read_deb_date = as.POSIXct(edit_read_deb_date, tz = "GMT"),
            read_fin_date = as.POSIXct(edit_read_fin_date, tz = "GMT"),
            keywords = paste(input$edit_keywords, collapse = ";"),
            cover = cover
        )
        
        return(editForm)
    })
    
    
    #### Updates ----
    observeEvent(input$edit_button, {
        
        showModal(
            if (length(input$books_tbl_rows_selected) > 1){
                modalDialog(
                    title = "Attention",
                    paste("Ne choisissez qu'une seule ligne !" ),
                    easyClose = TRUE,
                    footer = modalButton("Fermer"))
            } else if (length(input$books_tbl_rows_selected) < 1){
                modalDialog(
                    title = "Attention",
                    paste("Choisissez une ligne !"),
                    easyClose = TRUE,
                    footer = modalButton("Fermer"))
            })  
        
        if (length(input$books_tbl_rows_selected) == 1) {
            
            entry_form("submit_edit")
            
            book_values <- values$books_df[input$books_tbl_rows_selected,]
            
            updateTextInput(session, "edit_isbn", value = book_values$isbn)
            updateTextInput(session, "edit_title", value = book_values$title)
            updateTextInput(session, "edit_nbpages", value = book_values$pages)
            updateTextInput(session, "edit_duree_h", value = book_values$duree_h)
            updateTextInput(session, "edit_duree_min", value = str_pad(book_values$duree_min, 2, "left", 0))
            updateSelectInput(session, "edit_read", selected = book_values$read)
            updateSelectInput(session, "edit_genre", selected = book_values$genre)
            updateTextInput(session, "edit_pub_date", value = book_values$pub_date)
            updateTextInput(session, "edit_edition_date", value = book_values$edition_date)
            updateSelectInput(session, "edit_langue_vo", selected = book_values$langue_vo)
            updateSelectInput(session, "edit_pays_vo", selected = book_values$pays_vo)
            updateSelectInput(session, "edit_langue", selected = book_values$langue)
            updateSelectInput(session, "edit_format", selected = book_values$format)
            updateSelectInput(session, "edit_owner", selected = book_values$owner)
            updateTextInput(session, "edit_authors", 
                            value = gsub(";", ", ", book_values$authors))
            updateTextInput(session, "edit_translators", 
                            value = gsub(";", ", ", book_values$translators))
            updateTextInput(session, "edit_interpreters", 
                            value = gsub(";", ", ", book_values$interpreters))
            updateAwesomeCheckboxGroup(session, "edit_genders", 
                                       selected = strsplit(book_values$genders, ";")[[1]])
            updateSelectInput(session, "edit_keywords", 
                              selected = strsplit(book_values$keywords, ";")[[1]])
            
            # Format
            if(book_values$format == "Audio") {
                enable(selector = "#edit_duree_h")
                enable(selector = "#edit_duree_min")
                disable(selector = "#edit_nbpages")
            } else {
                disable(selector = "#edit_duree_h")
                disable(selector = "#edit_duree_min")
                enable(selector = "#edit_nbpages")
            }  
            
            # Dates
            if (book_values$read == "non") {
                disable("edit_read_deb_date")
                disable("edit_read_fin_date")
            } else {
                if (is.na(book_values$read_deb_date)) {
                    updateAirDateInput(session, 
                                       inputId = "edit_read_deb_date", 
                                       value = NULL)
                } else {
                    updateAirDateInput(session, 
                                       inputId = "edit_read_deb_date", 
                                       value = book_values$read_deb_date)
                }
                if (book_values$read == "oui") {
                    if (is.na(book_values$read_fin_date)) {
                        updateAirDateInput(session, 
                                           inputId = "edit_read_fin_date", 
                                           value = NULL)
                    } else {
                        updateAirDateInput(session, 
                                           inputId = "edit_read_fin_date", 
                                           value = book_values$read_fin_date)
                    }
                } else {
                    disable("edit_read_fin_date")
                }
            }
            
            # Image 
            
            img_path <- grep(book_values$isbn, list.files(path = "www/covers/"), value = T)
            
            if (length(img_path)) {
                edit_coverImg(paste0("www/covers/", img_path))
            } else {
                edit_coverImg("www/covers/dummy_cover.jpg")
            }
            
            toggleState("edit_resetupload_button", condition = book_values$cover)
            
            update_edit_coverImage()
        }
    })
    
    observeEvent(input$submit_edit, {
        
        edit_values <- editForm()
        
        cols <- c("title", "authors", "translators", "interpreters", "genders", "genre", 
                  "pub_date", "edition_date", "langue_vo", "pays_vo", "langue", 
                  "format",  "pages", "duree_h", "duree_min", "owner", 
                  "read", "read_deb_date", "read_fin_date", "keywords", "cover")
        values$books_df[input$books_tbl_row_last_clicked, cols] <- edit_values[cols]
        
        update_db()
        removeModal()
        
    })
    
    
    ### Suppression de lignes ----
    
    deleteData <- reactive({
        selected_ids <- values$books_df[input$books_tbl_rows_selected, isbn]
        values$books_df <- values$books_df[!(isbn %in% selected_ids)]
        
        file.remove(paste0("www/covers/", 
                           grep(selected_ids, list.files(path = "www/covers/"), value = T)))
        
    })
    
    observeEvent(input$delete_button, {
        
        nbrows <- length(input$books_tbl_rows_selected)
        if (nbrows < 1) {
            showModal(
                modalDialog(
                    title = "Attention",
                    paste("Choisissez au moins une ligne !"), 
                    easyClose = TRUE,
                    footer = modalButton("Fermer")
                )
            )
        }
        if (nbrows >= 1) {
            book_values <- values$books_df[input$books_tbl_rows_selected,]
            
            msg <- values$books_df[input$books_tbl_rows_selected, 
                                   sprintf("- <i>%s</i>, %s (%s)", title, authors, isbn)] %>% 
                paste(collapse = "<br>")
            
            showModal(
                modalDialog(
                    title = "Attention",
                    HTML("Êtes-vous sûr·e de vouloir supprimer :<br>", msg),
                    easyClose = TRUE,
                    footer = tagList(modalButton("Annuler"),
                                     actionButton("confirm_delete", "Supprimer", 
                                                  class = "orange-btn"))
                )
            )
        }
    })
    
    observeEvent(input$confirm_delete, {
        deleteData()
        update_db()
        
        removeModal()
    })
    
    
    ## Statistiques ----
    
    fmt_dtplot <- function(d, onlyread, bydate, date_deb = NA, date_fin = NA) {
        
        lu <- list("TRUE" = c("oui"), 
                   "FALSE" = c("oui", "dnf", "non"))[onlyread][[1]]
        
        if (bydate) {
            return(d[read %in% lu & between(read_fin_date, date_deb, date_fin),])
        } else {
            return(d[read %in% lu,])
        }
    }

    
    ### Bilan global ----
    
    
    output$plot_count <- renderPlot({
        d <- values$books_df
        rbind(
            rbind(d[, .(x = "Total", .N)],
                  d[read == "oui", .(x = "Lu", .N)])[, numplot := 1],
            d[, .N, by = .(x = format(read_fin_date, "%Y"))
            ][is.na(x), x := "Non daté"][, numplot := 2]
        ) %>%
            ggplot(aes(x = x, y = N, label = N)) +
            geom_bar(stat = "identity") +
            scale_y_continuous(limits = c(0, d[, .N] + 10)) +
            geom_text(vjust = -0.5, size = 5) +
            facet_wrap(~numplot, scales = "free_x") +
            labs(y = NULL, x = NULL) +
            theme(text = element_text(size = 16),
                  panel.grid.major.x = element_blank(),
                  strip.background = element_blank(),
                  strip.text = element_blank())
    })
    
    
    # output$plot_read <- renderPlot({
    #     
    #     values$books_df[, .(p = .N/books[, .N]), 
    #                     by = .(Lu = code_lu[read])] %>%
    #         ggplot(aes(x = "", y = p, fill = Lu)) +
    #         geom_bar(stat = "identity", width = 1) +
    #         scale_fill_brewer("", palette = "Pastel1") +
    #         coord_polar("y", start = 0) +
    #         geom_text(aes(label = paste0(round(p*100), "%")), 
    #                   position = position_stack(vjust = 0.5)) +
    #         labs(title = sprintf("%s livres dans la base", 
    #                              values$books_df[, .N])) +
    #         theme_void() +
    #         theme(plot.title = element_text(hjust = 0.5))
    #     
    # })
    # 
    # output$plot_year <- renderPlot({
    #     books[!is.na(read_fin_date)] %>%
    #         ggplot() +
    #         geom_bar(aes(format(read_fin_date, "%Y"))) +
    #         labs(x = "Année de fin de lecture",
    #              y = "Nombre de livres")
    # })
    
    
    ### Bilan par catégorie ----
    
    output$cat_table <- render_gt({
        
        sel_cat <- input$stat_cat
        dtplot <- fmt_dtplot(values$books_df, 
                             input$cat_onlyread, input$cat_readbydate,
                             input$cat_deb_read, input$cat_fin_read)
        
        dtplot <- rbind(dtplot[, .N, by = sel_cat][order(-N)],
                        dtplot[, .("Total", .N)], use.names = FALSE)
        
        if (sel_cat == "genders") {
            dtplot[, genders := sapply(genders, function(x) {
                paste(c(code_genders, "Total" = "Total"
                )[str_split(x, ";")[[1]]], collapse = ", ")})]
        }
        
        dtplot %>%
            gt() %>%
            tab_options(table.font.size = px(14),
                        column_labels.border.top.style = "hidden") %>%
            cols_label(.list = setNames(c("", "Livres"),
                                        c(sel_cat, "N"))) %>%
            cols_align(columns = 2, align = "center") %>%
            cols_width(1 ~ px(200),
                       2 ~ px(100)) %>%
            tab_style(style = cell_borders(sides = "top",
                                           color = "#D3D3D3",
                                           weight = px(2)),
                      locations = cells_body(rows = nrow(dtplot)))
    }
    )
    
    output$cat_plot <- renderPlot({
        
        sel_cat <- input$stat_cat
        dtplot <- fmt_dtplot(values$books_df, 
                             input$cat_onlyread, input$cat_readbydate,
                             input$cat_deb_read, input$cat_fin_read)
        
        dtplot <- dtplot[, .(p = .N/dtplot[, .N]), by = sel_cat]
        
        if (sel_cat == "genders") {
            dtplot[, genders := sapply(genders, function(x) {
                paste(code_genders[str_split(x, ";")[[1]]], collapse = ", ")})]
        }
        
        dtplot %>%
            ggplot(aes(x = "", y = p, fill = get(sel_cat))) +
            geom_bar(stat = "identity", width = 1) +
            scale_fill_brewer("", palette = "Pastel1") +
            coord_polar("y", start = 0) +
            geom_text(aes(label = paste0(round(p*100), "%")),
                      position = position_stack(vjust = 0.5),
                      size = 5) +
            theme_void() +
            theme(text = element_text(size = 16))
    })
    
    observeEvent(input$cat_onlyread, {
        toggle(id = "cat_readbydate", 
               condition = as.logical(input$cat_onlyread))
        
        if (!as.logical(input$cat_onlyread)) {
            hide(id = "cat_read_date")
            updateAwesomeCheckbox(session,
                                  "cat_readbydate",
                                  value = F)
        }
    })
    
    # observe({
    #     toggle(id = "cat_readbydate", 
    #            condition = as.logical(input$cat_onlyread))
    # })
    
    observe({
        toggle(id = "cat_read_date", 
               condition = input$cat_readbydate)
    })
    
    
    
    ## Préférences ----
    
    ### Colonnes sélectionnées par défaut ----
    output$select_newdefcols <- renderUI({
        fluidRow(
            lapply(1:5, function(i) {
                column(2,
                       awesomeCheckboxGroup(paste0("newdefcols", i),
                                            label = NULL,
                                            choices = setNames(names(labcols)[(4*i-3):(4*i)], labcols[(4*i-3):(4*i)]),
                                            selected = values$selected_cols,
                                            inline = F, status = "info")) 
            })
        )
    })
    
    observeEvent(input$change_defcols_button, {
        newselcols <- c(input$newdefcols1, input$newdefcols2, input$newdefcols3, 
                        input$newdefcols4, input$newdefcols5)
        values$selected_cols <- newselcols
        
        for(i in 1:6) {
            updateAwesomeCheckboxGroup(session,
                                       paste0("selcols", i),
                                       selected = newselcols)
        }
        
        write_yaml(values$selected_cols, "config/selected_cols.yml")
        shinyjs::html("newdefcolsMessage", HTML("<p class='success'>Les colonnes sélectionnées par défaut ont bien été modifiées !</p>"))
    })
    
    observeEvent(c(input$newdefcols), {
        shinyjs::html("newdefcolsMessage", "")
    })
    
    ### Modification des choix par défaut ----
    output$defvalue <- renderUI({
        
        if (input$coltochange == "read") {
            availchoices <- c("Non" = "non", 
                              "Oui" = "oui", 
                              "Pas fini" = "dnf")
        } else {
            availchoices <- values$choices[[input$coltochange]]
        }
        
        selectInput("newdefvalue",
                    "Valeur par défaut :",
                    choices = availchoices, 
                    selected = values$default_choices[[input$coltochange]])
    })
    
    observeEvent(input$change_default_button, {
        values$default_choices[[input$coltochange]] <- input$newdefvalue
        # print(values$default_choices)
        write_yaml(values$default_choices, "config/default_choices.yml")
        shinyjs::html("newdefvalueMessage", HTML("<p class='success'>La valeur par défaut a bien été modifiée !</p>"))
    })
    
    observeEvent(c(input$coltochange, input$newdefvalue), {
        shinyjs::html("newdefvalueMessage", "")
    })
    
    
    ### Ajout de nouveaux choix ----
    newval <- reactiveVal()
    newcol <- reactiveVal("genre")
    
    output$select_coltoaddto <- renderUI({
        selectInput("coltoaddto",
                    "Colonne :",
                    choices = setNames(
                        names(values$choices),
                        c(labcols, pays_vo = "Pays VO")[names(values$choices)]),
                    selected = newcol())
    })
    
    observeEvent(input$coltoaddto, {
        newcol(input$coltoaddto)
        shinyjs::html("availChoices",
                      HTML(sprintf("- %s<br>", grep("[a-zA-Z0-9]+", values$choices[[newcol()]], value = T)))
        )
    })
    
    observeEvent(input$add_choice_button, {
        
        newval(str_squish(input$newchoice))
        if (newval() == "") {
            shinyjs::html("newchoiceError", HTML("<p class='error'>Attention, aucune valeur n'a été rentrée !</p>"))
        } else {
            shinyjs::html("newchoiceError", "")
            
            if (input$coltoaddto == "keywords") {
                newval(gsub(" ", "-", tolower(newval())))
            } else {
                newval(str_to_sentence(newval()))
            }
            
            newcol(input$coltoaddto)
            
            showModal(
                modalDialog(
                    title = "Attention",
                    HTML("Êtes-vous sûr·e de vouloir ajouter le choix <strong>", newval(),
                         "</strong> à la catégorie <strong>", labcols[input$coltoaddto],
                         "</strong> ?"),
                    easyClose = TRUE,
                    footer = tagList(modalButton("Annuler"),
                                     actionButton("confirm_newchoice", "Ajouter", 
                                                  class = "orange-btn"))
                )
            )
        }
    })
    
    observeEvent(input$confirm_newchoice, {
        values$choices[[newcol()]] <- sort(c(values$choices[[newcol()]], newval()))
        
        write_yaml(values$choices, "config/choices.yml")
        
        newval(NULL)
        updateTextInput(session, "newchoice", value = "")
        removeModal()
    })
    
    observeEvent(c(input$coltoaddto, input$newchoice), {
        shinyjs::html("newchoiceError", "")
    })
    
    ### Réglages ----
    
    observeEvent(input$set_reset, {
        values$settings$reset <- input$set_reset
    })
    
    observeEvent(input$set_worldcat, {
        values$settings$worldcat <- input$set_worldcat
    })
    
    
    output$set_pageLength <- renderUI({
        selectInput("set_pageLength",
                    "Nombre de lignes à afficher dans le tableau :",
                    choices = c(10, 25, 50, 100),
                    selected = values$settings$pageLength)
    })
    
    observeEvent(input$set_pageLength, {
        values$settings$pageLength <- input$set_pageLength
    })
    
    
    output$set_isbnCase <- renderUI({
        selectInput("set_isbnCase",
                    "Affichage de l'acronyme isbn :",
                    choices = c("isbn",
                                "Isbn",
                                "ISBN"),
                    selected = values$settings$isbnCase)
    })
    
    observeEvent(input$set_isbnCase, {
        values$settings$isbnCase <- input$set_isbnCase
    })
    
    
    observeEvent(values$settings, {
        print("Updating settings file")
        write_yaml(values$settings, "config/settings.yml")
    }, ignoreInit = TRUE)
    
    session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)
