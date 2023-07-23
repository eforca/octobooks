tabPanel("Ajouter",
         value = "ajouter",
         fluidPage(
             
             # Side panel ----
             column(3,
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
                    ),
                    splitLayout(
                        style = "width: 300px; max-width: 100%;",
                        cellWidths = c("65%", "35%"),
                        checkboxGroupButtons("onmyshelf",
                                             choices = c("Dans ma bibliothèque" = TRUE),
                                             selected = config$default_choices$onmyshelf,
                                             status = "theme-light"),
                        tags$div(
                            style = "padding-top: 7px;",
                            awesomeCheckbox(
                                inputId = "signed",
                                label = "Dédicacé", 
                                value = FALSE,
                                status = "info"
                            ))
                    ),
                    # fluidRow(
                    #     column(6,s
                    #            checkboxGroupButtons("onmyshelf",
                    #                                 choices = c("Dans ma bibliothèque" = TRUE),
                    #                                 status = "theme-light")),
                    #     column(6,
                    #            checkboxGroupButtons("signed",
                    #                                 choices = c("Dédicacé" = TRUE),
                    #                                 status = "theme-light"))
                    # ),
                    awesomeRadio("read", "Lu", 
                                 c("Non" = "non",
                                   "Oui" = "oui", 
                                   "Pas fini" = "dnf"),
                                 selected = config$default_choices$read,
                                 inline = T,
                                 status = "info"),
                    tags$div(id = "read_date_div"),
             ),
             
             # Panel central ----
             column(9,
                    id = "add-panel",
                    
                    ## Titres, auteurices, image ----
                    fluidRow(
                        column(8,
                               
                               # Titre
                               textInput("titre", h3("Titre"),
                                         placeholder = "",
                                         width = "100%"),
                               
                               # Titre VO
                               hidden(
                                   tags$div(id = "titreVoDiv",
                                            textInput("titre_vo",
                                                      "Titre original",
                                                      placeholder = "",
                                                      width = "100%")
                                   )),
                               
                               # Auteurices
                               tags$div(id = "auteuricesMainDiv",
                                        strong("Auteurices"),
                                        actionButton('insertAutBtn', '+', 
                                                     class = "coloured-btn autandtrad"), 
                                        actionButton('removeAutBtn', '-', 
                                                     class = "coloured-btn autandtrad"),
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
                                                     class = "coloured-btn autandtrad"), 
                                        actionButton('removeTradBtn', '-', 
                                                     class = "coloured-btn autandtrad"),
                                        tags$div(id = "traducteuricesSubDiv",
                                                 fluidRow(id = "traducteuricesRow"))),
                               
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
                                       cellWidths = c("185px", "30px")
                                   ))
                        ),
                    ),
                    
                    ## Première ligne ----
                    fluidRow(
                        column(4, 
                               selectInput("genre", "Genre littéraire",
                                           choices = config$choices$genre,
                                           selected = config$default_choices$genre)
                        ),
                        column(4,
                               textInput("pub_date",
                                         "Année de première parution",
                                         placeholder = "")
                        ),
                        column(4,
                               splitLayout(
                                   style = "width: 296px; max-width: 100%;",
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
                    
                    ## Deuxième ligne ----
                    fluidRow(
                        column(4,
                               selectInput("format",
                                           "Format",
                                           choices = config$choices$format,
                                           selected = config$default_choices$format)
                        ),
                        column(4,
                               textInput("edition_date",
                                         "Année de l'édition",
                                         placeholder = "")),
                        column(4,
                               selectInput("langue",
                                           "Langue de l'édition",
                                           choices = config$choices$langue,
                                           selected = config$default_choices$langue)
                        )
                    ),
                    
                    ## Troisième ligne ----
                    fluidRow(
                        column(4,
                               selectInput("acqui_type",
                                           "Type d'acquisition",
                                           choices = config$choices$acqui_type,
                                           selected = config$default_choices$acqui_type)
                        ),
                        column(4,
                               textInput("acqui_date",
                                         "Année d'acquisition",
                                         placeholder = "")
                        ),
                        column(4,
                               selectInput("acqui_state",
                                           "État d'acquisition",
                                           choices = config$choices$acqui_state,
                                           selected = config$default_choices$acqui_state
                               )
                        )
                    ),
                    
                    ## Quatrième ligne ----
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
                    
                    ## Messages d'erreur et bouton ajouter ----
                    fluidRow(
                        id = "bottom-row",
                        column(8,
                               div(id = "addMessage")
                        ),
                        column(4,
                               tags$div(
                                   style = "width: 300px; max-width: 100%;",
                                   actionButton(inputId = "reinit_button", 
                                                label = "Réinitialiser"),
                                   actionButton(inputId = "add_button", 
                                                label = "Ajouter",
                                                class = "coloured-btn")   
                               )
                        ),
                    )
             ),
         ),
         div(id = "progress-message")
)