tabPanel("Préférences",
         value = "pref",
         navlistPanel(
             id = "pref_nav",
             tabPanel("Tableau de données",
                      h3("Colonnes à afficher par défaut"),
                      uiOutput("select_newdefcols"),
                      
                      actionButton(inputId = "change_defcols_button", 
                                   label = "Modifier",
                                   class = "coloured-btn"),
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
                                                  class = "coloured-btn"),
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
                                                  class = "coloured-btn"),
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
                      # fluidRow(
                      #     column(10,
                      #            awesomeRadio("set_worldcat",
                      #                         HTML("Optimiser la recherche d'image de courverture, notamment pour les éditions non-françaises :<br>
                      #                              <span style='font-weight:400; color:silver'>Attention, cette option peut rallonger significativement le temps de recherche</span>"),
                      #                         choices = c("Oui", "Non"),
                      #                         selected = config$settings$worldcat,
                      #                         inline = T,
                      #                         status = "info")
                      #     ),
                      # ),
                      fluidRow(
                          column(10,
                                 colorPickr("set_themeColour", 
                                            "Changer la couleur du thème :",
                                            selected = config$settings$themeColour,
                                            update = "change",
                                            swatches = c("#EEA236",
                                                         "#990000",
                                                         "#6A0D83",
                                                         "#0B0672",
                                                         "#89A6FF",
                                                         "#8BB9B9",
                                                         config$settings$themeColour),
                                            pickr_width = "20em",
                                            theme = "monolith",
                                            position = "right-end",
                                            preview = FALSE,
                                            interaction = list(
                                                hex= FALSE,
                                                rgba = FALSE,
                                                input = TRUE,
                                                save = FALSE,
                                                clear = FALSE
                                            )
                                 )
                          )
                      ),
                      br(),
                      # fluidRow(
                      #     column(8,
                      #            id = "set_pageLength_col",
                      #            selectInput("set_pageLength",
                      #                        HTML("Nombre de lignes à afficher dans le tableau :<br>
                      #                              <span style='font-weight:400; color:silver'>Attention, ce réglage ne s'appliquera qu'à la réouverture de l'application</span>"),
                      #                        choices = c(10, 25, 50, 100),
                      #                        width = "100%",
                      #                        selected = config$settings$pageLength)
                      #     ),
                      # ),
                      fluidRow(
                          column(8,
                                 id = "set_isbnCase_col",
                                 selectInput("set_isbnCase",
                                             HTML("Affichage de l'acronyme isbn :<br>
                                                           <span style='font-weight:400; color:silver'>Attention, ce réglage ne s'appliquera qu'à la réouverture de l'application</span>"),
                                             choices = c("isbn",
                                                         "Isbn",
                                                         "ISBN"),
                                             width = "100%",
                                             selected = config$settings$isbnCase)
                          ),
                      )
             ),
             widths = c(2,9)
         )
)