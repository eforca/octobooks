tabPanel("Stats",
         value = "stats",
         navlistPanel(
             id = "stats_nav",
             tabPanel("Bilan global",
                      h3("Ensemble des livres"),
                      h5(em("Le nombre de livres est précisé entre parenthèses"), style = "color: silver;"),
                      fluidRow(
                          column(10,
                                 plotOutput("plot_count_tot",
                                            height = "400px")
                          )
                      ),
                      h3("Nombre de livres par an"),
                      h5(em("L'année correspond à la date de fin de lecture"), style = "color: silver;"),
                      fluidRow(
                          column(10,
                                 plotOutput("plot_count_year",
                                            height = "300px")
                          )
                      )
             ),
             tabPanel("Bilan par catégorie",
                      value = "stats_bycat",
                      fluidRow(
                          id = "select_cat_row",
                          column(5,
                                 id = "select_cat_col1",
                                 selectInput("stat_cat", 
                                             "Catégorie",
                                             width = "100%",
                                             choices = setNames(stat_cats,
                                                                labcols[stat_cats])),
                                 
                                 
                                 pickerInput(
                                     inputId = "stat_cat_year",
                                     label = "Années", 
                                     width = "100%",
                                     choices = NULL,
                                     multiple = TRUE,
                                     options = pickerOptions(
                                         actionsBox = TRUE,
                                         liveSearch = TRUE,
                                         noneSelectedText = "Ensemble des livres",
                                         noneResultsText = "Aucun résultat",
                                         selectAllText = "Tout sélectionner",
                                         deselectAllText = "Tout désélectionner",
                                         selectedTextFormat = "count > 4",
                                         countSelectedText = "{0} années sélectionnées"
                                     )
                                 ),
                                 gt_output("cat_table")
                          ),
                          column(7,
                                 id = "select_cat_col2",
                                 awesomeRadio("stat_cat_fmt", "", 
                                              c("Livres" = "N",
                                                "%" = "p"),
                                              selected = "N",
                                              inline = TRUE,
                                              status = "info"),
                                 pickerInput(
                                     inputId = "stat_cat_choices",
                                     label = "", 
                                     width = "fit",
                                     inline = TRUE,
                                     choices = NULL,
                                     multiple = TRUE,
                                     options = pickerOptions(
                                         # actionsBox = TRUE,
                                         maxOptions = 8,
                                         liveSearch = TRUE,
                                         noneSelectedText = "Aucun choix",
                                         noneResultsText = "Aucun résultat",
                                         selectAllText = "Tout sélectionner",
                                         deselectAllText = "Tout désélectionner",
                                         selectedTextFormat = "count",
                                         countSelectedText = "{0} choix",
                                         maxOptionsText = "Vous ne pouvez sélectionner que 9 choix à la fois"
                                     ),
                                 ),
                                 plotOutput("cat_plot",
                                            height = "500px")
                          )
                      ),
                      # fluidRow(
                      #     id = "set_cat_row",
                      #     column(9,
                      #            awesomeRadio("cat_onlyread",
                      #                         label = NULL,
                      #                         choices = c("Tous les livres" = FALSE,
                      #                                     "Livres lus" = TRUE),
                      #                         inline = T,
                      #                         status = "info"),
                      #            awesomeCheckbox("cat_readbydate",
                      #                            "Par date de fin de lecture",
                      #                            status = "info"),
                      #            splitLayout(id = "cat_read_date",
                      #                        div("Fini entre", style = "padding-top : 5px"),
                      #                        airDatepickerInput("cat_deb_read",
                      #                                           label = NULL,
                      #                                           value = this_year[1]),
                      #                        div("et", style = "padding-top : 5px"),
                      #                        airDatepickerInput("cat_fin_read",
                      #                                           label = NULL,
                      #                                           value = this_year[2]),
                      #                        cellWidths = c("11%", "30%", "4%", "30%", "25%"),
                      #                        cellArgs = list(style = "text-align : center; vertical"))
                      #     ),
                      # ),
                      
             ),
             widths = c(2,9)
         ),
)