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