tabPanel("Table", 
         value = "table",
         fluidRow(
             id = "selcols-row",
             # column(1),
             uiOutput("selcols"),
         ),
         tags$div(id = "table_div",
                  fluidRow(
                      DT::DTOutput("books_tbl"),
                      downloadButton("download_button",
                                     "Télécharger (.csv)",
                                     style = "color: black !important;")
                  )
         )
)