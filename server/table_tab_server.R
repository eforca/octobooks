# Affichage du tableau ----

## Choix de colonnes ----
output$selcols <- renderUI({
    lapply(1:(length(labcols)%/%nb_per_col + 1), function(i) {
        if (i > length(labcols)%/%nb_per_col) {
            li <- nb_per_col*i-(nb_per_col-1); hi <- nb_per_col*(i-1) + length(labcols)%%nb_per_col
        } else {
            li <- nb_per_col*i-(nb_per_col-1); hi <- nb_per_col*i   
        }
        column(2,
               awesomeCheckboxGroup(paste0("selcols", i),
                                    label = NULL,
                                    choices = setNames(names(labcols)[li:hi], 
                                                       labcols[li:hi]),
                                    selected = user_config$selected_cols,
                                    inline = F, status = "info")
        )
    })
})


## Formatage du tableau ----
fmt_tbl <- function(book_table, selcols = user_config$selected_cols) {
    
    book_table$read <- code_lu[book_table$read]
    
    book_table$authors <- gsub(";", ", ", book_table$authors)
    book_table$translators <- gsub(";", ", ", book_table$translators)
    book_table$interpreters <- gsub(";", ", ", book_table$interpreters)
    
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
    book_table$score <- fifelse(book_table$score == "*", "★", book_table$score)
    book_table$onmyshelf <- fifelse(book_table$onmyshelf, "Oui", "Non")
    book_table$signed <- fifelse(book_table$signed, "Oui", "Non")
    
    
    
    setnames(book_table, old = names(labcols), new = labcols)
    
    return(book_table[, .SD, .SDcols = labcols[selcols]])
}

## Affichage du tableau ----
output$books_tbl <- DT::renderDataTable(expr = {
    selcols <- c(input$selcols1, input$selcols2, input$selcols3,
                 input$selcols4, input$selcols5, input$selcols6)
    
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
                scrollX = TRUE,
                stateSave = TRUE,
                # pageLength = config$settings$pageLength,
                dom = paste0("<'row'<'col-sm-12'Q>>",
                             "<'row edit_row'<'col-sm-6'B><'col-sm-6'f>>",
                             "<'row'<'col-sm-12'tr>>",
                             "<'row'<'col-sm-12'i>>",
                             "<'row'<'col-sm-12'p>>"),
                language = list(url = "fr-FR.json"),
                search = list(caseInsensitive = TRUE),
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
                scrollX = TRUE,
                stateSave = TRUE,
                order = isolate(input$books_tbl_state$order),
                # paging = TRUE,
                # pageLength = isolate(input$books_tbl_state$length),
                # pageLength = config$settings$pageLength,
                dom = paste0("<'row'<'col-sm-12'Q>>",
                             "<'row edit_row'<'col-sm-6'B><'col-sm-6'f>>",
                             "<'row'<'col-sm-12'tr>>",
                             "<'row'<'col-sm-12'i>>",
                             "<'row'<'col-sm-12'p>>"),
                language = list(url = "fr-FR.json"),
                search = list(caseInsensitive = TRUE),
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

## Téléchargement du tableau ----
output$download_button <- downloadHandler(
    filename = function() {
        paste0("octobooks_", Sys.Date(), ".csv")
    },
    content = function(file) {
        fwrite(values$books_df, file)
    }
)


# Modification de la base ----

## Affichage du formulaire ----
entry_form <- function(button_id) {
    
    showModal(
        modalDialog(
            id = "form-modal",
            size = "l",
            fluidPage(
                fluidRow(
                    column(4,
                           disabled(textInput("edit_isbn", user_config$settings$isbnCase)),
                    ),
                    column(4,
                           checkboxGroupButtons("edit_onmyshelf",
                                                label = " ",
                                                choices = c("Dans ma bibliothèque" = TRUE),
                                                justified = TRUE,
                                                status = "theme-light")
                    ),
                    column(4,
                           radioGroupButtons(
                               inputId = "edit_score",
                               label = " ",
                               choices = setNames(c(0:5, "*"),
                                                  c(0:5, "★")),
                               selected = character(0),
                               justified = TRUE,
                               individual = TRUE,
                               status = "theme-light"
                           )
                    )
                ),
                fluidRow(
                    column(8,
                           textInput("edit_title", "Titre", width = "100%"),
                           textInput("edit_title_vo", "Titre original", width = "100%"),
                           textInput("edit_authors", "Auteurices", width = "100%"),
                           textInput("edit_translators", "Traducteurices", width = "100%"),
                           textInput("edit_interpreters", "Interprètes", width = "100%"),
                           awesomeCheckboxGroup("edit_genders",
                                                NULL,
                                                choices = setNames(names(code_genders), code_genders),
                                                inline = T, status = "info")
                    ),
                    column(4,
                           tags$div(
                               id = "edit_imageDiv",
                               div(
                                   id = "edit_imageSubDiv",
                                   img(id = "edit_coverImage",
                                       src = "dummy_cover.jpg")
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
                               )
                           ),
                           tags$div(
                               style = "margin-top: 30px;",
                               checkboxGroupButtons("edit_signed",
                                                    label = " ",
                                                    choices = c("Dédicacé" = TRUE),
                                                    justified = TRUE,
                                                    status = "theme-light")
                           )
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
                           selectInput("edit_format",
                                       "Format",
                                       choices = values$choices$format)
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
                           selectInput("edit_acqui_type",
                                       "Type d'acquisition",
                                       choices = values$choices$acqui_type)
                    ),
                    column(4,
                           textInput("edit_acqui_date",
                                     "Date d'acquisition")
                    ),
                    column(4,
                           selectInput("edit_acqui_state",
                                       "État d'acquisition",
                                       choices = values$choices$acqui_state)
                    )
                ),
                
                fluidRow(
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
                    column(4,
                           selectInput("edit_owner",
                                       "Propriétaire",
                                       choices = values$choices$owner)
                    ),
                    column(4,
                           selectInput("edit_keywords",
                                       "Mots-clés",
                                       multiple = T,
                                       choices = values$choices$keywords)
                    )
                )
            ),
            easyClose = TRUE,
            footer = tagList(modalButton("Annuler"),
                             actionButton(button_id, "Valider", 
                                          class = "coloured-btn"))
        )
    )
}

### Pré-remplissage du formulaire ----
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
        updateCheckboxGroupButtons(session, "edit_onmyshelf", selected = book_values$onmyshelf)
        updateRadioGroupButtons(session, "edit_score", selected = book_values$score)
        updateCheckboxGroupButtons(session, "edit_signed", selected = book_values$signed)
        
        updateTextInput(session, "edit_title", value = book_values$title)
        updateTextInput(session, "edit_title_vo", value = book_values$title_vo)
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
        updateSelectInput(session, "edit_acqui_type", selected = book_values$acqui_type)
        updateTextInput(session, "edit_acqui_date", value = book_values$acqui_date)
        updateSelectInput(session, "edit_acqui_state", selected = book_values$acqui_state)
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
        
        # Titre original
        # if (book_values$langue == book_values$langue_vo) {
        #     disable("edit_title_vo")
        # }
        
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
        
        if (book_values$cover) {
            img_path <- grep(book_values$isbn, list.files(path = "www/covers/"), value = T)
            # img_path <- grep(book_values$isbn, list.files(user_path("data/covers/")), value = T)

            if (length(img_path)) {
                edit_coverImg(paste0("www/covers/", img_path))
            } else {
                edit_coverImg("www/dummy_cover.jpg")
            }
        } else {
            edit_coverImg("www/dummy_cover.jpg")
        }
        
        
        toggleState("edit_resetupload_button", condition = book_values$cover)
        
        update_edit_coverImage()
    }
})

### Format titre original ----
edit_title_vo_shown <- reactiveVal(FALSE)
observe({
    req(input$edit_langue_vo)
    req(input$edit_langue)
    
    if (input$edit_langue_vo == input$edit_langue) {
        updateTextInput(session, inputId = "edit_title_vo", value = "")
    }
    toggleState("edit_title_vo", condition = (input$edit_langue_vo != input$edit_langue))
})

### Format nombre de pages et durée ----
observeEvent(input$edit_format, {
    
    if (input$edit_format == "Audio")  {
        updateTextInput(session, inputId = "edit_nbpages", value = "")
        disable(selector = "#edit_nbpages")
        enable(selector = "#edit_duree_h")
        enable(selector = "#edit_duree_min")
        enable(selector = "#edit_interpreters")
    }
    
    if (input$edit_format %in% c("Papier", "Numérique"))  {
        updateTextInput(session, inputId = "edit_interpreters", value = "")
        updateTextInput(session, inputId = "edit_duree_h", value = "")
        updateTextInput(session, inputId = "edit_duree_min", value = "")
        enable(selector = "#edit_nbpages")
        disable(selector = "#edit_duree_h")
        disable(selector = "#edit_duree_min")
        disable(selector = "#edit_interpreters")
    }
})

### Dates de lecture ----
observeEvent(input$edit_read, {
    if (input$edit_read == "non") {
        updateAirDateInput(session, inputId = "edit_read_deb_date", value = NULL, clear = T)
        updateAirDateInput(session, inputId = "edit_read_fin_date", value = NULL, clear = T)
        updateRadioGroupButtons(session, "edit_score", selected = character(0), disabled = T)
        
        disable(id = "edit_read_deb_date")
        disable(id = "edit_read_fin_date")
        
    } else {
        enable(id = "edit_read_deb_date")
        enable(id = "edit_score")
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


### Image ----

edit_coverImg <- reactiveVal(value = "www/dummy_cover.jpg")

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
    edit_coverImg("www/dummy_cover.jpg")
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


## Récupération des valeurs du formulaire ----
editForm <- reactive({
    
    edit_read_deb_date <- NA_POSIXct_
    if (!is.null(input$edit_read_deb_date)) { 
        edit_read_deb_date <- input$edit_read_deb_date
    }
    edit_read_fin_date <- NA_POSIXct_
    if (!is.null(input$edit_read_fin_date)) { 
        edit_read_fin_date <- input$edit_read_fin_date
    }
    
    edit_onmyshelf <- FALSE
    if (!is.null(input$edit_onmyshelf)) {
        edit_onmyshelf <- as.logical(input$edit_onmyshelf)
    }
    
    edit_signed <- FALSE
    if (!is.null(input$edit_signed)) {
        edit_signed <- as.logical(input$edit_signed)
    }
    
    edit_score <- ""
    if (!is.null(input$edit_score)) { 
        edit_score <- input$edit_score
    }
    
    edit_nbpages <- NA
    edit_duree_h <- NA_integer_; edit_duree_min <- NA_integer_
    edit_interpreters <- NA_character_
    if(input$edit_format == "Audio") {
        edit_duree_h <- fifelse(input$edit_duree_h == "", 0, as.integer(input$edit_duree_h))
        edit_duree_min <- fifelse(input$edit_duree_min == "", 0, as.integer(input$edit_duree_min))
    } else {
        edit_nbpages <- as.integer(input$edit_nbpages)
    }
    
    img_name <- sprintf("cover_%s.%s", input$edit_isbn, file_ext(edit_coverImg()))
    urlImg <- paste0("www/covers/", img_name)
    # urlImg <- user_path(sprintf("data/covers/cover_%s.%s", input$edit_isbn, 
    #                             file_ext(edit_coverImg())))
    edit_cover <- md5sum("www/dummy_cover.jpg") != md5sum(edit_coverImg())
    if (!edit_cover) {
        imgfiles <- grep(input$edit_isbn, list.files(path = user_path("data/covers/")), value = T)
        if (length(imgfiles)) {
            file.remove(user_path(paste0("data/covers/", imgfiles)))
        }
    } else if (edit_cover && edit_coverImg() != urlImg) {
        imgfiles <- grep(input$edit_isbn, list.files(path = user_path("data/covers/")), value = T)
        if (length(imgfiles)) {
            file.remove(user_path(paste0("data/covers/", imgfiles)))
        }
        file.copy(edit_coverImg(), urlImg, overwrite = T)
        file.copy(edit_coverImg(), user_path(paste0("data/covers/", img_name)), overwrite = T)
    }
    
    editForm <- data.frame(
        isbn = input$edit_isbn,
        title = input$edit_title,
        title_vo = input$edit_title_vo,
        authors = fmt_semicol(str_trim(strsplit(input$edit_authors, ",")[[1]])),
        translators = fmt_semicol(str_trim(strsplit(input$edit_translators, ",")[[1]])),
        interpreters = fmt_semicol(str_trim(strsplit(input$edit_interpreters, ",")[[1]])),
        genders = paste(input$edit_genders, collapse = ";"),
        pages = edit_nbpages,
        duree_h = edit_duree_h,
        duree_min = edit_duree_min,
        genre = input$edit_genre,
        pub_date = as.integer(input$edit_pub_date),
        edition_date = as.integer(input$edit_edition_date),
        langue_vo = input$edit_langue_vo,
        pays_vo = input$edit_pays_vo,
        langue = input$edit_langue,
        acqui_type = input$edit_acqui_type,
        acqui_date = as.integer(input$edit_acqui_date),
        acqui_state = input$edit_acqui_state,
        format = input$edit_format,
        owner = input$edit_owner,
        read = input$edit_read,
        read_deb_date = as.POSIXct(edit_read_deb_date, tz = "GMT"),
        read_fin_date = as.POSIXct(edit_read_fin_date, tz = "GMT"),
        keywords = paste(input$edit_keywords, collapse = ";"),
        cover = edit_cover,
        score = edit_score,
        onmyshelf = edit_onmyshelf,
        signed = edit_signed
    )
    
    return(editForm)
})

observeEvent(input$submit_edit, {
    
    edit_values <- editForm()
    
    # Remplacer par names(books) ?
    # cols <- c("title", "title_vo", 
    #           "authors", "translators", "interpreters", 
    #           "genders", "genre", 
    #           "pub_date", "edition_date", 
    #           "langue_vo", "pays_vo", "langue", 
    #           "format",  "pages", "duree_h", "duree_min", "owner", 
    #           "read", "read_deb_date", "read_fin_date", 
    #           "keywords", "cover", "score", "onmyshelf")
    cols <- names(books)
    values$books_df[input$books_tbl_row_last_clicked, cols] <- edit_values[cols]
    
    update_db()
    removeModal()
    
})


## Suppression de lignes ----

deleteData <- reactive({
    selected_ids <- values$books_df[input$books_tbl_rows_selected, isbn]
    values$books_df <- values$books_df[!(isbn %in% selected_ids)]
    
    imgfiles <- grep(selected_ids, list.files(user_path("data/covers/")), value = T)
    if (length(imgfiles)) {
        file.remove(user_path(paste0("data/covers/", imgfiles)))
    }
    
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
                                              class = "coloured-btn"))
            )
        )
    }
})

observeEvent(input$confirm_delete, {
    deleteData()
    update_db()
    
    removeModal()
})