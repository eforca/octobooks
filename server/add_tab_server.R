shinyjs::runjs("$('#pub_date, #edition_date').attr('maxlength', 4);")

# Listes dynamiques ----

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


# Date de lecture ----

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
                              radioGroupButtons(
                                  inputId = "score",
                                  label = "Note",
                                  choices = setNames(c(0:5, "*"),
                                                     c(0:5, "★")),
                                  selected = character(0),
                                  justified = TRUE,
                                  individual = TRUE,
                                  status = "theme"
                              )
                              
                              
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



# Image de couverture ----

coverImg <- reactiveVal(value = "www/covers/dummy_cover.jpg")

# Update de l'image de couverture
update_coverImage <- function() {
    shinyjs::runjs(
        sprintf("var cover = document.getElementById('coverImage');
                    cover.setAttribute('src', '%s');", 
                sprintf("%s?%i", gsub("www/", "", coverImg()), as.integer(Sys.time())))
    )
}

# Changement de l'image de couverture
observeEvent(input$coverImageInput, {
    
    urlImg <- paste0("www/covers/temp_cover.",
                     file_ext(input$coverImageInput$datapath))
    file.copy(input$coverImageInput$datapath, urlImg, overwrite = T)
    coverImg(urlImg)
    update_coverImage()
    
})

# Reset de l'image de couverture
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

observeEvent(input$resetupload_button, {
    reset_coverInput()
    if (coverImg() != "www/covers/dummy_cover.jpg") {
        file.remove(coverImg())
        coverImg("www/covers/dummy_cover.jpg")
        update_coverImage()
    }
})

# Accès au bouton de reset de l'image de couverture
observe({
    toggleState("resetupload_button", 
                condition = coverImg() != "www/covers/dummy_cover.jpg")
})



# Auteurices et traducteurices ----

aut_inserted <- c("aut1")
autbtn_count <- reactiveVal(value = 1)

trad_inserted <- c()
tradbtn_count <- reactiveVal(value = 0)

obsTotrad <- list()

# Insertion d'un champ auteurice
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

# Suppression d'un champ auteurice
observeEvent(input$removeAutBtn, {
    if (autbtn_count() > 0) {
        id = aut_inserted[length(aut_inserted)]
        removeUI(selector = sprintf('.col-sm-4:has(#%s)', id))
        aut_inserted <<- aut_inserted[-length(aut_inserted)]
        autbtn_count(autbtn_count() - 1)
    }
})

# Insertion d'un champ traducteurice
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

# Suppression d'un champ traducteurice
observeEvent(input$removeTradBtn, {
    if (tradbtn_count() > 0) {
        id = trad_inserted[length(trad_inserted)]
        removeUI(selector = sprintf('.col-sm-4:has(#%s)', id))
        trad_inserted <<- trad_inserted[-length(trad_inserted)]
        tradbtn_count(tradbtn_count() - 1)
    }
})



# Format : nombre de pages ou durée, ajout des interprètes ----

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
                                  class = "coloured-btn autandtrad"), 
                     actionButton('removeIntBtn', '-', 
                                  class = "coloured-btn autandtrad"),
                     fluidRow(id = "interpretesRow",
                              column(4, 
                                     textInput(inputId = "int1", label = NULL)
                              ),
                     )
            )
        )
    }
})

# Insertion d'un champ interprète
observeEvent(input$insertIntBtn, {
    intbtn_count(intbtn_count() + 1)
    id <- paste0('int', intbtn_count())
    insertUI(
        selector = '#interpretesRow',
        ui = column(4, textInput(inputId = id, label = NULL))
    )
    int_inserted <<- c(int_inserted, id)
})

# Suppression d'un champ interpète
observeEvent(input$removeIntBtn, {
    if (intbtn_count() > 0) {
        id = int_inserted[length(int_inserted)]
        removeUI(selector = sprintf('.col-sm-4:has(#%s)', id))
        int_inserted <<- int_inserted[-length(int_inserted)]
        intbtn_count(intbtn_count() - 1)
    }
})



# Réinitialisation des inputs ----

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
    updateCheckboxGroupButtons(session, "onmyshelf", selected = character(0))
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
    reset_add()
})

# Réinitialisation des messages d'information
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
    
    shinyjs::disable(id = "isbnButton")
    
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
        shinyjs::html(id = "progress-message", "Searching for book info...")
        
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
            shinyjs::html(id = "progress-message", "Trying Decitre for cover...")
            res_decitre <- GET(sprintf("https://www.decitre.fr/livres/%s.html",
                                       isbn))
            if (status_code(res_decitre) == 200 && 
                content(res_decitre) %>%
                html_elements("img") %>%
                html_attr("src") %>%
                grep(isbn, .) %>% length()) {
                
                
                imgsrc <- content(res_decitre) %>%
                    html_elements("img") %>%
                    html_attr("src") %>%
                    grep(isbn, ., value = T) %>% 
                    grep(pattern = "475x500", value = T) %>%
                    `[[`(1)
                
                urlImg <- "www/covers/temp_cover.jpg"
                if (download.file(imgsrc, destfile = urlImg)) {
                    cat(sprintf("There was an error when downloading %s\n", imgsrc))
                } else {
                    coverImg(urlImg)
                    update_coverImage()
                    reset_coverInput()
                }
                
                
            } else if (values$settings$worldcat == "Oui") {
                print("Trying Worldcat for cover")
                shinyjs::html(id = "progress-message", "Trying Worldcat for cover...")
                
                
                tryCatch(
                    expr = {
                        print("Setting up server for image")
                        rD <- rsDriver(browser="chrome", chromever = "105.0.5195.52",
                                       extraCapabilities = list("chromeOptions" = list(args = list('--headless'))),
                                       port=4550L, verbose=F
                        )
                        remDr <- rD$client
                        
                        print("Charging web page")
                        shinyjs::html(id = "progress-message", "Charging web page...")
                        remDr$navigate(paste0("https://www.worldcat.org/fr/search?q=", isbn))
                        Sys.sleep(5)
                        
                        print("Searching for cover image")
                        shinyjs::html(id = "progress-message", "Searching for cover image...")
                        
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
                    },
                    error = function(e) {
                        cat("No result from image search\n")
                    },
                    finally = {
                        remDr$close()
                        rD$server$stop()
                        rD$server$process
                        rm(rD)
                        gc()
                    }
                )
                
                
                
            }
        }
        
    }
    
    update_coverImage()
    shinyjs::html(id = "loadmessage", "")
    shinyjs::html(id = "progress-message", "")
    
    shinyjs::enable(id = "isbnButton")
    
})


# Ajout d'un livre à la base ----

# Mise à jour de la base locale
update_db <- function() {
    fwrite(values$books_df, "data/octobooks.csv")
}

# Formatage des informations du livre
addbooks_df <- reactive({
    
    read_deb_date <- NA_POSIXct_
    if (!is.null(input$read_deb_date)) { 
        read_deb_date <- input$read_deb_date
    }
    read_fin_date <- NA_POSIXct_
    if (!is.null(input$read_fin_date)) { 
        read_fin_date <- input$read_fin_date
    }
    
    onmyshelf <- FALSE
    if (!is.null(input$onmyshelf)) { 
        onmyshelf <- as.logical(input$onmyshelf)
    }
    
    score <- ""
    if (!is.null(input$score)) { 
        score <- input$score
    }
    
    nbpages <- NA
    duree_h <- NA_integer_; duree_min <- NA_integer_
    interpreters <- NA_character_
    if(input$format == "Audio") {
        duree_h <- fifelse(input$duree_h == "", 0, as.integer(input$duree_h))
        duree_min <- fifelse(input$duree_min == "", 0, as.integer(input$duree_min))
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
        duree_h = duree_h,
        duree_min = duree_min,
        owner = input$owner,
        read = input$read,
        read_deb_date = as.POSIXct(read_deb_date, tz = "GMT"),
        read_fin_date = as.POSIXct(read_fin_date, tz = "GMT"),
        keywords = paste(input$keywords, collapse = ";"),
        cover = cover,
        score = score,
        onmyshelf = onmyshelf
    )
    
    return(addbooks_df)
})


# Ajout du livre
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
            reset_add()
        } 
    }
})