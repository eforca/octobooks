# Colonnes sélectionnées par défaut ----
output$select_newdefcols <- renderUI({
    fluidRow(
        lapply(1:(length(labcols)%/%nb_per_col + 1), function(i) {
            if (i > length(labcols)%/%nb_per_col) {
                li <- nb_per_col*i-(nb_per_col-1); hi <- nb_per_col*(i-1) + length(labcols)%%nb_per_col
            } else {
                li <- nb_per_col*i-(nb_per_col-1); hi <- nb_per_col*i   
            }
            column(2,
                   awesomeCheckboxGroup(paste0("newdefcols", i),
                                        label = NULL,
                                        choices = setNames(names(labcols)[li:hi], 
                                                           labcols[li:hi]),
                                        selected = config$selected_cols,
                                        inline = F, status = "info")
            )
        })
    )
})

observeEvent(input$change_defcols_button, {
    newselcols <- c(input$newdefcols1, input$newdefcols2, input$newdefcols3, 
                    input$newdefcols4, input$newdefcols5, input$newdefcols6)
    values$selected_cols <- newselcols
    
    for(i in 1:6) {
        updateAwesomeCheckboxGroup(session,
                                   paste0("selcols", i),
                                   selected = newselcols)
    }
    
    write_yaml(values$selected_cols, user_path("config/selected_cols.yml"))
    shinyjs::html("newdefcolsMessage", HTML("<p class='success'>Les colonnes sélectionnées par défaut ont bien été modifiées !</p>"))
})

observeEvent(c(input$newdefcols), {
    shinyjs::html("newdefcolsMessage", "")
})



# Choix par défaut ----

output$defvalue <- renderUI({
    
    if (input$coltochange == "read") {
        availchoices <- c("Non" = "non", 
                          "Oui" = "oui", 
                          "Pas fini" = "dnf")
    } else if (input$coltochange == "onmyshelf") {
        availchoices <- c("Non" = FALSE, 
                          "Oui" = TRUE)
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
    write_yaml(values$default_choices, user_path("config/default_choices.yml"))
    shinyjs::html("newdefvalueMessage", HTML("<p class='success'>La valeur par défaut a bien été modifiée !</p>"))
})

observeEvent(c(input$coltochange, input$newdefvalue), {
    shinyjs::html("newdefvalueMessage", "")
})



# Ajout de nouveaux choix ----

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
                                              class = "coloured-btn"))
            )
        )
    }
})

observeEvent(input$confirm_newchoice, {
    values$choices[[newcol()]] <- sort(c(values$choices[[newcol()]], newval()))
    
    write_yaml(values$choices, user_path("config/choices.yml"))
    
    newval(NULL)
    updateTextInput(session, "newchoice", value = "")
    removeModal()
})

observeEvent(c(input$coltoaddto, input$newchoice), {
    shinyjs::html("newchoiceError", "")
})



# Réglages ----

## Reset du formulaire d'ajout par défaut ----
observeEvent(input$set_reset, {
    values$settings$reset <- input$set_reset
})

# # Recherche d'image via worldcat
# observeEvent(input$set_worldcat, {
#     values$settings$worldcat <- input$set_worldcat
# })

## Couleur du thème ----
observeEvent(input$set_themeColour, {
    shinyjs::runjs(sprintf("document.documentElement.style.setProperty('--theme_colour', '%s');",
                           input$set_themeColour))
    values$settings$themeColour <- input$set_themeColour
}, ignoreInit = TRUE)


# ## Nombre de ligne du tableau
# observeEvent(input$set_pageLength, {
#     values$settings$pageLength <- input$set_pageLength
# })


## Format d'ISBN ----
observeEvent(input$set_isbnCase, {
    values$settings$isbnCase <- input$set_isbnCase
})

## Mise à jour des réglages ----
observeEvent(values$settings, {
    print("Updating settings file")
    write_yaml(values$settings, user_path("config/settings.yml"))
}, ignoreInit = TRUE)