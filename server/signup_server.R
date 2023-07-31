signupModal <- function() {
    modalDialog(
        title = "Inscription",
        id = "signup-modal",
        size = "s",
        fade = TRUE,
        style = "text-align: center;",
        textInput("signup_user", "Identifiant"),
        passwordInput("signup_password", "Mot de passe"),
        passwordInput("signup_verify_password", "Vérification du mot de passe"),
        passwordInput("signup_secret_key", "Clé secrète"),
        actionButton("signup_button", "Confirmer l'inscription", class = "coloured-btn",
                     width = "100%", style = "margin-top: 15px;"),
        footer = list(
            tags$p("Déjà inscrit·e ?", style = "text-align: center; font-weight: bold;"),
            actionButton("to_signin_button", "Se connecter", class = "btn-theme-light",
                         width = "100%", style = "margin-bottom: 15px;")
        )
    )
}

observeEvent(input$to_signin_button, {
    showModal(signinModal())
})


observeEvent(input$signup_button, {
    
    req(input$signup_user,
        input$signup_password,
        input$signup_verify_password,
        input$signup_secret_key)
    
    # Sign up validation -----
    formatted_log(user = input$signup_user, content = "sign up validation")
    
    validation_result <- all(
        grepl(pattern = "^[A-z0-9]{4,20}$", input$signup_user),
        grepl(pattern = "^[A-z0-9]{6,50}$", input$signup_password),
        input$signup_password == input$signup_verify_password,
        input$signup_secret_key == Sys.getenv(x = "SECRET_KEY")
    )
    
    
    if (validation_result == F) {
        formatted_log(
            user = input$signup_user, 
            error = T, 
            content = "sign up validation failed"
        )
        
        updateTextInput(
            session = session,
            inputId = "signup_password",
            value = ""
        )
        
        updateTextInput(
            session = session,
            inputId = "signup_verify_password",
            value = ""
        )
        
        if (input$signup_secret_key != Sys.getenv(x = "SECRET_KEY")) {
            generic_modal(content = "La clé secrète est incorrecte")
        } else {
            generic_modal(content = list(        
                "Quelque chose n'a pas fonctionné.",
                br(),
                "Veillez à respecter les formats suivants :", 
                br(), br(),
                tags$p("Identifiant de 4 à 20 lettres ou chiffres."),
                tags$p("Mot de passe de 6 à 50 lettres ou chiffres.")
            ))   
        }
    }
    
    req(validation_result)
    
    # Sign up a new user -----
    formatted_log(user = input$signup_user, content = "sign up a new user")
    
    withProgress(
        value = 1, 
        message = "Création du compte",
        expr = {
            conn <- connect_to_db()
            
            new_row <- data.frame(
                username = input$signup_user, 
                password = hmac(
                    key = Sys.getenv(x = "ENCRYPTION_KEY"),
                    object = input$signup_password,
                    algo = "sha512"
                ),
                created_at = strftime(x = Sys.time(), tz = "UTC")
            )
            
            try_result <- try(
                silent = T,
                expr = dbWriteTable(
                    conn = conn, 
                    name = "logins",
                    value = new_row,
                    overwrite = F,
                    append = T
                )
            )
            
            dbDisconnect(conn = conn)
        }
    )
    
    # Analyze sign up try result -----
    formatted_log(
        user = input$signup_user, 
        content = "analyze sign up try result"
    )
    
    if (try_result == T) {
        
        formatted_log(user = input$signup_user, content = "successful sign up")
        
        active_user$username <- input$signup_user
        source(file = "init_user.R", local = T)$value
        
        generic_modal(error = F, 
                      id = "signup_success-modal",
                      content = htmltools::HTML(paste0("Bienvenue <span style='font-weight: bold;'>", active_user$username, "</span>, votre compte a bien été créé !")))
        
        # showModal(signupSuccessModal())
        
    } else if (
        grepl(
            pattern = "duplicate key value violates unique constraint", 
            x = attributes(try_result)$condition$message
        )
    ) {
        
        formatted_log(
            user = input$signup_user, 
            error = T,
            content = "sign up failed due to taken username"
        )
        
        updateTextInput(
            session = session,
            inputId = "signup_password",
            value = ""
        )
        
        updateTextInput(
            session = session,
            inputId = "signup_verify_password",
            value = ""
        )
        
        generic_modal(content = "Quelqu'un a déjà choisi cet identifiant")
        
    } else {
        
        # Sign up failed due to unknown issue
        
        if (inherits(try_result, "try-error")) {
            formatted_log(
                user = input$signup_user, 
                error = T,
                content = attributes(try_result)$condition$message
            )
        }
        
        updateTextInput(
            session = session,
            inputId = "signup_password",
            value = ""
        )
        
        updateTextInput(
            session = session,
            inputId = "signup_verify_password",
            value = ""
        )

        generic_modal(content = "Désolé, quelque chose n'a pas fonctionné, veuillez réessayer.")
    }
    
    
})