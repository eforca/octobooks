signinModal <- function() {
    modalDialog(
        title = "Connexion",
        id = "signin-modal",
        size = "s",
        fade = TRUE,
        style = "text-align: center;",
        textInput("signin_user", "Identifiant"),
        passwordInput("signin_password", "Mot de passe"),
        actionButton("signin_button", "Se connecter", class = "coloured-btn",
                     width = "100%", style = "margin-top: 15px;"),
        footer = list(
            actionButton("to_signup_button", "Créer un compte", class = "btn-theme-light",
                         width = "100%", style = "margin-bottom: 15px;")
        )
    )
}

showModal(signinModal())

# observeEvent(input$signin_button, {
#     source(file = "init_user.R", local = T)$value
#     active_user$username <- user_id
#     removeModal()
# })

observeEvent(input$to_signup_button, {
    showModal(signupModal())
})

observeEvent(input$errorto_signup_button, {
    showModal(signupModal())
})

observeEvent(input$errorto_signin_button, {
    showModal(signinModal())
})



observeEvent(input$signin_button, {
    req(input$signin_user, input$signin_password)
    
    # Sign in validation -----
    formatted_log(user = input$signin_user, content = "sign in validation")
    
    withProgress(
        value = 1, 
        message = "Connexion",
        expr = {
            query <- sqlInterpolate(
                conn = ANSI(), 
                sql = read_sql_file(path = "sql/signin_validation.sql"),
                username = input$signin_user,
                password = hmac(
                    key = Sys.getenv(x = "ENCRYPTION_KEY"),
                    object = input$signin_password,
                    algo = "sha512"
                )
            )
            
            conn <- connect_to_db()
            
            try_result <- try(
                silent = T, 
                expr = dbGetQuery(conn = conn, statement = query)
            )
            
            dbDisconnect(conn = conn)
        }
    )
    
    # Analyze sign in try result -----
    formatted_log(
        user = input$signin_user, 
        content = "analyze sign in try result"
    )
    
    if (inherits(try_result, "try-error")) {
        
        # Sign in failed for unknown reasons
        formatted_log(
            user = input$signin_user, 
            error = T,
            content = attributes(try_result)$condition$message
        )
        
        updateTextInput(session = session, inputId = "signin_user", value = "")
        updateTextInput(session = session, inputId = "signin_password", value = "")

        generic_modal(content = "Quelque chose n'a pas fonctionné, veuillez réessayer.")
        
    } else if (nrow(try_result) == 0) {
        
        formatted_log(
            user = input$signin_user, 
            error = T,
            content = "sign in failed due to wrong credentials"
        )
        
        updateTextInput(session = session, inputId = "signin_user", value = "")
        updateTextInput(session = session, inputId = "signin_password", value = "")
        
        generic_modal(content = "L'identifiant et/ou le mot de passe sont incorrects")
        
    } else {
        
        formatted_log(user = input$signin_user, content = "successful sign in")
        
        generic_modal(error = F, 
                      id = "signin_success-modal",
                      content = htmltools::HTML(paste0("Youhou, bienvenue <span style='font-weight: bold;'>", try_result$username, "</span> !")))
        
        active_user$username <- try_result$username
        source(file = "init_user.R", local = T)$value

    }
})