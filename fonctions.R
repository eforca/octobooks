# Connexion à la base PostgreSQL
connect_to_db <- function() {
    dbConnect(
        drv = Postgres(),
        host = Sys.getenv(x = "DB_HOST"),
        port = Sys.getenv(x = "DB_PORT"),
        dbname = Sys.getenv(x = "DB_NAME"),
        user = Sys.getenv(x = "DB_USER"),
        password = Sys.getenv(x = "DB_PASSWORD"),
    )
}

# Lecture des fichiers SQL
read_sql_file <- function(path) {
    path %>% readLines() %>% paste(collapse = " ")
}

# Messages de log
formatted_log <- function(user, error = F, content) {
    message("User ", user, " -- ", if (error) "ERROR -- ", content, "\n")
}

# Message par modal
generic_modal <- function(error = T, content, id = NULL) {
    
    title <- if (error) {
        span("Arf...")
    } else {
        span("Ouaiis !")
    }
    
    footer <- if (error) {
        list(
            actionButton("errorto_signin_button", "Se connecter", class = "coloured-btn",
                         width = "100%", style = "margin: 5px 0px;"),
            actionButton("errorto_signup_button", "Créer un compte", class = "btn-theme-light",
                         width = "100%", style = "margin: 5px 0px;")
        )
    } else {
        modalButton("Fermer")
    }
    
    showModal(
        modalDialog(
            id = id,
            title = title,
            size = "s",
            easyClose = !error,
            footer = footer,
            content
        )
    )
}

