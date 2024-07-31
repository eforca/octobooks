# Chemins d'accès ----
user_path <<- function(path) file.path("users", active_user, path)

if (!file.exists("users")) dir.create("users")
if (!file.exists(user_path(""))) dir.create(user_path(""))


# Base ----

# Vérification de l'existence de la base
if (!file.exists(user_path("data"))) dir.create(user_path("data"))
if (!file.exists(user_path("data/octobooks.csv"))) {
    fwrite(
        data.table(
            isbn = character(),
            title = character(),
            title_vo = character(),
            authors = character(),
            translators = character(),
            interpreters = character(),
            genders = character(),
            genre = character(),
            pub_date = integer(),
            edition_date = integer(),
            langue_vo = character(),
            pays_vo = character(),
            langue = character(),
            acqui_type = character(),
            acqui_date = integer(),
            acqui_state = character(),
            format = character(),
            pages = integer(),
            duree_h = integer(),
            duree_min = integer(),
            owner = character(),
            read = character(),
            read_deb_date = POSIXct(),
            read_fin_date = POSIXct(),
            keywords = character(),
            cover = logical(),
            score = character(),
            onmyshelf = logical(),
            signed = logical()
        ),
        user_path("data/octobooks.csv")
    )
}

# Sauvegarde de la base si nécessaire
if (!file.exists(user_path("data/backups"))) dir.create(user_path("data/backups"))
lastsave <- paste0(
    user_path("data/backups/"),
    sort(list.files(user_path("data/backups")), decreasing = TRUE)[1]
)
if (lastsave == user_path("data/backups/NA") ||
    md5sum(user_path("data/octobooks.csv")) != md5sum(lastsave)) {
    cat("New backup\n")
    file.copy(
        from = user_path("data/octobooks.csv"),
        to = user_path(sprintf("data/backups/octobooks_%i.csv", as.integer(Sys.time())))
    )
}


# Pour retrouver la date et l'heure de création de la sauvegarde :
# as.POSIXct(n, origin = "1970-01-01")

# Importation de la base
books <- fread(user_path("data/octobooks.csv"),
    integer64 = "character",
    colClasses = list(
        character = c(
            "title", "title_vo",
            "authors", "translators", "interpreters",
            "genders", "genre", "langue_vo",
            "pays_vo", "langue", "format",
            "acqui_type", "acqui_state",
            "owner", "read", "keywords", "score"
        ),
        integer = c(
            "pub_date", "edition_date", "acqui_date",
            "pages", "duree_h", "duree_min"
        ),
        # POSIXct=c("read_deb_date", "read_fin_date"),
        logical = c("cover", "onmyshelf", "signed")
    )
)
books[, c(char_cols[2:5]) := lapply(.SD, gsub, pattern = '""', replacement = '"'), .SDcols = char_cols[2:5]]
books[, read_deb_date := as.POSIXct(read_deb_date, tz = "GMT")]
books[, read_fin_date := as.POSIXct(read_fin_date, tz = "GMT")]




# Création du dossier de couvertures si nécessaire
if (!file.exists(user_path("data/covers"))) dir.create(user_path("data/covers"))


# Ajout des couvertures de l'utilisateur dans les ressources
file.copy(list.files(user_path("data/covers"), full.names = TRUE), "www/covers")


# # Suppression des fichiers images temporaires si nécessaires
# if (length(grep("temp_cover", list.files(path = "www/covers/")))) {
#     file.remove(paste0("www/covers/",
#                        grep("temp_cover", list.files(path = "www/covers/"), value = T)))
# }
#
# # Backup des images de couverture
# if (!file.exists("data/covers")) dir.create("data/covers")
# sapply(setdiff(list.files("www/covers/"), list.files("data/covers/")),
#        function(f) file.copy(sprintf("www/covers/%s", f), sprintf("data/covers/%s", f))) %>%
#     invisible




# setcolorder(books, neworder = c("isbn", "title", "authors", "translators", "interpreters",
#                                 "genders", "genre", "pub_date", "edition_date", "langue_vo",
#                                 "pays_vo", "langue", "format", "pages", "duree_h", "duree_min",
#                                 "owner", "read", "read_deb_date", "read_fin_date", "keywords", "cover"))

# Config ----

# Import des fichiers de config (création si nécessaire)
config_files <- c("selected_cols", "default_choices", "choices", "settings")
if (!file.exists(user_path("config"))) dir.create(user_path("config"))
user_config <<- sapply(config_files, function(f) {
    fpath <- sprintf("config/%s.yml", f)
    if (!file.exists(user_path(fpath))) {
        file.copy(sprintf("config/%s.yml", f), user_path(fpath))
    }
    read_yaml(user_path(fpath))
})

# Récupération de tous les choix présents dans le tableau de données
for (x in names(user_config$choices)) {
    books_choices <- unique(books[[x]])
    if (length(books_choices) > 1 || !is.na(books_choices)) {
        if (x == "keywords") {
            books_keywords <- unlist(strsplit(books_choices, ";"))
            if (length(setdiff(books_keywords, user_config$choices$keywords))) {
                user_config$choices$keywords <- unique(books_keywords)
            }
        } else {
            if (length(setdiff(books_choices, user_config$choices[[x]]))) {
                user_config$choices[[x]] <- books_choices
            }
        }
    }
}

# Reactive values ----

# Base, choix proposés et choix par défaut
values$books_df <- books
values$selected_cols <- user_config$selected_cols
values$choices <- user_config$choices
values$default_choices <- user_config$default_choices
values$settings <- user_config$settings

observe({
    updateColorPickr(session, "set_themeColour",
        value = values$settings$themeColour
    )

    for (i in 1:6) {
        updateAwesomeCheckboxGroup(session,
            paste0("selcols", i),
            selected = values$selected_cols
        )
    }
})
