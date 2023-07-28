# Chemins d'accès ----
user_id <- "perso"
user_path <- function(path) file.path("users", user_id, path)

if (!file.exists("users")) dir.create("users")
if (!file.exists(user_path(""))) dir.create(user_path(""))

# Fonctions usuelles ----
str_isnum <- function(x) {!grepl("\\D", x)}
fmt_semicol <- function(v) {
    return(gsub("^;|;$", "", 
                gsub("(;){2,}", replacement = ";", paste(v, collapse = ";"))))
}

# Constantes ----

this_year <- as.POSIXct(paste0(format(Sys.Date(), "%Y"), 
                               c("-01-01", "-12-31")), 
                        tz = "UTC")


code_genders <- c("F" = "Femme(s)",
                  "H" = "Homme(s)",
                  "N" = "Non-binaire(s)",
                  "I" = "Je ne sais pas")

code_langue <- c("fre" = "Français",
                 "eng" = "Anglais",
                 "spa" = "Espagnol")

code_lu <- c("non" = "Non", 
             "oui" = "Oui", 
             "dnf" = "Pas fini")

stat_cats <- c("genre", "langue_vo", "genders", 
               "langue", "format", "owner", "keywords")

# Base ----

# Vérification de l'existence de la base
if (!file.exists(user_path("data"))) dir.create(user_path("data"))
if (!file.exists(user_path("data/octobooks.csv"))) {
    fwrite(data.table(isbn = character(),
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
                      signed = logical()),
           user_path("data/octobooks.csv"))
}

# Sauvegarde de la base si nécessaire
if (!file.exists(user_path("data/backups"))) dir.create(user_path("data/backups"))
lastsave <- paste0(user_path("data/backups/"), 
                   sort(list.files(user_path("data/backups")), decreasing = TRUE)[1])
if (lastsave == user_path("data/backups/NA") || 
    md5sum(user_path("data/octobooks.csv")) != md5sum(lastsave)) {
    cat("New backup")
    file.copy(from = user_path("data/octobooks.csv"), 
              to = user_path(sprintf("data/backups/octobooks_%i.csv", as.integer(Sys.time()))))
}


# Pour retrouver la date et l'heure de création de la sauvegarde :
# as.POSIXct(n, origin = "1970-01-01")

# Importation de la base
books <- fread(user_path("data/octobooks.csv"), 
               integer64 = "character",
               colClasses = list(character=c("title", "title_vo", 
                                             "authors", "translators", "interpreters",
                                             "genders", "genre", "langue_vo", 
                                             "pays_vo", "langue", "format",
                                             "acqui_type", "acqui_state",
                                             "owner", "read", "keywords", "score"),
                                 integer=c("pub_date", "edition_date", "acqui_date",
                                           "pages", "duree_h", "duree_min"),
                                 # POSIXct=c("read_deb_date", "read_fin_date"),
                                 logical=c("cover", "onmyshelf", "signed")))
books[, read_deb_date := as.POSIXct(read_deb_date, tz = "GMT")]
books[, read_fin_date := as.POSIXct(read_fin_date, tz = "GMT")]


# Création du dossier de couvertures si nécessaire
if (!file.exists(user_path("data/covers"))) dir.create(user_path("data/covers"))

# Suppression des fichiers images temporaires si nécessaires
if (length(list.files("www/covers"))) {
    file.remove(list.files("www/covers", full.names = TRUE))
}

# Ajout des couvertures de l'utilisateurs dans les ressources
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
config <- sapply(config_files, function(f) {
    fpath <- sprintf("config/%s.yml", f)
    if (!file.exists(user_path(fpath))) {
        file.copy(sprintf("config/%s.yml", f), user_path(fpath))
    }
    read_yaml(user_path(fpath))
})


# Noms des colonnes du tableau de données
labcols <- c(isbn = config$settings$isbnCase, 
             title = "Titre", 
             title_vo = "Titre original",
             authors = "Auteurices",
             translators = "Traducteurices",
             interpreters = "Interprètes",
             genders = "Genres",
             genre = "Genre littéraire",
             pub_date = "Parution",
             edition_date = "Édition",
             langue_vo = "Langue VO",
             langue = "Langue",
             acqui_type = "Type d'acquisition",
             acqui_date = "Date d'acquisition",
             acqui_state = "État d'acquisition",       
             format = "Format",
             pages = "Pages",
             duree = "Durée",
             owner = "Propriétaire",
             read = "Lu",
             onmyshelf = "Dans ma bibli",
             signed = "Dédicacé",
             read_deb_date = "Date début", 
             read_fin_date = "Date fin", 
             score = "Note",
             keywords = "Mots-clés",
             cover = "Couverture")

# Nombre de variables par colonne pour le choix des variables du tableau
nb_per_col <- ceiling(length(labcols)/6)
