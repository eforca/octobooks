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




# Suppression des fichiers images temporaires si nécessaires
if (length(list.files("www/covers"))) {
    file.remove(list.files("www/covers", full.names = TRUE))
}


# Config ----

# Import des fichiers de config (création si nécessaire)
config_files <- c("selected_cols", "default_choices", "choices", "settings")
config <- sapply(config_files, function(f) {
    read_yaml(sprintf("config/%s.yml", f))
})


# Types des variable pour import
char_cols <- c("isbn", "title", "title_vo", "authors", "translators", "interpreters",
               "genders", "genre", "langue_vo", "pays_vo", "langue", "format",
               "acqui_type", "acqui_state", "owner", "read", "keywords", "score")
date_cols <- c("pub_date", "edition_date", "acqui_date")
int_cols <- c("pages", "duree_h", "duree_min")
log_cols <- c("cover", "onmyshelf", "signed")


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
