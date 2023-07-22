### Fonctions usuelles ----
str_isnum <- function(x) {!grepl("\\D", x)}
fmt_semicol <- function(v) {
    return(gsub("^;|;$", "", 
                gsub("(;){2,}", replacement = ";", paste(v, collapse = ";"))))
}

### Constantes ----

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

### Base ----

# Vérification de l'existence de la base
if (!file.exists("data")) dir.create("data")
if (!file.exists("data/octobooks.csv")) {
    fwrite(data.table(isbn = character(),
                      title = character(),
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
                      onmyshelf = logical()),
           "data/octobooks.csv")
}

# Sauvegarde de la base si nécessaire
if (!file.exists("data/backups")) dir.create("data/backups")
lastsave <- paste0("data/backups/", sort(list.files("data/backups/"), decreasing = T)[1])
if (lastsave == "data/backups/NA" || md5sum("data/octobooks.csv") != md5sum(lastsave)) {
    cat("New backup")
    file.copy(from = "data/octobooks.csv",
              to = sprintf("data/backups/octobooks_%i.csv", as.integer(Sys.time())))
}

# Pour retrouver la date et l'heure de création de la sauvegarde :
# as.POSIXct(n, origin = "1970-01-01")


# Suppression des fichiers images temporaires si nécessaires
if (length(grep("temp_cover", list.files(path = "www/covers/")))) {
    file.remove(paste0("www/covers/", 
                       grep("temp_cover", list.files(path = "www/covers/"), value = T)))
}

# Backup des images de couverture
if (!file.exists("data/covers")) dir.create("data/covers")
sapply(setdiff(list.files("www/covers/"), list.files("data/covers/")),
       function(f) file.copy(sprintf("www/covers/%s", f), sprintf("data/covers/%s", f))) %>% 
    invisible

# Importation de la base
books <- fread("data/octobooks.csv", integer64 = "character",
               colClasses = list(character=c("title", "authors", "translators", "interpreters",
                                             "genders", "genre", "langue_vo", 
                                             "pays_vo", "langue", "format",
                                             "owner", "read", "keywords", "score"),
                                 integer=c("pub_date", "edition_date", "pages", 
                                           "duree_h", "duree_min"),
                                 # POSIXct=c("read_deb_date", "read_fin_date"),
                                 logical=c("cover", "onmyshelf")))
books[, read_deb_date := as.POSIXct(read_deb_date, tz = "GMT")]
books[, read_fin_date := as.POSIXct(read_fin_date, tz = "GMT")]


# setcolorder(books, neworder = c("isbn", "title", "authors", "translators", "interpreters",
#                                 "genders", "genre", "pub_date", "edition_date", "langue_vo", 
#                                 "pays_vo", "langue", "format", "pages", "duree_h", "duree_min", 
#                                 "owner", "read", "read_deb_date", "read_fin_date", "keywords", "cover"))

### Config ----

# Création des fichiers de config si nécessaire
config_files <- c("selected_cols", "default_choices", "choices", "settings")
config <- sapply(config_files, function(f) {
    fpath <- sprintf("config/%s.yml", f)
    if (file.exists(fpath)) {
        read_yaml(fpath)
    } else {
        file.copy(sprintf("config/init/%s.yml", f), fpath)
        read_yaml(fpath)
    }
})

labcols <- c(isbn = config$settings$isbnCase, 
             title = "Titre", 
             authors = "Auteurices",
             translators = "Traducteurices",
             interpreters = "Interprètes",
             genders = "Genres",
             genre = "Genre littéraire",
             pub_date = "Parution",
             edition_date = "Édition",
             langue_vo = "Langue VO",
             langue = "Langue",
             format = "Format",
             pages = "Pages",
             duree = "Durée",
             owner = "Propriétaire",
             read = "Lu",
             onmyshelf = "Dans ma bibli",
             read_deb_date = "Date début", 
             read_fin_date = "Date fin", 
             score = "Note",
             keywords = "Mots-clés",
             cover = "Couverture")