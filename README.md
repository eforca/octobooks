# octobooks

Octobooks is a reading tracker and book cataloguing application developped in shiny.

The application can also be run locally by lauching the app from R. Download this repo to get the app script and attached resources. The app can then be launched either from the R console (`shiny::runApp('Octobooks/app.R', launch.browser = TRUE)`, adapt path accordingly), or through the RStudio IDE ("Run App" button on source pane).

The authentification process uses PostgreSQL. The logins table inside the octobooks database can be created with the following command :

``` sql
CREATE TABLE logins (
username varchar(20) PRIMARY KEY,
password varchar(128) NOT NULL,
created_at varchar(19)
);
```

A .Renviron file should be created with the following variables :

``` r
ENCRYPTION_KEY=
DB_HOST=
DB_PORT=
DB_NAME=
DB_USER=
DB_PASSWORD=
```
