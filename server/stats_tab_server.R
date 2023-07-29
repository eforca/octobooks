# fmt_dtplot <- function(d, onlyread, bydate, date_deb = NA, date_fin = NA) {
#     
#     lu <- list("TRUE" = c("oui"), 
#                "FALSE" = c("oui", "dnf", "non"))[onlyread][[1]]
#     
#     if (bydate) {
#         return(d[read %in% lu & between(read_fin_date, date_deb, date_fin),])
#     } else {
#         return(d[read %in% lu,])
#     }
# }

# Bilan global ----

output$plot_count <- renderPlot({
    d <- copy(values$books_df)
    d[, `:=`(year = fcase(read != "oui", "Non lu",
                          is.na(read_fin_date), "Non daté",
                          rep_len(TRUE, nrow(d)), format(read_fin_date, "%Y")),
             status = fcase(read != "oui" & onmyshelf, "Pas lu\ndans ma bibli",
                            read == "oui" & onmyshelf, "Lu\ndans ma bibli",
                            read == "oui" & !onmyshelf, "Lu\npas dans ma bibli"))]
    
    dtplot <- rbind(
        d[, .N, by = .(x = status)][, numplot := 1],
        d[, .N, by = .(x = year)][, numplot := 2])
    
    dtplot[numplot == 2] %>%
        ggplot(aes(x = factor(x, levels = c("Pas lu\ndans ma bibli", 
                                            "Lu\ndans ma bibli",
                                            "Lu\npas dans ma bibli",
                                            "Non lu",
                                            "Non daté",
                                            sort(grep("[0-9]+", dtplot$x, value = T)))), 
                   y = N, label = N)) +
        geom_bar(stat = "identity", fill = values$settings$themeColour) +
        scale_y_continuous(limits = c(0, d[, .N] + 10)) +
        geom_text(vjust = -0.5, size = 5) +
        # facet_wrap(~numplot, scales = "free_x") +
        labs(y = NULL, x = NULL) +
        theme(text = element_text(size = 16),
              panel.grid.major.x = element_blank(),
              strip.background = element_blank(),
              strip.text = element_blank())
})

output$plot_count_month <- renderPlot({
    d <- values$books_df[!is.na(read_fin_date)]
    
    mois <- c("jan.", "fév.", "mars", "avril", "mai", "juin", 
              "juil.", "août", "sept.", "oct.", "nov.", "déc.")
    
    dtplot <- d[, .N, keyby = .(year, month)]
    rbind(dtplot,
          dtplot[, .(year = "Total", N = sum(N)), by = month])
    
    dtplot %>%
        ggplot(aes(x = forcats::fct_rev(factor(month, labels = mois)),
                   y = N, label = N)) +
        # geom_bar(stat = "identity", fill = values$settings$themeColour) +
        geom_bar(stat = "identity") +
        scale_y_continuous(limits = c(0, max(dtplot$N) + 10)) +
        geom_text(size = 4.5, hjust = -0.5) +
        # facet_wrap(~numplot, scales = "free_x") +
        labs(y = NULL, x = NULL) +
        coord_flip() +
        facet_wrap(~year) +
        theme(text = element_text(size = 16),
              panel.grid = element_blank()
              # strip.background = element_blank(),
              # strip.text = element_blank()
        )
})


# output$plot_read <- renderPlot({
#     
#     values$books_df[, .(p = .N/books[, .N]), 
#                     by = .(Lu = code_lu[read])] %>%
#         ggplot(aes(x = "", y = p, fill = Lu)) +
#         geom_bar(stat = "identity", width = 1) +
#         scale_fill_brewer("", palette = "Pastel1") +
#         coord_polar("y", start = 0) +
#         geom_text(aes(label = paste0(round(p*100), "%")), 
#                   position = position_stack(vjust = 0.5)) +
#         labs(title = sprintf("%s livres dans la base", 
#                              values$books_df[, .N])) +
#         theme_void() +
#         theme(plot.title = element_text(hjust = 0.5))
#     
# })
# 
# output$plot_year <- renderPlot({
#     books[!is.na(read_fin_date)] %>%
#         ggplot() +
#         geom_bar(aes(format(read_fin_date, "%Y"))) +
#         labs(x = "Année de fin de lecture",
#              y = "Nombre de livres")
# })


# Bilan par catégorie ----

## Données ----
fmt_dtplot <- function(d, sel_cat) {
    if (sel_cat == "genders") {
        dtplot <- d[, year := format(read_fin_date, "%Y")
        ][, c(code_genders) := lapply(names(code_genders), grepl, genders)
        ][, lapply(.SD, sum), .SDcols = code_genders, keyby = year] %>%
            melt(id.vars = "year", 
                 variable.name = sel_cat, value.name = "N")
        
    } else if (sel_cat == "keywords") {
        keywords_list <- sort(unique(unlist(str_split(d$keywords, ";"))))[-1]
        dtplot <- d[, year := format(read_fin_date, "%Y")
        ][, c(keywords_list) := lapply(keywords_list, grepl, keywords)
        ][, lapply(.SD, sum), .SDcols = keywords_list, keyby = year] %>%
            melt(id.vars = "year", 
                 variable.name = sel_cat, value.name = "N")
        
    } else {
        dtplot <- d[, year := format(read_fin_date, "%Y")
        ][, .N, keyby = c("year", sel_cat)]
    }
    
    rbind(dtplot, 
          dtplot[, .(year = "Total", N = sum(N)), by = sel_cat]
    )[, .(sel_cat = get(sel_cat), N, p = N/sum(N)), keyby = year
    ][is.na(year), year := "Non daté"
    ][order(year, -p)]
}

## Tableau ----
output$cat_table <- render_gt({
    
    sel_cat <- input$stat_cat
    dtplot <- fmt_dtplot(copy(values$books_df), sel_cat)
    sel_year <- "Total"
    
    if (sel_cat %in% c("genders", "keywords")) {
        dtplot <- dtplot[year == sel_year, .(sel_cat, N, p = paste0(round(p*100), "%"))]
    } else {
        dtplot <- rbind(dtplot[year == sel_year, .(sel_cat, N, p = paste0(round(p*100), "%"))],
                        dtplot[year == sel_year, .("Total", sum(N), "100%")], use.names = FALSE)
    }
    
    dtplot[, .(sel_cat, N, p)] %>%
        gt() %>%
        tab_options(table.font.size = px(14),
                    column_labels.border.top.style = "hidden") %>%
        cols_label(.list = setNames(c("", "Livres", "%"),
                                    c("sel_cat", "N", "p"))) %>%
        cols_align(columns = 1, align = "left") %>%
        cols_align(columns = 2:3, align = "center") %>%
        cols_width(1 ~ px(200),
                   2:3 ~ px(75)) %>%
        tab_style(style = cell_borders(sides = "top",
                                       color = "#D3D3D3",
                                       weight = fifelse(sel_cat %in% c("genders", "keywords"), px(1), px(2))),
                  locations = cells_body(rows = nrow(dtplot)))
}
)

## Graphe ----
output$cat_plot <- renderPlot({
    
    sel_cat <- input$stat_cat
    dtplot <- fmt_dtplot(copy(values$books_df), sel_cat)
    
    if (nrow(dtplot)) {
        
        dtplot <- dtplot[sel_cat %in% dtplot[year == "Total", sel_cat][1:9]]
        
        if (sel_cat %in% c("langue", "format")) {
            # if (FALSE) {
            dtplot %>%
                ggplot(aes(x = "", y = p, 
                           fill = factor(sel_cat, 
                                         levels = dtplot[year == "Total", sel_cat]))) +
                geom_bar(stat = "identity", width = 1) +
                scale_fill_brewer("", palette = "Pastel1") +
                coord_polar("y", start = 0) +
                geom_text(aes(label = fifelse(p <= 0.05, "", 
                                              paste0(round(p*100)))),
                          position = position_stack(vjust = 0.5),
                          size = 4.5) +
                facet_wrap(~year, nrow = 3) +
                theme_void() +
                theme(text = element_text(size = 16))
        } else if (sel_cat %in% c("genders", "keywords")) {
            
            dtplot %>%
                ggplot(aes(x = "", 
                           y = p, 
                           fill = factor(sel_cat, 
                                         levels = dtplot[year == "Total", sel_cat]))) +
                geom_bar(stat = "identity", width = 1, position = "dodge") +
                scale_fill_brewer("", palette = "Pastel1") +
                geom_text(aes(label = fifelse(p == 0, "", paste0(round(p*100))),
                              vjust = -0.25),
                          position = position_dodge(width = 1),
                          size = 4.5) +
                facet_wrap(~year, nrow = 2) +
                theme_void() +
                theme(text = element_text(size = 16))
        } else {
            dtplot %>% 
                ggplot(aes(x = "", 
                           y = p, 
                           fill = factor(sel_cat, 
                                         levels = dtplot[year == "Total", sel_cat]))) +
                geom_bar(stat = "identity", width = 1) +
                scale_fill_brewer("", palette = "Pastel1") +
                geom_text(aes(label = fifelse(p <= 0.05, "", 
                                              paste0(round(p*100)))),
                          position = position_stack(vjust = 0.5),
                          size = 4.5) +
                facet_wrap(~year, nrow = 2) +
                theme_void() +
                theme(text = element_text(size = 16))
        }
        
    }
})

# observeEvent(input$cat_onlyread, {
#     toggle(id = "cat_readbydate", 
#            condition = as.logical(input$cat_onlyread))
#     
#     if (!as.logical(input$cat_onlyread)) {
#         hide(id = "cat_read_date")
#         updateAwesomeCheckbox(session,
#                               "cat_readbydate",
#                               value = F)
#     }
# })

# observe({
#     toggle(id = "cat_readbydate", 
#            condition = as.logical(input$cat_onlyread))
# })

# observe({
#     toggle(id = "cat_read_date", 
#            condition = input$cat_readbydate)
# })