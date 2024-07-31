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

observeEvent(values$books_df, {
    updatePickerInput(session, "stat_cat_year",
        choices = c(sort(unique(format(values$books_df$read_deb_date, "%Y"))), "Non daté", "Non lu"),
        selected = c(sort(unique(format(values$books_df$read_deb_date, "%Y"))), "Non daté", "Non lu")
    )

    reac_dtplot(fmt_dtplot(copy(values$books_df), input$stat_cat))
    updatePickerInput(session, "stat_cat_choices",
        choices = reac_dtplot()[year == "Total", sel_cat],
        selected = reac_dtplot()[year == "Total", sel_cat][1:8]
    )
})


# Bilan global ----

output$plot_count_tot <- renderPlot({
    d <- copy(values$books_df)
    if (!nrow(d)) {
        return()
    }

    df <- d[, .N, keyby = .(onmyshelf, read)][, onmyshelf := factor(onmyshelf,
        levels = c(TRUE, FALSE),
        labels = c("oui", "non")
    )][, read := factor(read,
        levels = c("oui", "non", "dnf"),
        labels = c("oui", "non", "pas fini")
    )][order(onmyshelf, read)][, p := N / sum(N)][, csum := rev(cumsum(rev(p)))][, pos := shift(csum, type = "lead") + p / 2][is.na(pos), pos := p / 2]

    ggplot(
        df,
        aes(
            x = 1,
            y = p,
            fill = onmyshelf,
            pattern = read,
        )
    ) +
        geom_col_pattern(
            width = 1,
            color = "transparent",
            pattern_color = "white",
            pattern_fill = "white",
            pattern_angle = 45,
            pattern_density = 0.1,
            pattern_spacing = 0.025,
            pattern_key_scale_factor = 0.6
        ) +
        coord_polar(theta = "y") +
        scale_fill_manual("Dans ma\nbibliothèque", values = c(values$settings$themeColour, "grey90")) +
        scale_pattern_manual(values = c(non = "stripe", "pas fini" = "circle", oui = "none")) +
        labs(x = "", y = "", pattern = "Lu") +
        guides(
            fill = guide_legend(
                override.aes = list(pattern = "none"),
                order = 1
            ),
            pattern = guide_legend(
                override.aes = list(fill = values$settings$themeColour),
                order = 2
            )
        ) +
        geom_text_repel(aes(y = pos, label = sprintf("%i%%\n(%i)", round(p * 100), N)),
            segment.color = NA,
            size = 5, nudge_x = 0.9, show.legend = FALSE
        ) +
        theme(
            text = element_text(size = 16),
            panel.grid = element_blank(),
            axis.text = element_blank(),
            plot.margin = margin(
                t = 0,
                r = 0,
                b = 0,
                l = 0
            )
        )
})

output$plot_count_year <- renderPlot({
    d <- copy(values$books_df)
    if (!nrow(d)) {
        return()
    }

    d[, `:=`(
        year = fcase(
            read != "oui", "Non lu",
            is.na(read_fin_date), "Non daté",
            rep_len(TRUE, nrow(d)), format(read_fin_date, "%Y")
        ),
        status = fcase(
            read != "oui" & onmyshelf, "Pas lu\ndans ma bibli",
            read == "oui" & onmyshelf, "Lu\ndans ma bibli",
            read == "oui" & !onmyshelf, "Lu\npas dans ma bibli"
        )
    )]

    dtplot <- rbind(
        d[, .N, by = .(x = status)][, numplot := 1],
        d[, .N, by = .(x = year)][, numplot := 2]
    )

    dtplot[numplot == 2] %>%
        ggplot(aes(
            x = factor(x, levels = c(
                "Pas lu\ndans ma bibli",
                "Lu\ndans ma bibli",
                "Lu\npas dans ma bibli",
                "Non lu",
                "Non daté",
                sort(grep("[0-9]+", dtplot$x, value = T))
            )),
            y = N, label = N
        )) +
        geom_bar(stat = "identity", fill = values$settings$themeColour) +
        scale_y_continuous(limits = c(0, dtplot[numplot == 2, max(N)] + 10)) +
        geom_text(vjust = -0.5, size = 5) +
        # facet_wrap(~numplot, scales = "free_x") +
        labs(y = NULL, x = NULL) +
        theme(
            text = element_text(size = 16),
            panel.grid.major.x = element_blank(),
            strip.background = element_blank(),
            strip.text = element_blank()
        )
})

output$plot_count_month <- renderPlot({
    d <- values$books_df[!is.na(read_fin_date)]

    mois <- c(
        "jan.", "fév.", "mars", "avril", "mai", "juin",
        "juil.", "août", "sept.", "oct.", "nov.", "déc."
    )

    dtplot <- d[, .N, keyby = .(year, month)]
    rbind(
        dtplot,
        dtplot[, .(year = "Total", N = sum(N)), by = month]
    )

    dtplot %>%
        ggplot(aes(
            x = forcats::fct_rev(factor(month, labels = mois)),
            y = N, label = N
        )) +
        # geom_bar(stat = "identity", fill = values$settings$themeColour) +
        geom_bar(stat = "identity") +
        scale_y_continuous(limits = c(0, max(dtplot$N) + 10)) +
        geom_text(size = 4.5, hjust = -0.5) +
        # facet_wrap(~numplot, scales = "free_x") +
        labs(y = NULL, x = NULL) +
        coord_flip() +
        facet_wrap(~year) +
        theme(
            text = element_text(size = 16),
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
reac_dtplot <- reactiveVal(NULL)
fmt_dtplot <- function(d, sel_cat) {
    if (sel_cat == "genders") {
        dtplot <- d[, year := format(read_fin_date, "%Y")][read != "oui", year := "Non lu"][, c(code_genders) := lapply(names(code_genders), grepl, genders)][, lapply(.SD, sum), .SDcols = code_genders, keyby = year] %>%
            melt(
                id.vars = "year",
                variable.name = sel_cat, value.name = "N"
            )
    } else if (sel_cat == "keywords") {
        keywords_list <- sort(unique(unlist(str_split(d$keywords, ";"))))[-1]
        dtplot <- d[, year := format(read_fin_date, "%Y")][read != "oui", year := "Non lu"][, c(keywords_list) := lapply(keywords_list, grepl, keywords)][, lapply(.SD, sum), .SDcols = keywords_list, keyby = year] %>%
            melt(
                id.vars = "year",
                variable.name = sel_cat, value.name = "N"
            )
    } else {
        dtplot <- d[, year := format(read_fin_date, "%Y")][read != "oui", year := "Non lu"][, .N, keyby = c("year", sel_cat)]
    }

    rbind(
        dtplot,
        dtplot[, .(year = "Total", N = sum(N)), by = sel_cat]
    )[, .(sel_cat = as.character(get(sel_cat)), N, p = N / sum(N)), keyby = year][is.na(sel_cat), sel_cat := "Non renseigné"][is.na(year), year := "Non daté"][order(year, -p)]
}

observeEvent(input$stat_cat, {
    if (is.null(values$books_df)) {
        return()
    }

    reac_dtplot(fmt_dtplot(copy(values$books_df), input$stat_cat))
    updatePickerInput(session, "stat_cat_choices",
        choices = reac_dtplot()[year == "Total", sel_cat],
        selected = reac_dtplot()[year == "Total", sel_cat][1:8]
    )
})


## Tableau ----
output$cat_table <- render_gt({
    if (!nrow(values$books_df)) {
        return()
    }

    sel_cat <- input$stat_cat

    if (is.null(input$stat_cat_year)) {
        dtplot <- reac_dtplot()[year == "Total"]
    } else {
        dtplot <- reac_dtplot()[year %in% input$stat_cat_year,
            .(N = sum(N)),
            keyby = sel_cat
        ]
    }

    if (sel_cat %in% c("genders", "keywords")) {
        dtplot <- dtplot[, .(sel_cat, N, p = paste0(round(N * 100 / sum(N)), "%"))]
    } else {
        dtplot <- rbind(dtplot[, .(sel_cat, N, p = paste0(round(N * 100 / sum(N)), "%"))],
            dtplot[, .("Total", sum(N), "100%")],
            use.names = FALSE
        )
    }

    dtplot[order(N), .(sel_cat, N, p)] %>%
        gt() %>%
        tab_options(
            table.font.size = px(14),
            column_labels.border.top.style = "hidden"
        ) %>%
        cols_label(.list = setNames(
            c("", "Livres", "%"),
            c("sel_cat", "N", "p")
        )) %>%
        cols_align(columns = 1, align = "left") %>%
        cols_align(columns = 2:3, align = "center") %>%
        cols_width(
            1 ~ px(200),
            2:3 ~ px(75)
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "top",
                color = "#D3D3D3",
                weight = fifelse(sel_cat %in% c("genders", "keywords"), px(1), px(2))
            ),
            locations = cells_body(rows = nrow(dtplot))
        )
})

## Graphe ----
output$cat_plot <- renderPlot({
    sel_cat <- input$stat_cat
    dtplot <- reac_dtplot()[year %in% c(input$stat_cat_year, "Total")]
    nyears <- length(unique(dtplot$year))

    if (!nrow(dtplot)) {
        return()
    }

    dtplot <- rbind(
        dtplot[sel_cat %in% input$stat_cat_choices],
        dtplot[!(sel_cat %in% input$stat_cat_choices),
            .(sel_cat = "Autre", N = sum(N), p = sum(p)),
            keyby = "year"
        ]
    )

    if (!length(input$stat_cat_choices)) {
        pal <- pal_cats[9]
    } else if (dtplot[sel_cat == "Autre", .N]) {
        pal <- c(pal_cats[seq_along(input$stat_cat_choices)], pal_cats[9])
    } else {
        pal <- pal_cats[-9]
    }

    if (sel_cat %in% c("langue", "format")) {
        if (input$stat_cat_fmt == "p") {
            dtplot[, labs := fifelse(p <= 0.05, "", paste0(round(p * 100)))]
        } else {
            dtplot[, labs := fifelse(p <= 0.05, "", as.character(N))]
        }

        dtplot %>%
            ggplot(aes(
                x = "", y = p,
                fill = factor(sel_cat,
                    levels = dtplot[year == "Total", sel_cat]
                )
            )) +
            geom_bar(stat = "identity", width = 1) +
            scale_fill_manual("", values = pal) +
            coord_polar("y", start = 0) +
            geom_text(aes(label = labs),
                position = position_stack(vjust = 0.5),
                size = 4.5
            ) +
            facet_wrap(~year, nrow = 5) +
            theme_void() +
            theme(text = element_text(size = 16))
    } else if (sel_cat %in% c("genders", "keywords")) {
        if (input$stat_cat_fmt == "p") {
            dtplot[, labs := fifelse(p == 0, "", paste0(round(p * 100)))]
        } else {
            dtplot[, labs := fifelse(p == 0, "", as.character(N))]
        }

        dtplot %>%
            ggplot(aes(
                x = "",
                y = p,
                fill = factor(sel_cat,
                    levels = dtplot[year == "Total", sel_cat]
                )
            )) +
            geom_bar(stat = "identity", width = 1, position = "dodge") +
            scale_fill_manual("", values = pal) +
            geom_text(
                aes(
                    label = labs,
                    vjust = -0.25
                ),
                position = position_dodge(width = 1),
                size = 4.5
            ) +
            facet_wrap(~year, nrow = fifelse(nyears <= 5, 1, 2)) +
            theme_void() +
            theme(text = element_text(size = 16))
    } else {
        if (input$stat_cat_fmt == "p") {
            dtplot[, labs := fifelse(p <= 0.05, "", paste0(round(p * 100)))]
        } else {
            dtplot[, labs := fifelse(p <= 0.05, "", as.character(N))]
        }

        dtplot %>%
            ggplot(aes(
                x = "",
                y = p,
                fill = factor(sel_cat,
                    levels = dtplot[year == "Total", sel_cat]
                )
            )) +
            geom_bar(stat = "identity", width = 1) +
            scale_fill_manual("", values = pal) +
            geom_text(aes(label = labs),
                position = position_stack(vjust = 0.5),
                size = 4.5
            ) +
            facet_wrap(~year, nrow = fifelse(nyears <= 5, 1, 2)) +
            theme_void() +
            theme(text = element_text(size = 16))
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
