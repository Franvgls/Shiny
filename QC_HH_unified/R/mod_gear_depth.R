## Module: Gear vs Depth
##
## Compares HH data from the current campaign against the historical time
## series, plotting a chosen gear variable (WingSpread, DoorSpread, Netopening
## or Warplngt) against Depth. Historical data are split by SweepLngt with a
## regression line per group; current-campaign data are overlaid in red.
##
## Country auto-detection:
##   - The country selector is populated from the countries present in the
##     CURRENT file (which is typically a single-country vessel campaign).
##   - If a single country is detected in the current file, it is selected
##     automatically and the historical data are filtered to that country.
##   - If the current file has multiple countries, or none is loaded, the
##     user picks from the historical countries.
##
## Usage in master app.R:
##   UI:     gearDepthUI("gear")
##   Server: gearDepthServer("gear")
## No external reactiveVal is needed - this module loads its own two files.


# ---- Helper: normalise column names (module-scoped copy) ----
.normalize_hh_names_gd <- function(df) {
    if (is.null(df)) return(NULL)
    nms <- names(df); low <- tolower(nms)
    repl <- function(target, new) {
        i <- which(low == tolower(target))
        if (length(i) == 1) names(df)[i] <<- new
    }
    repl("survey", "Survey"); repl("year", "Year"); repl("quarter", "Quarter")
    repl("haulno", "HaulNo"); repl("haullong", "HaulLong"); repl("haullat", "HaulLat")
    repl("shootlong", "ShootLong"); repl("shootlat", "ShootLat")
    repl("country", "Country"); repl("ship", "Ship"); repl("towdir", "TowDir")
    repl("distance", "Distance"); repl("groundspeed", "GroundSpeed"); repl("hauldur", "HaulDur")
    repl("haulval", "HaulVal"); repl("depth", "Depth"); repl("sweeplngt", "SweepLngt")
    repl("wingspread", "WingSpread"); repl("doorspread", "DoorSpread")
    repl("netopening", "Netopening"); repl("warplngt", "Warplngt")
    df
}


# ================= UI function =================
gearDepthUI <- function(id) {
    ns <- NS(id)

    sidebarLayout(
        sidebarPanel(
            h4("Upload HH files"),
            fileInput(ns("hh_hist"),    "Time series (HH)",   accept = c(".csv", ".txt")),
            fileInput(ns("hh_current"), "Current survey (HH)", accept = c(".csv", ".txt")),
            hr(),
            selectInput(ns("country"), "Country:",
                        choices = c("All"), selected = "All"),
            uiOutput(ns("country_hint")),
            hr(),
            selectInput(ns("yvar"), "Variable to plot vs Depth:",
                        choices  = c("WingSpread","DoorSpread","Netopening","Warplngt"),
                        selected = "WingSpread"),
            helpText(tags$small(
                "Regression is y ~ log(Depth) (linear for Warplngt). ",
                "Historical regressions are split by SweepLngt and drawn ",
                "only within each group's Depth range."))
        ),
        mainPanel(
            h3("Depth vs selected variable (Time series + Current survey)"),
            plotOutput(ns("plot"), width = "100%", height = "600px",
                       click = ns("click")),
            h4("Selected point:"),
            DT::dataTableOutput(ns("selected_table")),
            hr(),
            h4("Preview current survey"),
            DT::dataTableOutput(ns("current_table"))
        )
    )
}


# ================= Server function =================
gearDepthServer <- function(id) {
    moduleServer(id, function(input, output, session) {

        # ---- Read files ----
        hist_data <- reactive({
            req(input$hh_hist)
            df <- read.csv(input$hh_hist$datapath, stringsAsFactors = FALSE)
            df <- .normalize_hh_names_gd(df)
            validate(need("Depth" %in% names(df), "Time series file lacks 'Depth' column"))
            df$Depth <- suppressWarnings(as.numeric(df$Depth))
            if ("HaulVal" %in% names(df)) df <- df[df$HaulVal == "V", ]
            df <- df[!is.na(df$Depth) & df$Depth > 0, ]
            df
        })

        current_data <- reactive({
            req(input$hh_current)
            df <- read.csv(input$hh_current$datapath, stringsAsFactors = FALSE)
            df <- .normalize_hh_names_gd(df)
            validate(need("Depth" %in% names(df), "Current file lacks 'Depth' column"))
            df$Depth <- suppressWarnings(as.numeric(df$Depth))
            if ("HaulVal" %in% names(df)) df <- df[df$HaulVal == "V", ]
            df <- df[!is.na(df$Depth) & df$Depth > 0, ]
            df
        })

        # ---- Country auto-detection ----
        # Priority: current file countries. Fallback: historical countries.
        available_countries <- reactive({
            cur <- tryCatch(current_data(), error = function(e) NULL)
            if (!is.null(cur) && "Country" %in% names(cur)) {
                avail <- sort(unique(na.omit(cur$Country)))
                if (length(avail) > 0) return(list(source = "current", countries = avail))
            }
            hist <- tryCatch(hist_data(), error = function(e) NULL)
            if (!is.null(hist) && "Country" %in% names(hist)) {
                avail <- sort(unique(na.omit(hist$Country)))
                if (length(avail) > 0) return(list(source = "historical", countries = avail))
            }
            list(source = "none", countries = character(0))
        })

        # ---- Update selectInput reactively ----
        observe({
            ac <- available_countries()
            avail <- ac$countries
            src   <- ac$source

            if (length(avail) == 0) {
                updateSelectInput(session, "country",
                                  choices = c("All"), selected = "All")
            } else if (length(avail) == 1 && src == "current") {
                # Single-vessel campaign: auto-select
                updateSelectInput(session, "country",
                                  choices  = avail,
                                  selected = avail)
            } else {
                current_sel <- isolate(input$country)
                new_sel <- if (!is.null(current_sel) && current_sel %in% c("All", avail)) current_sel else "All"
                updateSelectInput(session, "country",
                                  choices  = c("All", avail),
                                  selected = new_sel)
            }
        })

        output$country_hint <- renderUI({
            ac <- available_countries()
            if (length(ac$countries) == 0) {
                tags$small(style = "color:#888;", "Load HH files to enable country filter.")
            } else if (length(ac$countries) == 1 && ac$source == "current") {
                tags$small(style = "color:#117864;",
                           HTML(paste0("Auto-detected from current file: <b>", ac$countries, "</b>")))
            } else if (ac$source == "current") {
                tags$small(style = "color:#117864;",
                           paste0("Current file has ", length(ac$countries),
                                  " countries - pick one to compare"))
            } else {
                tags$small(style = "color:#888;",
                           "Countries taken from historical file (no current loaded).")
            }
        })

        # ---- Reset highlighted point when country changes ----
        selected_row <- reactiveVal(NULL)
        observeEvent(input$country, {
            selected_row(NULL)
        }, ignoreInit = TRUE)

        # ---- Filtered datasets ----
        .filter_ctry <- function(df, ctry) {
            if (is.null(df) || nrow(df) == 0) return(df)
            if (is.null(ctry) || ctry == "All") return(df)
            if (!"Country" %in% names(df)) return(df)
            df[!is.na(df$Country) & df$Country == ctry, , drop = FALSE]
        }
        hist_filtered    <- reactive({ .filter_ctry(hist_data(),    input$country) })
        current_filtered <- reactive({
            req(input$hh_current)
            .filter_ctry(current_data(), input$country)
        })

        # ---- Main plot ----
        output$plot <- renderPlot({
            req(hist_data())
            hist <- hist_filtered()
            yvar <- input$yvar
            ctry <- input$country

            validate(need(yvar %in% names(hist),
                          paste0("Time series file does not have column '", yvar, "'.")))
            validate(need(nrow(hist) > 0,
                          paste0("No historical hauls for country '", ctry, "'.")))

            hist[[yvar]] <- suppressWarnings(as.numeric(hist[[yvar]]))
            hist <- hist[!is.na(hist[[yvar]]) & hist[[yvar]] > 0, ]

            cur <- NULL
            if (!is.null(input$hh_current)) {
                cur <- current_filtered()
                validate(need(yvar %in% names(cur),
                              paste0("Current file does not have column '", yvar, "'.")))
                cur[[yvar]] <- suppressWarnings(as.numeric(cur[[yvar]]))
                cur <- cur[!is.na(cur[[yvar]]) & cur[[yvar]] > 0, ]
            }

            all_x <- c(hist$Depth,   if (!is.null(cur)) cur$Depth)
            all_y <- c(hist[[yvar]], if (!is.null(cur)) cur[[yvar]])
            xlim <- if (length(all_x) > 0) range(all_x, na.rm = TRUE) else c(0, 1)
            # Net opening is read against the seabed: y axis from 0 to max * 1.1.
            # Door/wing spread and warp keep the data range (R adds 4% margin).
            ylim <- if (length(all_y) == 0) c(0, 1)
                    else if (yvar == "Netopening") c(0, max(all_y, na.rm = TRUE) * 1.1)
                    else range(all_y, na.rm = TRUE)

            plot_main <- if (is.null(ctry) || ctry == "All") {
                paste("Depth vs", yvar)
            } else {
                paste0("Depth vs ", yvar, "  \u2014  ", ctry)
            }

            plot(NA, xlim = xlim, ylim = ylim,
                 xlab = "Depth (m)", ylab = yvar, main = plot_main,
                 cex.lab = 1.4, cex.axis = 1.2, cex.main = 1.6)

            # Historical points + regression per SweepLngt
            if ("SweepLngt" %in% names(hist)) {
                tipos <- sort(unique(na.omit(hist$SweepLngt)))
                pal   <- c("steelblue","darkorange","darkgreen","purple",
                           "brown","magenta","cyan4","gold3")
                cols  <- pal[seq_along(tipos)]
                labels <- character(0)
                for (i in seq_along(tipos)) {
                    t <- tipos[i]
                    sub <- hist[!is.na(hist$SweepLngt) & hist$SweepLngt == t &
                                !is.na(hist$Depth) & !is.na(hist[[yvar]]), ]
                    if (nrow(sub) == 0) next
                    points(sub$Depth, sub[[yvar]], pch = 21, bg = cols[i], col = "black")
                    rango <- range(sub$Depth, na.rm = TRUE)
                    if (nrow(sub) > 5 && (rango[2] - rango[1]) > 0) {
                        fmla <- if (yvar == "Warplngt") {
                            as.formula(paste(yvar, "~ Depth"))
                        } else {
                            as.formula(paste(yvar, "~ log(Depth)"))
                        }
                        mod <- tryCatch(lm(fmla, data = sub), error = function(e) NULL)
                        if (!is.null(mod)) {
                            dseq  <- seq(rango[1], rango[2], length.out = 250)
                            preds <- predict(mod, newdata = data.frame(Depth = dseq))
                            lines(dseq, preds, col = cols[i], lwd = 4)
                            labels <- c(labels, paste0("SweepLngt=", t,
                                                       " (", round(rango[1]), "\u2013", round(rango[2]), " m)",
                                                       if (yvar == "Warplngt") " [linear]" else " [log]"))
                        }
                    } else {
                        labels <- c(labels, paste0("SweepLngt=", t, " (", nrow(sub), " pts)"))
                    }
                }
                if (length(labels) > 0) {
                    legend("topright", legend = labels, col = cols[seq_along(tipos)],
                           lwd = 4, pt.bg = cols[seq_along(tipos)], pch = 21,
                           bty = "n", cex = 0.95)
                }
            } else {
                points(hist$Depth, hist[[yvar]], pch = 21, bg = "grey", col = "black")
            }

            # Current campaign points
            if (!is.null(cur) && nrow(cur) > 0) {
                points(cur$Depth, cur[[yvar]], pch = 21, bg = "red", col = "red", cex = 1.4)
            }

            # Highlighted click
            sel <- selected_row()
            if (!is.null(sel)) {
                sx <- suppressWarnings(as.numeric(sel$Depth))
                sy <- suppressWarnings(as.numeric(sel[[yvar]]))
                if (!is.na(sx) && !is.na(sy))
                    points(sx, sy, pch = 21, bg = "green", col = "black", cex = 2.2)
            }

            # Footer with counts + missing-country warning
            ntxt <- paste0("Hist: ", nrow(hist), " pts",
                           if (!is.null(cur)) paste0("  |  Current: ", nrow(cur), " pts") else "")
            if (!is.null(ctry) && ctry != "All") {
                ntxt <- paste0(ntxt, "  |  Country: ", ctry)
                hist_all <- hist_data()
                if ("Country" %in% names(hist_all) &&
                    !ctry %in% unique(na.omit(hist_all$Country))) {
                    ntxt <- paste0(ntxt, "  (WARNING: '", ctry, "' not in historical file)")
                }
            }
            mtext(ntxt, side = 1, line = -1.2, adj = 0.01, cex = 0.9)
        })

        # ---- Click handling (only on current-campaign points) ----
        observeEvent(input$click, {
            req(current_data())
            cur  <- current_filtered()
            yvar <- input$yvar
            validate(need(yvar %in% names(cur), "Column not available in this file."))
            cur[[yvar]] <- suppressWarnings(as.numeric(cur[[yvar]]))
            cur <- cur[!is.na(cur[[yvar]]) & cur[[yvar]] > 0 &
                       !is.na(cur$Depth)  & cur$Depth  > 0, ]

            if (nrow(cur) == 0) { selected_row(NULL); return() }

            near <- nearPoints(cur, input$click,
                               xvar = "Depth", yvar = yvar,
                               threshold = 8, maxpoints = 1, addDist = TRUE)
            if (nrow(near) == 1) selected_row(near) else selected_row(NULL)
        })

        # ---- Selected point table ----
        output$selected_table <- DT::renderDT({
            sel <- selected_row()
            if (is.null(sel) || nrow(sel) == 0) return(NULL)
            DT::datatable(sel, options = list(dom = 't', paging = FALSE, scrollX = TRUE),
                          rownames = FALSE)
        })

        # ---- Current-survey preview (filtered by country) ----
        output$current_table <- DT::renderDT({
            req(current_data())
            DT::datatable(current_filtered(),
                          options = list(pageLength = 8, scrollX = TRUE),
                          rownames = FALSE)
        })

    })
}
