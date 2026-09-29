## Module: QC Dist
##
## Reviews distance/speed/course errors from a HH data.frame.
## Auto-detects the country if the HH contains a single one; otherwise
## presents a selectInput for the user to pick one country (or "All"
## for a multi-panel dashboard view).
##
## Usage in master app.R:
##   UI:     qcDistUI("qc_dist")
##   Server: qcDistServer("qc_dist", hh_data = hh_data)
## where hh_data is a reactiveVal holding the loaded HH data.frame.


# ---- Helper: coerce HH columns to numeric and DATRAS -9 -> NA ----
# Error columns are NOT computed here: the click handler uses the table
# returned by qcHaulsDist(allHauls = TRUE), i.e. exactly the plotted values
# (including its haversine fill of missing Distance and its roundings).
.prep_qc_hh <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    for (col in c("HaulNo","Year","Quarter","Distance","GroundSpeed",
                  "HaulDur","TowDir","ShootLong","ShootLat","HaulLong","HaulLat")) {
        if (col %in% names(df)) df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
    for (col in c("Distance","GroundSpeed","TowDir","HaulDur",
                  "ShootLat","ShootLong","HaulLat","HaulLong")) {
        if (col %in% names(df)) {
            bad <- !is.na(df[[col]]) & df[[col]] == -9
            df[[col]][bad] <- NA
        }
    }
    df
}

.cols_dist <- c("Survey","Year","Quarter","quarter","Country","HaulNo",
                "Distance","GroundSpeed","HaulDur","TowDir",
                "dist.hf","dist.vel","vel.dist",
                "error.dist","error.vel","rumb","error.rumb")


# ================= UI function =================
qcDistUI <- function(id) {
    ns <- NS(id)   # every input/output goes through ns(); this is the module namespace

    sidebarLayout(
        sidebarPanel(
            selectInput(ns("errorType"), "Error type:",
                        choices = c("Dist","Speed","Course"), selected = "Dist"),
            numericInput(ns("pcerror"), "Acceptable error (%):",
                         value = 2, min = 0, step = 0.1),
            selectInput(ns("country"), "Country:",
                        choices = c("All"), selected = "All"),
            uiOutput(ns("country_hint")),
            helpText(tags$small(
                "Click selection of individual hauls only works in ",
                "single-country view (base R multi-panel plots do not ",
                "report which panel was clicked)."))
        ),
        mainPanel(
            verbatimTextOutput(ns("info")),
            plotOutput(ns("plot"), click = ns("click"),
                       width = "100%", height = "700px"),
            DT::dataTableOutput(ns("click_info"))
        )
    )
}


# ================= Server function =================
qcDistServer <- function(id, hh_data) {
    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        # --- Reactive: country list detected from the HH ---
        available_countries <- reactive({
            df <- hh_data()
            if (is.null(df) || !"Country" %in% names(df)) return(character(0))
            sort(unique(na.omit(df$Country)))
        })

        # --- Update selectInput when countries change ---
        # Auto-select the country when there is only one in the file.
        observe({
            avail <- available_countries()
            if (length(avail) == 0) {
                updateSelectInput(session, "country",
                                  choices = c("All"), selected = "All")
            } else if (length(avail) == 1) {
                updateSelectInput(session, "country",
                                  choices  = avail,
                                  selected = avail)
            } else {
                # multi-country: give "All" + the actual list
                current <- isolate(input$country)
                new_sel <- if (!is.null(current) && current %in% c("All", avail)) current else "All"
                updateSelectInput(session, "country",
                                  choices  = c("All", avail),
                                  selected = new_sel)
            }
        })

        # --- Hint under the selector ---
        output$country_hint <- renderUI({
            avail <- available_countries()
            if (length(avail) == 0) {
                tags$small(style = "color:#888;", "Load a HH file first.")
            } else if (length(avail) == 1) {
                tags$small(style = "color:#117864;",
                           HTML(paste0("Auto-detected: <b>", avail, "</b>")))
            } else {
                tags$small(style = "color:#117864;",
                           paste0("Multi-country file (",
                                  length(avail),
                                  " countries) - pick one for click-selection"))
            }
        })

        # --- Reset selected point when country changes ---
        selected <- reactiveVal(NULL)
        plotted  <- reactiveVal(NULL)   # table returned by qcHaulsDist for the current plot
        observeEvent(input$country, {
            selected(NULL)
        }, ignoreInit = TRUE)

        # --- Filtered subset (Survey/Year/Quarter + country) ---
        qc_subset <- reactive({
            df <- hh_data()
            req(df)
            sr <- unique(na.omit(df$Survey))[1]
            yr <- unique(na.omit(df$Year))[1]
            qt <- unique(na.omit(df$Quarter))[1]
            df_sub <- subset(df, Survey == sr & Year == yr & Quarter == qt)
            ctry <- input$country
            if (!is.null(ctry) && ctry != "All" && "Country" %in% names(df_sub)) {
                df_sub <- df_sub[!is.na(df_sub$Country) & df_sub$Country == ctry, ]
            }
            list(df = df_sub, sr = sr, yr = yr, qt = qt, country = ctry)
        })

        # --- Info panel ---
        output$info <- renderPrint({
            df <- hh_data()
            if (is.null(df)) {
                cat("\u26A0\uFE0F No file uploaded yet.\n")
                return()
            }
            sub <- qc_subset()
            cat("Survey:        ", sub$sr, "\n", sep = "")
            cat("Year/Quarter:  ", sub$yr, " / ", sub$qt, "\n", sep = "")
            cat("Country filter:", sub$country, "\n", sep = " ")
            cat("Hauls in view: ", nrow(sub$df), "\n", sep = "")
            # qcHaulsDist fills missing Distance with the haversine distance
            # between shoot and haul positions, so the Dist error of those
            # hauls is ~0 by construction (only rounding) and not informative.
            d <- .prep_qc_hh(sub$df)
            if ("Distance" %in% names(d)) {
                n_na <- sum(is.na(d$Distance))
                if (n_na > 0)
                    cat("\n⚠️ Distance missing (-9/NA) in ", n_na, " of ", nrow(d),
                        " hauls: replaced by haversine distance between positions.\n",
                        "   Dist error is ~0 for them (not informative); Speed error ",
                        "compares speed x duration with that distance.\n", sep = "")
            }
        })

        # --- Main plot ---
        output$plot <- renderPlot({
            sub <- qc_subset()
            validate(need(nrow(sub$df) > 0,
                          "No records for current Survey/Year/Quarter/Country selection."))

            # allHauls = TRUE returns the table the plot was drawn from; it is
            # stored so clicks are matched against exactly the plotted values.
            res <- NeAtlIBTS64::qcHaulsDist(
                Survey   = .prep_qc_hh(sub$df),
                years    = sub$yr,
                quarter  = sub$qt,
                getICES  = FALSE,
                allHauls = TRUE,
                error    = input$errorType,
                pc.error = input$pcerror
            )
            plotted(if (is.data.frame(res)) res else NULL)

            # Highlight selected point (mono-country only)
            sel <- selected()
            if (!is.null(sub$country) && sub$country != "All" &&
                !is.null(sel) && nrow(sel) >= 1) {
                ev <- switch(input$errorType,
                             "Dist"   = "error.dist",
                             "Speed"  = "error.vel",
                             "Course" = "error.rumb")
                if (ev %in% names(sel) && !is.na(sel[[ev]]) && !is.na(sel$HaulNo)) {
                    points(as.numeric(sel$HaulNo), as.numeric(sel[[ev]]),
                           pch = 21, bg = "green", cex = 2.4)
                }
            }
        })

        # --- Click handler ---
        observeEvent(input$click, {
            if (is.null(input$country) || input$country == "All") {
                selected(NULL); return()
            }
            df_plot <- plotted()
            if (is.null(df_plot) || nrow(df_plot) == 0) { selected(NULL); return() }

            ev <- switch(input$errorType,
                         "Dist"   = "error.dist",
                         "Speed"  = "error.vel",
                         "Course" = "error.rumb")
            if (!("HaulNo" %in% names(df_plot)) || !(ev %in% names(df_plot))) {
                selected(NULL); return()
            }

            df_click <- df_plot[!is.na(df_plot$HaulNo) & !is.na(df_plot[[ev]]), ]
            near <- nearPoints(df_click, input$click,
                               xvar = "HaulNo", yvar = ev,
                               maxpoints = 1, addDist = TRUE, threshold = 20)
            if (nrow(near) == 1) selected(near) else selected(NULL)
        })

        # --- Table with clicked point ---
        output$click_info <- DT::renderDT({
            sel <- selected()
            if (is.null(sel) || nrow(sel) == 0) return(NULL)
            keep <- intersect(.cols_dist, names(sel))
            DT::datatable(sel[, keep, drop = FALSE],
                          options = list(dom = 't', paging = FALSE, scrollX = TRUE),
                          rownames = FALSE)
        })

    })
}
