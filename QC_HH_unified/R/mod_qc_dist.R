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


# ---- Helper: compute error columns (mirrors qcHaulsDist internals) ----
.compute_qc_errors <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)

    numify <- function(x) suppressWarnings(as.numeric(x))
    for (col in c("HaulNo","Year","Quarter","Distance","GroundSpeed",
                  "HaulDur","TowDir","ShootLong","ShootLat","HaulLong","HaulLat")) {
        if (col %in% names(df)) df[[col]] <- numify(df[[col]])
    }

    # DATRAS -9 sentinel -> NA
    for (col in c("Distance","GroundSpeed","TowDir","HaulDur",
                  "ShootLat","ShootLong","HaulLat","HaulLong")) {
        if (col %in% names(df)) {
            bad <- !is.na(df[[col]]) & df[[col]] == -9
            df[[col]][bad] <- NA
        }
    }

    dist_vel <- (df$HaulDur / 60) * df$GroundSpeed * 1852
    ok_hf <- !is.na(df$ShootLong) & !is.na(df$ShootLat) &
             !is.na(df$HaulLong)  & !is.na(df$HaulLat)
    dist_hf <- rep(NA_real_, nrow(df))
    if (any(ok_hf)) {
        dist_hf[ok_hf] <- geosphere::distHaversine(
            as.matrix(df[ok_hf, c("ShootLong","ShootLat")]),
            as.matrix(df[ok_hf, c("HaulLong","HaulLat")]))
    }
    vel_dist <- (dist_hf / 1852) / (df$HaulDur / 60)

    err_dist <- ifelse(!is.na(df$Distance) & df$Distance != 0,
                       (dist_hf - df$Distance) * 100 / df$Distance, NA_real_)
    err_vel  <- ifelse(!is.na(df$Distance) & df$Distance != 0,
                       (dist_vel - df$Distance) * 100 / df$Distance, NA_real_)

    rumb <- rep(NA_real_, nrow(df))
    if (any(ok_hf)) {
        rumb[ok_hf] <- geosphere::bearingRhumb(
            as.matrix(df[ok_hf, c("ShootLong","ShootLat")]),
            as.matrix(df[ok_hf, c("HaulLong","HaulLat")]))
    }
    err_rumb <- ifelse(!is.na(rumb) & !is.na(df$TowDir), rumb - df$TowDir, NA_real_)

    df$dist.vel   <- dist_vel
    df$dist.hf    <- round(dist_hf)
    df$vel.dist   <- round(vel_dist, 1)
    df$error.dist <- round(err_dist, 2)
    df$error.vel  <- round(err_vel, 2)
    df$rumb       <- round(rumb, 1)
    df$error.rumb <- round(err_rumb, 1)
    df
}

.cols_dist <- c("Survey","Year","Quarter","Country","HaulNo",
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
        })

        # --- Main plot ---
        output$plot <- renderPlot({
            sub <- qc_subset()
            validate(need(nrow(sub$df) > 0,
                          "No records for current Survey/Year/Quarter/Country selection."))

            df_plot <- .compute_qc_errors(sub$df)

            NeAtlIBTS64::qcHaulsDist(
                Survey   = df_plot,
                years    = sub$yr,
                quarter  = sub$qt,
                getICES  = FALSE,
                error    = input$errorType,
                pc.error = input$pcerror
            )

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
            sub <- qc_subset()
            df_plot <- .compute_qc_errors(sub$df)

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
