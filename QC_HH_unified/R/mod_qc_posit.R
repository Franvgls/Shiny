## Module: QC Posit
##
## Plots haul positions on a map (tracks or points) and lets the user click
## on individual hauls to inspect their record. Reads from the top-level
## hh_data reactiveVal shared with qc_dist.
##
## Country filter behaves like qc_dist:
##   - Single country in file  -> auto-selected
##   - Multi-country            -> selectInput with "All" + countries
##   - "All" is a valid working mode here (single-panel plot, click works)
##
## Usage in master app.R:
##   UI:     qcPositUI("qc_posit")
##   Server: qcPositServer("qc_posit", hh_data = hh_data)


# ================= UI function =================
qcPositUI <- function(id) {
    ns <- NS(id)

    sidebarLayout(
        sidebarPanel(
            selectInput(ns("graf"), "Plot type:",
                        choices = c("Tracks","Points"), selected = "Tracks"),
            selectInput(ns("country"), "Country:",
                        choices = c("All"), selected = "All"),
            uiOutput(ns("country_hint")),
            helpText(tags$small(
                "Click on any haul to see its record below. The country ",
                "filter narrows the view to a single country if desired."))
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
qcPositServer <- function(id, hh_data) {
    moduleServer(id, function(input, output, session) {

        # --- Country list detected from HH ---
        available_countries <- reactive({
            df <- hh_data()
            if (is.null(df) || !"Country" %in% names(df)) return(character(0))
            sort(unique(na.omit(df$Country)))
        })

        # --- Populate selectInput reactively ---
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
                current <- isolate(input$country)
                new_sel <- if (!is.null(current) && current %in% c("All", avail)) current else "All"
                updateSelectInput(session, "country",
                                  choices  = c("All", avail),
                                  selected = new_sel)
            }
        })

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
                                  " countries) - 'All' shows the whole map"))
            }
        })

        # --- Reset selection when the country filter changes ---
        selected <- reactiveVal(NULL)
        observeEvent(input$country, {
            selected(NULL)
        }, ignoreInit = TRUE)

        plot_warn <- reactiveVal(character(0))   # warnings raised while plotting

        # --- Filtered subset (Survey/Year/Quarter + country) ---
        posit_subset <- reactive({
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
            sub <- posit_subset()
            cat("Survey:        ", sub$sr, "\n", sep = "")
            cat("Year/Quarter:  ", sub$yr, " / ", sub$qt, "\n", sep = "")
            cat("Country filter:", sub$country, "\n", sep = " ")
            cat("Hauls in view: ", nrow(sub$df), "\n", sep = "")
            for (msg in plot_warn()) cat("\n⚠️ ", msg, "\n", sep = "")
        })

        # --- Main plot ---
        output$plot <- renderPlot({
            sub <- posit_subset()
            validate(need(nrow(sub$df) > 0,
                          "No records for current Survey/Year/Quarter/Country selection."))

            # Warnings (e.g. hauls with missing positions) are collected
            # and shown in the info panel.
            w <- character(0)
            withCallingHandlers(
                NeAtlIBTS64::qcHaulsPosit(
                    Survey  = sub$df,
                    years   = sub$yr,
                    quarter = sub$qt,
                    getICES = FALSE,
                    Hpoints = (input$graf == "Points"),
                    ti      = FALSE
                ),
                warning = function(x) {
                    w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning")
                })
            plot_warn(unique(w))

            # Highlight selected haul (click works in single-panel plots)
            sel <- selected()
            if (!is.null(sel) && nrow(sel) >= 1) {
                sx <- suppressWarnings(as.numeric(sel$HaulLong))
                sy <- suppressWarnings(as.numeric(sel$HaulLat))
                if (!is.na(sx) && !is.na(sy)) {
                    points(sx, sy, pch = 21, bg = "green", col = "black", cex = 2.2)
                }
            }
        })

        # --- Click handler ---
        observeEvent(input$click, {
            sub <- posit_subset()
            if (nrow(sub$df) == 0) { selected(NULL); return() }

            df_click <- sub$df
            df_click$HaulLong <- suppressWarnings(as.numeric(df_click$HaulLong))
            df_click$HaulLat  <- suppressWarnings(as.numeric(df_click$HaulLat))
            df_click <- df_click[!is.na(df_click$HaulLong) & !is.na(df_click$HaulLat), ]

            if (nrow(df_click) == 0) { selected(NULL); return() }

            near <- nearPoints(df_click, input$click,
                               xvar = "HaulLong", yvar = "HaulLat",
                               maxpoints = 1, addDist = TRUE, threshold = 12)
            if (nrow(near) == 1) selected(near) else selected(NULL)
        })

        # --- Table with clicked haul ---
        .cols_posit <- c("Survey","Year","Quarter","Country","Ship","HaulNo",
                         "ShootLat","ShootLong","HaulLat","HaulLong",
                         "Distance","GroundSpeed","HaulDur","TowDir","Depth")
        output$click_info <- DT::renderDT({
            sel <- selected()
            if (is.null(sel) || nrow(sel) == 0) return(NULL)
            keep <- intersect(.cols_posit, names(sel))
            DT::datatable(sel[, keep, drop = FALSE],
                          options = list(dom = 't', paging = FALSE, scrollX = TRUE),
                          rownames = FALSE)
        })

    })
}
