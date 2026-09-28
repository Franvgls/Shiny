library(shiny)
library(bslib)
library(DT)
library(dplyr)

ui <- fluidPage(

  # ====== Corporate header with logo ======
  div(
    style = "
            display: flex;
            align-items: center;
            justify-content: flex-start;
            background-color: #e8f6f3;
            border-bottom: 3px solid #117864;
            padding: 12px 20px;
        ",
    tags$img(
      src = "IMBUS_logo.png",
      height = "80px",
      style = "margin-right: 25px;"
    ),
    div(
      h2(
        "HH Gear Quality Control",
        style = "margin: 0; color: #0b5345; font-weight: bold;"
      ),
      p(
        "Warp length, net opening, wingspread, doorspread vs depth",
        style = "margin: 0; font-size: 16px; color: #117864;"
      )
    )
  ),

  theme = bs_theme(version = 5, bootswatch = "minty"),

  # ======  Principal layout ======
  sidebarLayout(

    sidebarPanel(
      h4("Upload HH files"),
      fileInput("hh_hist", "Time series (HH)", accept = c(".csv", ".txt")),
      fileInput("hh_current", "Current survey (HH)", accept = c(".csv", ".txt")),
      hr(),
      selectInput("country", "Country:",
                  choices = c("All"), selected = "All"),
      helpText(tags$small(
        "For multi-country surveys (e.g. NS-IBTS), pick one country ",
        "to compare its historical time series with the current campaign. ",
        "Choices are populated from the current-survey file (or the ",
        "historical file if no current is loaded).")),
      hr(),
      selectInput("yvar", "Variable to plot vs Depth:",
                  choices = c("WingSpread", "DoorSpread", "Netopening", "Warplngt"),
                  selected = "WingSpread"),
      helpText("Regression is y ~ log(Depth). Regression is plotted only within the range of Depth for each SweepLngt.")
    ),

    mainPanel(
      h3("Depth vs selected variable (Time series + Current survey)"),
      plotOutput("gearplot", width = "100%", height = "600px", click = "plot_click"),
      h4("Selected point:"),
      DTOutput("selected_table"),
      hr(),
      h4("Preview current survey"),
      DTOutput("current_table")
    )
  )
)

server <- function(input, output, session) {

  # read time series HH file
  hist_data <- reactive({
    req(input$hh_hist)
    df <- read.csv(input$hh_hist$datapath, stringsAsFactors = FALSE)
    names(df) <- make.names(names(df))
    validate(
      need("Depth" %in% names(df), "Time series file does not have Depth column")
    )
    df <- dplyr::filter(df, Depth > 0 & HaulVal == "V")
    df
  })

  # Read current HH file
  current_data <- reactive({
    req(input$hh_current)
    df <- read.csv(input$hh_current$datapath, stringsAsFactors = FALSE)
    names(df) <- make.names(names(df))
    validate(
      need("Depth" %in% names(df), "Current file does not have Depth column")
    )
    df <- dplyr::filter(df, Depth > 0 & HaulVal == "V")
    df
  })

  # ---- Populate the country selector ----
  # Priority: current survey countries; fallback: historical countries.
  observe({
    cur <- tryCatch(current_data(), error = function(e) NULL)
    hist <- tryCatch(hist_data(), error = function(e) NULL)

    avail <- character(0)
    if (!is.null(cur) && "Country" %in% names(cur)) {
      avail <- sort(unique(na.omit(cur$Country)))
    } else if (!is.null(hist) && "Country" %in% names(hist)) {
      avail <- sort(unique(na.omit(hist$Country)))
    }

    current_sel <- isolate(input$country)
    new_sel <- if (!is.null(current_sel) && current_sel %in% c("All", avail)) current_sel else "All"

    updateSelectInput(session, "country",
                      choices  = c("All", avail),
                      selected = new_sel)
  })

  # Reset highlighted point if country changes
  observeEvent(input$country, {
    selected_row(NULL)
  }, ignoreInit = TRUE)

  selected_row <- reactiveVal(NULL)

  # ---- Filtered datasets (applied consistently across plot / click / tables) ----
  filter_by_country <- function(df, ctry) {
    if (is.null(df) || nrow(df) == 0) return(df)
    if (is.null(ctry) || ctry == "All") return(df)
    if (!"Country" %in% names(df)) return(df)
    df[!is.na(df$Country) & df$Country == ctry, , drop = FALSE]
  }

  hist_filtered <- reactive({
    filter_by_country(hist_data(), input$country)
  })

  current_filtered <- reactive({
    req(input$hh_current)
    filter_by_country(current_data(), input$country)
  })

  output$gearplot <- renderPlot({
    req(hist_data())
    hist <- hist_filtered()
    yvar <- input$yvar
    ctry <- input$country

    # check column exists in time series file
    validate(need(yvar %in% names(hist), paste0("Time series file does not have column '", yvar, "'.") ))
    validate(need(nrow(hist) > 0,
                  paste0("No historical hauls for country '", ctry, "'.")))

    # check numeric
    hist[[yvar]] <- suppressWarnings(as.numeric(hist[[yvar]]))
    hist$Depth   <- suppressWarnings(as.numeric(hist$Depth))

    # filter positive values for yvar (discard -9/NA/<=0)
    hist <- hist[!is.na(hist[[yvar]]) & hist[[yvar]] > 0 &
                   !is.na(hist$Depth)  & hist$Depth  > 0, ]

    # current data (if there are)
    cur <- NULL
    if (!is.null(input$hh_current)) {
      cur <- current_filtered()
      validate(need(yvar %in% names(cur), paste0("Current file does not have column '", yvar, "'.") ))
      cur[[yvar]] <- suppressWarnings(as.numeric(cur[[yvar]]))
      cur$Depth   <- suppressWarnings(as.numeric(cur$Depth))
      cur <- cur[!is.na(cur[[yvar]]) & cur[[yvar]] > 0 &
                   !is.na(cur$Depth)  & cur$Depth  > 0, ]
    }

    # Joint ranges for axes
    all_x <- c(hist$Depth,   if (!is.null(cur)) cur$Depth)
    all_y <- c(hist[[yvar]], if (!is.null(cur)) cur[[yvar]])
    xlim <- if (length(all_x) > 0) range(all_x, na.rm = TRUE) else c(0, 1)
    ylim <- if (length(all_y) > 0) range(all_y, na.rm = TRUE) else c(0, 1)

    # Build title (includes country if filtering)
    plot_main <- if (is.null(ctry) || ctry == "All") {
      paste("Depth vs", yvar)
    } else {
      paste0("Depth vs ", yvar, "  \u2014  ", ctry)
    }

    plot(NA, xlim = xlim, ylim = ylim,
         xlab = "Depth (m)", ylab = yvar,
         main = plot_main,
         cex.lab = 1.4, cex.axis = 1.2, cex.main = 1.6)

    # If there is SweepLngt in time series, separate by sweep and draw points + regression
    if ("SweepLngt" %in% names(hist)) {
      tipos <- sort(unique(na.omit(hist$SweepLngt)))
      pal <- c("steelblue", "darkorange", "darkgreen", "purple",
               "brown", "magenta", "cyan4", "gold3")
      cols <- pal[seq_along(tipos)]
      labels <- character(0)
      for (i in seq_along(tipos)) {
        t <- tipos[i]
        sub <- subset(hist, SweepLngt == t & !is.na(Depth) & !is.na(hist[[yvar]]) )
        if (nrow(sub) == 0) next
        points(sub$Depth, sub[[yvar]], pch = 21, bg = cols[i], col = "black")
        rango <- range(sub$Depth, na.rm = TRUE)
        if (nrow(sub) > 5 && (rango[2] - rango[1]) > 0) {
          if (yvar == "Warplngt") {
            fmla <- as.formula(paste(yvar, "~ Depth"))
          } else {
            fmla <- as.formula(paste(yvar, "~ log(Depth)"))
          }
          mod <- tryCatch(lm(fmla, data = sub), error = function(e) NULL)
          if (!is.null(mod)) {
            dseq <- seq(rango[1], rango[2], length.out = 250)
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
        legend("topright", legend = labels, col = cols[seq_along(tipos)], lwd = 4,
               pt.bg = cols[seq_along(tipos)], pch = 21, bty = "n", cex = 0.95)
      }
    } else {
      points(hist$Depth, hist[[yvar]], pch = 21, bg = "grey", col = "black")
    }

    # Overlay current survey points (red)
    if (!is.null(cur) && nrow(cur) > 0) {
      points(cur$Depth, cur[[yvar]], pch = 21, bg = "red", col = "red", cex = 1.4)
    }

    # Highlight selected point (large green)
    sel <- selected_row()
    if (!is.null(sel)) {
      sx <- suppressWarnings(as.numeric(sel$Depth))
      sy <- suppressWarnings(as.numeric(sel[[yvar]]))
      if (!is.na(sx) && !is.na(sy)) points(sx, sy, pch = 21, bg = "green", col = "black", cex = 2.2)
    }

    # Info footer: point counts + missing-country warning
    ntxt <- paste0("Hist: ", nrow(hist), " pts",
                   if (!is.null(cur)) paste0("  |  Current: ", nrow(cur), " pts") else "")
    if (!is.null(ctry) && ctry != "All") {
      ntxt <- paste0(ntxt, "  |  Country: ", ctry)
      # Warn if current country is absent from historical file
      hist_all <- hist_data()
      if ("Country" %in% names(hist_all) &&
          !ctry %in% unique(na.omit(hist_all$Country))) {
        ntxt <- paste0(ntxt, "  (WARNING: '", ctry, "' not in historical file)")
      }
    }
    mtext(ntxt, side = 1, line = -1.2, adj = 0.01, cex = 0.9)
  })

  # Detect clicks on points in the current file (already filtered by country)
  observeEvent(input$plot_click, {
    req(current_data())
    cur <- current_filtered()
    yvar <- input$yvar
    validate(need(yvar %in% names(cur), "Column not available in this file."))
    cur[[yvar]] <- suppressWarnings(as.numeric(cur[[yvar]]))
    cur$Depth   <- suppressWarnings(as.numeric(cur$Depth))
    cur <- cur[!is.na(cur[[yvar]]) & cur[[yvar]] > 0 &
                 !is.na(cur$Depth)  & cur$Depth  > 0, ]

    if (nrow(cur) == 0) { selected_row(NULL); return() }

    near <- nearPoints(cur, input$plot_click,
                       xvar = "Depth", yvar = yvar,
                       threshold = 8, maxpoints = 1, addDist = TRUE)
    if (nrow(near) == 1) selected_row(near) else selected_row(NULL)
  })

  output$selected_table <- renderDT({
    sel <- selected_row()
    if (is.null(sel) || nrow(sel) == 0) return(NULL)
    datatable(sel, options = list(dom = 't', paging = FALSE))
  })

  output$current_table <- renderDT({
    req(current_data())
    datatable(current_filtered(),
              options = list(pageLength = 8, scrollX = TRUE))
  })
}

shinyApp(ui, server)
