## Module: Outliers
##
## Downloads HH data from DATRAS for a chosen survey / year range / quarters,
## displays four gear-parameter plots vs Depth (Warplngt, WingSpread,
## DoorSpread, Netopening), lets the user click-select outliers, remove them,
## and export a cleaned CSV.
##
## Country filter (new): if the downloaded dataset contains a single Country
## (typical of national surveys like SP-PORC, IE-IGFS, etc.) it is
## auto-detected; if it contains several (NS-IBTS), the user picks one for
## per-fleet analysis, or "All" to work across countries.
##
## Deleting selected rows always removes them from the full dataset (not just
## the filtered view). Restore rewinds to the last download.
##
## Usage in master app.R:
##   UI:     outliersUI("outliers")
##   Server: outliersServer("outliers")
## No shared reactiveVal - this module manages its own downloaded dataset.


# ==================== UI ====================
outliersUI <- function(id) {
  ns <- NS(id)

  tagList(
    titlePanel("HH outlier selector \u2014 download from DATRAS, inspect, remove outliers, save"),

    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(ns("survey"), "Survey:",
                    choices  = c("EVHOE","FR-CGFS","FR-WCGFS","IE-IGFS","IE-IAMS",
                                 "NIGFS","NS-IBTS","PT-IBTS","SCOROC","SCOWCGFS",
                                 "SP-ARSA","SP-NORTH","SP-PORC"),
                    selected = "SP-PORC"),
        sliderInput(ns("years"), "Years (range):",
                    min = 1983, max = as.numeric(format(Sys.Date(), "%Y")),
                    value = c(2015, 2022), step = 1, sep = ""),
        checkboxGroupInput(ns("quarters"), "Quarters:",
                           choices = 1:4, selected = 4, inline = TRUE),
        actionButton(ns("btn_download"), "\u2B07\uFE0F Download HH from DATRAS",
                     class = "btn-primary"),
        hr(),
        selectInput(ns("country"), "Country:",
                    choices = c("All"), selected = "All"),
        uiOutput(ns("country_hint")),
        hr(),
        actionButton(ns("btn_delete"),  "\U0001F5D1\uFE0F Delete selected rows",
                     class = "btn-danger"),
        actionButton(ns("btn_restore"), "\u21BA Restore original"),
        hr(),
        downloadButton(ns("download_clean"), "Download cleaned CSV"),
        hr(),
        verbatimTextOutput(ns("status")),
        helpText(tags$small(
          "-9 values are converted to NA on download. ",
          "Click on a point toggles its selection. ",
          "Deleting affects the whole dataset regardless of the country filter."))
      ),

      mainPanel(
        tabsetPanel(id = ns("plots_tabs"),
                    tabPanel("WarpLength vs Depth", value = "warp",
                             plotOutput(ns("plot_warp"), height = "550px",
                                        click = ns("click_plot")),
                             h4("Selected rows:"),
                             DT::DTOutput(ns("table_selected"))),
                    tabPanel("WingSpread vs Depth", value = "wing",
                             plotOutput(ns("plot_wing"), height = "550px",
                                        click = ns("click_plot")),
                             h4("Selected rows:"),
                             DT::DTOutput(ns("table_selected"))),
                    tabPanel("DoorSpread vs Depth", value = "door",
                             plotOutput(ns("plot_door"), height = "550px",
                                        click = ns("click_plot")),
                             h4("Selected rows:"),
                             DT::DTOutput(ns("table_selected"))),
                    tabPanel("Netopening vs Depth", value = "net",
                             plotOutput(ns("plot_net"), height = "550px",
                                        click = ns("click_plot")),
                             h4("Selected rows:"),
                             DT::DTOutput(ns("table_selected"))),
                    tabPanel("All data preview",
                             DT::DTOutput(ns("table_all")))
        )
      )
    )
  )
}


# ==================== SERVER ====================
outliersServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # State
    original_df   <- reactiveVal(NULL)
    current_df    <- reactiveVal(NULL)
    selected_ids  <- reactiveVal(integer(0))

    # Columns kept for tables / export
    keepcols_std <- c("Survey","Year","Quarter","Country","Ship","HaulNo",
                      "ShootLat","ShootLong","HaulLat","HaulLong","Depth","HaulVal",
                      "Distance","Warplngt","SweepLngt","DoorSpread",
                      "WingSpread","Netopening","TowDir")

    # Helper: -9 -> NA across numeric columns
    convert_neg9_to_na <- function(df) {
      if (is.null(df)) return(df)
      for (nm in names(df)) {
        if (is.numeric(df[[nm]])) {
          df[[nm]][df[[nm]] == -9] <- NA
        } else {
          tmp <- suppressWarnings(as.numeric(df[[nm]]))
          if (!all(is.na(tmp))) {
            tmp[tmp == -9] <- NA
            df[[nm]] <- tmp
          }
        }
      }
      df
    }

    # -------- Download from DATRAS --------
    observeEvent(input$btn_download, {
      req(input$survey, input$years, input$quarters)
      years_seq <- seq(input$years[1], input$years[2])
      quarters  <- unique(as.numeric(input$quarters))

      # Raise timeout: NS-IBTS full periods can be > 1MB and DATRAS is
      # slow at times; the default 60s often trips.
      old_to <- getOption("timeout")
      options(timeout = 600)
      on.exit(options(timeout = old_to), add = TRUE)

      showNotification("Starting download...", type = "message", duration = 2)

      all_list <- list()
      failed <- character(0)
      for (y in years_seq) {
        for (q in quarters) {
          tag <- paste0(input$survey, " ", y, " Q", q)
          message("Downloading: ", tag)
          tmp <- tryCatch(
            icesDatras::getDATRAS("HH", input$survey, y, q),
            error = function(e) { message("  -> failed: ", e$message); NULL })
          if (!is.null(tmp) && is.data.frame(tmp) && nrow(tmp) > 0) {
            tmp$Year <- as.numeric(y)
            tmp$Quarter <- as.numeric(q)
            all_list[[length(all_list) + 1]] <- tmp
          } else {
            failed <- c(failed, tag)
          }
        }
      }

      if (length(all_list) == 0) {
        showNotification("\u274C No HH data downloaded for the selection.",
                         type = "error", duration = 8)
        original_df(NULL); current_df(NULL); selected_ids(integer(0))
        return()
      }

      df <- do.call(rbind, all_list)
      names(df) <- make.names(names(df))
      df <- convert_neg9_to_na(df)

      if ("Depth" %in% names(df))
        df$Depth <- suppressWarnings(as.numeric(df$Depth))
      if ("HaulVal" %in% names(df))
        df <- df[!(df$HaulVal %in% "I"), , drop = FALSE]
      keep_idx <- rep(TRUE, nrow(df))
      if ("Depth" %in% names(df))
        keep_idx <- keep_idx & !is.na(df$Depth) & df$Depth > 0
      df <- df[keep_idx, , drop = FALSE]

      df$.rowid <- seq_len(nrow(df))
      original_df(df); current_df(df); selected_ids(integer(0))

      msg <- paste0("\u2705 Downloaded ", nrow(df), " rows.")
      if (length(failed) > 0)
        msg <- paste0(msg, "  (", length(failed), " year-quarter combinations returned no data)")
      showNotification(msg, type = "message", duration = 6)
    })

    # -------- Country filter --------
    available_countries <- reactive({
      df <- current_df()
      if (is.null(df) || !"Country" %in% names(df)) return(character(0))
      sort(unique(na.omit(df$Country)))
    })

    observe({
      avail <- available_countries()
      if (length(avail) == 0) {
        updateSelectInput(session, "country",
                          choices = c("All"), selected = "All")
      } else if (length(avail) == 1) {
        updateSelectInput(session, "country",
                          choices = avail, selected = avail)
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
        tags$small(style = "color:#888;", "No data downloaded yet.")
      } else if (length(avail) == 1) {
        tags$small(style = "color:#117864;",
                   HTML(paste0("Single country dataset: <b>", avail, "</b>")))
      } else {
        tags$small(style = "color:#117864;",
                   paste0(length(avail),
                          " countries present. Pick one to focus on a single fleet."))
      }
    })

    # -------- Filtered view (used for plotting and click) --------
    df_view <- reactive({
      df <- current_df()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      ctry <- input$country
      if (is.null(ctry) || ctry == "All" || !"Country" %in% names(df)) return(df)
      df[!is.na(df$Country) & df$Country == ctry, , drop = FALSE]
    })

    # -------- Status pane --------
    output$status <- renderPrint({
      df    <- current_df()
      dfv   <- df_view()
      n_sel <- length(selected_ids())
      if (is.null(df)) { cat("No data loaded."); return() }
      cat("Total rows:   ", nrow(df),                              "\n", sep = "")
      cat("In view:      ", ifelse(is.null(dfv), 0, nrow(dfv)),    "\n", sep = "")
      cat("Selected:     ", n_sel,                                  "\n", sep = "")
      cat("Countries:    ", paste(available_countries(), collapse = ", "), sep = "")
    })

    # -------- Plot helper --------
    # Draws points coloured by SweepLngt with an optional regression, then
    # overlays currently-selected rows as green triangles. Uses df_view
    # for plotting but full df for locating selected points (so a
    # selection made in one country filter is still shown if the user
    # switches; those in other countries fall outside xlim/ylim).
    draw_by_sweep <- function(dfp, yvar, regression_type = c("log","linear")) {
      regression_type <- match.arg(regression_type)
      if ("SweepLngt" %in% names(dfp)) {
        tipos <- sort(unique(na.omit(dfp$SweepLngt)))
      } else tipos <- character(0)
      pal <- c("steelblue","darkorange","darkgreen","purple",
               "brown","magenta","cyan4","gold3")
      if (length(tipos) == 0) {
        points(dfp$Depth, dfp[[yvar]], pch = 21, bg = "grey", col = "black")
        return(invisible(NULL))
      }
      labels <- character(0)
      for (i in seq_along(tipos)) {
        t <- tipos[i]
        sub <- dfp[dfp$SweepLngt == t &
                     !is.na(dfp$Depth) & !is.na(dfp[[yvar]]), , drop = FALSE]
        if (nrow(sub) == 0) next
        points(sub$Depth, sub[[yvar]], pch = 21, bg = pal[i], col = "black")
        rng <- range(sub$Depth, na.rm = TRUE)
        if (nrow(sub) > 5 && diff(rng) > 0) {
          fmla <- if (regression_type == "log")
            as.formula(paste(yvar, "~ log(Depth)"))
          else as.formula(paste(yvar, "~ Depth"))
          mod <- tryCatch(lm(fmla, data = sub), error = function(e) NULL)
          if (!is.null(mod)) {
            dseq  <- seq(rng[1], rng[2], length.out = 200)
            preds <- tryCatch(predict(mod, newdata = data.frame(Depth = dseq)),
                              error = function(e) NULL)
            if (!is.null(preds)) lines(dseq, preds, col = pal[i], lwd = 2)
          }
        }
        labels <- c(labels, paste0("Sweep=", t,
                                   " (", round(rng[1]), "\u2013", round(rng[2]),
                                   "m, n=", nrow(sub), ")"))
      }
      legend("topright", legend = labels, bty = "n", cex = 0.85)
    }

    # Generic plot function replacing four almost-identical renderPlots
    plot_var <- function(dfv, yvar, ylab, main_title, regression_type) {
      if (is.null(dfv) || nrow(dfv) == 0) {
        plot.new(); title("No data. Click 'Download HH from DATRAS' to load data.")
        return()
      }
      if (!(yvar %in% names(dfv))) {
        plot.new(); title(paste(yvar, "not present in data.")); return()
      }
      dfv$Depth  <- suppressWarnings(as.numeric(dfv$Depth))
      dfv[[yvar]] <- suppressWarnings(as.numeric(dfv[[yvar]]))
      dfp <- dfv[!is.na(dfv$Depth) & !is.na(dfv[[yvar]]), , drop = FALSE]
      if (nrow(dfp) == 0) {
        plot.new(); title(paste("No valid", yvar, "vs Depth points.")); return()
      }
      plot(NA,
           xlim = range(dfp$Depth, na.rm = TRUE),
           ylim = range(dfp[[yvar]], na.rm = TRUE),
           xlab = "Depth (m)", ylab = ylab, main = main_title)
      draw_by_sweep(dfp, yvar, regression_type = regression_type)

      # overlay selected rows (from full current_df, but only if visible)
      sel <- selected_ids()
      if (length(sel) > 0) {
        selrows <- current_df()
        selrows <- selrows[selrows$.rowid %in% sel, , drop = FALSE]
        selrows$Depth   <- suppressWarnings(as.numeric(selrows$Depth))
        selrows[[yvar]] <- suppressWarnings(as.numeric(selrows[[yvar]]))
        selrows <- selrows[!is.na(selrows$Depth) & !is.na(selrows[[yvar]]), ,
                           drop = FALSE]
        if (nrow(selrows) > 0)
          points(selrows$Depth, selrows[[yvar]],
                 pch = 24, bg = "green", col = "black", cex = 1.6)
      }
    }

    output$plot_warp <- renderPlot({
      plot_var(df_view(), "Warplngt",  "Warplngt (m)",    "Warplngt vs Depth",   "linear")
    })
    output$plot_wing <- renderPlot({
      plot_var(df_view(), "WingSpread", "WingSpread (m)",  "WingSpread vs Depth", "log")
    })
    output$plot_door <- renderPlot({
      plot_var(df_view(), "DoorSpread", "DoorSpread (m)",  "DoorSpread vs Depth", "log")
    })
    output$plot_net <- renderPlot({
      plot_var(df_view(), "Netopening", "Netopening (m)", "Net opening vs Depth","log")
    })

    # -------- Click handler --------
    observeEvent(input$click_plot, {
      dfv <- df_view()
      if (is.null(dfv) || nrow(dfv) == 0) return()
      tab <- isolate(input$plots_tabs); if (is.null(tab)) tab <- "warp"
      yvar <- switch(tab,
                     "warp" = "Warplngt",
                     "wing" = "WingSpread",
                     "door" = "DoorSpread",
                     "net"  = "Netopening",
                     "Warplngt")
      if (!(yvar %in% names(dfv))) return()
      dfv$Depth   <- suppressWarnings(as.numeric(dfv$Depth))
      dfv[[yvar]] <- suppressWarnings(as.numeric(dfv[[yvar]]))
      df2 <- dfv[!is.na(dfv$Depth) & !is.na(dfv[[yvar]]), , drop = FALSE]
      if (nrow(df2) == 0) return()

      near <- nearPoints(df2, input$click_plot,
                         xvar = "Depth", yvar = yvar,
                         threshold = 8, maxpoints = 1)
      if (nrow(near) == 0) return()

      rid <- near$.rowid[1]
      cur_sel <- selected_ids()
      cur_sel <- if (rid %in% cur_sel) setdiff(cur_sel, rid) else unique(c(cur_sel, rid))
      selected_ids(cur_sel)
    })

    # -------- Delete / Restore --------
    observeEvent(input$btn_delete, {
      df <- current_df()
      if (is.null(df)) return()
      sel <- selected_ids()
      if (length(sel) == 0) {
        showNotification("No rows selected to delete.", type = "warning"); return()
      }
      keep <- !(df$.rowid %in% sel)
      df2  <- df[keep, , drop = FALSE]
      df2$.rowid <- seq_len(nrow(df2))
      current_df(df2); selected_ids(integer(0))
      showNotification(paste0("Deleted ", sum(!keep),
                              " rows. Remaining: ", nrow(df2)),
                       type = "message")
    })

    observeEvent(input$btn_restore, {
      df <- original_df()
      if (is.null(df)) return()
      current_df(df); selected_ids(integer(0))
      showNotification("Restored original downloaded data.", type = "message")
    })

    # -------- Tables (single output referenced from 4 tabs) --------
    selected_table_df <- reactive({
      df <- current_df()
      if (is.null(df)) return(NULL)
      sel <- selected_ids()
      if (length(sel) == 0) return(NULL)
      dfsel <- df[df$.rowid %in% sel, , drop = FALSE]
      keep <- intersect(keepcols_std, names(dfsel))
      dfsel[, keep, drop = FALSE]
    })

    output$table_selected <- DT::renderDT({
      dat <- selected_table_df()
      if (is.null(dat)) return(NULL)
      DT::datatable(dat, options = list(dom = 't', paging = FALSE, scrollX = TRUE),
                    rownames = FALSE)
    })

    output$table_all <- DT::renderDT({
      df <- df_view()
      if (is.null(df)) return(NULL)
      keep <- intersect(keepcols_std, names(df))
      DT::datatable(df[, keep, drop = FALSE],
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    })

    # -------- CSV download --------
    output$download_clean <- downloadHandler(
      filename = function() {
        sr   <- input$survey
        yrs  <- paste0(input$years[1], "-", input$years[2])
        qstr <- paste(input$quarters, collapse = "")
        ctry <- input$country
        ctry_tag <- if (!is.null(ctry) && ctry != "All") paste0("_", ctry) else ""
        paste0("HH_", sr, "_", yrs, "_Q", qstr, ctry_tag, "_cleaned.csv")
      },
      content = function(file) {
        df <- current_df()
        if (is.null(df)) {
          write.csv(data.frame(), file, row.names = FALSE); return()
        }
        # Apply country filter only if user picked one (otherwise export
        # the full cleaned dataset).
        ctry <- input$country
        if (!is.null(ctry) && ctry != "All" && "Country" %in% names(df))
          df <- df[!is.na(df$Country) & df$Country == ctry, , drop = FALSE]
        keep <- intersect(keepcols_std, names(df))
        write.csv(df[, keep, drop = FALSE], file, row.names = FALSE)
      }
    )

  })
}
