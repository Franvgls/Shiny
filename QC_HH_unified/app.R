## Master app for QC of HH survey data
## Loads a single HH CSV file at the top level, then dispatches to different
## QC modules via tabs. Gear vs Depth manages its own two files, and Outliers
## downloads its own data straight from DATRAS; both are independent flows
## kept under the same navbar for convenience.
##
## Modules live in R/ and are auto-loaded by Shiny at startup.

library(shiny)
library(bslib)
library(DT)
library(NeAtlIBTS64)
library(icesDatras)
library(geosphere)


# ---- Helper: normalise HH column names (shared by modules using upload) ----
normalize_hh_names <- function(df) {
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


# ============================== UI ==============================
ui <- tagList(

    div(
        style = "
            display: flex;
            align-items: center;
            justify-content: flex-start;
            background-color: #e8f6f3;
            border-bottom: 3px solid #117864;
            padding: 12px 20px;
        ",
        tags$img(src = "IMBUS_logo.png", height = "80px",
                 style = "margin-right: 25px;"),
        div(
            h2("HH Survey Quality Control",
               style = "margin: 0; color: #0b5345; font-weight: bold;"),
            p("One upload, several QC tools",
              style = "margin: 0; font-size: 16px; color: #117864;")
        )
    ),

    navbarPage(
        title = "QC Tools",
        id    = "mainnav",
        theme = bslib::bs_theme(version = 5, bootswatch = "minty"),

        tabPanel("Upload HH",
                 sidebarLayout(
                     sidebarPanel(
                         fileInput("hh_file", "Load HH CSV file",
                                   accept = c(".csv", ".txt")),
                         helpText("Load a single HH file. Tabs 'QC Dist' and 'QC Posit' ",
                                  "read from this upload. 'Gear vs Depth' and 'Outliers' ",
                                  "manage their own data sources.")
                     ),
                     mainPanel(
                         verbatimTextOutput("info_global")
                     )
                 )
        ),

        tabPanel("QC Dist",       qcDistUI("qc_dist")),
        tabPanel("QC Posit",      qcPositUI("qc_posit")),
        tabPanel("Gear vs Depth", gearDepthUI("gear")),
        tabPanel("Outliers",      outliersUI("outliers"))

        # Coming later:
        # tabPanel("Map Hist", mapHistUI("map_hist"))
    )
)


# ============================ SERVER ============================
server <- function(input, output, session) {

    hh_data <- reactiveVal(NULL)

    observeEvent(input$hh_file, {
        req(input$hh_file)
        df <- read.csv(input$hh_file$datapath, stringsAsFactors = FALSE)
        df <- normalize_hh_names(df)
        hh_data(df)
    })

    output$info_global <- renderPrint({
        df <- hh_data()
        if (is.null(df)) {
            cat("\u26A0\uFE0F No file loaded yet.\n")
            return()
        }
        cat("Records:  ", nrow(df), "\n", sep = "")
        cat("Surveys:  ", paste(unique(na.omit(df$Survey)),  collapse = ", "), "\n", sep = "")
        cat("Years:    ", paste(unique(na.omit(df$Year)),    collapse = ", "), "\n", sep = "")
        cat("Quarters: ", paste(unique(na.omit(df$Quarter)), collapse = ", "), "\n", sep = "")
        if ("Country" %in% names(df)) {
            cat("Countries:", paste(sort(unique(na.omit(df$Country))), collapse = ", "), "\n", sep = " ")
        }
        if ("Ship" %in% names(df)) {
            cat("Ships:    ", paste(sort(unique(na.omit(df$Ship))),    collapse = ", "), "\n", sep = "")
        }
    })

    qcDistServer("qc_dist",   hh_data = hh_data)
    qcPositServer("qc_posit", hh_data = hh_data)
    gearDepthServer("gear")
    outliersServer("outliers")
}


shinyApp(ui, server)
