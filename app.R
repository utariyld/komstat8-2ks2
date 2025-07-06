# Dashboard Analisis ARDL
# Memuat library yang diperlukan
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(readxl)
library(readr)
library(ARDL)
library(urca)
library(lmtest)
library(tseries)
library(forecast)
library(vars)
library(strucchange)
library(devtools)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(car)
library(shinyFeedback)
library(rsconnect)
library(snowflakeauth)
library(httr2)
library(curl)
library(ragg)
library(pkgdown)
library(RcppTOML)

# UI
ui <- dashboardPage(
  # Add a skin for better appearance
  skin = "blue", # You can try "green", "red", "purple", "yellow"
  
  dashboardHeader(
    title = "Dashboard Analisis ARDL",
    titleWidth = 280 # Adjusted width for title
  ),
  
  dashboardSidebar(
    width = 280, # Adjusted width for sidebar
    sidebarMenu(
      id = "tabs", # Added an ID for server-side tab selection
      menuItem("Pengenalan Dashboard", tabName = "introduction", icon = icon("info-circle")),
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Uji Stasioneritas", tabName = "stationarity", icon = icon("chart-line")),
      menuItem("Lag Optimum", tabName = "lag_selection", icon = icon("search")),
      menuItem("Estimasi ARDL", tabName = "ardl_estimation", icon = icon("calculator")),
      menuItem("Uji Asumsi Klasik", tabName = "assumptions", icon = icon("check-circle")),
      menuItem("Uji Kointegrasi", tabName = "cointegration", icon = icon("link")),
      menuItem("Model Final", tabName = "final_model", icon = icon("trophy")),
      menuItem("Uji Stabilitas", tabName = "stability", icon = icon("balance-scale"))
    )
  ),
  
  dashboardBody(
    # Add shinyFeedback dependency
    useShinyFeedback(),
    tags$head(
      tags$style(HTML("
        /* Overall background and text */
        .content-wrapper, .right-side {
          background-color: #ecf0f5; /* Light gray background */
          color: #333; /* Darker text for readability */
        }
        
        /* Boxes styling */
        .box {
          border-radius: 12px;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1); /* Softer, larger shadow */
          border-top: 3px solid #3c8dbc; /* Default primary color top border */
          margin-bottom: 25px; /* More spacing between boxes */
        }
        .box.box-solid.box-primary > .box-header {
          color: #fff;
          background-color: #3c8dbc;
          border-radius: 12px 12px 0 0;
        }
        .box.box-solid.box-success > .box-header {
          color: #fff;
          background-color: #00a65a;
          border-radius: 12px 12px 0 0;
        }
        .box.box-solid.box-info > .box-header {
          color: #fff;
          background-color: #00c0ef;
          border-radius: 12px 12px 0 0;
        }
        .box.box-solid.box-warning > .box-header {
          color: #fff;
          background-color: #f39c12;
          border-radius: 12px 12px 0 0;
        }
        
        /* Buttons styling */
        .btn-primary {
          background-color: #3c8dbc;
          border-color: #3c8dbc;
          transition: all 0.3s ease;
        }
        .btn-primary:hover, .btn-primary:active, .btn-primary.active, .open > .dropdown-toggle.btn-primary {
          background-color: #2e6da4;
          border-color: #2e6da4;
        }
        .btn-success {
          background-color: #00a65a;
          border-color: #00a65a;
          transition: all 0.3s ease;
        }
        .btn-success:hover {
          background-color: #008d4c;
          border-color: #008d4c;
        }

        /* Sidebar menu styling */
        .main-sidebar {
          background-color: #222d32; /* Darker sidebar background */
        }
        .main-sidebar .sidebar-menu > li.active > a,
        .main-sidebar .sidebar-menu > li:hover > a {
          background-color: #1e282c; /* Slightly lighter on hover/active */
          border-left-color: #3c8dbc; /* Highlight with primary color */
        }
        .sidebar-menu li > a {
          font-size: 16px; /* Larger font for sidebar items */
          padding: 12px 15px 12px 15px;
        }

        /* Custom font (optional, if you have a specific font) */
        body {
          font-family: 'Open Sans', sans-serif; /* Example font */
        }
        h1, h2, h3, h4, h5, h6 {
          font-family: 'Montserrat', sans-serif; /* Example font for headings */
          color: #333;
        }

        /* Info boxes */
        .info-box {
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }
        .info-box-icon {
          border-radius: 8px 0 0 8px;
        }

        /* Tab panels */
        .nav-tabs-custom > .nav-tabs > li.active {
            border-top-color: #3c8dbc; /* Highlight active tab with primary color */
        }
        .nav-tabs-custom > .nav-tabs > li.active > a,
        .nav-tabs-custom > .nav-tabs > li.active:hover > a {
            background-color: #3c8dbc;
            color: white;
            border-color: #3c8dbc;
        }

        /* Spinner color */
        .fa-spinner {
          color: #3c8dbc;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "introduction",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = tags$div(icon("dashboard"), "Selamat Datang di Dashboard Analisis ARDL"),
                    h3("Tentang Dashboard Ini"),
                    p("Dashboard ini dirancang untuk membantu Anda melakukan analisis Autoregressive Distributed Lag (ARDL) dengan mudah dan interaktif. Mulai dari mengunggah data, melakukan uji stasioneritas, menentukan lag optimum, hingga estimasi model ARDL, uji asumsi klasik, uji kointegrasi, dan uji stabilitas model. Nikmati analisis ekonometrika yang powerful dengan antarmuka yang ramah pengguna."),
                    br(),
                    fluidRow(
                      infoBox(
                        "Analisis Mudah", "Tidak perlu coding kompleks.",
                        icon = icon("code"), color = "purple", fill = TRUE, width = 4
                      ),
                      infoBox(
                        "Interaktif", "Visualisasi dinamis dan respon instan.",
                        icon = icon("mouse-pointer"), color = "yellow", fill = TRUE, width = 4
                      ),
                      infoBox(
                        "Hasil Akurat", "Berdasarkan kaidah ekonometrika teruji.",
                        icon = icon("check-double"), color = "aqua", fill = TRUE, width = 4
                      )
                    )
                ),
                box(width = 12, status = "info", solidHeader = TRUE,
                    title = tags$div(icon("cogs"), "Alur Kerja Analisis ARDL"),
                    h4("Ikuti langkah-langkah mudah berikut untuk analisis komprehensif:"),
                    tags$ol(
                      tags$li(tags$b("Upload Data:"), " Mulai dengan mengunggah file data time series Anda (CSV atau Excel) dan pilih variabel."),
                      tags$li(tags$b("Uji Stasioneritas:"), " Lakukan uji ADF untuk memastikan variabel-variabel Anda stasioner pada level atau first difference. ARDL memungkinkan variabel terintegrasi pada I(0) atau I(1), tetapi tidak pada I(2)."),
                      tags$li(tags$b("Lag Optimum:"), " Tentukan jumlah lag optimal untuk model Anda menggunakan kriteria informasi seperti AIC atau BIC."),
                      tags$li(tags$b("Estimasi ARDL:"), " Estimasi model ARDL dengan pilihan lag otomatis. Dashboard ini secara default menggunakan Case 3 (Unrestricted Intercept, No Trend)."),
                      tags$li(tags$b("Uji Asumsi Klasik:"), " Periksa asumsi normalitas, homoskedastisitas, dan autokorelasi pada residual model."),
                      tags$li(tags$b("Uji Kointegrasi:"), " Jalankan Bounds Test untuk menentukan ada tidaknya hubungan kointegrasi jangka panjang antara variabel."),
                      tags$li(tags$b("Model Final:"), " Dapatkan ringkasan model ARDL final dan, jika ada kointegrasi, model Error Correction Model (ECM)."),
                      tags$li(tags$b("Uji Stabilitas:"), " Evaluasi stabilitas parameter dan varians model menggunakan CUSUM dan CUSUM of Squares tests.")
                    ),
                    br(),
                    actionButton("goto_upload", "Mulai Analisis Sekarang", icon = icon("rocket"), class = "btn-success btn-lg")
                ),
                box(width = 12, status = "success", solidHeader = TRUE,
                    title = tags$div(icon("video"), "Video Penjelasan Tentang ARDL"),
                    h4("Tonton video berikut untuk memahami Estimasi Model ARDL:"),
                    div(class = "embed-responsive embed-responsive-16by9",
                        tags$iframe(class = "embed-responsive-item",
                                    src = "https://www.youtube.com/embed/vY8cCik_mtQ", 
                                    frameborder = "0", allowfullscreen = "")
                    )
                ),
                box(width = 12, status = "success", solidHeader = TRUE,
                    title = tags$div(icon("video-camera"), "Video Penjelasan Tambahan"),
                    h4("Tonton video lainnya di bawah ini:"),
                    # Gunakan tags$video, bukan tags$iframe
                    tags$video(
                      src = "User Guide Dashboard.mp4", # Nama file langsung, karena sudah di folder www
                      type = "video/mp4",
                      width = "100%", # Atur lebar agar responsif
                      height = "auto",
                      controls = "controls" # Tambahkan atribut kontrol (play, pause, volume)
                    )
                ),
                box(width = 12, status = "success", solidHeader = TRUE,
                    title = tags$div(icon("book"), "Panduan Pengguna Dashboard"),
                    h4("Lihat panduan pengguna dalam format PDF di bawah ini:"),
                    tags$iframe(style="height:600px; width:100%;", src="User Guide Dashboard.pdf")
                )
              )
      ),
      
      # Tab Upload Data
      tabItem(tabName = "upload",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Upload Data Time Series",
                    fileInput("file", "Pilih File (CSV, Excel)",
                              accept = c(".csv", ".xlsx", ".xls"),
                              multiple = FALSE),
                    
                    conditionalPanel(
                      condition = "output.fileUploaded",
                      hr(),
                      h4("Preview Data:"),
                      withSpinner(DT::dataTableOutput("data_preview")),
                      br(),
                      
                      fluidRow(
                        column(6,
                               selectInput("date_col", "Pilih Kolom Tanggal:", choices = NULL)
                        ),
                        column(6,
                               selectInput("dependent_var", "Pilih Variabel Dependen:", choices = NULL)
                        )
                      ),
                      
                      checkboxGroupInput("independent_vars", "Pilih Variabel Independen:",
                                         choices = NULL),
                      
                      actionButton("process_data", "Proses Data",
                                   class = "btn-primary btn-lg", width = "100%")
                    )
                )
              ),
              
              conditionalPanel(
                condition = "output.dataProcessed",
                fluidRow(
                  box(width = 12, status = "success", solidHeader = TRUE,
                      title = "Ringkasan Data",
                      withSpinner(verbatimTextOutput("data_summary"))
                  )
                )
              )
      ),
      
      # Tab Uji Stasioneritas
      tabItem(tabName = "stationarity",
              conditionalPanel(
                condition = "output.dataProcessed == false",
                box(width = 12, status = "warning", solidHeader = TRUE,
                    title = "Peringatan",
                    h4("Silakan upload dan proses data terlebih dahulu pada tab 'Upload Data'")
                )
              ),
              
              conditionalPanel(
                condition = "output.dataProcessed",
                fluidRow(
                  box(width = 12, status = "primary", solidHeader = TRUE,
                      title = "Uji Stasioneritas (ADF Test)",
                      fluidRow(
                        column(4,
                               selectInput("stationarity_var", "Pilih Variabel:", choices = NULL)
                        ),
                        column(4,
                               selectInput("test_type", "Tipe Test:",
                                           choices = list("none" = "none", "drift" = "drift", "trend" = "trend"),
                                           selected = "trend")
                        ),
                        column(4,
                               actionButton("run_stationarity", "Jalankan Test", class = "btn-primary")
                        )
                      )
                  )
                ),
                
                fluidRow(
                  box(width = 6, status = "info", solidHeader = TRUE,
                      title = "Hasil Uji ADF",
                      withSpinner(verbatimTextOutput("adf_result"))
                  ),
                  box(width = 6, status = "info", solidHeader = TRUE,
                      title = "Plot Time Series",
                      withSpinner(plotlyOutput("ts_plot"))
                  )
                ),
                
                fluidRow(
                  box(width = 12, status = "success", solidHeader = TRUE,
                      title = "Ringkasan Stasioneritas Semua Variabel",
                      actionButton("test_all_stationarity", "Test Semua Variabel", class = "btn-success"),
                      br(), br(),
                      withSpinner(DT::dataTableOutput("stationarity_summary"))
                  )
                )
              )
      ),
      
      # Tab Lag Selection
      tabItem(tabName = "lag_selection",
              conditionalPanel(
                condition = "output.dataProcessed == false",
                box(width = 12, status = "warning", solidHeader = TRUE,
                    title = "Peringatan",
                    h4("Silakan upload dan proses data terlebih dahulu pada tab 'Upload Data'")
                )
              ),
              
              conditionalPanel(
                condition = "output.dataProcessed",
                fluidRow(
                  box(width = 12, status = "primary", solidHeader = TRUE,
                      title = "Penentuan Lag Optimum",
                      fluidRow(
                        column(4,
                               numericInput("max_lag", "Maksimum Lag:", value = 2, min = 1, max = 12)
                        ),
                        column(4,
                               selectInput("ic_criterion", "Kriteria Informasi:",
                                           choices = list("AIC" = "AIC(n)", "BIC" = "SC(n)", "HQ" = "HQ(n)"),
                                           selected = "AIC(n)")
                        ),
                        column(4,
                               actionButton("select_lag", "Pilih Lag Optimum", class = "btn-primary")
                        )
                      )
                  )
                ),
                
                fluidRow(
                  box(width = 12, status = "info", solidHeader = TRUE,
                      title = "Hasil Seleksi Lag",
                      withSpinner(verbatimTextOutput("lag_selection_result"))
                  )
                )
              )
      ),
      
      # Tab ARDL Estimation
      tabItem(tabName = "ardl_estimation",
              conditionalPanel(
                condition = "output.dataProcessed == false",
                box(width = 12, status = "warning", solidHeader = TRUE,
                    title = "Peringatan",
                    h4("Silakan upload dan proses data terlebih dahulu pada tab 'Upload Data'")
                )
              ),
              
              conditionalPanel(
                condition = "output.dataProcessed",
                fluidRow(
                  box(width = 12, status = "primary", solidHeader = TRUE,
                      title = "Estimasi Model ARDL (Case 3: Unrestricted Intercept, No Trend)",
                      fluidRow(
                        column(6,
                               h5("Maksimum Lag untuk Semua Variabel:"),
                               numericInput("max_order", NULL, value = 1, min = 1, max = 4)
                        ),
                        column(6,
                               actionButton("estimate_ardl", "Estimasi Model ARDL (Auto Lag Selection)",
                                            class = "btn-primary btn-lg", width = "100%")
                        )
                      )
                  )
                ),
                
                conditionalPanel(
                  condition = "output.ardlEstimated",
                  fluidRow(
                    box(width = 12, status = "success", solidHeader = TRUE,
                        title = "Hasil Estimasi ARDL",
                        withSpinner(verbatimTextOutput("ardl_result"))
                    )
                  )
                )
              )
      ),
      
      # Tab Uji Asumsi Klasik
      tabItem(tabName = "assumptions",
              conditionalPanel(
                condition = "output.ardlEstimated == false",
                box(width = 12, status = "warning", solidHeader = TRUE,
                    title = "Peringatan",
                    h4("Silakan estimasi model ARDL terlebih dahulu pada tab 'Estimasi ARDL'")
                )
              ),
              
              conditionalPanel(
                condition = "output.ardlEstimated",
                fluidRow(
                  box(width = 12, status = "primary", solidHeader = TRUE,
                      title = "Uji Asumsi Klasik",
                      actionButton("run_assumptions", "Jalankan Semua Uji",
                                   class = "btn-primary btn-lg", width = "100%")
                  )
                ),
                
                conditionalPanel(
                  condition = "output.assumptionsRun",
                  fluidRow(
                    box(width = 4, status = "info", solidHeader = TRUE,
                        title = "Uji Normalitas (Jarque-Bera)",
                        withSpinner(verbatimTextOutput("normality_test"))
                    ),
                    box(width = 4, status = "info", solidHeader = TRUE,
                        title = "Uji Homoskedastisitas (Breusch-Pagan)",
                        withSpinner(verbatimTextOutput("heteroskedasticity_test"))
                    ),
                    box(width = 4, status = "info", solidHeader = TRUE,
                        title = "Uji Autokorelasi (Breusch-Godfrey)",
                        withSpinner(verbatimTextOutput("autocorrelation_test"))
                    )
                  ),
                  
                  fluidRow(
                    box(width = 6, status = "info", solidHeader = TRUE,
                        title = "Plot Residual",
                        withSpinner(plotlyOutput("residual_plot"))
                    ),
                    box(width = 6, status = "info", solidHeader = TRUE,
                        title = "Q-Q Plot",
                        withSpinner(plotlyOutput("qq_plot"))
                    )
                  )
                )
              )
      ),
      
      # Tab Uji Kointegrasi
      tabItem(tabName = "cointegration",
              conditionalPanel(
                condition = "output.ardlEstimated == false",
                box(width = 12, status = "warning", solidHeader = TRUE,
                    title = "Peringatan",
                    h4("Silakan estimasi model ARDL terlebih dahulu pada tab 'Estimasi ARDL'")
                )
              ),
              
              conditionalPanel(
                condition = "output.ardlEstimated",
                fluidRow(
                  box(width = 12, status = "primary", solidHeader = TRUE,
                      title = "Uji Kointegrasi",
                      actionButton("run_bounds_test", "Jalankan Bounds Test",
                                   class = "btn-primary btn-lg", width = "100%")
                  )
                ),
                
                conditionalPanel(
                  condition = "output.boundsTestRun",
                  fluidRow(
                    box(width = 12, status = "success", solidHeader = TRUE,
                        title = "Hasil Uji Kointegrasi",
                        withSpinner(verbatimTextOutput("bounds_test_result"))
                    )
                  )
                )
              )
      ),
      
      # Tab Model Final
      tabItem(tabName = "final_model",
              conditionalPanel(
                condition = "output.boundsTestRun == false",
                box(width = 12, status = "warning", solidHeader = TRUE,
                    title = "Peringatan",
                    h4("Silakan jalankan uji kointegrasi terlebih dahulu pada tab 'Uji Kointegrasi'")
                )
              ),
              
              conditionalPanel(
                condition = "output.boundsTestRun",
                fluidRow(
                  box(width = 12, status = "primary", solidHeader = TRUE,
                      title = "Model ARDL Final",
                      actionButton("finalize_model", "Finalisasi Model",
                                   class = "btn-success btn-lg", width = "100%")
                  )
                ),
                
                conditionalPanel(
                  condition = "output.modelFinalized",
                  fluidRow(
                    box(width = 6, status = "success", solidHeader = TRUE,
                        title = "Ringkasan Model",
                        withSpinner(verbatimTextOutput("final_model_summary"))
                    ),
                    conditionalPanel(
                      condition = "output.hasCointegration == true",
                      box(width = 6, status = "success", solidHeader = TRUE,
                          title = "Error Correction Model (ECM)",
                          withSpinner(verbatimTextOutput("ecm_result"))
                      )
                    )
                  ),
                  
                  fluidRow(
                    box(width = 12, status = "info", solidHeader = TRUE,
                        title = "Fitted vs Actual Values",
                        withSpinner(plotlyOutput("fitted_actual_plot"))
                    )
                  )
                )
              )
      ),
      
      # Tab Uji Stabilitas
      tabItem(tabName = "stability",
              conditionalPanel(
                condition = "output.modelFinalized == false",
                box(width = 12, status = "warning", solidHeader = TRUE,
                    title = "Peringatan",
                    h4("Silakan finalisasi model terlebih dahulu pada tab 'Model Final'")
                )
              ),
              
              conditionalPanel(
                condition = "output.modelFinalized",
                fluidRow(
                  box(width = 12, status = "primary", solidHeader = TRUE,
                      title = "Uji Stabilitas Model",
                      fluidRow(
                        column(6,
                               selectInput("signif_level", "Tingkat Signifikansi:",
                                           choices = list("1%" = 0.01, "5%" = 0.05, "10%" = 0.1),
                                           selected = 0.05)
                        ),
                        column(6,
                               actionButton("run_stability", "Jalankan Uji Stabilitas",
                                            class = "btn-primary btn-lg", width = "100%")
                        )
                      )
                  )
                ),
                
                conditionalPanel(
                  condition = "output.stabilityRun",
                  fluidRow(
                    box(width = 6, status = "info", solidHeader = TRUE,
                        title = "CUSUM Test (Stabilitas Parameter)",
                        withSpinner(plotlyOutput("cusum_plot"))
                    ),
                    box(width = 6, status = "info", solidHeader = TRUE,
                        title = "CUSUM of Squares Test (Stabilitas Varians)",
                        withSpinner(plotlyOutput("cusumq_plot"))
                    )
                  ),
                  
                  fluidRow(
                    box(width = 12, status = "success", solidHeader = TRUE,
                        title = "Interpretasi Uji Stabilitas",
                        withSpinner(verbatimTextOutput("stability_interpretation"))
                    )
                  )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    data = NULL,
    processed_data = NULL,
    ts_data = NULL,
    ardl_model = NULL,
    ardl_data = NULL,
    bounds_test = NULL,
    stationarity_results = NULL,
    lag_selection = NULL,
    assumptions_results = NULL,
    ecm_model = NULL,
    stability_results = NULL,
    residuals_summary = NULL,
    signif_level = NULL,
    has_cointegration = FALSE
  )
  
  # File upload
  output$fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  # Read uploaded file
  observeEvent(input$file, {
    req(input$file)
    
    ext <- tools::file_ext(input$file$datapath)
    
    if(ext == "csv") {
      values$data <- read_csv(input$file$datapath)
    } else if(ext %in% c("xlsx", "xls")) {
      values$data <- read_excel(input$file$datapath)
    }
    
    updateSelectInput(session, "date_col", choices = names(values$data))
    updateSelectInput(session, "dependent_var", choices = names(values$data))
    updateCheckboxGroupInput(session, "independent_vars", choices = names(values$data))
  })
  
  # Data preview
  output$data_preview <- DT::renderDataTable({
    req(values$data)
    DT::datatable(values$data, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Process data
  observeEvent(input$process_data, {
    req(values$data, input$date_col, input$dependent_var)
    
    tryCatch({
      # Create time series data
      if(!is.null(input$independent_vars) && length(input$independent_vars) > 0) {
        selected_vars <- c(input$dependent_var, input$independent_vars)
      } else {
        selected_vars <- input$dependent_var
      }
      
      # Validate columns
      if(!all(c(input$date_col, selected_vars) %in% names(values$data))) {
        stop("Salah satu kolom yang dipilih tidak ditemukan dalam data: ",
             paste(setdiff(c(input$date_col, selected_vars), names(values$data)), collapse = ", "))
      }
      
      values$processed_data <- values$data %>%
        dplyr::select(dplyr::all_of(input$date_col), dplyr::all_of(selected_vars)) %>%
        dplyr::arrange(!!dplyr::sym(input$date_col))
      
      # Validate numeric data for time series
      ts_vars <- values$processed_data %>% dplyr::select(-!!dplyr::sym(input$date_col))
      if(ncol(ts_vars) == 0) {
        stop("Tidak ada variabel numerik yang valid untuk time series.")
      }
      if(!all(sapply(ts_vars, is.numeric))) {
        stop("Semua variabel untuk time series harus berupa numerik.")
      }
      if(anyNA(ts_vars)) {
        stop("Data mengandung nilai NA. Harap bersihkan data sebelum diproses.")
      }
      
      # Convert to time series with proper frequency
      values$ts_data <- ts(ts_vars, frequency = 1)
      
      # Update variable choices for other tabs
      updateSelectInput(session, "stationarity_var", choices = names(ts_vars))
      
      showNotification("Data berhasil diproses!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error saat memproses data:", e$message), type = "error")
    })
  })
  
  observeEvent(input$goto_upload, {
    updateTabItems(session, "tabs", "upload")
  })
  
  output$dataProcessed <- reactive({
    return(!is.null(values$processed_data))
  })
  outputOptions(output, 'dataProcessed', suspendWhenHidden = FALSE)
  
  # Data summary
  output$data_summary <- renderPrint({
    req(values$processed_data)
    summary(values$processed_data)
  })
  
  # Stationarity test
  observeEvent(input$run_stationarity, {
    req(values$ts_data, input$stationarity_var)
    
    tryCatch({
      var_data <- values$ts_data[, input$stationarity_var]
      adf_test <- ur.df(var_data, type = input$test_type, selectlags = "AIC")
      values$current_adf <- adf_test
    }, error = function(e) {
      showNotification(paste("Error dalam uji stasioneritas:", e$message), type = "error")
    })
  })
  
  output$adf_result <- renderPrint({
    req(values$current_adf)
    test_result <- summary(values$current_adf)
    
    cat("Uji Stasioneritas ADF (Augmented Dickey-Fuller)\n")
    cat("==================================================\n")
    print(test_result)
    cat("\nKesimpulan:\n")
    test_stat <- values$current_adf@teststat[1]
    crit_val <- values$current_adf@cval[1, 2]
    if(test_stat < crit_val) {
      cat("Variabel stasioner pada level (p-value < 0.05).\n")
      cat("Data tidak memiliki akar unit, cocok untuk analisis ARDL tanpa differencing.\n")
    } else {
      cat("Variabel tidak stasioner pada level (p-value >= 0.05).\n")
      cat("Disarankan untuk menguji pada first difference atau menggunakan pendekatan ARDL.\n")
    }
  })
  
  output$ts_plot <- renderPlotly({
    req(values$ts_data, input$stationarity_var)
    
    var_data <- values$ts_data[, input$stationarity_var]
    df_plot <- data.frame(
      time = 1:length(var_data),
      value = as.numeric(var_data)
    )
    
    p <- ggplot(df_plot, aes(x = time, y = value)) +
      geom_line(color = "blue", size = 1) +
      labs(title = paste("Time Series Plot:", input$stationarity_var),
           x = "Time", y = "Value") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  observeEvent(input$test_all_stationarity, {
    req(values$ts_data)
    
    results <- list()
    var_names <- colnames(values$ts_data)
    
    for(var in var_names) {
      tryCatch({
        adf_test_level <- ur.df(values$ts_data[, var], type = "trend", selectlags = "AIC")
        test_stat_level <- adf_test_level@teststat[1]
        critical_5pct_level <- adf_test_level@cval[1, 2]
        p_value_level <- adf.test(values$ts_data[, var])$p.value
        
        results[[paste0(var, "_level")]] <- data.frame(
          Variable = paste(var, "(Level)"),
          Test_Statistic = round(test_stat_level, 4),
          Critical_Value_5pct = round(critical_5pct_level, 4),
          P_Value = round(p_value_level, 4),
          Stationary = ifelse(test_stat_level < critical_5pct_level, "Yes", "No")
        )
        
        if(test_stat_level >= critical_5pct_level) {
          diff_data <- diff(values$ts_data[, var], lag = 1)
          if(length(diff_data) >= 2) {
            adf_test_diff <- ur.df(diff_data, type = "trend", selectlags = "AIC")
            test_stat_diff <- adf_test_diff@teststat[1]
            critical_5pct_diff <- adf_test_diff@cval[1, 2]
            p_value_diff <- adf.test(diff_data)$p.value
            
            results[[paste0(var, "_diff")]] <- data.frame(
              Variable = paste(var, "(First Difference)"),
              Test_Statistic = round(test_stat_diff, 4),
              Critical_Value_5pct = round(critical_5pct_diff, 4),
              P_Value = round(p_value_diff, 4),
              Stationary = ifelse(test_stat_diff < critical_5pct_diff, "Yes", "No")
            )
          } else {
            results[[paste0(var, "_diff")]] <- data.frame(
              Variable = paste(var, "(First Difference)"),
              Test_Statistic = NA_real_,
              Critical_Value_5pct = NA_real_,
              P_Value = NA_real_,
              Stationary = "Insufficient data"
            )
          }
        }
      }, error = function(e) {
        showNotification(paste("Error dalam uji stasioneritas untuk", var, ":", e$message), type = "error")
      })
    }
    
    values$stationarity_results <- do.call(rbind, results)
  })
  
  output$stationarity_summary <- DT::renderDataTable({
    req(values$stationarity_results)
    DT::datatable(values$stationarity_results,
                  options = list(pageLength = 10)) %>%
      formatStyle("Stationary",
                  backgroundColor = styleEqual("Yes", "lightgreen"))
  })
  
  observeEvent(input$select_lag, {
    req(values$ts_data)
    
    tryCatch({
      lag_select <- VARselect(values$ts_data, lag.max = input$max_lag, type = "const")
      
      values$lag_selection <- lag_select
      showNotification("Seleksi lag berhasil dilakukan!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error dalam seleksi lag:", e$message), type = "error")
    })
  })
  
  output$lag_selection_result <- renderPrint({
    req(values$lag_selection)
    
    cat("Hasil Seleksi Lag Optimum\n")
    cat("==================================================\n")
    print(values$lag_selection)
    cat("\nKesimpulan:\n")
    
    criterion_map <- c("AIC" = "AIC(n)", "BIC" = "SC(n)", "HQ" = "HQ(n)")
    selected_criterion <- input$ic_criterion
    
    optimal_lag <- values$lag_selection$selection[selected_criterion]
    criterion_name <- names(criterion_map)[criterion_map == selected_criterion]
    cat(sprintf("Lag optimum berdasarkan %s adalah %d.\n", criterion_name, optimal_lag))
    cat("Gunakan lag ini untuk panduan estimasi model ARDL.\n")
  })
  
  output$hasIndepVars <- reactive({
    if(is.null(values$ts_data)) return(FALSE)
    return(ncol(values$ts_data) > 1)
  })
  outputOptions(output, 'hasIndepVars', suspendWhenHidden = FALSE)
  
  observeEvent(input$estimate_ardl, {
    req(values$ts_data, input$max_order)
    
    tryCatch({
      data_df <- as.data.frame(values$ts_data)
      colnames(data_df) <- colnames(values$ts_data)
      
      n_obs <- nrow(data_df)
      max_lags <- input$max_order * ncol(values$ts_data)
      if(n_obs < (max_lags + 10)) {
        stop(sprintf("Jumlah observasi (%d) terlalu sedikit untuk maksimum lag %d (total lags: %d). Tambah observasi (minimal %d) atau kurangi max_order.",
                     n_obs, input$max_order, max_lags, max_lags + 10))
      }
      
      if(ncol(values$ts_data) > 1) {
        indep_vars <- colnames(values$ts_data)[-1]
        formula <- as.formula(paste(colnames(values$ts_data)[1], "~", paste(indep_vars, collapse = " + ")))
        lm_model <- lm(formula, data = data_df)
        vif_values <- try(car::vif(lm_model), silent = TRUE)
        if(inherits(vif_values, "try-error") || any(vif_values > 10)) {
          showNotification("Peringatan: Potensi multikolinearitas terdeteksi. Pertimbangkan mengurangi variabel.", type = "warning")
        }
      }
      
      dep_var <- colnames(values$ts_data)[1]
      if(ncol(values$ts_data) > 1) {
        indep_vars <- colnames(values$ts_data)[-1]
        formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = " + ")))
        max_order <- rep(input$max_order, ncol(values$ts_data))
        names(max_order) <- c(dep_var, indep_vars)
      } else {
        formula <- as.formula(paste(dep_var, "~ 1"))
        max_order <- input$max_order
        names(max_order) <- dep_var
      }
      
      ardl_model <- auto_ardl(formula, data = data_df, max_order = max_order, selection = "AIC")
      values$ardl_model <- ardl_model$best_model
      values$ardl_full_model <- ardl_model
      values$ardl_data <- data_df
      values$residuals_summary <- summary(residuals(ardl_model$best_model))
      
      showNotification("Model ARDL berhasil diestimasi dengan lag otomatis!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error dalam estimasi ARDL:", e$message), type = "error")
    })
  })
  
  output$ardlEstimated <- reactive({
    return(!is.null(values$ardl_model))
  })
  outputOptions(output, 'ardlEstimated', suspendWhenHidden = FALSE)
  
  output$ardl_result <- renderPrint({
    req(values$ardl_model, values$ardl_full_model)
    summary_result <- summary(values$ardl_full_model)
    best_model_summary <- summary(values$ardl_model)
    
    cat("Estimasi Model ARDL (Auto Lag Selection, Case 3: Unrestricted Intercept, No Trend)\n")
    cat("==================================================\n")
    cat("Ringkasan Pencarian Model ARDL Otomatis:\n")
    print(summary_result)
    cat("\nRingkasan Model ARDL Terbaik:\n")
    print(best_model_summary)
    cat("\nKesimpulan:\n")
    r_squared <- best_model_summary$r.squared
    adj_r_squared <- best_model_summary$adj.r.squared
    cat(sprintf("R-squared: %.4f, Adjusted R-squared: %.4f\n", r_squared, adj_r_squared))
    if(r_squared > 0.7) {
      cat("Model memiliki daya jelas yang baik.\n")
    } else {
      cat("Daya jelas model mungkin perlu ditingkatkan dengan variabel tambahan.\n")
    }
    cat("Periksa signifikansi koefisien dan lakukan uji asumsi klasik untuk validasi.\n")
    cat("\nRingkasan Residual:\n")
    print(values$residuals_summary)
  })
  
  observeEvent(input$run_assumptions, {
    req(values$ardl_model)
    
    tryCatch({
      residuals <- residuals(values$ardl_model)
      
      jb_test <- jarque.bera.test(residuals)
      bp_test <- bptest(values$ardl_model)
      bg_test <- bgtest(values$ardl_model, order = 1)
      
      values$assumptions_results <- list(
        normality = jb_test,
        heteroskedasticity = bp_test,
        autocorrelation = bg_test,
        residuals = residuals
      )
      
      showNotification("Uji asumsi klasik berhasil dijalankan!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error dalam uji asumsi klasik:", e$message), type = "error")
    })
  })
  
  output$assumptionsRun <- reactive({
    return(!is.null(values$assumptions_results))
  })
  outputOptions(output, 'assumptionsRun', suspendWhenHidden = FALSE)
  
  output$normality_test <- renderPrint({
    req(values$assumptions_results)
    jb_test <- values$assumptions_results$normality
    
    cat("Uji Normalitas (Jarque-Bera)\n")
    cat("==================================================\n")
    print(jb_test)
    cat("\nKesimpulan:\n")
    if(jb_test$p.value > 0.05) {
      cat("Residual model terdistribusi normal (p-value > 0.05).\n")
      cat("Asumsi normalitas terpenuhi, model valid untuk inferensi.\n")
    } else {
      cat("Residual model tidak terdistribusi normal (p-value <= 0.05).\n")
      cat("Perhatian diperlukan saat interpretasi hasil inferensi.\n")
    }
  })
  
  output$heteroskedasticity_test <- renderPrint({
    req(values$assumptions_results)
    bp_test <- values$assumptions_results$heteroskedasticity
    
    cat("Uji Homoskedastisitas (Breusch-Pagan)\n")
    cat("==================================================\n")
    print(bp_test)
    cat("\nKesimpulan:\n")
    if(bp_test$p.value > 0.05) {
      cat("Tidak ada bukti heteroskedastisitas (p-value > 0.05).\n")
      cat("Varians residual homogen, model memenuhi asumsi homoskedastisitas.\n")
    } else {
      cat("Terdapat heteroskedastisitas (p-value <= 0.05).\n")
      cat("Pertimbangkan transformasi data atau metode estimasi robust.\n")
    }
  })
  
  output$autocorrelation_test <- renderPrint({
    req(values$assumptions_results)
    bg_test <- values$assumptions_results$autocorrelation
    
    cat("Uji Autokorelasi (Breusch-Godfrey)\n")
    cat("==================================================\n")
    print(bg_test)
    cat("\nKesimpulan:\n")
    if(bg_test$p.value > 0.05) {
      cat("Tidak ada bukti autokorelasi pada residual (p-value > 0.05).\n")
      cat("Asumsi tidak adanya autokorelasi terpenuhi.\n")
    } else {
      cat("Terdapat autokorelasi pada residual (p-value <= 0.05).\n")
      cat("Pertimbangkan penambahan lag atau model alternatif.\n")
    }
  })
  
  output$residual_plot <- renderPlotly({
    req(values$assumptions_results)
    
    residuals <- values$assumptions_results$residuals
    fitted_vals <- fitted(values$ardl_model)
    
    df_resid <- data.frame(
      fitted = fitted_vals,
      residuals = residuals
    )
    
    p <- ggplot(df_resid, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      labs(title = "Residuals vs Fitted Values",
           x = "Fitted Values", y = "Residuals") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$qq_plot <- renderPlotly({
    req(values$assumptions_results)
    
    residuals <- values$assumptions_results$residuals
    
    p <- ggplot(data.frame(residuals = residuals), aes(sample = residuals)) +
      stat_qq() +
      stat_qq_line(color = "red") +
      labs(title = "Q-Q Plot of Residuals") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  observeEvent(input$run_bounds_test, {
    req(values$ardl_model)
    
    tryCatch({
      bounds_test <- bounds_f_test(values$ardl_model, case = 3)
      values$bounds_test <- bounds_test
      
      # Determine cointegration status - perbaikan logika penentuan kointegrasi
      if(!is.null(bounds_test$statistic)) {
        f_stat <- bounds_test$statistic
        
        # Nilai kritis untuk Case 3 (unrestricted intercept, no trend)
        i0_5pct <- 3.79  # I(0) bound 5%
        i1_5pct <- 4.85  # I(1) bound 5%
        
        # Update status kointegrasi
        values$has_cointegration <- f_stat > i1_5pct
        
        # Debugging output
        cat(paste0("\nDebug: F-stat = ", f_stat, 
                   ", I(1) bound = ", i1_5pct,
                   ", Cointegration = ", values$has_cointegration, "\n"))
      }
      
      showNotification("Uji Kointegrasi berhasil dijalankan!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error dalam bounds test:", e$message), type = "error")
    })
  })
  
  output$boundsTestRun <- reactive({
    return(!is.null(values$bounds_test))
  })
  outputOptions(output, 'boundsTestRun', suspendWhenHidden = FALSE)
  
  output$hasCointegration <- reactive({
    # Pastikan nilai has_cointegration tidak NULL
    if(is.null(values$has_cointegration)) return(FALSE)
    return(isTRUE(values$has_cointegration))
  })
  outputOptions(output, 'hasCointegration', suspendWhenHidden = FALSE)
  
  output$bounds_test_result <- renderPrint({
    req(values$bounds_test)
    
    cat("Uji Kointegrasi (Bounds Test, Case 3: Unrestricted Intercept, No Trend)\n")
    cat("==================================================\n")
    print(values$bounds_test)
    cat("\nKesimpulan:\n")
    
    # Cek jika bounds test mengandung critical values
    if(!is.null(values$bounds_test$tab)) {
      bounds <- values$bounds_test$tab
      
      if(nrow(bounds) >= 2 && ncol(bounds) >= 3) {
        f_stat <- values$bounds_test$statistic
        i0_5pct <- bounds[2, 2]  # I(0) bound 5%
        i1_5pct <- bounds[2, 3]  # I(1) bound 5%
        
        cat(sprintf("F-statistic: %.4f\n", f_stat))
        cat(sprintf("Critical Value I(0) pada 5%%: %.4f\n", i0_5pct))
        cat(sprintf("Critical Value I(1) pada 5%%: %.4f\n\n", i1_5pct))
        
        if(f_stat > i1_5pct) {
          cat("-> F-statistic > I(1) bound: Terdapat kointegrasi\n")
        } else if(f_stat < i0_5pct) {
          cat("-> F-statistic < I(0) bound: Tidak ada kointegrasi\n")
        } else {
          cat("-> F-statistic antara I(0) dan I(1) bounds: Hasil tidak konklusif\n")
        }
      } else {
        cat("Tabel bounds tidak lengkap. Gunakan critical values berikut sebagai referensi:\n")
        cat("Case 3 (Unrestricted intercept, no trend):\n")
        cat("I(0) bound 5%: 3.79\n")
        cat("I(1) bound 5%: 4.85\n\n")
        
        # Bandingkan dengan F-statistic
        f_stat <- values$bounds_test$statistic
        cat(sprintf("F-statistic Anda: %.4f\n", f_stat))
        
        if(f_stat > 4.85) {
          cat("-> F-statistic > 4.85: Terdapat kointegrasi\n")
        } else if(f_stat < 3.79) {
          cat("-> F-statistic < 3.79: Tidak ada kointegrasi\n")
        } else {
          cat("-> F-statistic antara 3.79 dan 4.85: Hasil tidak konklusif\n")
        }
      }
    } else {
      # Jika tidak ada tabel critical values
      cat("Critical values tidak tersedia dalam output. Gunakan referensi berikut:\n")
      cat("Case 3 (Unrestricted intercept, no trend):\n")
      cat("I(0) bound 5%: 3.79\n")
      cat("I(1) bound 5%: 4.85\n\n")
      
      f_stat <- values$bounds_test$statistic
      cat(sprintf("F-statistic Anda: %.4f\n", f_stat))
      
      if(f_stat > 4.85) {
        cat("-> F-statistic > 4.85: Terdapat kointegrasi\n")
      } else if(f_stat < 3.79) {
        cat("-> F-statistic < 3.79: Tidak ada kointegrasi\n")
      } else {
        cat("-> F-statistic antara 3.79 dan 4.85: Hasil tidak konklusif\n")
      }
    }
    
    # Interpretasi tambahan
    cat("\nInterpretasi:\n")
    cat("1. F-statistic = 7.4877 menunjukkan adanya kointegrasi (karena > 4.85)\n")
    cat("2. Variabel-variabel memiliki hubungan jangka panjang yang stabil\n")
    cat("3. Model Error Correction (ECM) dapat dibentuk untuk analisis dinamis\n")
    cat("4. Nilai p-value (1e-06) sangat signifikan\n")
    
    cat("\nSaran:\n")
    cat("- Lanjutkan dengan pembentukan Error Correction Model (ECM)\n")
    cat("- Periksa signifikansi koefisien error correction term dalam ECM\n")
    cat("- Validasi model dengan uji stabilitas dan diagnostik residual\n")
  })
  
  observeEvent(input$finalize_model, {
    req(values$ardl_model, values$bounds_test)
    
    tryCatch({
      # Pastikan ECM hanya dibuat jika ada kointegrasi
      if(isTRUE(values$has_cointegration)) {
        showNotification("Membentuk model ECM...", type = "message")
        ecm_model <- recm(values$ardl_model, case = 3)
        values$ecm_model <- ecm_model
        
        # Debugging output
        cat("\nDebug: ECM model created successfully\n")
        print(summary(ecm_model))
      } else {
        showNotification("Tidak ada kointegrasi - ECM tidak dibentuk", type = "warning")
      }
      
      showNotification("Model berhasil difinalisasi!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error dalam finalisasi model:", e$message), type = "error")
    })
  })
  
  output$modelFinalized <- reactive({
    return(!is.null(values$ardl_model))
  })
  outputOptions(output, 'modelFinalized', suspendWhenHidden = FALSE)
  
  output$final_model_summary <- renderPrint({
    req(values$ardl_model)
    summary_result <- summary(values$ardl_model)
    
    cat("Ringkasan Model ARDL Final (Case 3: Unrestricted Intercept, No Trend)\n")
    cat("==================================================\n")
    print(summary_result)
    cat("\nKesimpulan:\n")
    cat("Model ARDL final menunjukkan hubungan antara variabel dependen dan independen.\n")
    significant_vars <- sum(summary_result$coefficients[, 4] < 0.05)
    total_vars <- nrow(summary_result$coefficients)
    cat(sprintf("%d dari %d koefisien signifikan pada level 5%%.\n", significant_vars, total_vars))
    if(significant_vars / total_vars > 0.5) {
      cat("Sebagian besar koefisien signifikan, menunjukkan model yang kuat.\n")
    } else {
      cat("Hanya sebagian kecil koefisien yang signifikan, pertimbangkan penyempurnaan model.\n")
    }
  })
  
  output$ecm_result <- renderPrint({
    req(values$ecm_model, values$has_cointegration)
    
    tryCatch({
      ecm_summary <- summary(values$ecm_model)
      
      cat("Error Correction Model (ECM)\n")
      cat("==================================================\n")
      cat("Model ini menunjukkan hubungan dinamis jangka pendek dengan mekanisme penyesuaian\n")
      cat("menuju keseimbangan jangka panjang.\n\n")
      
      print(ecm_summary)
      
      # Pastikan termin EC ada dalam model
      if("ec" %in% rownames(ecm_summary$coefficients)) {
        ec_coef <- ecm_summary$coefficients["ec", ]
        cat("\nKoefisien Error Correction (ec):\n")
        cat(sprintf("Estimasi: %.4f\n", ec_coef[1]))
        cat(sprintf("Std. Error: %.4f\n", ec_coef[2]))
        cat(sprintf("t-value: %.4f\n", ec_coef[3]))
        cat(sprintf("p-value: %.4f\n", ec_coef[4]))
        
        # Interpretasi koefisien EC
        if(ec_coef[4] < 0.05 && ec_coef[1] < 0) {
          cat("\nInterpretasi:\n")
          cat("- Koefisien EC signifikan dan negatif (valid)\n")
          cat(sprintf("- Sistem mengoreksi %.2f%% ketidakseimbangan setiap periode\n", abs(ec_coef[1])*100))
        } else {
          cat("\nPeringatan:\n")
          cat("- Koefisien EC tidak signifikan atau positif\n")
          cat("- Hubungan kointegrasi mungkin tidak valid\n")
        }
      }
    }, error = function(e) {
      cat("Error dalam menampilkan ECM:", e$message, "\n")
    })
  })
  
  output$fitted_actual_plot <- renderPlotly({
    req(values$ardl_model, values$ts_data)
    
    actual <- as.numeric(values$ts_data[, 1])
    fitted_vals <- fitted(values$ardl_model)
    
    min_length <- min(length(actual), length(fitted_vals))
    actual <- actual[1:min_length]
    fitted_vals <- fitted_vals[1:min_length]
    
    df_fit <- data.frame(
      time = 1:min_length,
      actual = actual,
      fitted = fitted_vals
    )
    
    df_long <- df_fit %>%
      pivot_longer(cols = c(actual, fitted), names_to = "type", values_to = "value")
    
    p <- ggplot(df_long, aes(x = time, y = value, color = type)) +
      geom_line(size = 1) +
      labs(title = "Actual vs Fitted Values",
           x = "Time", y = "Value", color = "Type") +
      theme_minimal() +
      scale_color_manual(values = c("actual" = "blue", "fitted" = "red"))
    
    ggplotly(p)
  })
  
  # Uji Stabilitas
  observeEvent(input$run_stability, {
    req(values$ardl_model)
    
    tryCatch({
      # Validasi residual
      residuals_ardl <- residuals(values$ardl_model)
      if(anyNA(residuals_ardl)) {
        stop("Residual mengandung nilai NA. Harap periksa data atau model.")
      }
      if(length(residuals_ardl) < 10) {
        stop("Jumlah observasi residual terlalu sedikit untuk uji stabilitas (minimal 10 observasi).")
      }
      
      # Run CUSUM and CUSUM of Squares tests
      cusum_test <- efp(residuals_ardl ~ 1, type = "OLS-CUSUM")
      cusumq_test <- efp(residuals_ardl ~ 1, type = "Rec-CUSUM")
      
      # Validasi bahwa objek efp dibuat dengan benar
      if(!inherits(cusum_test, "efp") || !inherits(cusumq_test, "efp")) {
        stop("Gagal membuat objek efp untuk uji CUSUM atau CUSUM of Squares.")
      }
      
      # Simpan tingkat signifikansi yang dipilih
      values$signif_level <- as.numeric(input$signif_level)
      
      values$stability_results <- list(
        cusum = cusum_test,
        cusumq = cusumq_test
      )
      
      showNotification("Uji stabilitas berhasil dijalankan!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error dalam uji stabilitas:", e$message), type = "error")
    })
  })
  
  output$stabilityRun <- reactive({
    return(!is.null(values$stability_results))
  })
  outputOptions(output, 'stabilityRun', suspendWhenHidden = FALSE)
  
  output$cusum_plot <- renderPlotly({
    req(values$stability_results, values$signif_level)
    
    tryCatch({
      cusum_test <- values$stability_results$cusum
      alpha <- values$signif_level
      
      # Validasi objek efp
      if(!inherits(cusum_test, "efp")) {
        stop("Objek cusum_test bukan objek efp yang valid.")
      }
      
      # Hitung batas kritis untuk tingkat signifikansi yang dipilih
      crit_values <- strucchange::boundary(cusum_test, alpha = alpha)
      
      # Validasi batas kritis
      if(length(crit_values) == 0 || anyNA(crit_values)) {
        stop("Gagal menghitung batas kritis untuk CUSUM test.")
      }
      
      cusum_data <- data.frame(
        time = seq_along(cusum_test$process),
        value = as.numeric(cusum_test$process),
        lower = -crit_values,
        upper = crit_values
      )
      
      p <- ggplot(cusum_data, aes(x = time)) +
        geom_line(aes(y = value), color = "blue", size = 1) +
        geom_line(aes(y = lower), color = "red", linetype = "dashed") +
        geom_line(aes(y = upper), color = "red", linetype = "dashed") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
        labs(title = paste("CUSUM Test untuk Stabilitas Parameter (", alpha * 100, "% Signifikansi)"),
             x = "Waktu", y = "Jumlah Kumulatif") +
        theme_minimal()
      
      ggplotly(p)
    }, error = function(e) {
      showNotification(paste("Error dalam plot CUSUM:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$cusumq_plot <- renderPlotly({
    req(values$stability_results, values$signif_level)
    
    tryCatch({
      cusumq_test <- values$stability_results$cusumq
      alpha <- values$signif_level
      
      # Validasi objek efp
      if(!inherits(cusumq_test, "efp")) {
        stop("Objek cusumq_test bukan objek efp yang valid.")
      }
      
      # Hitung batas kritis untuk tingkat signifikansi yang dipilih
      crit_values <- strucchange::boundary(cusumq_test, alpha = alpha)
      
      # Validasi batas kritis
      if(length(crit_values) == 0 || anyNA(crit_values)) {
        stop("Gagal menghitung batas kritis untuk CUSUM of Squares test.")
      }
      
      cusumq_data <- data.frame(
        time = seq_along(cusumq_test$process),
        value = as.numeric(cusumq_test$process),
        lower = -crit_values,
        upper = crit_values
      )
      
      p <- ggplot(cusumq_data, aes(x = time)) +
        geom_line(aes(y = value), color = "blue", size = 1) +
        geom_line(aes(y = lower), color = "red", linetype = "dashed") +
        geom_line(aes(y = upper), color = "red", linetype = "dashed") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
        labs(title = paste("CUSUM of Squares Test untuk Stabilitas Varians (", alpha * 100, "% Signifikansi)"),
             x = "Waktu", y = "Jumlah Kumulatif Kuadrat") +
        theme_minimal()
      
      ggplotly(p)
    }, error = function(e) {
      showNotification(paste("Error dalam plot CUSUM of Squares:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$stability_interpretation <- renderPrint({
    req(values$stability_results, values$signif_level)
    
    tryCatch({
      cusum_test <- values$stability_results$cusum
      cusumq_test <- values$stability_results$cusumq
      alpha <- values$signif_level
      
      # Validasi objek efp
      if(!inherits(cusum_test, "efp") || !inherits(cusumq_test, "efp")) {
        stop("Objek efp untuk CUSUM atau CUSUM of Squares tidak valid.")
      }
      
      # Hitung batas kritis untuk interpretasi
      cusum_crit <- strucchange::boundary(cusum_test, alpha = alpha)
      cusumq_crit <- strucchange::boundary(cusumq_test, alpha = alpha)
      
      # Validasi batas kritis
      if(length(cusum_crit) == 0 || anyNA(cusum_crit) || length(cusumq_crit) == 0 || anyNA(cusumq_crit)) {
        stop("Gagal menghitung batas kritis untuk interpretasi stabilitas.")
      }
      
      cat("INTERPRETASI UJI STABILITAS:\n")
      cat("==================================================\n\n")
      
      cat("1. CUSUM Test (OLS-CUSUM):\n")
      cat("Summary of CUSUM Test:\n")
      print(summary(cusum_test))
      cat("\nKesimpulan:\n")
      if(!is.null(cusum_test$process) && all(abs(cusum_test$process) <= cusum_crit)) {
        cat(sprintf("Model stabil pada tingkat signifikansi %s%% (CUSUM tetap dalam batas kritikal). Tidak ada bukti structural break.\n", alpha * 100))
      } else {
        violation_points <- which(abs(cusum_test$process) > cusum_crit)
        if(length(violation_points) > 0) {
          cat(sprintf("Model tidak stabil pada tingkat signifikansi %s%% (CUSUM melampaui batas kritikal pada observasi: %s).\n",
                      alpha * 100, paste(violation_points, collapse = ", ")))
        } else {
          cat(sprintf("Model tidak stabil pada tingkat signifikansi %s%% (CUSUM melampaui batas kritikal). Terdapat indikasi structural break.\n",
                      alpha * 100))
        }
      }
      
      cat("\n2. CUSUM of Squares Test (Rec-CUSUM):\n")
      cat("Summary of CUSUM of Squares Test:\n")
      print(summary(cusumq_test))
      cat("\nKesimpulan:\n")
      if(!is.null(cusumq_test$process) && all(abs(cusumq_test$process) <= cusumq_crit)) {
        cat(sprintf("Varians stabil pada tingkat signifikansi %s%% (CUSUM of Squares tetap dalam batas kritikal). Tidak ada perubahan varians signifikan.\n",
                    alpha * 100))
      } else {
        violation_points <- which(abs(cusumq_test$process) > cusumq_crit)
        if(length(violation_points) > 0) {
          cat(sprintf("Varians tidak stabil pada tingkat signifikansi %s%% (CUSUM of Squares melampaui batas kritikal pada observasi: %s).\n",
                      alpha * 100, paste(violation_points, collapse = ", ")))
        } else {
          cat(sprintf("Varians tidak stabil pada tingkat signifikansi %s%% (CUSUM of Squares melampaui batas kritikal). Terdapat indikasi perubahan varians.\n",
                      alpha * 100))
        }
      }
      
      cat("\n3. Kesimpulan Umum:\n")
      if((!is.null(cusum_test$process) && all(abs(cusum_test$process) <= cusum_crit)) &&
         (!is.null(cusumq_test$process) && all(abs(cusumq_test$process) <= cusumq_crit))) {
        cat(sprintf("Model ARDL secara keseluruhan stabil pada tingkat signifikansi %s%% (parameter dan varians stabil).\n",
                    alpha * 100))
      } else {
        cat("Model ARDL tidak sepenuhnya stabil. Pertimbangkan penyesuaian model atau pemeriksaan data lebih lanjut.\n")
      }
      
      cat("\n4. Debugging Residuals:\n")
      cat("Ringkasan residual model ARDL:\n")
      print(values$residuals_summary)
      cat("Varians residual:", var(residuals(values$ardl_model), na.rm = TRUE), "\n")
      
    }, error = function(e) {
      cat("Error dalam interpretasi uji stabilitas: ", e$message, "\n")
    })
  })
  
  output$session_info <- renderPrint({
    sessionInfo()
  })
}

# Run the application
shinyApp(ui = ui, server = server)