library(shiny)
library(bs4Dash)
library(plotly)
library(DT)
library(shinyWidgets)
library(fs)
library(glue)
library(stringr)
library(data.table)
library(MASS)
library(dplyr)
library(mgcv) 
library(purrr)
library(arrow)
library(lightgbm)
library(ggplot2)
library(tibble)
library(readr)
library(flexdashboard)




ui <- dashboardPage(
  dashboardHeader(title = "Stock Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Instructions", tabName = "instructions", icon = icon("search")),
      menuItem("Data Inspection", tabName = "data", icon = icon("table")),
      menuItem("Stock Summary", tabName = "stock", icon = icon("chart-bar")),
      menuItem("Model Recommendation", tabName = "recommend", icon = icon("thumbs-up")),
      menuItem("Baseline HAR-RV Model", tabName = "har_base", icon = icon("chart-line")),
      menuItem("Stepwise HAR-RV Model", tabName = "model", icon = icon("chart-line")),
      menuItem("GAM Model",      tabName = "gam",      icon = icon("project-diagram")),
      menuItem("LightGBM Model", tabName = "lgbm",     icon = icon("bolt"))
    )
  ),
  dashboardBody(
    tabItems(
      # Intro page
      tabItem(
        tabName = "instructions",
        h1("Instructions"),
        h5("This is the ShinyApp made by group Optiver 13."),
        p("Below described are the purposes of each section and how to make use of them."),
        fluidRow(
          box(
            title       = "Data Inspection",
            width       = 12,
            solidHeader = TRUE,
            status      = "primary",
            HTML("<p>In this tab, you can take a look at the raw data provided by Optiver. The following can be done:</p>
                 <ul>
                 <li>View the data for any stock</li>
                 <li>View the data for a specific Time ID for any selected stock</li>
                 <li>View the variance in Bid Prices and Ask Prices of the stock over the recorded 10 minutes via an informative time series visualisation</li>")
          )
        ),
        fluidRow(
          box(
            title       = "Stock Summary",
            width       = 12,
            solidHeader = TRUE,
            status = "info",
            HTML("<p>In this tab, you can look at the metrics we have calculated per interval for each stock to build our models on.</p>
                 <p>You can select any stock(s) from the ones listed under the <strong>Choose Stocks</strong> option and view the <strong>Realised Volatility</strong>, <strong>Weighted Average Price</strong>, <strong>Bid-Ask Spread</strong> and the mean <strong>Log Returns</strong>.</p>
                 <p><strong>Realised Volatility</strong> is showin as a time series distribution over the 20 aggregated time buckets for each Time ID which the selected stock contains. The other features are plotted as boxplots to highlight how they are distributed.</p>")
          )
        ),
        fluidRow(
          box(
            title       = "Model Recommendation",
            width       = 12,
            solidHeader = TRUE,
            status = "success",
            HTML("<p>This tab highlights our model evaluation metrics and the specific <strong>S-Score</strong> we engineered to rank each model's performance.</p>
                 <p>You can select any stock you would want to make a trade on, and the model that achieved the highest S-Score for that specific stock will be recommended. The S-Scores achieved by the other models we engineered are also displayed. The accuracy of the reccomended model can be seen under the <strong>Forecasting Confidence</strong> section in the same page.</p>
                 <p>If you wish to see how all the models perform on this specific stock with different weights applied to the S-Score, you can adjust it under the <strong>Adjust S-score Weight</strong> section in the page. S-Score can be adjusted from 0 to 1, which depending on the stock, may also affect the prediction accuracy of the recommended model.</p>")
          )
        ),
        fluidRow(
          box(
            title       = "Baseline & Stepwise Heterogeneous Autoregressive model of Realized Volatility Models",
            width       = 12,
            solidHeader = TRUE,
            status = "warning",
            HTML("<p>The 2 tabs of <strong>Baseline HAR-RV Model</strong> and <strong>Stepwise HAR-RV Model</strong> contain both the HAR-RV made by group members <strong>Owen</strong> and <strong>Yifei</strong>.</p>
                 <p>After you have obtained the recommended model for your preferred stock from the previous section, you can navigate to the tabs of any of these specific models and select the stock number. Our app then runs the model on this specific stock, where it splits the data into an 80% training set and a 20% testing set. Once the model is ready, you can view the model's <strong>Metrics</strong>, <strong>QLIKE Distribution</strong> and the <strong>Predicted vs Actual</strong> plot showing the volatility prediction.</p>
                 <p>The S-Score obtained for this model for the selected stock can also be compared to how the same model performs over all the other stocks in the dataset under <strong>Model S-Score Across All Stocks</strong> in the form of a boxplot showing how the scores are distributed. The percentile rank for the current stock can also be viewed.</p>")
          )
        ),
        fluidRow(
          box(
            title       = "Generalised Additive Model",
            width       = 12,
            solidHeader = TRUE,
            status = "info",
            HTML("<p>The GAM tab contains the Generalised Additive Model made by group members <strong>Faisal</strong> and <strong>Hamza</strong>.</p>
                 <p>If this model is recommended to you under the Model Recommendation section, please navigate here and select the specific stock number as you desire.</p>
                 <p>This page displays the values obtained after applying GAM to the stock. The model <strong>Metrics</strong>, <strong>QLIKE Distribution</strong> and a scatter plot showing the <strong>Predicted vs Actual</strong> volatility.</p>
                 <p>The S Score distribution of GAM applied to this stock can also be compared to the S Score obtained from applying GAM to other stocks under <strong>Model S-Scores Across All Stocks</strong>, where the percentile rank of the specific stock can also be viewed.</p>")
          )
        ),
        fluidRow(
          box(
            title       = "Light Gradient Boosting Machine Model",
            width       = 12,
            solidHeader = TRUE,
            status = "success",
            HTML("<p>This tab contains the LightGBM model made by group members <strong>Daniella</strong> and <strong>Zoe</strong>.</p>
                 <p>If a LightGBM model was reccomended for the stock you have selected, please navigate to this tab and select the desired stock. The app applies the model to this stock and displays the <strong>Metrics</strong>, <strong>QLIKE Distribution</strong> and the <strong>Predicted vs Actual</strong> values in the form a scatter plot.</p>
                 <p>Like the other models, the S Score can for the selected stock can be viewed in comparision to how the model performs on all other stocks under <strong>Model S Score Across All Stocks</strong> which also shows the percentile rank of this model.</p>
                 <p>LightGBM and GAM, just like HAR, both split the data into an 80% training and 20% testing set.")
          )
        )
      ),
      # Data Inspection
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title      = "Overview: Optiver Order Book Data",
            width      = 12,
            solidHeader= TRUE,
            status     = "primary",
            HTML("
    <p>Optiver has provided a rich, high-frequency order book dataset covering 100+ stocks to support volatility forecasting—crucial for market-making strategies.</p>
    <ul>
      <li><strong>Objective:</strong> Accurately predict short-term volatility to inform quoting and hedging decisions.</li>
      <li><strong>Data:</strong> 1-second snapshots of bid/ask prices and sizes across multiple levels and time buckets.</li>
      <li><strong>Explore:</strong> Use the controls below to filter by stock and time interval, inspect the raw table, or visualize bid/ask price dynamics.</li>
    </ul>
  ")
          )
        ),
        fluidRow(
          box(title = "Selection Controls", width = 12, solidHeader = TRUE, status = "primary",
              fluidRow(
                column(6, pickerInput("stockSE", "Select Stock", choices = NULL, multiple = FALSE)),
                column(6, selectizeInput("timeSE", "Select time_id", choices = NULL))
              )
          )
        ),
        fluidRow(
          column(12,
                 tabBox(width = NULL, height = "600px",
                        tabPanel("Data Table", DTOutput("data_table")),
                        tabPanel("Data Visualization", plotlyOutput("data_plot", height = "500px"))
                 )
          )
        )
      ),
      # Stock Summary
      tabItem(
        tabName = "stock",
        fluidRow(
          box(
            title      = "Overview: Summary Statistics",
            width      = 12,
            solidHeader= TRUE,
            status     = "info",
            HTML("
    <p>From the raw Optiver order‐book data, we’ve computed key per‐stock, per‐interval metrics to feed our volatility models:</p>
    <ul>
      <li><strong>Realised Volatility:</strong> The root‐sum‐of‐squares of intra‐bucket log‐returns.</li>
      <li><strong>Weighted Average Price (WAP):</strong> Size‐weighted midpoint of bid/ask quotes.</li>
      <li><strong>Bid–Ask Spread:</strong> Relative distance between best ask and bid prices.</li>
      <li><strong>Mean Log‐Return:</strong> Average log‐return within each 30s bucket.</li>
    </ul>
    <p>Use the selector below to choose one or more stocks, then expand each plot to compare these summary statistics across your chosen stocks</p>
    </ul>
  ")
          )
        ),
        fluidRow(
          bs4Dash::box(
            title = "Select Stocks",
            width = 4,
            solidHeader = TRUE,
            status = "info",
            pickerInput(
              inputId = "stocks_summary",
              label = "Stocks:",
              choices = NULL,
              multiple = TRUE,
              options = list(`actions-box` = TRUE)
            )
          ),
          
          bs4Dash::box(
            title = "Select Time ID",
            width = 4,
            solidHeader = TRUE,
            status = "info",
            selectInput(
              inputId = "vol_time_id",
              label = "Time ID:",
              choices = NULL  # filled in server
            )
          )
        ),
        fluidRow(
          bs4Dash::box(
            title = "Realised Volatility",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            plotlyOutput("vol_ts", height = "400px")
          )
        ),
        fluidRow(
          bs4Dash::box(title = "Weighted Average Price", width = 12, solidHeader = TRUE, status = "info",
                       collapsible = TRUE, collapsed = TRUE, plotlyOutput("wap_box"))
        ),
        fluidRow(
          bs4Dash::box(title = "Bid–Ask Spread", width = 12, solidHeader = TRUE, status = "info",
                       collapsible = TRUE, collapsed = TRUE, plotlyOutput("spread_box"))
        ),
        fluidRow(
          bs4Dash::box(title = "Mean Log-Return", width = 12, solidHeader = TRUE, status = "info",
                       collapsible = TRUE, collapsed = TRUE, plotlyOutput("logret_box"))
        )
      ),
      
      
      # ──────────────────────────────────────────────────────────────────────────────
      # NEW: Model Recommendation tab
      tabItem(
        tabName = "recommend",
        fluidRow(
          box(
            title      = "Overview: S-Score Recommendation",
            width      = 12,
            solidHeader= TRUE,
            status     = "success",
            # wrap your LaTeX and HTML in withMathJax
            withMathJax(
              h4("1. What is the S score?"),
              p("The S score combines test-set R\\(^2\\) and QLIKE into one normalized accuracy measure:"),
              # use double backslashes inside R strings
              p('$$
                 A_{\\rm QLIKE} = \\frac{1}{1 + \\mathrm{QLIKE}}, \\quad
                 A_{R^2} = R^2_{\\rm test}, \\quad
                 S(\\alpha) = \\alpha\\,A_{R^2} + (1-\\alpha)\\,A_{\\rm QLIKE}.
                 $$'),
              
              h4("2. Why use it?"),
              tags$ul(
                tags$li(strong("Normalized:"), " both metrics lie in [0,1]."),
                tags$li(strong("Balanced:"),   " captures both goodness-of-fit and scaled error."),
                tags$li(strong("Flexible:"),   " choose \\(\\alpha\\in[0,1]\\) to weight R² vs QLIKE.")
              ),
              h4("3. How to interpret it?"),
              
              p("The S-score lies in [0, 1], where higher ⇒ better predictive accuracy.  You compare frameworks by their S-scores and pick the largest."),
              
              tags$ul(
                tags$li(
                  strong("Values near 1:"), 
                  " excellent fit and low scaled error."
                ),
                tags$li(
                  strong("Values near 0:"), 
                  " poor fit or high error."
                ),
                tags$li(
                  strong("Relative ranking:"), 
                  " directly compare across models; the highest wins."
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title      = "Select Stock",
            width      = 4,
            solidHeader= TRUE,
            status     = "success",
            pickerInput(
              inputId = "stockRE",
              label   = "Stock:",
              choices = NULL,       # will be populated from available_stocks()
              multiple= FALSE
            )
          ),
          
          column(
            width = 1  # <-- empty spacer column to push next box right
          ),
          
          
          
          box(
            title = "Adjust S-score Weight (α)", 
            width = 4, 
            status = "success", 
            solidHeader = TRUE,
            
            sliderInput(
              inputId = "alpha",
              label = NULL,
              min = 0, max = 1, step = 0.05, value = 0.5,
              ticks = TRUE
            ),
            
            helpText(HTML("α = 1 prioritizes R² (fit), α = 0 prioritizes QLIKE (error minimization)"))
          ),
          box(
            title = "Comparison Across Model Frameworks",
            width = 12,
            solidHeader = TRUE,
            status = "success",
            
            # Toggle buttons
            radioButtons(
              inputId = "view_choice",
              label = NULL,
              choices = c("Plot", "Table"),
              selected = "Plot",
              inline = TRUE
            ),
            
            # Show DataTable when Table is selected
            conditionalPanel(
              condition = "input.view_choice == 'Table'",
              DTOutput("s_table")
            ),
            
            # Show Plot when Plot is selected
            conditionalPanel(
              condition = "input.view_choice == 'Plot'",
              plotOutput("s_plot")
            )
          )
          
        ),
        fluidRow(
          box(
            title      = "Recommended Model",
            width      = 8,
            solidHeader= TRUE,
            status     = "success",
            uiOutput("best_model"),
            style = "font-size: 32px"
          ),
          box(
            title = "Forecasting Confidence",
            width = 4,
            solidHeader = TRUE,
            status = "success",
            gaugeOutput("predictability_meter_gauge", height = "160px"),
            p("How easy the volatility was to forecast (0 = hard, 1 = easy).", style = "text-align:center; font-size:13px; color:black;")
          )
        )
        
      ),
      
      
      tabItem(
        tabName = "har_base",
        fluidRow(
          box(
            title = "Baseline HAR-RV Model", width = 4, solidHeader = TRUE, status = "warning",
            pickerInput("stockSE_har_base", "Select Stock:", choices = NULL, multiple = FALSE)
          ),
          box(
            title = "Metrics", width = 8, solidHeader = TRUE, status = "warning",
            fluidRow(
              box(width = 6, title = "Median QLIKE", textOutput("har_base_med_qlike")),
              box(width = 6, title = "Test R²", textOutput("har_base_r2_test"))
            )
          )
        ),
        fluidRow(
          box(
            title = "QLIKE Distribution", width = 6, solidHeader = TRUE, status = "warning",
            plotlyOutput("har_base_qlike_box")
          ),
          box(
            title = "Predicted vs Actual", width = 6, solidHeader = TRUE, status = "warning",
            plotlyOutput("har_base_pred_scatter")
          )
        ),
        fluidRow(
          box(
            title = "Model S-Score Across All Stocks", width = 12, solidHeader = TRUE, status = "warning",
            plotlyOutput("har_base_s_boxplot"),
            uiOutput("har_base_s_interpretation")
            
          )
        )
      ),
      
      # ──────────────────────────────────────────────────────────────────────────────
      
      # Stepwise HAR-RV Panel
      tabItem(
        tabName = "model",
        fluidRow(
          box(
            title = "Stepwise HAR-RV Model", width = 4, solidHeader = TRUE, status = "warning",
            pickerInput("stockSE_model", "Select Stock:", choices = NULL, multiple = FALSE)
          ),
          box(
            title = "Metrics", width = 8, solidHeader = TRUE, status = "warning",
            fluidRow(
              box(width = 6, title = "Median QLIKE", textOutput("med_qlike")),
              box(width = 6, title = "Test R²", textOutput("r2_test"))
            )
          )
        ),
        fluidRow(
          box(
            title = "QLIKE Distribution", width = 6, solidHeader = TRUE, status = "warning",
            plotlyOutput("qlike_box")
          ),
          box(
            title = "Predicted vs Actual", width = 6, solidHeader = TRUE, status = "warning",
            plotlyOutput("pred_scatter")
          )
        ),
        fluidRow(
          box(
            title = "Model S-Score Across All Stocks", width = 12, solidHeader = TRUE, status = "warning",
            plotlyOutput("har_stepwise_s_boxplot"),
            uiOutput("har_stepwise_s_interpretation")
          )
        )
      ),
      
      
      # GAM Tab
      tabItem(
        tabName = "gam",
        fluidRow(
          box(
            title = "GAM Model", width = 4, solidHeader = TRUE, status = "info",
            pickerInput("stockSE_gam", "Select Stock:", choices = NULL, multiple = FALSE)
          ),
          box(
            title = "Metrics", width = 8, solidHeader = TRUE, status = "info",
            fluidRow(
              box(width = 6, title = "Median QLIKE", textOutput("gam_med_qlike")),
              box(width = 6, title = "Test R²", textOutput("gam_r2_test"))
            )
          )
        ),
        fluidRow(
          box(
            title = "QLIKE Distribution", width = 6, solidHeader = TRUE, status = "info",
            plotlyOutput("gam_qlike_box")
          ),
          box(
            title = "Predicted vs Actual", width = 6, solidHeader = TRUE, status = "info",
            plotlyOutput("gam_pred_scatter")
          )
        ),
        fluidRow(
          box(
            title = "Model S-Score Across All Stocks", width = 12, solidHeader = TRUE, status = "info",
            plotlyOutput("gam_s_boxplot"),
            uiOutput("gam_s_interpretation")
          )
          
        )
      ),
      
      
      # LightGBM Tab
      tabItem(
        tabName = "lgbm",
        fluidRow(
          box(title = "LightGBM Model", width = 4, solidHeader = TRUE, status = "success",
              pickerInput("stockSE_lgbm", "Select Stock:", choices = NULL, multiple = FALSE)
          ),
          box(title = "Metrics", width = 8, solidHeader = TRUE, status = "success",
              fluidRow(
                box(width = 6, title = "Median QLIKE", textOutput("lgbm_med_qlike")),
                box(width = 6, title = "Test R²", textOutput("lgbm_r2_test"))
              )
          )
        ),
        fluidRow(
          box(title = "QLIKE Distribution", width = 6, solidHeader = TRUE, status = "success",
              plotlyOutput("lgbm_qlike_box")
          ),
          box(title = "Predicted vs Actual", width = 6, solidHeader = TRUE, status = "success",
              plotlyOutput("lgbm_pred_scatter")
          )
        ),
        
        fluidRow(
          box(
            title = "Model S-Score Across All Stocks",
            width = 12,
            solidHeader = TRUE,
            status = "success",
            plotlyOutput("lgbm_s_boxplot"),
            uiOutput("lgbm_s_interpretation")
          )
        )
      )
      
    )
  ),
  title = "Stock Data Dashboard",
  skin = "blue"
)

server <- function(input, output, session) {
  # available stocks
  available_stocks <- reactive({
    files <- list.files("YOUR_PATH_TO/individual_book_train", pattern = "^stock_\\d+\\.csv$")
    sort(as.numeric(str_extract(files, "\\d+")))
  })
  
  
  # update selectors
  observe({
    updatePickerInput(session, "stockSE", choices = available_stocks())
    updatePickerInput(session, "stocks_summary", choices = available_stocks())
    updatePickerInput(session, "stockSE_model", choices = available_stocks())
    updatePickerInput(session, "stockRE", choices = available_stocks())
    updatePickerInput(session, "stockSE_gam", choices = available_stocks())
    updatePickerInput(session, "stockSE_lgbm", choices = available_stocks())
    updatePickerInput(session, "stockSE_har_base", choices = available_stocks())
  })
  
  # — load your pre‐computed S‐scores CSV once at startup
  all_scores <- fread("./metrics/R2_QLIKE.csv")
  s_scores_all <- fread("./metrics/All_model_S_scores.csv")
  
  
  # Data Inspection logic
  getData <- reactive({ req(input$stockSE); fread(fs::path("YOUR_PATH_TO/individual_book_train", glue("stock_{input$stockSE}.csv"))) })
  observeEvent(input$stockSE, { updateSelectizeInput(session, "timeSE", choices = unique(getData()$time_id), server = TRUE) })
  getData2 <- reactive({ req(input$timeSE); df <- getData(); df[time_id == input$timeSE] })
  output$data_table <- renderDT(datatable(getData2(), options=list(pageLength=10, scrollX=TRUE)))
  output$data_plot <- renderPlotly({
    df <- getData2()
    inter2 <- rbind(
      df[, .(seconds_in_bucket, price = bid_price1, what='bid.1')],
      df[, .(seconds_in_bucket, price = ask_price1, what='ask.1')],
      df[, .(seconds_in_bucket, price = bid_price2, what='bid.2')],
      df[, .(seconds_in_bucket, price = ask_price2, what='ask.2')]
    )
    plot_ly(inter2, x=~seconds_in_bucket, y=~price, color=~what, type='scatter', mode='lines')
  })
  
  # Stock Summary logic (unchanged)
  summary_data <- reactive({
    req(input$stocks_summary)
    dfs <- lapply(input$stocks_summary, function(stk) {
      df <- fread(fs::path('YOUR_PATH_TO/individual_book_train', glue('stock_{stk}.csv')))
      df[, mid := (bid_price1 + ask_price1)/2]
      df[, volatility := sd(diff(log(mid))), by = time_id]
      df[, wap := (bid_price1 * ask_size1 + ask_price1 * bid_size1)/(bid_size1 + ask_size1)]
      df[, spread := (ask_price1/bid_price1) - 1]
      df[, logret := mean(diff(log(mid))), by = time_id]
      sum_df <- df[, .(
        volatility = unique(volatility),
        wap        = mean(wap),
        spread     = mean(spread),
        logret     = unique(logret)
      ), by = time_id]
      sum_df[, stock_id := as.factor(stk)]
      sum_df
    })
    rbindlist(dfs)
  })
  add_stat_traces <- function(p, data, stat_col, stat_label) {
    for(stk in unique(data$stock_id)){
      dfstk <- data[stock_id == stk]
      mean_val <- mean(dfstk[[stat_col]], na.rm=TRUE)
      mean_str <- formatC(mean_val, format='f', digits=8)
      p <- p %>% add_trace(
        x = dfstk[[stat_col]], type='box', orientation='h', name = paste0('stock_', stk, ': ', mean_str)
      )
    }
    p %>% layout(xaxis = list(title = stat_label), yaxis = list(showticklabels = FALSE))
  }
  output$vol_box    <- renderPlotly({ add_stat_traces(plot_ly(), summary_data(), 'volatility', 'Realised Volatility') })
  output$wap_box    <- renderPlotly({ add_stat_traces(plot_ly(), summary_data(), 'wap', 'Weighted Average Price') })
  output$spread_box <- renderPlotly({ add_stat_traces(plot_ly(), summary_data(), 'spread', 'Bid–Ask Spread') })
  output$logret_box <- renderPlotly({ add_stat_traces(plot_ly(), summary_data(), 'logret', 'Mean Log-Return') })
  
  #Time Series Plot
  rv_stocks <- reactive({
    files <- list.files("YOUR_PATH_TO/individual_book_train", pattern = "^stock_\\d+\\.csv$")
    # extract the digits, coerce to numeric, sort, then back to character
    stock_nums <- as.numeric(stringr::str_extract(files, "\\d+"))
    sort_nums  <- sort(stock_nums, na.last = NA)
    as.character(sort_nums)
  })
  
  
  observe({
    updatePickerInput(
      session, "stocks_summary",
      choices = rv_stocks()
    )
  })
  
  observeEvent(input$stocks_summary, {
    all_ids <- unique(unlist(lapply(input$stocks_summary, function(stk) {
      dt <- fread(glue::glue("YOUR_PATH_TO/individual_book_train/stock_{stk}.csv"))
      dt$time_id
    })))
    updateSelectInput(session, "vol_time_id", choices = sort(all_ids))
  })
  
  comp_vol <- function(x) sqrt(sum(x^2))
  
  getRVData <- reactive({
    req(input$stocks_summary, input$vol_time_id)
    lapply(input$stocks_summary, function(stk) {
      dt <- fread(glue::glue("YOUR_PATH_TO/individual_book_train/stock_{stk}.csv"))
      dt <- dt[time_id == input$vol_time_id]
      dt[, mid := (bid_price1 + ask_price1)/2]
      dt[, log_ret := c(NA, diff(log(mid)))]
      dt[, bucket := ceiling(seconds_in_bucket / 30)]
      out <- dt[, .(rv = comp_vol(na.omit(log_ret))), by = bucket]
      out$stock_id <- as.character(stk)
      out
    }) |> data.table::rbindlist()
  })
  
  output$vol_ts <- renderPlotly({
    df <- getRVData()
    req(nrow(df) > 0)
    plot_ly(df, x = ~bucket, y = ~rv, color = ~stock_id,
            type = "scatter", mode = "lines+markers") %>%
      layout(title = glue::glue(""),
             xaxis = list(title = "Time Bucket"),
             yaxis = list(
               title = "Realised Volatility",
               tickformat = ".4f"  # << correct placement
             )
      )
  })
  
  
  
  
  
  
  
  # — Reactive subset for the selected stock (wide form)
  # -- Reactive: Calculate live S-scores based on alpha and selected stock --
  rec_data <- reactive({
    req(input$stockRE, input$alpha)
    
    key <- paste0("stock_", input$stockRE)
    row <- all_scores[stock_id == key]
    alpha <- input$alpha
    
    # Calculate S-scores dynamically
    S_lightgbm <- alpha * row$R2_lightgbm + (1 - alpha) * (1 / (1 + row$QLIKE_lightgbm))
    S_gam <- alpha * row$R2_gam + (1 - alpha) * (1 / (1 + row$QLIKE_gam))
    S_har_base <- alpha * row$R2_har_base + (1 - alpha) * (1 / (1 + row$QLIKE_har_base))
    S_har_stepwise <- alpha * row$R2_har_stepwise + (1 - alpha) * (1 / (1 + row$QLIKE_har_stepwise))
    
    # Determine best model
    scores <- c(S_lightgbm, S_gam, S_har_base, S_har_stepwise)
    model_names <- c("LightGBM", "GAM", "HAR (baseline)", "HAR (stepwise)")
    
    data.table(
      S_lightgbm = S_lightgbm,
      S_gam = S_gam,
      S_har_base = S_har_base,
      S_har_stepwise = S_har_stepwise,
      Recommendation = model_names[which.max(scores)]
    )
  })
  
  # Right in your server, alongside the other outputs:
  output$debug_rec_data <- renderPrint({
    req(input$stockRE)
    # show what input$stockRE actually is
    cat("input$stockRE = ", input$stockRE, "\n\n")
    # then show the subset you’re getting
    print(all_scores[stock_id == input$stockRE])
    
    print(head(all_scores))
  })
  
  
  # — Render the “Recommendation” text
  output$best_model <- renderUI({
    req(rec_data())
    rec <- rec_data()$Recommendation
    tags$div(
      "The recommended model for the selected stock is ",
      strong(rec)
    )
  })
  
  output$s_plot <- renderPlot({
    req(rec_data())
    
    # Step 1: Extract and rename columns
    dt <- rec_data()[, .(S_lightgbm, S_gam, S_har_base, S_har_stepwise)]
    colnames(dt) <- c("LightGBM", "GAM", "HAR (baseline)", "HAR (stepwise)")
    
    # Step 2: Reshape
    df_long <- data.table::melt(
      dt,
      measure.vars = names(dt),
      variable.name = "Model",
      value.name = "S_score"
    )
    
    # Step 3: Identify best model
    best_model <- df_long$Model[which.max(df_long$S_score)]
    
    # Step 4: Set colors
    df_long$fill_color <- ifelse(df_long$Model == best_model, "limegreen", "black")
    
    # Step 5: Plot (no geom_text)
    ggplot(df_long, aes(x = Model, y = S_score, fill = fill_color)) +
      geom_col(width = 0.6, color = "black") +
      scale_fill_identity() +
      labs(
        title = "Model S-score Comparison",
        x = NULL,
        y = "S-score"
      ) +
      ylim(0, 1.05) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 13),
        panel.grid.major.x = element_blank()
      )
  })
  
  
  
  
  # — Render a small table of just the S‐score columns
  output$s_table <- renderDT({
    req(rec_data())
    # select only the S_ columns
    dt <- rec_data()[, .(S_lightgbm, S_gam, S_har_base, S_har_stepwise)]
    # rename for display
    setnames(dt,
             c("S_lightgbm", "S_gam", "S_har_base", "S_har_stepwise"),
             c("LightGBM",   "GAM",   "HAR (baseline)", "HAR (stepwise)"))
    datatable(round(dt, 4), rownames = FALSE, options = list(dom = "t"))
  })
  
  #how hard it is to predict a stock meter
  output$predictability_meter_gauge <- renderGauge({
    req(input$stockRE)
    
    alpha <- 0.5
    stock_lookup <- paste0("stock_", input$stockRE)
    
    # Filter to the selected stock
    row <- all_scores[stock_id == stock_lookup]
    
    if (nrow(row) == 0) return(NULL)  # fail-safe
    
    # Extract QLIKE and R2 for each model
    qlikes <- as.numeric(unlist(row[, .(QLIKE_lightgbm, QLIKE_gam, QLIKE_har_base, QLIKE_har_stepwise)]))
    r2s    <- as.numeric(unlist(row[, .(R2_lightgbm, R2_gam, R2_har_base, R2_har_stepwise)]))
    
    # Convert QLIKE to accuracy
    a_qlike <- 1 / (1 + qlikes)
    a_r2    <- r2s  # already in [0,1], may be negative (keep as-is)
    
    # Compute S-scores at alpha = 0.5
    s_scores <- alpha * a_r2 + (1 - alpha) * a_qlike
    predictability <- mean(s_scores)
    
    # Render gauge
    gauge(
      value = round(predictability, 3),
      min = 0,
      max = 1,
      label = "Predictability",
      symbol = "",
      sectors = gaugeSectors(
        success = c(0.75, 1),
        warning = c(0.5, 0.75),
        danger  = c(0, 0.5)
      )
    )
  })
  
  
  
  # Baseline HAR-RV logic ------------------------------------------------------
  
  
  har_base_results <- reactive({
    req(input$stockSE_har_base)
    # 1) load & preprocess
    stock <- fread(fs::path("YOUR_PATH_TO/individual_book_train", glue("stock_{input$stockSE_har_base}.csv")))
    
    
    stock$WAP <- (stock$bid_price1 * stock$ask_size1 + stock$ask_price1 * stock$bid_size1) / (stock$bid_size1 + stock$ask_size1)
    
    stock$BidAskSpread <- (stock$ask_price1 / stock$bid_price1) - 1
    
    #-------- Compute log returns for each second of each time ID --------#
    log_r1 <- list()
    time_ID <- unique(stock$time_id)
    time_IDs <- head(time_ID, 500)
    for (j in 1 : length(time_IDs)) {
      sec <- stock %>% filter(time_id == time_IDs[j]) %>% pull(seconds_in_bucket)
      price <- stock %>% filter(time_id == time_IDs[j]) %>% pull(WAP)
      log_r <- log(price[-1] / price[1:(length(price) - 1)])
      log_r1[[j]] <- data.frame(time = sec[-1], log_return = log_r)
      time.no.change <- (1:600)[!(1:600 %in% log_r1[[j]]$time)]
      if (length(time.no.change) > 0) {
        new.df <- data.frame(time = time.no.change, log_return = 0)
        log_r1[[j]] <- rbind(log_r1[[j]], new.df)
        log_r1[[j]] <- log_r1[[j]][order(log_r1[[j]]$time), ]
      }
    }
    
    #------ Create 30 second time buckets and compute volatility and other stats -------#
    comp_vol <- function(x) {
      return(sqrt(sum(x ^ 2)))
    }
    
    vol <- list()
    for (k in 1 : length(log_r1)) {
      log_r1[[k]] <- log_r1[[k]] %>% mutate(time_bucket = ceiling(time / 30))
      vol[[k]] <- aggregate(log_return ~ time_bucket, data = log_r1[[k]], FUN = comp_vol)
      colnames(vol[[k]]) <- c('time_bucket', 'volatility')
    }
    
    n   <- length(vol)
    df.simple <- data.frame(
      realised = rep(NA_real_, n),
      matrix(NA_real_, nrow = n, ncol = 19,
             dimnames = list(NULL, paste0("vol", 1:19)))
    )
    
    for (l in 1:length(vol)) {
      df.simple$realised[l] <- vol[[l]]$volatility[20]
      
      for (m in 1:19) {
        df.simple[l, paste0("vol", m)] <-
          mean(vol[[l]]$volatility[(20 - m):19])
      }
    }
    
    
    #------ Set train and test indexes -------#
    train_indices <- sample(1:nrow(df.simple), size = 0.8 * nrow(df.simple))
    train_data <- df.simple[train_indices, ]
    test_data <- df.simple[-train_indices, ]
    
    actuals <- test_data$realised
    
    
    base.model <- lm(realised ~ vol19 + vol5 + vol1, train_data)
    
    
    #----- Compute stepwise QLIKE and test set R squared -----#
    predictions <- predict(base.model, newdata = test_data)
    qlike_vals <- (actuals / predictions) - log(actuals / predictions) - 1
    
    SSR <- sum((actuals - predictions)^2)
    SST <- sum((actuals - mean(actuals))^2)
    
    R2_test <- 1 - SSR/SST
    
    
    # Compute median QLIKE
    med_qlike <- median(qlike_vals)
    
    list(
      qlike      = qlike_vals,      # numeric vector
      med_qlike  = median(qlike_vals),
      R2         = R2_test,
      actuals    = actuals,
      preds      = predictions
    )
  })
  
  # hook up your new outputs
  output$har_base_med_qlike <- renderText({
    sprintf("%.4f", har_base_results()$med_qlike)
  })
  output$har_base_r2_test <- renderText({
    sprintf("%.4f", har_base_results()$R2)
  })
  output$har_base_qlike_box <- renderPlotly({
    res <- har_base_results()
    plot_ly(x = res$qlike, type = 'box', orientation = 'h', showlegend = FALSE) %>%
      layout(
        xaxis = list(title = 'QLIKE'),
        yaxis = list(showticklabels = FALSE)
      )
  })
  output$har_base_pred_scatter <- renderPlotly({
    res <- har_base_results()
    rng <- range(c(res$actuals, res$preds), na.rm = TRUE)
    plot_ly(x = res$actuals, y = res$preds, type = 'scatter', mode = 'markers') %>%
      add_lines(x = rng, y = rng, line = list(dash = 'dot')) %>%
      layout(xaxis = list(title = 'Actual Volatility'),
             yaxis = list(title = 'Predicted Volatility'))
  })
  
  output$har_base_s_boxplot <- renderPlotly({
    req(input$stockSE_har_base)
    
    dt <- s_scores_all[, .(stock_id, S = S_har_base)]
    dt[, selected := ifelse(stock_id == paste0("stock_", input$stockSE_har_base), "Selected", "Other")]
    
    # Filter to valid scores only
    dt <- dt[S >= 0 & S <= 1]
    
    plot_ly(
      data = dt,
      x = ~S,
      type = "box",
      name = "S-scores",
      orientation = "h",
      boxpoints = "all",
      jitter = 0.3,
      pointpos = 0,
      color = ~selected,
      colors = c("gray", "red"),
      marker = list(size = 7),
      showlegend = FALSE
    ) %>%
      layout(
        title = "HAR (baseline) S-scores Across Stocks",
        xaxis = list(title = "S-score"),
        yaxis = list(title = "", showticklabels = FALSE),
        annotations = list(
          list(
            x = dt[selected == "Selected", S],
            y = 0.5,
            text = paste("Stock:", input$stockSE_har_base),
            showarrow = FALSE,
            font = list(color = "red", size = 12)
          )
        )
      )
  })
  
  output$har_base_s_interpretation <- renderUI({
    req(input$stockSE_har_base)
    
    dt <- s_scores_all[, .(stock_id, S = S_har_base)]
    dt <- dt[S >= 0 & S <= 1]
    dt[, percentile := ecdf(S)(S)]
    
    selected_id <- paste0("stock_", input$stockSE_har_base)
    selected_row <- dt[stock_id == selected_id]
    
    pct_val <- selected_row$percentile
    pct <- round(pct_val * 100)
    
    HTML(paste0("Percentile Rank: ", pct, "%"))
  })
  
  
  
  
  # Stepwise HAR-RV logic ------------------------------------------------------
  model_results <- reactive({
    req(input$stockSE_model)
    # load and preprocess
    stock <- fread(fs::path('YOUR_PATH_TO/individual_book_train', glue('stock_{input$stockSE_model}.csv')))
    
    
    stock$WAP <- (stock$bid_price1 * stock$ask_size1 + stock$ask_price1 * stock$bid_size1) / (stock$bid_size1 + stock$ask_size1)
    
    stock$BidAskSpread <- (stock$ask_price1 / stock$bid_price1) - 1
    
    #-------- Compute log returns for each second of each time ID --------#
    log_r1 <- list()
    time_ID <- unique(stock$time_id)
    time_IDs <- head(time_ID, 500)
    for (j in 1 : length(time_IDs)) {
      sec <- stock %>% filter(time_id == time_IDs[j]) %>% pull(seconds_in_bucket)
      price <- stock %>% filter(time_id == time_IDs[j]) %>% pull(WAP)
      log_r <- log(price[-1] / price[1:(length(price) - 1)])
      log_r1[[j]] <- data.frame(time = sec[-1], log_return = log_r)
      time.no.change <- (1:600)[!(1:600 %in% log_r1[[j]]$time)]
      if (length(time.no.change) > 0) {
        new.df <- data.frame(time = time.no.change, log_return = 0)
        log_r1[[j]] <- rbind(log_r1[[j]], new.df)
        log_r1[[j]] <- log_r1[[j]][order(log_r1[[j]]$time), ]
      }
    }
    
    #------ Create 30 second time buckets and compute volatility and other stats -------#
    comp_vol <- function(x) {
      return(sqrt(sum(x ^ 2)))
    }
    
    vol <- list()
    for (k in 1 : length(log_r1)) {
      log_r1[[k]] <- log_r1[[k]] %>% mutate(time_bucket = ceiling(time / 30))
      vol[[k]] <- aggregate(log_return ~ time_bucket, data = log_r1[[k]], FUN = comp_vol)
      colnames(vol[[k]]) <- c('time_bucket', 'volatility')
    }
    
    n   <- length(vol)
    df.simple <- data.frame(
      realised = rep(NA_real_, n),
      matrix(NA_real_, nrow = n, ncol = 19,
             dimnames = list(NULL, paste0("vol", 1:19)))
    )
    
    for (l in 1:length(vol)) {
      df.simple$realised[l] <- vol[[l]]$volatility[20]
      
      for (m in 1:19) {
        df.simple[l, paste0("vol", m)] <-
          mean(vol[[l]]$volatility[(20 - m):19])
      }
    }
    set.seed(3888)
    
    #------ Set train and test indexes -------#
    train_indices <- sample(1:nrow(df.simple), size = 0.8 * nrow(df.simple))
    train_data <- df.simple[train_indices, ]
    test_data <- df.simple[-train_indices, ]
    
    actuals <- test_data$realised
    
    
    
    #------ Train the full model -------#
    full.formula <- as.formula(
      paste("realised ~", paste(paste0("vol", 1:19), collapse = " + "))
    )
    full.model <- lm(full.formula, data = train_data)
    
    
    #----- Train the stepwise model -----#
    step.model <- stepAIC(full.model,
                          direction = "both",
                          trace     = FALSE,
                          k = log(nrow(train_data)))
    
    
    #----- Compute stepwise QLIKE and test set R squared -----#
    predictions <- predict(step.model, newdata = test_data)
    qlike_vals <- (actuals / predictions) - log(actuals / predictions) - 1
    
    SSR <- sum((actuals - predictions)^2)
    SST <- sum((actuals - mean(actuals))^2)
    
    R2_test_stepwise <- 1 - SSR/SST
    
    
    # Compute median QLIKE
    med_qlike <- median(qlike_vals)
    
    list(qlike=qlike_vals, med_qlike=median(qlike_vals), R2=R2_test_stepwise, actuals=actuals, preds=predictions)
  })
  output$qlike_box <- renderPlotly({
    res <- model_results()
    plot_ly(
      x           = res$qlike, 
      type        = 'box', 
      orientation = 'h', 
      showlegend  = FALSE
    ) %>%
      layout(
        xaxis = list(title = 'QLIKE'), 
        yaxis = list(showticklabels = FALSE, title = '')
      )
  })
  output$med_qlike <- renderText({ sprintf('%.4f', model_results()$med_qlike) })
  output$r2_test   <- renderText({ sprintf('%.4f', model_results()$R2) })
  output$pred_scatter <- renderPlotly({
    res <- model_results()
    rng <- range(c(res$actuals, res$preds), na.rm=TRUE)
    plot_ly(x=res$actuals, y=res$preds, type='scatter', mode='markers') %>%
      add_lines(x=rng, y=rng, line=list(dash='dot')) %>%
      layout(xaxis=list(title='Actual Volatility'), yaxis=list(title='Predicted Volatility', showlegend = FALSE))
  })
  #stepwise boxplot
  output$har_stepwise_s_boxplot <- renderPlotly({
    req(input$stockSE_model)
    
    dt <- s_scores_all[, .(stock_id, S = S_har_stepwise)]
    dt[, selected := ifelse(stock_id == paste0("stock_", input$stockSE_model), "Selected", "Other")]
    
    # Clean invalid values
    dt <- dt[S >= 0 & S <= 1]
    
    plot_ly(
      data = dt,
      x = ~S,
      type = "box",
      name = "S-scores",  # keeps points in one group
      orientation = "h",
      boxpoints = "all",
      jitter = 0.3,
      pointpos = 0,
      color = ~selected,
      colors = c("gray", "red"),
      marker = list(size = 7),
      showlegend = FALSE
    ) %>%
      layout(
        title = "Stepwise HAR-RV S-scores Across Stocks",
        xaxis = list(title = "S-score"),
        yaxis = list(title = "", showticklabels = FALSE),
        annotations = list(
          list(
            x = dt[selected == "Selected", S],
            y = 0.5,
            text = paste("Stock:", input$stockSE_model),
            showarrow = FALSE,
            font = list(color = "red", size = 12)
          )
        )
      )
  })
  output$har_stepwise_s_interpretation <- renderUI({
    req(input$stockSE_model)
    
    dt <- s_scores_all[, .(stock_id, S = S_har_stepwise)]
    dt <- dt[S >= 0 & S <= 1]
    dt[, percentile := ecdf(S)(S)]
    
    selected_id <- paste0("stock_", input$stockSE_model)
    selected_row <- dt[stock_id == selected_id]
    
    pct_val <- selected_row$percentile
    pct <- round(pct_val * 100)
    
    HTML(paste0("Percentile Rank: ", pct, "%"))
  })
  
  
  
  
  
  #-------------------------------------------------------------------------------
  
  # GAM LOGIC --------------------------------------------------------------------
  
  gam_results <- reactive({
    req(input$stockSE_gam)
    
    # 1) load & compute WAP / spread
    stk <- fread(fs::path("YOUR_PATH_TO/individual_book_train", glue("stock_{input$stockSE_gam}.csv"))) %>%
      mutate(
        WAP          = (bid_price1 * ask_size1 + ask_price1 * bid_size1) / (bid_size1 + ask_size1),
        BidAskSpread = ask_price1 / bid_price1 - 1
      )
    
    # 2) build 30s‐bucket log returns
    time_IDs <- unique(stk$time_id)[1:500]
    log_r    <- vector("list", length(time_IDs))
    
    for (i in seq_along(time_IDs)) {
      tmp <- stk %>% 
        filter(time_id == time_IDs[i]) %>% 
        arrange(seconds_in_bucket)
      
      lr_df <- tibble(
        time       = tmp$seconds_in_bucket[-1],
        log_return = log(tmp$WAP[-1] / tmp$WAP[-nrow(tmp)])
      )
      # fill missing times with zero-return
      full_t <- 1:600
      miss   <- setdiff(full_t, lr_df$time)
      if (length(miss)) {
        lr_df <- bind_rows(lr_df, tibble(time = miss, log_return = 0))
      }
      log_r[[i]] <- lr_df %>%
        arrange(time) %>%
        mutate(time_bucket = ceiling(time / 30))
    }
    
    # 3) aggregate to volatility per bucket
    comp_vol <- function(x) sqrt(sum(x^2))
    vol <- map(log_r, ~ .x %>% group_by(time_bucket) %>% 
                 summarise(volatility = comp_vol(log_return), .groups="drop"))
    
    # 4) build feature frame
    df.simple <- map2_df(vol, time_IDs, ~{
      v <- .x$volatility
      tibble(
        time_id  = .y,
        realised = v[20],
        vol19    = mean(v[1:19]),
        vol5     = mean(v[14:19]),
        vol1     = v[19]
      )
    })
    
    # 5) train/test split
    set.seed(2025)
    n         <- nrow(df.simple)
    train_idx <- sample(n, size = 0.8 * n)
    train     <- df.simple[train_idx, ]
    test      <- df.simple[-train_idx, ]
    
    # 6) fit the GAM
    gm <- gam(
      realised ~ s(vol19, k=10) + s(vol5, k=8) + s(vol1, k=5) + ti(vol5, vol1),
      data   = train,
      method = "REML"
    )
    
    # 7) make predictions & compute metrics
    preds <- predict(gm, newdata = test)
    rss   <- sum((test$realised - preds)^2)
    tss   <- sum((test$realised - mean(train$realised))^2)
    R2    <- 1 - rss/tss
    
    actuals <- test$realised
    qlike_pt <- actuals / preds - log(actuals / preds) - 1
    med_qlike <- median(qlike_pt, na.rm=TRUE)
    
    list(
      model      = gm,
      actuals    = actuals,
      preds      = preds,
      R2         = R2,
      qlike      = qlike_pt,
      med_qlike  = med_qlike
    )
  })
  
  # then hook up your outputs:
  
  output$gam_med_qlike <- renderText({
    sprintf("%.4f", gam_results()$med_qlike)
  })
  output$gam_r2_test <- renderText({
    sprintf("%.4f", gam_results()$R2)
  })
  output$gam_qlike_box <- renderPlotly({
    res <- gam_results()
    plot_ly(x = res$qlike, type='box', orientation='h', showlegend=FALSE) %>%
      layout(
        xaxis = list(title='QLIKE'),
        yaxis = list(showticklabels=FALSE)
      )
  })
  output$gam_pred_scatter <- renderPlotly({
    res <- gam_results()
    rng <- range(c(res$actuals, res$preds), na.rm=TRUE)
    plot_ly(x=res$actuals, y=res$preds, mode='markers', type='scatter') %>%
      add_lines(x=rng, y=rng, line=list(dash='dot')) %>%
      layout(
        xaxis = list(title='Actual Volatility'),
        yaxis = list(title='Predicted Volatility')
      )
  })
  
  output$gam_s_boxplot <- renderPlotly({
    req(input$stockSE_gam)
    
    dt <- s_scores_all[, .(stock_id, S = S_gam)]
    dt[, selected := ifelse(stock_id == paste0("stock_", input$stockSE_gam), "Selected", "Other")]
    
    # Filter to only valid [0, 1] scores
    dt <- dt[S >= 0 & S <= 1]
    
    plot_ly(
      data = dt,
      x = ~S,
      type = "box",
      name = "S-scores",
      boxpoints = "all",
      orientation = "h",
      jitter = 0.3,
      pointpos = 0,
      color = ~selected,
      colors = c("gray", "red"),
      marker = list(size = 7),
      showlegend = FALSE
    ) %>%
      layout(
        title = "GAM S-scores Across Stocks",
        xaxis = list(title = "S-score"),
        yaxis = list(title = "", showticklabels = FALSE),
        annotations = list(
          list(
            x = dt[selected == "Selected", S],
            y = 0.5,
            text = paste("Stock:", input$stockSE_gam),
            showarrow = FALSE,
            font = list(color = "red", size = 12)
          )
        )
      )
  })
  
  output$gam_s_interpretation <- renderUI({
    req(input$stockSE_gam)
    
    dt <- s_scores_all[, .(stock_id, S = S_gam)]
    dt <- dt[S >= 0 & S <= 1]
    dt[, percentile := ecdf(S)(S)]
    
    selected_id <- paste0("stock_", input$stockSE_gam)
    selected_row <- dt[stock_id == selected_id]
    
    pct_val <- selected_row$percentile
    pct <- round(pct_val * 100)
    
    HTML(paste0("Percentile Rank: ", pct, "%"))
  })
  
  
  
  # LightGBM LOGIC ---------------------------------------------------------------
  
  wap_bas = function(stock) {
    stock$WAP = (stock$bid_price1 * stock$ask_size1 + stock$ask_price1 * stock$bid_size1) /
      (stock$bid_size1 + stock$ask_size1)
    stock$BidAskSpread = (stock$ask_price1 / stock$bid_price1) - 1
    return(stock)
  }
  
  
  log_returns = function(stock, max_time_ids = 500) {
    log_r_list = list()
    time_IDs = unique(stock$time_id)[1:max_time_ids]
    
    for (i in seq_along(time_IDs)) {
      sec = stock |>  filter(time_id == time_IDs[i]) |>  pull(seconds_in_bucket)
      price = stock |>  filter(time_id == time_IDs[i]) |>  pull(WAP)
      
      log_r = log(price[-1] / price[1:(length(price) - 1)])
      log_r_list[[i]] = data.frame(time = sec[-1], log_return = log_r)
      missing_time = setdiff(1:600, log_r_list[[i]]$time)
      
      if (length(missing_time) > 0) {
        fill = data.frame(time = missing_time, log_return = 0)
        log_r_list[[i]] = rbind(log_r_list[[i]], fill)
        log_r_list[[i]] = log_r_list[[i]][order(log_r_list[[i]]$time), ]
      }
    }
    return(log_r_list)
  }
  
  compute_volatility = function(log_r_list) {
    vol_list = list()
    comp_vol = function(x) sqrt(sum(x^2))
    for (i in seq_along(log_r_list)) {
      temp = log_r_list[[i]] |>  mutate(time_bucket = ceiling(time / 30))
      vol_df = aggregate(log_return ~ time_bucket, data = temp, FUN = comp_vol)
      colnames(vol_df) = c('time_bucket', 'volatility')
      vol_list[[i]] = vol_df
    }
    return(vol_list)
  }
  
  ## Feature Engineering
  simple_df = function(stock, vol_list) {
    n = length(vol_list)
    df = data.frame(realised = rep(NA, n), wap_mean = rep(NA, n), bas_mean = rep(NA, n))
    
    for (j in 1:19) {
      df[[paste0("vol", j)]] = rep(NA, n)
    }
    time_IDs = unique(stock$time_id)[1:n]
    
    for (i in 1:n) {
      vol_vals = vol_list[[i]]$volatility
      if (length(vol_vals) < 20) next
      
      df$realised[i] = vol_vals[20]
      
      for (j in 1:19) {
        if (j == 1) {
          df[i, paste0("vol", j)] = vol_vals[19]
        } else {
          df[i, paste0("vol", j)] <- mean(vol_vals[(20 - j):(19)])
        }
      }
      
      stock_temp = stock |>  filter(time_id == time_IDs[i])
      df$wap_mean[i] = mean(stock_temp$WAP, na.rm = TRUE)
      df$bas_mean[i] = mean(stock_temp$BidAskSpread, na.rm = TRUE)
    }
    return(na.omit(df))
  }
  
  
  volatility_prediction = function(stock_path, max_features = 3, alpha = 0.5) {
    stock = read_csv(stock_path) |>  wap_bas()
    log_r_list = log_returns(stock)
    vol_list = compute_volatility(log_r_list)
    df = simple_df(stock, vol_list)
    
    set.seed(3888)
    train_idx = sample(1:nrow(df), 0.8 * nrow(df))
    train_df = df[train_idx, ]
    test_df = df[-train_idx, ]
    x_train = train_df[, setdiff(names(train_df), "realised")]
    y_train = train_df$realised
    x_test = test_df[, setdiff(names(test_df), "realised")]
    y_test = test_df$realised
    
    model_tmp = lgb.train(
      params = list(objective = "regression", metric = "rmse", verbose = -1),
      data = lgb.Dataset(data = as.matrix(x_train), label = y_train),
      nrounds = 30
    )
    
    shap_values = predict(model_tmp, data.matrix(x_train), type = 'contrib')
    shap_df = as.data.frame(shap_values[, -ncol(shap_values), drop = FALSE])
    colnames(shap_df) = colnames(x_train)
    mean_shap = colMeans(abs(shap_df))
    ordered_shap = sort(mean_shap, decreasing = TRUE)
    top_features = names(ordered_shap)[1:min(length(ordered_shap), max_features)]
    
    train_missing = setdiff(top_features, colnames(x_train))
    for (m in train_missing) x_train[[m]] = 0
    x_train_sel = x_train[, top_features, drop = FALSE]
    
    model_sel = lgb.train(
      params = list(objective = "regression", metric = "rmse", verbose = -1),
      data = lgb.Dataset(data = data.matrix(x_train_sel), label = y_train),
      nrounds = 50
    )
    
    valid_features = intersect(top_features, colnames(x_test))
    if (length(valid_features) < length(top_features)) {
      missing = setdiff(top_features, valid_features)
      for (m in missing) x_test[[m]] = 0
      valid_features = top_features
    }
    
    preds = predict(model_sel, as.matrix(x_test[, valid_features, drop = FALSE]))
    valid_idx = which(preds > 0 & y_test > 0 & is.finite(preds) & is.finite(y_test))
    preds = preds[valid_idx]
    y_test = y_test[valid_idx]
    
    qlike = (y_test / preds) - log(y_test / preds) - 1
    median_qlike = median(qlike, na.rm = TRUE)
    
    ss_total = sum((y_test - mean(y_test))^2)
    ss_resid = sum((y_test - preds)^2)
    r_squared = 1 - (ss_resid / ss_total)
    
    
    return(list(model = model_sel,
                top_features = top_features,
                actuals = y_test,
                preds = preds,
                r_squared = r_squared,
                qlike_vals = qlike,
                median_qlike = median_qlike))
  }
  
  
  
  
  
  
  lightgbm_results <- reactive({
    req(input$stockSE_lgbm)
    
    csv_file <- as.character(fs::path("YOUR_PATH_TO/individual_book_train", glue("stock_{input$stockSE_lgbm}.csv")))
    
    # pass that string in
    lgbm <- volatility_prediction(csv_file)
    
    list(qlike=lgbm$qlike_vals, med_qlike=median(lgbm$qlike_vals), R2=lgbm$r_squared, actuals=lgbm$actuals, preds=lgbm$preds, model=lgbm$model)
    
  })
  
  
  
  output$lgbm_med_qlike <- renderText({
    sprintf("%.4f", lightgbm_results()$med_qlike)
  })
  
  # 2. Test R²
  output$lgbm_r2_test <- renderText({
    sprintf("%.4f", lightgbm_results()$R2)
  })
  
  # 3. QLIKE distribution boxplot
  output$lgbm_qlike_box <- renderPlotly({
    res <- lightgbm_results()
    plot_ly(
      x           = res$qlike,
      type        = 'box',
      orientation = 'h',
      showlegend  = FALSE
    ) %>%
      layout(
        xaxis = list(title = 'QLIKE'),
        yaxis = list(showticklabels = FALSE)
      )
  })
  
  # 4. Predicted vs Actual scatter
  output$lgbm_pred_scatter <- renderPlotly({
    res <- lightgbm_results()
    rng <- range(c(res$actuals, res$preds), na.rm = TRUE)
    
    plot_ly(
      x    = res$actuals,
      y    = res$preds,
      type = 'scatter',
      mode = 'markers'
    ) %>%
      add_lines(x = rng, y = rng, line = list(dash = 'dot')) %>%
      layout(
        xaxis = list(title = 'Actual Volatility'),
        yaxis = list(title = 'Predicted Volatility')
      )
  })
  
  output$lgbm_model_summary <- renderPrint({
    # make sure the reactive has run
    res <- lightgbm_results()
    # print out the model object:
    # you can swap this for summary(res$model) or str(res$model)
    print(res$model)
  })
  
  # LightGBM S-score boxplot
  output$lgbm_s_boxplot <- renderPlotly({
    req(input$stockSE_lgbm)
    
    dt <- s_scores_all[, .(stock_id, S = S_lightgbm)]
    dt[, selected := ifelse(stock_id == paste0("stock_", input$stockSE_lgbm), "Selected", "Other")]
    
    # Filter to only valid [0, 1] scores
    dt <- dt[S >= 0 & S <= 1]
    
    plot_ly(
      data = dt,
      x = ~S,
      type = "box",
      name = "S-scores",
      orientation = "h",
      boxpoints = "all",
      jitter = 0.3,
      pointpos = 0,
      color = ~selected,
      colors = c("gray", "red"),
      marker = list(size = 7),
      showlegend = FALSE
    ) %>%
      layout(
        title = "LightGBM S-scores Across Stocks",
        xaxis = list(title = "S-score"),
        yaxis = list(title = "", showticklabels = FALSE),
        annotations = list(
          list(
            x = dt[selected == "Selected", S],
            y = 0.5,
            text = paste("Stock:", input$stockSE_lgbm),
            showarrow = FALSE,
            font = list(color = "red", size = 12)
          )
        )
      )
  })
  
  output$lgbm_s_interpretation <- renderUI({
    req(input$stockSE_lgbm)
    
    dt <- s_scores_all[, .(stock_id, S = S_lightgbm)]
    dt <- dt[S >= 0 & S <= 1]
    dt[, percentile := ecdf(S)(S)]
    
    selected_id <- paste0("stock_", input$stockSE_lgbm)
    selected_row <- dt[stock_id == selected_id]
    
    pct_val <- selected_row$percentile
    pct <- round(pct_val * 100)
    
    HTML(paste0("Percentile Rank: ", pct, "%"))
  })
  
  
  
  
  
}


shinyApp(ui, server)
