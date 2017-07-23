
run_dashboard <- function(){
  #' dashboard
  #'
  #' @import shiny
  #' @import shinydashboard
  #' @import dplyr
  #' @import plotly
  #'
  #' @export
  fund.return <- calc_return_series("fund")
  contract.return <- calc_return_series("ticker")

  min.date <- min(fund.return$Date)
  max.date <- max(fund.return$Date)

  ui <- dashboardPage(
    dashboardHeader(title = "Shawn Lin"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
        menuItem("Charts", tabName = "charts", icon = icon("th")),
        dateRangeInput(inputId = "Date", label = "Data Range",
                       start = min.date, end = max.date,
                       min = min.date, max = max.date, format = "yyyy-mm-dd")
        )
      ),

    dashboardBody(
      tabItems(
        tabItem(
          tabName = "summary",
          fluidRow(
            column(
              width = 8,
              box(title = "Fund Return vs Benchmark", status = "primary", solidHeader = TRUE,
                  plotlyOutput("plot.fund.return"), width = 12),
              box(title = "Summary Statistics",
                  selectInput("summary", "Summary Type:",
                              c("Performance" = "perf",
                                "CAPM" = "capm",
                                "Drawdown" = "dd")),
                  dataTableOutput("summary"), width = 12)
            ),
            box(title = "Fund Return Data",
                dataTableOutput("fund.data"), width =4)
          )
        ),

        tabItem(
          tabName = "charts",
          fluidRow(
            box(title = "Return by Contract",
                plotlyOutput("plot.contract.return"), width = 8),
            box(title = "Contract Allocation",
                plotlyOutput("plot.contract.allocation"), width = 4)
          ),
          fluidRow(
            box(title = "Fund NAV",
                plotlyOutput("plot.fund.nav"))
          )
        )
      )
    )
  )

  server <- function(input, output) {
    fund.data <- reactive(
      fund.return %>% filter(Date >= input$Date[1] & Date <= input$Date[2])
    )
    contract.data <- reactive(
      contract.return %>% filter(Date >= input$Date[1] & Date <= input$Date[2])
    )

    output$fund.data <- renderDataTable(
      fund.data() %>%
        merge(., calc_total_return(., "fund"), by = c("Date", "Strategy")) %>%
        merge(calc_NAV(contract.data()), by = c("Date")) %>%
        filter(Strategy == "Fund") %>%
        mutate(Return = scales::percent(as.numeric(Return)),
               Total.Return = scales::percent(as.numeric(Total.Return))) %>%
        select(Date, Return, Total.Return, Fund.NAV)
    )

    summary.type <- reactive(input$summary)

    output$summary <- renderDataTable(
      if (summary.type() == "perf") {
        result <- calc_summary(fund.data(), "quick")
        result$Stat <- row.names(result)
        return(result %>% select(Stat, everything()))
      } else if (summary.type() == "capm") {
        result <- calc_summary(fund.data(), "CAPM")
        result$Stat <- row.names(result)
        return(result %>% select(Stat, everything()) %>% as.data.frame())
      } else if (summary.type() == "dd") {
        calc_summary(fund.data(), "Drawdown")
      }
    )

    output$plot.fund.return <- renderPlotly(
      plot_vami(fund.data())
    )

    output$plot.contract.return <- renderPlotly(
      plot_asset_return(contract.data())
    )

    output$plot.contract.allocation <- renderPlotly(
      plot_allocation()
    )

    output$plot.fund.nav <- renderPlotly(
      plot_NAV(calc_NAV(contract.data()))
    )

  }

  shinyApp(ui, server)
}

