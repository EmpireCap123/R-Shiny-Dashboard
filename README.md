# R-Shiny-Dashboard
library(DT)
library(leaflet)
library(shiny)
library(shinydashboard)
library(snakecase)

source(file = "./globals.R")

dashboardPage(
  dashboardHeader(
    title = "Quote Close Dashboard",
    titleWidth = "15%",
    dropdownActionMenu(
      title = "Download Data",
      icon = icon("download"),
      tags$li(
        class = "dropdown",
        downloadButton("downloadData", "Download Full Dataset")
      ),
      tags$li(
        class = "dropdown",
        downloadButton("downloadFilteredData", "Download Filtered Dataset")
      )
    )
  ),
  dashboardSidebar(
    width = "15%",
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(src = "accordion.js")
    ),
    sidebarMenu(
      id = "sidebarmenu",
      br(),
      textOutput("timeStamp"),
      tags$hr(),
      h4(HTML("&nbsp"), "Dashboard Views"),
      menuItem(
        "State Activity",
        tabName = "stateActivity",
        icon = icon("map")
      ),
      menuItem(
        "Monthly Activity",
        tabName = "monthlyActivity",
        icon = icon("calendar")
      ),
      menuItem(
        "Total Sales",
        tabName = "totalSales",
        icon = icon("usd")
      ),
      menuItem(
        "Quotes Per Day",
        tabName = "quotesPerDay",
        icon = icon("commenting")
      ),
      menuItem(
        "Agency Performance",
        tabName = "agencyPerformance",
        icon = icon("users")
      ),
      menuItem(
        "FAQ",
        tabName = "faq",
        icon = icon("question")
      ),
      tags$hr(),
      h4(HTML("&nbsp"), "Filters"),
      actionButton(
        inputId = "go",
        label = "Apply Selected Filters",
        width = "90%"
      ),
      sidebarMenuOutput("filterMenu")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "stateActivity",
        fluidRow(
          box(
            title = "BoP Activity by State",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("bopActivityByStateGraph")
          ),
          box(
            title = "BoP Activity by State",
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("bopActivityByStateTable")
          )
        ),
        fluidRow(
          box(
            title = "Auto Activity by State",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("bapActivityByStateGraph")
          ),
          box(
            title = "Auto Activity by State",
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("bapActivityByStateTable")
          )
        ),
        fluidRow(
          box(
            title = "Total Activity by State",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("totalActivityByStateGraph")
          ),
          box(
            title = "Total Activity by State",
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("totalActivityByStateTable")
          )
        )
      ),
      tabItem(
        tabName = "monthlyActivity",
        fluidRow(
          box(
            title = "BoP Activity by Submission Month",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("bopActivityByMonthGraph")
          ),
          box(
            title = "BoP Activity by Submission Month",
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("bopActivityByMonthTable")
          )
        ),
        fluidRow(
          box(
            title = "Auto Activity by Submission Month",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("bapActivityByMonthGraph")
          ),
          box(
            title = "Auto Activity by Submission Month",
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("bapActivityByMonthTable")
          )
        ),
        fluidRow(
          box(
            title = "Total Activity by Submission Month",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("totalActivityByMonthGraph")
          ),
          box(
            title = "Total Activity by Submission Month",
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("totalActivityByMonthTable")
          )
        )
      ),
      tabItem(
        tabName = "totalSales",
        fluidRow(
          box(
            title = "Connect Sales by State",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("salesByStateGraph")
          ),
          box(
            title = "Connect Sales by State",
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("salesByStateTable")
          )
        ),
        fluidRow(
          box(
            title = "Connect Sales by Bind Month",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("salesByMonthGraph")
          ),
          box(
            title = "Connect Sales by Bind Month",
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("salesByMonthTable")
          )
        )
      ),
      tabItem(
        tabName = "quotesPerDay",
        fluidRow(
          box(
            title = "Quotes by Day",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("dailyQuotesGraph")
          ),
          box(
            title = "Quotes by Day",
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("dailyQuotesTable")
          )
        )
      ),
      tabItem(
        tabName = "agencyPerformance",
        fluidRow(
          box(
            title = "Agency Performance",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            DT::dataTableOutput("agencyPerformanceTable")
          )
        )
      ),
      tabItem(
        tabName = "faq",
        fluidRow(column(9, includeMarkdown("./www/faq.md")))
      )
    )
  )
)
