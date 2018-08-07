# R-Shiny-Dashboard
library(DT)
library(ggplot2)
library(leaflet)
library(lubridate)
library(reshape2)
library(shiny)
library(shinydashboard)
library(snakecase)

data <- readRDS(file = "data/quoteCloseData.RDS")

source(file = "./globals.R")

shinyServer(function(input, output, session) {

################################################################################
#
# Variables and UDFs
#
################################################################################

  lineOfBusinessChoices     <- unique(sort(data$lineOfBusiness))
  baseStateChoices          <- unique(sort(data$baseState))
  regionChoices             <- unique(sort(data$regionName))
  subStatusChoices          <- unique(sort(data$submissionStatus))
  subDayOfWeekChoices       <- levels(data$submissionDayOfWeek)
  agencyNameChoices         <- unique(sort(data$agencyName))
  clManagerChoices          <- unique(sort(data$clManager))
  clAccountExecChoices      <- unique(sort(data$clAccountExecutiveVerified))
  plTerritoryManagerChoices <- unique(sort(data$plTerritoryManagerVerified))
  programDescChoices        <- unique(sort(data$programDescription))
  naicsDescChoices          <- unique(sort(data$naicsDescription))
  channelChoices            <- unique(sort(data$Channel))

################################################################################
#
# Action Buttons and Labels
#
################################################################################

  output$downloadData <- downloadHandler(
    filename = paste0("quoteCloseData", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(data, file, row.names = FALSE)
    }
  )

  output$downloadFilteredData <- downloadHandler(
    filename = paste0("quoteCloseData-Filtered", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(filterData(), file, row.names = FALSE)
    }
  )

  output$timeStamp <- renderText({
    paste("Data Current as of", max(data$submissionDate))
  })

################################################################################
#
# Filters
#
################################################################################

  ##############################################################################
  #
  # UI Elements
  #
  ##############################################################################

  output$lineOfBusinessCheckbox <- renderUI({
    checkboxGroupInput(
      inputId = "lineOfBusiness",
      label = NULL,
      choices = lineOfBusinessChoices,
      selected = lineOfBusinessChoices
    )
  })
  outputOptions(output, "lineOfBusinessCheckbox", suspendWhenHidden = FALSE)

  output$baseStateCheckbox <- renderUI({
    tags$div(
      align = "left",
      class = "multicol",
      checkboxGroupInput(
        # inputId should match the column name from the base data set that it's
        # intended to filter.
        inputId = "baseState",
        label = NULL,
        choices = baseStateChoices,
        selected = baseStateChoices
      )
    )
  })
  outputOptions(output, "baseStateCheckbox", suspendWhenHidden = FALSE)

  output$submissionStatusCheckbox <- renderUI({
    checkboxGroupInput(
      inputId = "submissionStatus",
      label = NULL,
      choices = subStatusChoices,
      selected = subStatusChoices
    )
  })
  outputOptions(output, "submissionStatusCheckbox", suspendWhenHidden = FALSE)

  output$agencySelectize <- renderUI({
    selectizeInput(
      inputId = "agencyName",
      label = NULL,
      multiple = TRUE,
      choices = c("All", agencyNameChoices),
      selected = "All"
    )
  })
  outputOptions(output, "agencySelectize", suspendWhenHidden = FALSE)

  output$submissionDate <- renderUI({
    dateRangeInput(
      inputId = "submissionDate",
      label = NULL,
      start = max(data$submissionDate) %m-% years(1),
      end = max(data$submissionDate)
    )
  })
  outputOptions(output, "submissionDate", suspendWhenHidden = FALSE)

  output$subDayOfWeekCheckbox <- renderUI({
    checkboxGroupInput(
      inputId = "submissionDayOfWeek",
      label = NULL,
      choices = subDayOfWeekChoices,
      selected = subDayOfWeekChoices
    )
  })
  outputOptions(output, "subDayOfWeekCheckbox", suspendWhenHidden = FALSE)

  output$programDescCheckbox <- renderUI({
    checkboxGroupInput(
      inputId = "programDescription",
      label = NULL,
      choices = programDescChoices,
      selected = programDescChoices
    )
  })
  outputOptions(output, "programDescCheckbox", suspendWhenHidden = FALSE)

  output$naicsDescSelectize <- renderUI({
    selectizeInput(
      inputId = "naicsDescription",
      label = NULL,
      multiple = TRUE,
      choices = c("All", naicsDescChoices),
      selected = c("All")
    )
  })
  outputOptions(output, "naicsDescSelectize", suspendWhenHidden = FALSE)

  output$regionCheckbox <- renderUI({
    checkboxGroupInput(
      inputId = "regionName",
      label = NULL,
      choices = regionChoices,
      selected = regionChoices
    )
  })
  outputOptions(output, "regionCheckbox", suspendWhenHidden = FALSE)

  output$clManagerSelectize <- renderUI({
    selectizeInput(
      inputId = "clManager",
      label = NULL,
      multiple = TRUE,
      choices = c("All", clManagerChoices),
      selected = c("All")
    )
  })
  outputOptions(output, "clManagerSelectize", suspendWhenHidden = FALSE)

  output$clAESelectize <- renderUI({
    selectizeInput(
      inputId = "clAccountExecutive",
      label = NULL,
      multiple = TRUE,
      choices = c("All", clAccountExecChoices),
      selected = c("All")
    )
  })
  outputOptions(output, "clAESelectize", suspendWhenHidden = FALSE)

  output$plTMSelectize <- renderUI({
    selectizeInput(
      inputId = "plTerritoryManager",
      label = NULL,
      multiple = TRUE,
      choices = c("All", plTerritoryManagerChoices),
      selected = c("All")
    )
  })
  outputOptions(output, "plTMSelectize", suspendWhenHidden = FALSE)

  output$channelCheckbox <- renderUI({
    checkboxGroupInput(
      inputId = "channel",
      label = NULL,
      choices = channelChoices,
      selected = channelChoices
    )
  })
  outputOptions(output, "channelCheckbox", suspendWhenHidden = FALSE)

  ##############################################################################
  #
  # Filter Names and Behaviors
  #
  ##############################################################################

  menuOptions <- list(
    outputs = list(
      # To create a new filter menu item add a named entry for the desired UI
      # element to this list. List entry names should match the corresponding
      # inputId value from their respective UI elements.
      lineOfBusiness = sidebarMenu(
        menuItem(
          text = "Line of Business",
          icon = icon("briefcase"),
          uiOutput("lineOfBusinessCheckbox")
        )
      ),
      baseState = sidebarMenu(
        menuItem(
          text = "Base State",
          icon = icon("map"),
          actionButton(
            inputId = "selectStates",
            label = "Select / Unselect All States"
          ),
          uiOutput("baseStateCheckbox")
        )
      ),
      regionName = sidebarMenu(
        menuItem(
          text = "Region",
          icon = icon("globe"),
          uiOutput("regionCheckbox")
        )
      ),
      submissionStatus = sidebarMenu(
        menuItem(
          text = "Submission Status",
          icon = icon("thumbs-up"),
          actionButton(
            inputId = "selectSubStatus",
            label = "Select / Unselect All Statuses"
          ),
          uiOutput("submissionStatusCheckbox")
        )
      ),
      agencyName = sidebarMenu(
        menuItem(
          text = "Agency Name (Secondary)",
          icon = icon("users"),
          uiOutput("agencySelectize")
        )
      ),
      clManager = sidebarMenu(
        menuItem(
          text = "Commercial Lines Manager",
          icon = icon("user"),
          uiOutput("clManagerSelectize")
        )
      ),
      clAccountExecutive = sidebarMenu(
        menuItem(
          text = "Commercial Lines AE",
          icon = icon("user-circle-o"),
          uiOutput("clAESelectize")
        )
      ),
      plTerritoryManager = sidebarMenu(
        menuItem(
          text = "Personal Lines TM",
          icon = icon("user-circle-o"),
          uiOutput("plTMSelectize")
        )
      ),
      submissionDate = sidebarMenu(
        menuItem(
          text = "Submission Date",
          icon = icon("calendar"),
          uiOutput("submissionDate")
        )
      ),
      submissionDayOfWeek = sidebarMenu(
        menuItem(
          text = "Submission Day of Week",
          icon = icon("calendar-check-o"),
          uiOutput("subDayOfWeekCheckbox")
        )
      ),
      programDescription = sidebarMenu(
        menuItem(
          text = "Program Description",
          icon = icon("car"),
          uiOutput("programDescCheckbox")
        )
      ),
      naicsDescription = sidebarMenu(
        menuItem(
          text = "NAICS Description",
          icon = icon("building"),
          uiOutput("naicsDescSelectize")
        )
      ),
      channel = sidebarMenu(
        menuItem(
          text = "Channel",
          icon = icon("clipboard"),
          uiOutput("channelCheckbox")
        )
      )
    ),
    filters = list(
      # To define a new filter's behavior add a named entry for that filter to
      # this list. List entry names should match the corresponding inputId value
      # from their respective UI elements.
      lineOfBusiness = "data$lineOfBusiness %in% input$lineOfBusiness",
      baseState = "data$baseState %in% input$baseState",
      regionName = "data$regionName %in% input$regionName",
      submissionStatus = "data$submissionStatus %in% input$submissionStatus",
      agencyName = "data$agencyName %in% input$agencyName",
      clManager = "data$clManager %in% input$clManager",
      clAccountExecutive =
        "data$clAccountExecutiveVerified %in% input$clAccountExecutive",
      plTerritoryManager =
        "data$plTerritoryManagerVerified %in% input$plTerritoryManager",
      submissionDate = "data$submissionDate >= input$submissionDate[1] &
                        data$submissionDate <= input$submissionDate[2]",
      submissionDayOfWeek =
        "data$submissionDayOfWeek %in% input$submissionDayOfWeek",
      programDescription =
        "data$programDescription %in% input$programDescription",
      naicsDescription = "data$naicsDescription %in% input$naicsDescription",
      channel = "data$Channel %in% input$channel"
    )
  )

  ##############################################################################
  #
  # Active Filters for each Tab
  #
  ##############################################################################

  tabItems <- list(
    # To add a new filter to a tab, add its name to the corresponding vector
    # below. List entry names should match the tabName values of their
    # respective menu items from the UI file.
    stateActivity = c(
      "baseState",
      "regionName",
      "agencyName",
      "submissionDate",
      "programDescription",
      "naicsDescription",
      "channel",
      "clManager",
      "clAccountExecutive",
      "plTerritoryManager"
    ),
    monthlyActivity = c(
      "baseState",
      "regionName",
      "agencyName",
      "submissionDate",
      "programDescription",
      "naicsDescription",
      "channel",
      "clManager",
      "clAccountExecutive",
      "plTerritoryManager"
    ),
    totalSales = c(
      "baseState",
      "regionName",
      "agencyName",
      "submissionDate",
      "programDescription",
      "naicsDescription",
      "channel",
      "clManager",
      "clAccountExecutive",
      "plTerritoryManager"
    ),
    quotesPerDay = c(
      "lineOfBusiness",
      "baseState",
      "regionName",
      "agencyName",
      "submissionStatus",
      "submissionDate",
      "submissionDayOfWeek",
      "programDescription",
      "naicsDescription",
      "channel",
      "clManager",
      "clAccountExecutive",
      "plTerritoryManager"
    ),
    agencyPerformance = c(
      "lineOfBusiness",
      "baseState",
      "agencyName",
      "submissionDate",
      "channel",
      "clAccountExecutive",
      "plTerritoryManager"
    ),
    faq = c()
  )

  ##############################################################################
  #
  # Render Filter Menu
  #
  ##############################################################################

  output$filterMenu <- renderMenu({
    tags$ul(
      class = "filter-menu",
      # Grabs the sidebarMenu items for each entry in the menuOptions outputs
      # list that match the defined tabItems for the currently selected tab.
      menuOptions$outputs[tabItems[[input$sidebarmenu]]],
      tags$div(
        id = "filterMenu",
        class = "sidebarMenuSelectedTabItem",
        'data-value' = "filtersAreLoaded"
      )
    )
  })

  ##############################################################################
  #
  # Select Button Observers
  #
  ##############################################################################

  observeEvent(input$selectStates, {
    if((input$selectStates %% 2) == 0) {
      updateCheckboxGroupInput(
        session = session,
        inputId = "baseState",
        choices = baseStateChoices,
        selected = baseStateChoices
      )
    } else {
      updateCheckboxGroupInput(
        session = session,
        inputId = "baseState",
        choices = baseStateChoices,
        selected = c()
      )
    }
  })

  observeEvent(input$selectSubStatus, {
    if((input$selectSubStatus %% 2) == 0) {
      updateCheckboxGroupInput(
        session = session,
        inputId = "submissionStatus",
        choices = subStatusChoices,
        selected = subStatusChoices
      )
    } else {
      updateCheckboxGroupInput(
        session = session,
        inputId = "submissionStatus",
        choices = subStatusChoices,
        selected = c()
      )
    }
  })

  ##############################################################################
  #
  # Apply Filters to Data
  #
  ##############################################################################

  filterData <- reactive({
    input$go

    isolate({
      if(is.null(input$filterMenu)) {
        # No filtration performed on initial load.
        filteredData <- data
      } else {
        filterString <- getFilterString(
          # Constructs a string of all of our filtration criteria from the
          # filters section of the menuOptions list.
          filterList = menuOptions$filters[tabItems[[input$sidebarmenu]]],
          input = reactiveValuesToList(input)
        )
        # Parse then evaluate this string as code
        filteredData <- eval(parse(text = filterString))
      }
    })
    return(filteredData)
  })

################################################################################
#
# State Activity Tab
#
################################################################################

  ##############################################################################
  #
  # BoP Activity by State Chart
  #
  ##############################################################################

  output$bopActivityByStateGraph <- renderPlot({
    filteredData <- filterData()

    mergedData <- aggregateStateData(
      data = filteredData,
      lineOfBusiness = "BP7BusinessOwners"
    )
    colnames(mergedData) <- prettifyColumns(colnames(mergedData))

    bopPlot <- ggplot(
      data = mergedData,
      aes(x = `Base State`, y = `Activity Count`)
    ) +
      geom_bar(
        aes(fill = `Activity Type`),
        stat = "identity",
        position = "dodge"
      ) +
      # Change the color options to be gentler and more appealing
      scale_fill_manual(values = c("#4477AA", "#46C646", "#E06C75")) +
      theme_light(base_size = 16) +
      theme(
        legend.position = "top", legend.box = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    return(bopPlot)
  })

  ##############################################################################
  #
  # BoP Activity by State Map
  #
  ##############################################################################

  # output$bopActivityByStateGraph <- renderPlot({
  #   filteredData <- filterData()
  #
  #   mergedData <- aggregateStateData(
  #     data = filteredData,
  #     lineOfBusiness = "BP7BusinessOwners"
  #   )
  #
  # })

  ##############################################################################
  #
  # BoP Activity by State Table
  #
  ##############################################################################

  output$bopActivityByStateTable <- DT::renderDataTable({
    filteredData <- filterData()

    mergedData <- aggregateStateData(
      data = filteredData,
      lineOfBusiness = "BP7BusinessOwners"
    )
    quotedPremData <- filteredData[
      !is.na(filteredData$premiumAmount) &
      filteredData$lineOfBusiness == "BP7BusinessOwners",
    ]
    quotedPremData <- aggregate(
      x = list(activityCount = quotedPremData$premiumAmount),
      by = list(baseState = quotedPremData$baseState),
      FUN = "sum"
    )
    quotedPremData$activityType <- "quotedPremium"

    salePremData <- filteredData[
      !is.na(filteredData$premiumAmount) &
      filteredData$lineOfBusiness == "BP7BusinessOwners" &
      filteredData$boundPolicyIndicator == "Y",
    ]
    salePremData <- aggregate(
      x = list(activityCount = salePremData$premiumAmount),
      by = list(baseState = salePremData$baseState),
      FUN = "sum"
    )
    salePremData$activityType <- "boundPremium"

    mergedData <- rbind(mergedData, quotedPremData, salePremData)
    mergedData <- mergedData[
      mergedData$baseState %in%
      mergedData[mergedData$activityType == "Sales", "baseState"],
    ]
    mergedData <- dcast(
      data = mergedData,
      formula = baseState ~ activityType,
      value.var = "activityCount"
    )
    mergedData$closeRate <- mergedData$Sales / mergedData$Quotes

    mergedData <- mergedData[,
      c(
        "baseState",
        "Submissions",
        "Quotes",
        "Sales",
        "closeRate",
        "quotedPremium",
        "boundPremium"
      )
    ]
    mergedData <- rbind(
      mergedData,
      c("Total", colSums(mergedData[, 2:ncol(mergedData)]))
    )
    mergedData[nrow(mergedData), "closeRate"] <-
      as.numeric(mergedData[nrow(mergedData), "Sales"]) /
      as.numeric(mergedData[nrow(mergedData), "Quotes"])
    colnames(mergedData) <- prettifyColumns(colnames(mergedData))

    activityTable <- DT::datatable(
      mergedData,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        ordering = FALSE,
        info = FALSE,
        searching = FALSE,
        scrollX = TRUE
      )
    )
    activityTable <- formatPercentage(activityTable, "Close Rate")
    activityTable <- formatCurrency(
      activityTable,
      c("Quoted Premium", "Bound Premium"),
      digits = 0
    )

    # Format table and highlight the last row which contains column totals
    activityTable <- formatStyle(
      table = activityTable,
      columns = "Sales",
      target = "row",
      fontWeight = styleEqual(
        max(as.integer(mergedData$Sales)),
        "bold"
      )
    )
  })

  ##############################################################################
  #
  # BaP Activity by State Chart
  #
  ##############################################################################

  output$bapActivityByStateGraph <- renderPlot({
    filteredData <- filterData()

    mergedData <- aggregateStateData(
      data = filteredData,
      lineOfBusiness = "CA7CommAuto"
    )

    colnames(mergedData) <- prettifyColumns(colnames(mergedData))

    bapPlot <- ggplot(
      data = mergedData,
      aes(x = `Base State`, y = `Activity Count`)
    ) +
      geom_bar(
        aes(fill = `Activity Type`),
        stat = "identity",
        position = "dodge"
      ) +
      # Change the color options to be gentler and more appealing
      scale_fill_manual(values = c("#4477AA", "#46C646", "#E06C75")) +
      theme_light(base_size = 16) +
      theme(
        legend.position = "top", legend.box = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    return(bapPlot)
  })

  ##############################################################################
  #
  # BaP Activity by State Table
  #
  ##############################################################################

  output$bapActivityByStateTable <- DT::renderDataTable({
    filteredData <- filterData()

    mergedData <- aggregateStateData(
      data = filteredData,
      lineOfBusiness = "CA7CommAuto"
    )
    quotedPremData <- filteredData[
      !is.na(filteredData$premiumAmount) &
      filteredData$lineOfBusiness == "CA7CommAuto",
    ]
    quotedPremData <- aggregate(
      x = list(activityCount = quotedPremData$premiumAmount),
      by = list(baseState = quotedPremData$baseState),
      FUN = "sum"
    )
    quotedPremData$activityType <- "quotedPremium"

    salePremData <- filteredData[
      !is.na(filteredData$premiumAmount) &
      filteredData$lineOfBusiness == "CA7CommAuto" &
      filteredData$boundPolicyIndicator == "Y",
    ]
    salePremData <- aggregate(
      x = list(activityCount = salePremData$premiumAmount),
      by = list(baseState = salePremData$baseState),
      FUN = "sum"
    )
    salePremData$activityType <- "boundPremium"

    mergedData <- rbind(mergedData, quotedPremData, salePremData)
    mergedData <- mergedData[
      mergedData$baseState %in%
      mergedData[mergedData$activityType == "Sales", "baseState"],
    ]
    mergedData <- dcast(
      data = mergedData,
      formula = baseState ~ activityType,
      value.var = "activityCount"
    )
    mergedData$closeRate <- mergedData$Sales / mergedData$Quotes
    mergedData <- rbind(
      mergedData,
      c("Total", colSums(mergedData[, 2:ncol(mergedData)]))
    )
    mergedData[nrow(mergedData), "closeRate"] <-
      as.numeric(mergedData[nrow(mergedData), "Sales"]) /
      as.numeric(mergedData[nrow(mergedData), "Quotes"])

    mergedData <- mergedData[,
      c(
        "baseState",
        "Submissions",
        "Quotes",
        "Sales",
        "closeRate",
        "quotedPremium",
        "boundPremium"
      )
    ]
    colnames(mergedData) <- prettifyColumns(colnames(mergedData))

    activityTable <- DT::datatable(
      mergedData,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        ordering = FALSE,
        info = FALSE,
        searching = FALSE,
        scrollX = TRUE
      )
    )
    activityTable <- formatPercentage(activityTable, "Close Rate")
    activityTable <- formatCurrency(
      activityTable,
      c("Quoted Premium", "Bound Premium"),
      digits = 0
    )
    activityTable <- formatStyle(
      table = activityTable,
      columns = "Sales",
      target = "row",
      fontWeight = styleEqual(
        max(as.integer(mergedData$Sales)),
        "bold"
      )
    )
  })

  ##############################################################################
  #
  # Total Activity by State Chart
  #
  ##############################################################################

  output$totalActivityByStateGraph <- renderPlot({
    filteredData <- filterData()

    mergedData <- aggregateStateData(
      data = filteredData,
      lineOfBusiness = c("CA7CommAuto", "BP7BusinessOwners")
    )

    colnames(mergedData) <- prettifyColumns(colnames(mergedData))

    totalPlot <- ggplot(
      data = mergedData,
      aes(x = `Base State`, y = `Activity Count`)
    ) +
      geom_bar(
        aes(fill = `Activity Type`),
        stat = "identity",
        position = "dodge"
      ) +
      # Change the color options to be gentler and more appealing
      scale_fill_manual(values = c("#4477AA", "#46C646", "#E06C75")) +
      theme_light(base_size = 16) +
      theme(
        legend.position = "top", legend.box = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    return(totalPlot)
  })

  ##############################################################################
  #
  # Total Activity by State Table
  #
  ##############################################################################

  output$totalActivityByStateTable <- DT::renderDataTable({
    filteredData <- filterData()

    mergedData <- aggregateStateData(
      data = filteredData,
      lineOfBusiness = c("CA7CommAuto", "BP7BusinessOwners")
    )
    quotedPremData <- filteredData[!is.na(filteredData$premiumAmount), ]
    quotedPremData <- aggregate(
      x = list(activityCount = quotedPremData$premiumAmount),
      by = list(baseState = quotedPremData$baseState),
      FUN = "sum"
    )
    quotedPremData$activityType <- "quotedPremium"

    salePremData <- filteredData[
      !is.na(filteredData$premiumAmount) &
      filteredData$boundPolicyIndicator == "Y",
    ]
    salePremData <- aggregate(
      x = list(activityCount = salePremData$premiumAmount),
      by = list(baseState = salePremData$baseState),
      FUN = "sum"
    )
    salePremData$activityType <- "boundPremium"

    mergedData <- rbind(mergedData, quotedPremData, salePremData)
    mergedData <- mergedData[
      mergedData$baseState %in%
      mergedData[mergedData$activityType == "Sales", "baseState"],
    ]
    mergedData <- dcast(
      data = mergedData,
      formula = baseState ~ activityType,
      value.var = "activityCount"
    )
    mergedData$closeRate <- mergedData$Sales / mergedData$Quotes
    mergedData <- rbind(
      mergedData,
      c("Total", colSums(mergedData[, 2:ncol(mergedData)]))
    )
    mergedData[nrow(mergedData), "closeRate"] <-
      as.numeric(mergedData[nrow(mergedData), "Sales"]) /
      as.numeric(mergedData[nrow(mergedData), "Quotes"])

    mergedData <- mergedData[,
      c(
        "baseState",
        "Submissions",
        "Quotes",
        "Sales",
        "closeRate",
        "quotedPremium",
        "boundPremium"
      )
    ]
    colnames(mergedData) <- prettifyColumns(colnames(mergedData))

    activityTable <- DT::datatable(
      mergedData,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        ordering = FALSE,
        info = FALSE,
        searching = FALSE,
        scrollX = TRUE
      )
    )
    activityTable <- formatPercentage(activityTable, "Close Rate")
    activityTable <- formatCurrency(
      activityTable,
      c("Quoted Premium", "Bound Premium"),
      digits = 0
    )
    activityTable <- formatStyle(
      table = activityTable,
      columns = "Sales",
      target = "row",
      fontWeight = styleEqual(
        max(as.integer(mergedData$Sales)),
        "bold"
      )
    )
  })

################################################################################
#
# Monthly Activity Tab
#
################################################################################

  ##############################################################################
  #
  # BoP Activity by Month Chart
  #
  ##############################################################################

  output$bopActivityByMonthGraph <- renderPlot({
    filteredData <- filterData()

    mergedData <- aggregateMonthData(
      data = filteredData,
      lineOfBusiness = "BP7BusinessOwners"
    )

    colnames(mergedData) <- prettifyColumns(colnames(mergedData))

    bopPlot <- ggplot(
      data = mergedData,
      aes(x = Date, y = `Activity Count`)
    ) +
      geom_bar(
        aes(fill = `Activity Type`),
        stat = "identity",
        position = "dodge"
      ) +
      # Change the color options to be gentler and more appealing
      scale_fill_manual(values = c("#4477AA", "#46C646", "#E06C75")) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      theme_light(base_size = 16) +
      theme(
        legend.position = "top", legend.box = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    return(bopPlot)
  })

  ##############################################################################
  #
  # BoP Activity by Month Table
  #
  ##############################################################################

  output$bopActivityByMonthTable <- DT::renderDataTable({
    filteredData <- filterData()

    mergedData <- aggregateMonthData(
      data = filteredData,
      lineOfBusiness = "BP7BusinessOwners"
    )
    quotedPremData <- filteredData[
      !is.na(filteredData$premiumAmount) &
      filteredData$lineOfBusiness == "BP7BusinessOwners",
    ]
    quotedPremData <- aggregate(
      x = list(activityCount = quotedPremData$premiumAmount),
      by = list(
        month = month(quotedPremData$submissionDate, label = TRUE),
        year = year(quotedPremData$submissionDate)
      ),
      FUN = "sum"
    )
    quotedPremData$activityType <- "quotedPremium"
    quotedPremData$date <- as.Date(
      paste(quotedPremData$month, quotedPremData$year, "01", sep = "-"),
      format = "%b-%Y-%d"
    )

    salePremData <- filteredData[
      !is.na(filteredData$premiumAmount) &
      filteredData$boundPolicyIndicator == "Y" &
      filteredData$lineOfBusiness == "BP7BusinessOwners", ,
    ]
    salePremData <- aggregate(
      x = list(activityCount = salePremData$premiumAmount),
      by = list(
        month = month(salePremData$submissionDate, label = TRUE),
        year = year(salePremData$submissionDate)
      ),
      FUN = "sum"
    )
    salePremData$activityType <- "boundPremium"
    salePremData$date <- as.Date(
      paste(salePremData$month, salePremData$year, "01", sep = "-"),
      format = "%b-%Y-%d"
    )

    mergedData <- rbind(mergedData, quotedPremData, salePremData)
    mergedData <- dcast(
      data = mergedData,
      formula = year + month ~ activityType,
      value.var = "activityCount"
    )
    mergedData$closeRate <- mergedData$Sales / mergedData$Quotes

    mergedData <- mergedData[
      order(mergedData$year, mergedData$month, decreasing = TRUE),
    ]
    mergedData <- mergedData[,
      c(
        "month",
        "year",
        "Submissions",
        "Quotes",
        "Sales",
        "closeRate",
        "quotedPremium",
        "boundPremium"
      )
    ]
    mergedData$month <- as.character(mergedData$month)
    mergedData$year <- as.character(mergedData$year)

    mergedData <- rbind(
      mergedData,
      c("Total", "-", colSums(mergedData[, 3:ncol(mergedData)]))
    )
    mergedData[nrow(mergedData), "closeRate"] <-
      as.numeric(mergedData[nrow(mergedData), "Sales"]) /
      as.numeric(mergedData[nrow(mergedData), "Quotes"])
    colnames(mergedData) <- prettifyColumns(colnames(mergedData))

    activityTable <- DT::datatable(
      mergedData,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        ordering = FALSE,
        info = FALSE,
        searching = FALSE,
        scrollX = TRUE
      )
    )
    activityTable <- formatPercentage(activityTable, "Close Rate")
    activityTable <- formatCurrency(
      activityTable,
      c("Quoted Premium", "Bound Premium"),
      digits = 0
    )
    # Format table and highlight the last row which contains column totals
    activityTable <- formatStyle(
      table = activityTable,
      columns = "Sales",
      target = "row",
      fontWeight = styleEqual(
        max(as.integer(mergedData$Sales)),
        "bold"
      )
    )
  })

  ##############################################################################
  #
  # BaP Activity by Month Chart
  #
  ##############################################################################

  output$bapActivityByMonthGraph <- renderPlot({
    filteredData <- filterData()

    mergedData <- aggregateMonthData(
      data = filteredData,
      lineOfBusiness = "CA7CommAuto"
    )

    colnames(mergedData) <- prettifyColumns(colnames(mergedData))

    bapPlot <- ggplot(data = mergedData, aes(x = Date, y = `Activity Count`)) +
      geom_bar(
        aes(fill = `Activity Type`),
        stat = "identity",
        position = "dodge"
      ) +
      # Change the color options to be gentler and more appealing
      scale_fill_manual(values = c("#4477AA", "#46C646", "#E06C75")) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      theme_light(base_size = 16) +
      theme(
        legend.position = "top", legend.box = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    return(bapPlot)
  })

  ##############################################################################
  #
  # BaP Activity by Month Table
  #
  ##############################################################################

  output$bapActivityByMonthTable <- DT::renderDataTable({
    filteredData <- filterData()

    mergedData <- aggregateMonthData(
      data = filteredData,
      lineOfBusiness = "CA7CommAuto"
    )
    quotedPremData <- filteredData[
      !is.na(filteredData$premiumAmount) &
      filteredData$lineOfBusiness == "CA7CommAuto",
    ]
    quotedPremData <- aggregate(
      x = list(activityCount = quotedPremData$premiumAmount),
      by = list(
        month = month(quotedPremData$submissionDate, label = TRUE),
        year = year(quotedPremData$submissionDate)
      ),
      FUN = "sum"
    )
    quotedPremData$activityType <- "quotedPremium"
    quotedPremData$date <- as.Date(
      paste(quotedPremData$month, quotedPremData$year, "01", sep = "-"),
      format = "%b-%Y-%d"
    )

    salePremData <- filteredData[
      !is.na(filteredData$premiumAmount) &
      filteredData$boundPolicyIndicator == "Y" &
      filteredData$lineOfBusiness == "CA7CommAuto",
    ]
    salePremData <- aggregate(
      x = list(activityCount = salePremData$premiumAmount),
      by = list(
        month = month(salePremData$submissionDate, label = TRUE),
        year = year(salePremData$submissionDate)
      ),
      FUN = "sum"
    )
    salePremData$activityType <- "boundPremium"
    salePremData$date <- as.Date(
      paste(salePremData$month, salePremData$year, "01", sep = "-"),
      format = "%b-%Y-%d"
    )

    mergedData <- rbind(mergedData, quotedPremData, salePremData)
    mergedData <- dcast(
      data = mergedData,
      formula = year + month ~ activityType,
      value.var = "activityCount"
    )
    mergedData$closeRate <- mergedData$Sales / mergedData$Quotes

    mergedData <- mergedData[
      order(mergedData$year, mergedData$month, decreasing = TRUE),
    ]
    mergedData <- mergedData[,
      c(
        "month",
        "year",
        "Submissions",
        "Quotes",
        "Sales",
        "closeRate",
        "quotedPremium",
        "boundPremium"
      )
    ]
    mergedData$month <- as.character(mergedData$month)
    mergedData$year <- as.character(mergedData$year)

    mergedData <- rbind(
      mergedData,
      c("Total", "-", colSums(mergedData[, 3:ncol(mergedData)]))
    )
    mergedData[nrow(mergedData), "closeRate"] <-
      as.numeric(mergedData[nrow(mergedData), "Sales"]) /
      as.numeric(mergedData[nrow(mergedData), "Quotes"])
    colnames(mergedData) <- prettifyColumns(colnames(mergedData))

    activityTable <- DT::datatable(
      mergedData,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        ordering = FALSE,
        info = FALSE,
        searching = FALSE,
        scrollX = TRUE
      )
    )
    activityTable <- formatPercentage(activityTable, "Close Rate")
    activityTable <- formatCurrency(
      activityTable,
      c("Quoted Premium", "Bound Premium"),
      digits = 0
    )
    # Format table and highlight the last row which contains column totals
    activityTable <- formatStyle(
      table = activityTable,
      columns = "Sales",
      target = "row",
      fontWeight = styleEqual(
        max(as.integer(mergedData$Sales)),
        "bold"
      )
    )
  })

  ##############################################################################
  #
  # Total Activity by Month Chart
  #
  ##############################################################################

  output$totalActivityByMonthGraph <- renderPlot({
    filteredData <- filterData()

    mergedData <- aggregateMonthData(
      data = filteredData,
      lineOfBusiness = c("BP7BusinessOwners", "CA7CommAuto")
    )

    colnames(mergedData) <- prettifyColumns(colnames(mergedData))

    totalPlot <- ggplot(
      data = mergedData,
      aes(x = Date, y = `Activity Count`)
    ) +
      geom_bar(
        aes(fill = `Activity Type`),
        stat = "identity",
        position = "dodge"
      ) +
      # Change the color options to be gentler and more appealing
      scale_fill_manual(values = c("#4477AA", "#46C646", "#E06C75")) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      theme_light(base_size = 16) +
      theme(
        legend.position = "top", legend.box = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    return(totalPlot)
  })

  ##############################################################################
  #
  # Total Activity by Month Table
  #
  ##############################################################################

  output$totalActivityByMonthTable <- DT::renderDataTable({
    filteredData <- filterData()

    mergedData <- aggregateMonthData(
      data = filteredData,
      lineOfBusiness = c("BP7BusinessOwners", "CA7CommAuto")
    )
    quotedPremData <- filteredData[!is.na(filteredData$premiumAmount), ]
    quotedPremData <- aggregate(
      x = list(activityCount = quotedPremData$premiumAmount),
      by = list(
        month = month(quotedPremData$submissionDate, label = TRUE),
        year = year(quotedPremData$submissionDate)
      ),
      FUN = "sum"
    )
    quotedPremData$activityType <- "quotedPremium"
    quotedPremData$date <- as.Date(
      paste(quotedPremData$month, quotedPremData$year, "01", sep = "-"),
      format = "%b-%Y-%d"
    )

    salePremData <- filteredData[
      !is.na(filteredData$premiumAmount) &
      filteredData$boundPolicyIndicator == "Y",
    ]
    salePremData <- aggregate(
      x = list(activityCount = salePremData$premiumAmount),
      by = list(
        month = month(salePremData$submissionDate, label = TRUE),
        year = year(salePremData$submissionDate)
      ),
      FUN = "sum"
    )
    salePremData$activityType <- "boundPremium"
    salePremData$date <- as.Date(
      paste(salePremData$month, salePremData$year, "01", sep = "-"),
      format = "%b-%Y-%d"
    )

    mergedData <- rbind(mergedData, quotedPremData, salePremData)
    mergedData <- dcast(
      data = mergedData,
      formula = year + month ~ activityType,
      value.var = "activityCount"
    )
    mergedData$closeRate <- mergedData$Sales / mergedData$Quotes

    mergedData <- mergedData[
      order(mergedData$year, mergedData$month, decreasing = TRUE),
    ]
    mergedData <- mergedData[,
      c(
        "month",
        "year",
        "Submissions",
        "Quotes",
        "Sales",
        "closeRate",
        "quotedPremium",
        "boundPremium"
      )
    ]
    mergedData$month <- as.character(mergedData$month)
    mergedData$year <- as.character(mergedData$year)

    mergedData <- rbind(
      mergedData,
      c("Total", "-", colSums(mergedData[, 3:ncol(mergedData)]))
    )
    mergedData[nrow(mergedData), "closeRate"] <-
      as.numeric(mergedData[nrow(mergedData), "Sales"]) /
      as.numeric(mergedData[nrow(mergedData), "Quotes"])

    colnames(mergedData) <- prettifyColumns(colnames(mergedData))

    activityTable <- DT::datatable(
      mergedData,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        ordering = FALSE,
        info = FALSE,
        searching = FALSE,
        scrollX = TRUE
      )
    )
    activityTable <- formatPercentage(activityTable, "Close Rate")
    activityTable <- formatCurrency(
      activityTable,
      c("Quoted Premium", "Bound Premium"),
      digits = 0
    )

    # Format table and highlight the last row which contains column totals
    activityTable <- formatStyle(
      table = activityTable,
      columns = "Sales",
      target = "row",
      fontWeight = styleEqual(
        max(as.integer(mergedData$Sales)),
        "bold"
      )
    )
  })

################################################################################
#
# Total Sales Tab
#
################################################################################

  ##############################################################################
  #
  # Sales by State Table
  #
  ##############################################################################

  output$salesByStateTable <- DT::renderDataTable({
    filteredData <- filterData()
    salesData <- filteredData[filteredData$boundPolicyIndicator == "Y", ]

    salesByState <- aggregate(
      x = list(sales = salesData$policyNumber),
      by = list(
        baseState = salesData$baseState,
        lineOfBusiness = salesData$lineOfBusiness
      ),
      FUN = "length"
    )

    salesByState <- dcast(
      data = salesByState,
      formula = baseState ~ lineOfBusiness,
      value.var = "sales"
    )
    salesByState[is.na(salesByState)] <- 0

    # Add row and column totals
    salesByState$stateTotal <- rowSums(salesByState[2:ncol(salesByState)])
    salesByState <- rbind(
      salesByState,
      c("LoB Total", colSums(salesByState[, 2:ncol(salesByState)]))
    )

    colnames(salesByState) <- prettifyColumns(colnames(salesByState))

    salesTable <- DT::datatable(
      salesByState,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        ordering = FALSE,
        info = FALSE,
        searching = FALSE,
        scrollX = TRUE
      )
    )

    # Format table and highlight the last row which contains column totals
    formatStyle(
      salesTable,
      "State Total",
      target = "row",
      fontWeight = styleEqual(
        max(as.integer(salesByState$`State Total`)),
        "bold"
      )
    )
  })

  ##############################################################################
  #
  # Sales by State Graph
  #
  ##############################################################################

  output$salesByStateGraph <- renderPlot({
    filteredData <- filterData()
    salesData <- filteredData[filteredData$boundPolicyIndicator == "Y", ]

    salesByState <- aggregate(
      x = list(sales = salesData$policyNumber),
      by = list(
        baseState = salesData$baseState,
        lineOfBusiness = salesData$lineOfBusiness
      ),
      FUN = "length"
    )

    # Convert state names to abbreviations to save space and clean up formatting
    salesByState$baseState <- state.abb[
      match(salesByState$baseState, state.name)
    ]
    colnames(salesByState) <- prettifyColumns(colnames(salesByState))

    statePlot <- ggplot(data = salesByState, aes(x = `Base State`, y = Sales)) +
      # Bar chart grouped by State then Line of Business
      geom_bar(
        aes(fill = `Line Of Business`),
        stat = "identity",
        position = "dodge"
      ) +
      # Change the color options to be gentler and more appealing
      scale_fill_manual(values = c("#4477AA", "#46C646")) +
      theme_light(base_size = 16) +
      theme(legend.position = "top", legend.box = "horizontal")

    return(statePlot)
  })

  ##############################################################################
  #
  # Sales by Month Table
  #
  ##############################################################################

  output$salesByMonthTable <- DT::renderDataTable({
    filteredData <- filterData()
    salesData <- filteredData[filteredData$boundPolicyIndicator == "Y", ]

    salesByMonth <- aggregate(
      x = list(sales = salesData$policyNumber),
      by = list(
        soldMonth = month(salesData$boundDate, label = TRUE),
        soldYear = year(salesData$boundDate),
        lineOfBusiness = salesData$lineOfBusiness
      ),
      FUN = "length"
    )

    salesByMonth <- dcast(
      data = salesByMonth,
      formula = soldYear + soldMonth ~ lineOfBusiness,
      value.var = "sales"
    )
    salesByMonth[is.na(salesByMonth)] <- 0

    # Add row and column totals
    salesByMonth$monthlyTotal <- rowSums(salesByMonth[3:ncol(salesByMonth)])
    salesByMonth <- rbind(
      salesByMonth,
      c("LoB Total", "-", colSums(salesByMonth[, 3:ncol(salesByMonth)]))
    )

    colnames(salesByMonth) <- prettifyColumns(colnames(salesByMonth))

    salesTable <- DT::datatable(
      salesByMonth,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        ordering = FALSE,
        info = FALSE,
        searching = FALSE,
        scrollX = TRUE
      )
    )

    # Format table and highlight the last row which contains column totals
    formatStyle(
      salesTable,
      "Monthly Total",
      target = "row",
      fontWeight = styleEqual(
        max(as.integer(salesByMonth$`Monthly Total`)),
        "bold"
      )
    )
  })

  ##############################################################################
  #
  # Sales by Month Graph
  #
  ##############################################################################

  output$salesByMonthGraph <- renderPlot({
    filteredData <- filterData()
    salesData <- filteredData[filteredData$boundPolicyIndicator == "Y", ]

    salesByMonth <- aggregate(
      x = list(sales = salesData$policyNumber),
      by = list(
        soldMonth = month(salesData$boundDate, label = TRUE),
        soldYear = year(salesData$boundDate),
        lineOfBusiness = salesData$lineOfBusiness
      ),
      FUN = "length"
    )
    salesByMonth$soldDate <- as.Date(
      paste(
        salesByMonth$soldYear,
        salesByMonth$soldMonth,
        "01",
        sep = "-"
        ),
      format = "%Y-%b-%d"
    )

    monthPlot <- ggplot(data = salesByMonth, aes(x = soldDate, y = sales)) +
      geom_line(aes(color = lineOfBusiness), size = 3) +
      geom_point(aes(color = lineOfBusiness), size = 5) +
      scale_color_manual(values = c("#4477AA", "#46C646")) +
      theme_light(base_size = 16) +
      theme(legend.position = "top", legend.box = "horizontal")

    return(monthPlot)
  })

################################################################################
#
# Quotes per Day Tab
#
################################################################################

  ##############################################################################
  #
  # Daily Quotes and Sales Table
  #
  ##############################################################################

  output$dailyQuotesTable <- DT::renderDataTable({
    filteredData <- filterData()
    filteredData[is.na(filteredData$firstQuotedDate), "quoteCount"] <- 0
    filteredData[!is.na(filteredData$firstQuotedDate), "quoteCount"] <- 1
    filteredData$subCount <- 1

    quotesPerDay <- aggregate(
      x = list(
        quotes = filteredData$quoteCount,
        submissions = filteredData$subCount
      ),
      by = list(submissionDate = filteredData$submissionDate),
      FUN = "sum"
    )

    quotesPerDay$completedQuoteRate <-
      quotesPerDay$quotes / quotesPerDay$submissions

    colnames(quotesPerDay) <- prettifyColumns(colnames(quotesPerDay))

    quotesTable <- DT::datatable(
      quotesPerDay,
      rownames = FALSE,
      options = list(
        paging = TRUE,
        ordering = FALSE,
        info = FALSE,
        searching = FALSE,
        scrollX = TRUE
      )
    )

    formatPercentage(
      quotesTable,
      "Completed Quote Rate"
    )
  })

  ##############################################################################
  #
  # Daily Quotes and Sales Graph
  #
  ##############################################################################

  output$dailyQuotesGraph <- renderPlot({
    filteredData <- filterData()

    filteredData[is.na(filteredData$firstQuotedDate), "quoteCount"] <- 0
    filteredData[!is.na(filteredData$firstQuotedDate), "quoteCount"] <- 1
    filteredData$subCount <- 1

    quotesPerDay <- aggregate(
      x = list(
        quotes = filteredData$quoteCount,
        submissions = filteredData$subCount
      ),
      by = list(submissionDate = filteredData$submissionDate),
      FUN = "sum"
    )

    colnames(quotesPerDay) <- prettifyColumns(colnames(quotesPerDay))

    dqsPlot <- ggplot(data = quotesPerDay, aes(x = `Submission Date`)) +
      geom_line(aes(y = `Quotes`, color = "Quotes"), size = 1) +
      geom_line(aes(y = `Submissions`, color = "Submissions"), size = 1) +
      scale_color_manual(values = c("#4477AA", "#46C646")) +
      theme_light(base_size = 16) +
      theme(legend.position = "top", legend.box = "horizontal")

    return(dqsPlot)
  })

################################################################################
#
# Agency Performance Tab
#
################################################################################

  ##############################################################################
  #
  # Agency Performance Table
  #
  ##############################################################################

  output$agencyPerformanceTable <- DT::renderDataTable({
    filteredData <- filterData()

    agencyCloseData <- aggregate(
      x = list(activityCount = filteredData$submissionNumber),
      by = list(
        agencyName = filteredData$agencyName,
        boundPolicyInd = filteredData$boundPolicyIndicator
      ),
      FUN = "length"
    )

    agencyCloseData[
      agencyCloseData$boundPolicyInd == "Y", "boundPolicyInd"] <- "sales"
    agencyCloseData[
      agencyCloseData$boundPolicyInd == "N", "boundPolicyInd"] <- "quotes"

    agencyCloseData <- dcast(
      data = agencyCloseData,
      formula = agencyName ~ boundPolicyInd,
      value.var = "activityCount",
      fill = 0
    )

    agencyCloseData$closeRate <- NA
    agencyCloseData$quoteVolumePercentile <- NA
    agencyCloseData$saleVolumePercentile <- NA
    agencyCloseData$closeRatePercentile <- NA

    agencyCloseData <- within(agencyCloseData, {
      quotes <- quotes + sales
      closeRate <- sales / quotes
      quoteVolumePercentile <- rank(quotes) / length(quotes)
      saleVolumePercentile <- rank(sales) / length(sales)
      closeRatePercentile <- rank(closeRate) / length(closeRate)
    })

    colnames(agencyCloseData) <- prettifyColumns(colnames(agencyCloseData))

    agencyCloseTable <- DT::datatable(
      agencyCloseData,
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = 25,
        ordering = TRUE,
        info = FALSE,
        searching = TRUE,
        scrollX = TRUE
      )
    )

    formatPercentage(
      agencyCloseTable,
      c(
        "Close Rate",
        "Quote Volume Percentile",
        "Sale Volume Percentile",
        "Close Rate Percentile"
      )
    )
  })
})
