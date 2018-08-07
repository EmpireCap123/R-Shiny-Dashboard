# R-Shiny-Dashboard
prettifyColumns <- function(x) {
  # Applies Title Case formatting and spacing to a string. Used to convert
  # camelCase column names into properly-formatted labels for charts, etc.
  #
  # Args:
  #   x: A character string to format.
  #
  # Returns:
  #   A character string in Title Case format.
  x <- to_upper_camel_case(x)
  x <- gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", x)
  return(x)
}

getFilterString <- function(filterList, input = NULL) {
  # Constructs a string of all of our filtration criteria from the filters
  # section of the menuOptions list.
  #
  # Args:
  #   filterList: The contents of menuOptions$filters for all UI elements
  #               active in the currently-selected tab.
  #   input: A list of all reactive values from the server's scope.
  #
  # Returns: R code formatted as a string to be parsed and evaluated in the
  #          server's scope.
  for(i in names(filterList)) {
    # If a user has selected "All" in a selectize menu, then we don't want to
    # filter any of those results, so we remove that filter from our list.
    if("All" %in% input[[i]]) {
      filterList[i] <- NULL
    }
  }
  filterString <- paste0("data[", paste(filterList, collapse = " & "), ", ]")
  return(filterString)
}

aggregateStateData <- function(data, lineOfBusiness) {
  data <- data[data$lineOfBusiness %in% lineOfBusiness, ]
  data[is.na(data$firstQuotedDate), "activityType"] <- "Submissions"
  data[!is.na(data$firstQuotedDate), "activityType"] <- "Quotes"

  activityData <- aggregate(
    x = list(activityCount = data$submissionStatus),
    by = list(
      baseState = data$baseState,
      activityType = data$activityType
    ),
    FUN = "length"
  )
  activityData <- dcast(
    data = activityData,
    formula = baseState ~ activityType,
    value.var = "activityCount"
  )

  activityData[is.na(activityData)] <- 0
  # All quotes are submissions but not all submissions are quotes
  activityData$Submissions <- activityData$Quote + activityData$Submission

  activityData <- melt(
    data = activityData,
    id.vars = "baseState",
    variable.name = "activityType",
    value.name = "activityCount"
  )

  salesData <- data[data$boundPolicyIndicator == "Y", ]
  salesData <- aggregate(
    x = list(activityCount = salesData$boundPolicyIndicator),
    by = list(baseState = salesData$baseState),
    FUN = "length"
  )
  salesData$activityType <- "Sales"

  # Combine data and filter out states where sales are impossible
  mergedData <- rbind(activityData, salesData)
  mergedData <- mergedData[
    mergedData$baseState %in%
    mergedData[mergedData$activityType == "Sales", "baseState"],
  ]
  mergedData$activityType <- factor(
    mergedData$activityType,
    levels = c("Submissions", "Quotes", "Sales")
  )
  return(mergedData)
}

aggregateMonthData <- function(data, lineOfBusiness) {
  data <- data[data$lineOfBusiness %in% lineOfBusiness, ]
  data[is.na(data$firstQuotedDate), "activityType"] <- "Submissions"
  data[!is.na(data$firstQuotedDate), "activityType"] <- "Quotes"

  activityData <- aggregate(
    x = list(activityCount = data$submissionStatus),
    by = list(
      month = month(data$submissionDate, label = TRUE),
      year = year(data$submissionDate),
      activityType = data$activityType
    ),
    FUN = "length"
  )
  activityData <- dcast(
    data = activityData,
    formula = month + year ~ activityType,
    value.var = "activityCount"
  )
  activityData[is.na(activityData)] <- 0
  # All quotes are submissions but not all submissions are quotes
  activityData$Submissions <- activityData$Quote + activityData$Submission

  activityData <- melt(
    data = activityData,
    id.vars = c("month", "year"),
    variable.name = "activityType",
    value.name = "activityCount"
  )

  salesData <- data[data$boundPolicyIndicator == "Y", ]
  salesData <- aggregate(
    x = list(activityCount = salesData$boundPolicyIndicator),
    by = list(
      month = month(salesData$submissionDate, label = TRUE),
      year = year(salesData$submissionDate)
    ),
    FUN = "length"
  )
  salesData$activityType <- "Sales"

  # Combine data
  mergedData <- rbind(activityData, salesData)
  mergedData$activityType <- factor(
    mergedData$activityType,
    levels = c(
      "Submissions",
      "Quotes",
      "Sales",
      "Quoted Premium",
      "Bound Premium"
    )
  )
  mergedData$date <- as.Date(
    paste(mergedData$month, mergedData$year, "01", sep = "-"),
    format = "%b-%Y-%d"
  )
  return(mergedData)
}

accordionMenuItem <- function(text, ..., icon = NULL) {
  # Creates an accordion menu compatible with a Shinydashboard sidebar menu
  #
  # Args:
  #   text: The text to display as the menu label
  #   icon: An icon() to display alongside the text label.
  #   ...: An arbitrary number of Shiny UI elements to include within the menu.
  #
  # Returns:
  #   Shiny-compatible HTML object that expands and contracts on click and can
  #   contain multiple other shiny UI elements.
  uiList <- list(...)
  tags$li(
    tags$a(class = "accordion", href = "#", icon, tags$span(text)),
    tags$ul(class = "panel", uiList)
  )
}

dropdownActionMenu <-
  function (..., title = NULL, icon = NULL, header = NULL) {
    # Creates a Shinydashboard compatible drop down menu that can accept UI
    # elements other than the small number of pre-packaged ones that are native
    # to Shinydashboard.
    #
    # Args:
    #   ...: An arbitrary number of Shiny UI elements to include within the menu
    #   title: The text to display as the menu label
    #   icon: An icon() to display alongside the text label.
    #   header: An optional header to add to the drop down
    #
    # Returns:
    #   Shiny-compatible HTML object that lives in the top right corner of the
    #   dashboard, expands and contracts on click and can contain multiple other
    #   shiny UI elements.
    items <- list(...)
    lapply(items, shinydashboard:::tagAssert, type = "li")
    type <- "notifications"
    dropdownClass <- paste0("dropdown ", type, "-menu")
    tags$li(
      class = dropdownClass,
      a(
        href = "#",
        class = "dropdown-toggle",
        `data-toggle` = "dropdown",
        icon,
        title
      ),
      tags$ul(
        class = "dropdown-menu",
        if(!is.null(header)) {
          tags$li(class = "header", header)
        },
        tags$li(tags$ul(class = "menu", items))
      )
    )
  }
