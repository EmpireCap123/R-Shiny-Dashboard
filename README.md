# R-Shiny-Dashboard
if(!require(lubridate)) {
  install.packages("lubridate")
}
if(!require(RODBC)) {
  install.packages("RODBC")
}
if(!require(snakecase)) {
  install.packages("snakecase")
}
if(!require(xlsx)) {
  install.packages("xlsx")
}
library(lubridate)
library(RODBC)
library(snakecase)
library(xlsx)

quoteCloseQuery <- paste(
  readLines("Source/quoteCloseGW.sql"),
  collapse = "\n"
)
credentials <- read.csv("Source/credentials.csv", stringsAsFactors = FALSE)
agencyAssignData <- read.xlsx(
  file = "Source/Agency Assignment List Final.xlsx",
  stringsAsFactors = FALSE,
  sheetIndex = 1
)

if(Sys.info()["sysname"] == "Windows") {
  driver = paste0(
    "driver={SQL Server};",
    "trusted_connection=true;"
  )
} else {
  driver = paste0(
    "driver=FreeTDS;",
    "uid=", credentials$usr, ";",
    "pwd=", credentials$pw, ";"
  )
}

policyCenterConn <- odbcDriverConnect(
  paste0(
    driver,
    "server=SAE1DGWSQLP23;",
    "port=1433;",
    "database=PolicyCenter;"
  )
)

quoteCloseData <- sqlQuery(
  channel = policyCenterConn,
  believeNRows = FALSE,
  stringsAsFactors = FALSE,
  query = quoteCloseQuery
)

# agencyAssignData$agencyAssignCode <- substr(
#   agencyAssignData$nameAndCode,
#   start = 1,
#   stop = 5
# )
# agencyAssignData$agencyAssignName <- substr(
#   agencyAssignData$nameAndCode,
#   start = 9,
#   stop = nchar(agencyAssignData$nameAndCode)
# )
#
# agencyAssignData$primaryAgentCode <- substr(
#   agencyAssignData$primaryAgentCodeAndName,
#   start = 1,
#   stop = 5
# )
# agencyAssignData$primaryAgentName <- substr(
#   agencyAssignData$primaryAgentCodeAndName,
#   start = 9,
#   stop = nchar(agencyAssignData$primaryAgentCodeAndName)
# )
#
# agencyAssignData$superPrimaryAgentCode <- substr(
#   agencyAssignData$superPrimaryAgentCodeAndName,
#   start = 1,
#   stop = 5
# )
# agencyAssignData$superPrimaryAgentName <- substr(
#   agencyAssignData$superPrimaryAgentCodeAndName,
#   start = 9,
#   stop = nchar(agencyAssignData$superPrimaryAgentCodeAndName)
# )

quoteCloseData <- merge(
  x = quoteCloseData,
  y = agencyAssignData,
  by.x = "agencyCode",
  by.y = "secref",
  all.x = TRUE
)

# Filter out the test policies that were innappropiately made in Prod.
quoteCloseData <- quoteCloseData[
  !quoteCloseData$agencyName %in% c(
    "State Auto Internal Test Agency v2",
    "State Auto Internal Farm Mutual Test Agency"
  ) &
  quoteCloseData$regionName %in% c(
    "Central Region",
    "Eastern Region",
    "Southern Region",
    "Midwestern Region",
    "Western Region",
    NA
  ),
]

quoteCloseData[is.na(quoteCloseData$regionName), "regionName"] <- "None"

quoteCloseData$submissionDayOfWeek <- factor(
  x = quoteCloseData$submissionDayOfWeek,
  levels = c(
    "Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday"
  )
)

# Prevent any ambiguity about date formats
quoteCloseData <- within(quoteCloseData, {
  submissionDate <- as.Date(submissionDate, "%Y-%m-%d")
  firstQuotedDate <- as.Date(firstQuotedDate, "%Y-%m-%d")
  rateAsOfDate <- as.Date(rateAsOfDate, "%Y-%m-%d")
  quotedPolicyEffectiveDate <- as.Date(quotedPolicyEffectiveDate, "%Y-%m-%d")
  quotedPolicyExpirationDate <- as.Date(quotedPolicyExpirationDate, "%Y-%m-%d")
  boundDate <- as.Date(boundDate, "%Y-%m-%d")
})

saveRDS(object = quoteCloseData, file = "data/quoteCloseData.RDS")
odbcCloseAll()
