#getting data from uk gov API

#newCasesByPublishDate New cases (by publish date)
#cumCasesByPublishDate Cumulative cases (by publish date)
#newDeathsByDeathDate New deaths (by death date)
#cumDeathsByDeathDate Cumulative deaths (by death date)

#areaType values
#overview Overview data for the United Kingdom
#nation:Nation data (England, Northern Ireland, Scotland, and Wales)
#region: Region data
#nhsRegion: NHS Region data
#utla: Upper-tier local authority data
#ltla:Lower-tier local authority data

#endpoint <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=england&structure={"date":"date","newCases":"newCasesByPublishDate"}'

# endpoint <- paste('https://api.coronavirus.data.gov.uk/v1/data?',
#                   'filters=areaType=nation;',
#                   'areaName=england&structure={"Date":"date","TotalDeaths":"cumDeathsByDeathDate","IncDeaths":"newDeathsByDeathDate"}',
#                   sep="")

library(httr)

AREA_TYPE = "nation"
AREA_NAME = "england"

endpoint <- "https://api.coronavirus.data.gov.uk/v1/data"

# Create filters:

filters_country <- c(
  sprintf("areaType=%s", AREA_TYPE)
  ,sprintf("areaName=%s", AREA_NAME)
)

filters_overview <- sprintf("areaType=%s", "overview")

filters <- filters_overview #filters_overview #filters_country

# Create the structure as a list or a list of lists:
structure_brokendown_oldmetric <- list(
  date = "date", 
  name = "areaName", 
  code = "areaCode", 
  cases = list(
    daily = "newCasesByPublishDate",
    cumulative = "cumCasesByPublishDate"
  ), 
  deaths = list(
    daily = "newDeathsByDeathDate",
    cumulative = "cumDeathsByDeathDate"
  )
) #that metric appears to have been removed for now

structure_brokendown <- list(
  date = "date", 
  name = "areaName", 
  code = "areaCode", 
  cases = list(
    daily = "newCasesByPublishDate",
    cumulative = "cumCasesByPublishDate"
  ), 
  deaths = list(
    daily = "newDeaths28DaysByPublishDate",
    cumulative = "cumDeaths28DaysByPublishDate"
  )
)

# Create the structure as a list or a list of lists:
structure_death_tidy <- list(
  Date = "date", 
  Area = "areaType",
  Deaths_Increase = "newDeathsByDeathDate",
  Deaths = "cumDeathsByDeathDate"
)

structure <- structure_brokendown_oldmetric #structure_brokendown_oldmetric structure_brokendown #structure_death_tidy

# The "httr::GET" method automatically encodes 
# the URL and its parameters:
httr::GET(
  # Concatenate the filters vector using a semicolon.
  url = endpoint,
  
  # Convert the structure to JSON (ensure 
  # that "auto_unbox" is set to TRUE).
  query = list(
    filters = paste(filters, collapse = ";"),
    structure = jsonlite::toJSON(structure, auto_unbox = TRUE)
  ),
  
  # The API server will automatically reject any
  # requests that take longer than 10 seconds to 
  # process.
  timeout(100)
) -> response

# Handle errors:
if (response$status_code >= 400) {
  err_msg = httr::http_status(response)
  stop(err_msg)
}

# Convert response from binary to JSON:
json_text <- content(response, "text")
data = jsonlite::fromJSON(json_text)

print(data)

# Store the encoded URL for inspection:
url <- response$url

print(url)

# apiData <-bind_cols(Date=data$data$date,Deaths = data$data$deaths$cumulative)
# 
# %>%
#   rename(Date= date, Deaths = deaths.cumulative)
