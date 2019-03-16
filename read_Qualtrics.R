load_pkgs <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  suppressPackageStartupMessages(sapply(pkg,
                                        require, character.only = TRUE))
}

load_pkgs(c("httr", "jsonlite", 'readr', 'Hmisc', 'dplyr', 'docstring'))

"Parse HTTP response from Qualtrics and return 'result' field
from content"
get_result_from_HTTP <- function(res) {
  return (fromJSON(rawToChar(res$content))[["result"]])
}

"Check if HTTP response is invalid and print error message if needed"
assert_status_OK <- function(res, msg) {
  if (res$status_code != 200) {
    stop(msg)
  }
}

"Read survey results to zip file containg csv"
download_ZIP <- function(dataCenter, surveyID, API_token) {
  base_url <- paste0("https://",
                     dataCenter,
                     "/API/v3/surveys/",
                     surveyID,
                     "/export-responses")
  base_headers <- add_headers ("X-API-TOKEN" = API_token,
                               "Content-Type" = "application/json")
  resp <-
    POST(base_url, body = '{"format":"csv"}', base_headers)
  assert_status_OK(
    resp,
    "Failed to POST export (maybe wrong Qualtrics arguments
    (Data Center / survey ID / API token))?"
    )
  
  progressId <- get_result_from_HTTP(resp)$`progressId`
  prog <- 0
  while (prog != 100)
  {
    resp <-
      GET(paste(base_url, progressId, sep = "/"), base_headers)
    assert_status_OK(resp, "Failed to GET progress")
    prog <- get_result_from_HTTP(resp)$`percentComplete`
    
  }
  fileId <- get_result_from_HTTP(resp)$`fileId`
  resp <-
    GET(paste(base_url, fileId, "file", sep = "/"),
        add_headers ("X-API-TOKEN" = API_token))
  assert_status_OK(resp, "Failed to GET file")
  writeBin(resp$content, "QUALTRIX.zip", useBytes = TRUE)
  
}

"Extract csv from zip and load to tibble format"
load_tibble <- function (file_name) {
  df_names1 <-
    read_csv(file_name, n_max = 0, col_types = cols()) %>% names()
  df_names2 <-
    read_csv(
      file_name,
      n_max = 2,
      col_types = cols(),
      col_names = FALSE
    )[2, ]
  df        <-
    read_csv(file_name,
             skip = 3,
             col_types = cols(),
             col_names = df_names1)
  for (i in seq_along(df)) {
    Hmisc::label(df[, i]) <- df_names2[i]
  }
  return(df)
}


read_Qualtrics <- function(dataCenter, surveyID, API_token) {
  #' Reading data from Qualtrics
  #'
  #' Get survey results in tibble format and try to delete zip
  #' @param dataCenter qualtrics hostname for your organization (e.g., "az1.qualtrics.com")
  #' @param surveyID the survey ID code starting with SV_ (e.g., "SV_czG0xcwKradWRyB")
  #' @param API_token API token of qualtrics account
  download_ZIP(dataCenter, surveyID, API_token)
  file_name <- unzip("QUALTRIX.zip")
  res <- load_tibble(file_name)
  # Only works if R has the required permissions
  try(file.remove(file_name))
  try(file.remove('QUALTRIX.zip'))
  return(res)
}
