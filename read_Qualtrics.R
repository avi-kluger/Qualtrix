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


read_Qualtrics <- function(surveyID, 
                           dataCenter = Sys.getenv("dataCenter"), 
                           API_token  = Sys.getenv("api_key")) {
  #' Reading data from Qualtrics
  #'
  #' Get survey results in tibble format and try to delete zip
  #' @usage read_Qualtrics (surveyID, 
  #'                        dataCenter = Sys.getenv("dataCenter"), 
  #'                        API_token  = Sys.getenv("api_key"))
  #' @param surveyID the survey ID code starting with SV_ (e.g., "SV_czG0xcwKradWRyB")
  #' @param dataCenter qualtrics hostname for your organization (e.g., "az1.qualtrics.com").  The default is dataCenter = Sys.getenv("dataCenter")
  #' @param API_token API token of qualtrics account.  The default is API_token  = Sys.getenv("api_key")
  #' @note The defaults could become useful, if dataCenter and API_Token are set once in the environment.
  #' @author  This function was written by Avi Kluger \email{avik@@savion.huji.ac.il} with help from Nadav Kluger.
  #' @examples
  #' 
  #' # Not run:
  #' df <- read_Qualtrics(surveyID = "SV_xxxxxxxxxxxxxxx", 
  #'                      dataCenter = "xxxx.qualtrics.com", 
  #'                      API_token  = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
  #'                   
  #' # Set dataCenter and API key in global environment once
  #' Sys.setenv("dataCenter" = "az1.qualtrics.com")
  #' Sys.setenv("api_key"    = "<replace with API token>")
  #' 
  #' # Use the function's default and supply only the first argument
  #' df <- read_Qualtrics("SV_e9W83Izkp3gY9Q9")
  #' 
  #' # End(**Not run**)
  download_ZIP(surveyID, dataCenter = Sys.getenv("dataCenter"), 
                           API_token  = Sys.getenv("api_key"))
  file_name <- unzip("QUALTRIX.zip")
  res <- load_tibble(file_name)
  # Only works if R has the required permissions
  try(file.remove(file_name))
  try(file.remove('QUALTRIX.zip'))
  return(res)
}
? read_Qualtrics
