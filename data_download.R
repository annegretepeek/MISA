library(httr)
library(jsonlite)

# ----------------------------
# Configuration
# ----------------------------

owner <- "annegretepeek"
repo  <- "EMTA_data"
branch <- "main"

token <- Sys.getenv("GITHUB_PAT")

dest_root <- "data"

# ----------------------------
# Helpers
# ----------------------------

github_headers <- add_headers(
  Authorization = paste("Bearer", token),
  Accept = "application/vnd.github+json"
)

download_file <- function(repo_path, dest_file) {
  
  raw_url <- paste0(
    "https://raw.githubusercontent.com/",
    owner, "/", repo, "/", branch, "/", repo_path
  )
  
  dir.create(dirname(dest_file),
             recursive = TRUE,
             showWarnings = FALSE)
  
  res <- GET(raw_url, github_headers)
  
  stop_for_status(res)
  
  writeBin(
    content(res, "raw"),
    dest_file
  )
  
  message("Downloaded: ", repo_path)
}

download_parquet_folder <- function(repo_folder, dest_folder) {
  
  api_url <- paste0(
    "https://api.github.com/repos/",
    owner, "/", repo,
    "/contents/", repo_folder,
    "?ref=", branch
  )
  
  res <- GET(api_url, github_headers)
  
  stop_for_status(res)
  
  files <- fromJSON(
    content(res, "text", encoding = "UTF-8")
  )
  
  parquet_files <- files[
    files$type == "file" &
      grepl("\\.parquet$", files$name),
  ]
  
  dir.create(dest_folder,
             recursive = TRUE,
             showWarnings = FALSE)
  
  for(i in seq_len(nrow(parquet_files))) {
    
    download_file(
      parquet_files$path[i],
      file.path(dest_folder,
                parquet_files$name[i])
    )
  }
}

# ----------------------------
# Download single file
# ----------------------------

download_file(
  "data/apps/active_companies.parquet",
  file.path(dest_root,
            "active_companies.parquet")
)

# ----------------------------
# Download dataset folders
# ----------------------------

download_parquet_folder(
  "data/apps/company_year",
  file.path(dest_root,
            "company_year")
)

download_parquet_folder(
  "data/apps/emta_quarterly",
  file.path(dest_root,
            "emta_quarterly")
)