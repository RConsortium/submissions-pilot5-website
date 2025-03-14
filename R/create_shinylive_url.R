url_encode_dir <- function(dir,
                           language = c("auto", "r", "py"),
                           mode = c("editor", "app"),
                           hide_header = FALSE,
                           exclude = "rsconnect/") {
  stopifnot(fs::dir_exists(dir))
  
  language <- rlang::arg_match(language)
  mode <- rlang::arg_match(mode)
  hide_header <- hide_header && mode == "app"
  
  files <- fs::dir_ls(
    dir,
    recurse = TRUE,
    type = "file",
    regexp = exclude,
    invert = TRUE
  )
  files <- fs::path_abs(files)
  files <- files[!grepl("^(\\.|_)", fs::path_file(files))]
  
  rgx_app <- switch(language,
                    r = "(^app|(ui|server)).*\\.[Rr])",
                    py = "^app.+\\.py$",
                    auto = "^app|((ui|server)\\.[Rr])")
  
  idx_app <- which(grepl(rgx_app, fs::path_file(files)))
  if (length(idx_app) == 0) {
    stop(switch(
      language,
      r = "No app.R, ui.R or server.R found in ",
      py = "No app.py found in ",
      auto = "No app.R, app.py or ui/server.R found in "
    ), dir)
  }
  
  idx_app <- idx_app[1]
  path_root <- fs::path_dir(files[idx_app])
  files <- c(files[idx_app], files[-idx_app])
  
  if (language == "auto") {
    language <- tolower(fs::path_ext(files[1]))
  }
  
  names <- fs::path_rel(files, path_root)
  bundle <- unname(Map(as_file_list, files, names))
  bundle <- jsonlite::toJSON(bundle,
                             auto_unbox = TRUE,
                             null = "null",
                             na = "null")
  URI <- lzstring::compressToEncodedURIComponent(bundle)
  URI <- gsub("/", "-", URI)
  sprintf(
    "https://%s/%s/%s/#%scode=%s",
    getOption("shinylive.host", "shinylive.io"),
    language,
    mode,
    if (hide_header)
      "h=0&"
    else
      "",
    URI
  )
}

as_file_list <- function(path, name = fs::path_file(path), type = NULL) {
  if (is.null(type)) {
    ext <- tolower(fs::path_ext(path))
    type <- if (ext %in% text_file_extensions()) "text" else "binary"
  } else {
    rlang::arg_match(type, c("text", "binary"))
  }
  
  content <-
    if (type == "text") {
      read_utf8(path)
    } else {
      rlang::check_installed("base64enc", "for binary file encoding.")
      base64enc::base64encode(read_raw(path))
    }
  
  ret <- list(name = name, content = content)
  if (type == "binary") ret$type <- "binary"
  
  ret
}

text_file_extensions <- function() {
  c(
    "r", "rmd", "rnw", "rpres", "rhtml", "qmd",
    "py", "ipynb", "js", "ts", "jl",
    "html", "css", "scss", "less", "sass",
    "tex", "txt", "md", "markdown", "html", "htm",
    "json", "yml", "yaml", "xml", "svg",
    "sh", "bash", "zsh", "fish", "bat", "cmd",
    "sql", "csv", "tsv", "tab",
    "log", "dcf", "ini", "cfg", "conf", "properties", "env", "envrc",
    "gitignore", "gitattributes", "gitmodules", "gitconfig", "gitkeep",
    "htaccess", "htpasswd", "htgroups", "htdigest"
  )
}


raw_to_utf8 <- function(data) {
  res <- rawToChar(data)
  Encoding(res) <- "UTF-8"
  res
}

read_raw <- function(file) {
  readBin(file, "raw", n = file.info(file, extra_cols = FALSE)$size)
}

# Read file as UTF-8
read_utf8 <- function(file) {
  res <- read_raw(file)
  raw_to_utf8(res)
}

get_default_branch <- function(gh_owner,
                               gh_repo,
                               access_token = Sys.getenv("GITHUB_PAT")) {
  url <- glue::glue("https://api.github.com/repos/{owner}/{repo}")
  response <- httr::GET(url, httr::add_headers(Authorization = paste("token", access_token)))
  file_content <- httr::content(response, "parsed")
  file_content$default_branch
}

download_github_repo <- function(owner,
                                 repo,
                                 branch = NULL,
                                 destdir = NULL,
                                 access_token = Sys.getenv("GITHUB_PAT")) {
  # Create a temporary directory if none is provided
  if (is.null(destdir)) {
    destdir <- tempdir()
  }
  if (is.null(branch)) {
    branch <- get_default_branch(gh_owner = owner,
                                 gh_repo = repo,
                                 access_token = access_token)
  }
  # Create the zip file URL
  zip_url <- paste0("https://github.com/",
                    owner,
                    "/",
                    repo,
                    "/archive/",
                    branch,
                    ".zip")
  message("Downloading from: ", zip_url)
  # Create a temporary file for the zip
  temp_zip <- tempfile(fileext = ".zip")
  # Download the zip file
  download_status <- tryCatch({
    download.file(zip_url,
                  destfile = temp_zip,
                  mode = "wb",
                  quiet = TRUE)
    TRUE
  }, error = function(e) {
    message("Error downloading repository: ", e$message)
    FALSE
  })
  
  if (!download_status) {
    return(NULL)
  }
  # Extract location
  extract_dir <- file.path(destdir, paste0(repo, "-", branch))
  # Extract the zip file
  unzip_status <- tryCatch({
    unzip(temp_zip, exdir = destdir)
    TRUE
  }, error = function(e) {
    message("Error extracting zip file: ", e$message)
    FALSE
  })
  # Remove the temporary zip file
  unlink(temp_zip)
  
  if (!unzip_status) {
    return(NULL)
  }
  message("Repository successfully downloaded and extracted to: ",
          extract_dir)
  return(extract_dir)
}
