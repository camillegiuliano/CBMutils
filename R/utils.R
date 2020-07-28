#' readSqlFile
#'
#' DESCRIPTION NEEDED
#'
#' @param filePath Path to sqlite database file.
#'
#' @export
readSqlFile <- function(filePath) {
  fileconn <- file(filePath, "r")
  sqlString <- readLines(fileconn)
  sqlString <- paste(sqlString, collapse = " ")
  gsub("\t", "", sqlString)
  close(fileconn)
  return(sqlString)
}

#' query
#'
#' DESCRIPTION NEEDED
#'
#' @param dbPath Path to sqlite database file.
#' @param sql SQL statement with to execute in the database.
#'
#' @export
#' @importFrom RSQLite dbConnect dbDisconnect dbGetQuery
query <- function(dbPath, sql) {
  con <- dbConnect(dbDriver("SQLite"), dbPath)
  table <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return(table)
}

#' getTable
#'
#' DESCRIPTION NEEDED
#'
#' @param filename DESCRIPTION NEEDED
#' @param dbPath DESCRIPTION NEEDED
#' @param sqlDir DESCRIPTION NEEDED
#'
#' @export
#' @importFrom RSQLite dbConnect dbDisconnect
getTable <- function(filename, dbPath, sqlDir) {
  con <- dbConnect(dbDriver("SQLite"), dbPath)
  filePath <- file.path(sqlDir, filename)
  table <- query(dbPath, readSqlFile(filePath))
  dbDisconnect(con)
  return(table)
}

#' Hashing functions
#'
#' DESCRIPTION NEEDED
#'
#' @param x DESCRIPTION NEEDED
#'
#' @export
#' @rdname hash
hash <- function(x) {
  e <- new.env(
    hash = TRUE, size = nrow(x),
    parent = emptyenv()
  )
  apply(x, 1, function(col) {
    assign(toString(col[1]), col[2:length(col)], envir = e)
  })

  return(e)
}

#' @export
#' @rdname hash
matrixHash <- function(x) {
  keys <- unique(x[, 1])
  e <- new.env(hash = TRUE, size = length(keys), parent = emptyenv())
  apply(as.matrix(keys), 1, function(key) {
    assign(toString(key), x[x[, 1] == key, 2:ncol(x)], envir = e)
  })
  return(e)
}

#' getIdentityCoordinateMatrix
#'
#' DESCRIPTION NEEDED
#'
#' @param size Numeric indicating the number of rows and columns in the matrix.
#' @export
getIdentityCoordinateMatrix <- function(size) {
  return(cbind(1:size, 1:size, rep(1.0, size)))
}
