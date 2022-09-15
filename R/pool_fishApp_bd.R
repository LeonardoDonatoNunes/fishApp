#' Conecta ao banco do fishApp
#'
#' @return
#' @export
#'
#' @examples
pool_fishApp_bd <- function() {
  
  conn <- pool::dbPool(
    drv = DBI::dbDriver("PostgreSQL"),
    host = "localhost",
    user = "postgres",
    password = "postgres",
    dbname = "fishapp",
    port = 5432
  )
  
  return(conn)
  
}
