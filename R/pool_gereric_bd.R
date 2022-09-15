#' Cria uma conexão generica
#'
#' @return bd_pool
#' @export
#'
#' @examples pool_generic_bd()
pool_generic_bd <- function() {
  
  conn <- dbPool(
    drv = dbDriver("PostgreSQL"),
    host = "localhost",
    user = "postgres",
    password = "postgres",
    port = 5432
  )
  
  return(conn)
  
}
