#' Testa se o banco do fishApp existe
#'
#' @return
#' @export
#'
#' @examples
teste_existencia_bd <- function() {
  
  teste <-
    DBI::dbCanConnect(
      drv = dbDriver("PostgreSQL"),
      host = "localhost",
      user = "postgres",
      password = "postgres",
      dbname = "fishapp",
      port = 5432
    )
  
  return(teste)
  
}
