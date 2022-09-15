#' Cria o banco de dados
#'
#' @param con 
#'
#' @return data base created
#' @export
#'
#' @examples create_fishApp_bd(con)
create_fishApp_bd <- function(con) {
  
  DBI::dbGetQuery(
    con,
    "DROP DATABASE IF EXISTS fishApp;"
  )
  
  DBI::dbGetQuery(
    con,
    "CREATE DATABASE fishApp
    WITH
    OWNER = postgres
    ENCODING = 'UTF8'
    LC_COLLATE = 'Portuguese_Brazil.1252'
    LC_CTYPE = 'Portuguese_Brazil.1252'
    TABLESPACE = pg_default
    CONNECTION LIMIT = -1
    IS_TEMPLATE = False;
    "
  )
  
  DBI::dbGetQuery(
    con,
    "
    COMMENT ON DATABASE fishApp
    IS 'Banco de dados da aplica??o fishApp';
    "
  )
  
  
}
