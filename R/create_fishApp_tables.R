#' Cria as tabelas do banco de dados
#'
#' @param con 
#'
#' @return tables
#' @export
#'
#' @examples create_fishApp_tables(con)
create_fishApp_tables <- function(con) {
  
  query_list <- list(
    
    projetos = "
    CREATE TABLE projetos (
      id SERIAL PRIMARY KEY,
      nome VARCHAR NOT NULL,
      sigla VARCHAR,
      cidade VARCHAR NOT NULL,
      estado VARCHAR,
      pais VARCHAR,
      lat DOUBLE PRECISION NOT NULL,
      lon DOUBLE PRECISION NOT NULL
    );
    ",
    
    pessoas = "
    CREATE TABLE  pessoas (
      id SERIAL PRIMARY KEY,
      nome VARCHAR NOT NULL,
      cargo VARCHAR NOT NULL
    );
    ",
    
    pessoa_projeto = "
    CREATE TABLE  pessoa_projeto (
      pessoa_id int REFERENCES pessoas (id) ON UPDATE CASCADE ON DELETE CASCADE,
      projeto_id int REFERENCES projetos (id) ON UPDATE CASCADE ON DELETE CASCADE,
      CONSTRAINT pessoa_projeto_pkey PRIMARY KEY (pessoa_id, projeto_id)
    );
    ",
    
    equipamentos = "
    CREATE TABLE  equipamentos (
      id SERIAL PRIMARY KEY,
      projeto_id INTEGER NOT NULL,
      numero_serie VARCHAR NOT NULL,
      tipo VARCHAR NOT NULL,
      marca VARCHAR NOT NULL,
      modelo VARCHAR NOT NULL,
      CONSTRAINT equipamento_projeto_FK FOREIGN KEY (projeto_id) REFERENCES projetos(id) ON DELETE CASCADE ON UPDATE CASCADE
    );
    ",
    
    tipo_local = "
    CREATE TABLE  tipo_local (
      id SERIAL PRIMARY KEY,
      nome VARCHAR NOT NULL,
      descricao VARCHAR NOT NULL
    );",
    
    locais = "
    CREATE TABLE  locais (
      id SERIAL PRIMARY KEY,
      projeto_id INTEGER NOT NULL, 
      nome VARCHAR NOT NULL,
      descricao VARCHAR NOT NULL,
      tipo_id INTEGER NOT NULL,
      lat DOUBLE PRECISION NOT NULL,
      lon DOUBLE PRECISION NOT NULL,
      CONSTRAINT locais_projeto_pkey FOREIGN KEY (projeto_id) REFERENCES projetos(id) ON DELETE CASCADE ON UPDATE CASCADE,
      CONSTRAINT local_tipo_pkey FOREIGN KEY (tipo_id) REFERENCES tipo_local(id) ON DELETE CASCADE ON UPDATE CASCADE

    );
    ",
    
    peixes = "
    CREATE TABLE  peixes (
      id SERIAL PRIMARY KEY,
      especie VARCHAR NOT NULL,
      nome_comum VARCHAR,
      genero VARCHAR,
      familia VARCHAR
    );
    ",
    
    marcacao = "
    CREATE TABLE  marcacao (
      id SERIAL PRIMARY KEY,
      pessoa_id INTEGER NOT NULL,
      peixe_id INTEGER NOT NULL,
      marcador_id INTEGER NOT NULL,
      pescador_id INTEGER NOT NULL,
      transmissor_id VARCHAR NOT NULL,
      frequencia DOUBLE PRECISION NOT NULL,
      canal INTEGER,
      radio_id INTEGER,
      acustica_id INTEGER,
      tbar_id  VARCHAR NOT NULL,
      data_hora_captura TIMESTAMP NOT NULL,
      data_hora_soltura TIMESTAMP NOT NULL,
      local_captura_id INTEGER NOT NULL,
      local_soltura_id INTEGER NOT NULL,
      CONSTRAINT marcacao_peixes_FK FOREIGN KEY (peixe_id) REFERENCES peixes(id) ON DELETE CASCADE ON UPDATE CASCADE,
      CONSTRAINT marcacao_pessoas_FK_1 FOREIGN KEY (marcador_id) REFERENCES pessoas(id) ON DELETE CASCADE ON UPDATE CASCADE,
      CONSTRAINT marcacao_pessoas_FK_2 FOREIGN KEY (pescador_id) REFERENCES pessoas(id) ON DELETE CASCADE ON UPDATE CASCADE,
      CONSTRAINT marcacao_locais_FK_1 FOREIGN KEY (local_captura_id) REFERENCES locais(id) ON DELETE CASCADE ON UPDATE CASCADE,
      CONSTRAINT marcacao_locais_FK_2 FOREIGN KEY (local_soltura_id) REFERENCES locais(id) ON DELETE CASCADE ON UPDATE CASCADE
    );
    ",
    
    bases_fixas = "
    CREATE TABLE  bases_fixas (
      id SERIAL PRIMARY KEY,
      projeto_id INTEGER NOT NULL,
      nome_base VARCHAR NOT NULL,
      local_id INTEGER NOT NULL,
      equipamento_id INTEGER NOT NULL,
      data_hora_ini TIMESTAMP NOT NULL,
      data_hora_fim TIMESTAMP,
      CONSTRAINT bases_fixas_projeto_FK FOREIGN KEY (projeto_id) REFERENCES projetos(id) ON DELETE CASCADE ON UPDATE CASCADE,
      CONSTRAINT bases_fixas_local_FK FOREIGN KEY (local_id) REFERENCES locais(id) ON DELETE CASCADE ON UPDATE CASCADE,
      CONSTRAINT equipamentos_local_FK FOREIGN KEY (equipamento_id) REFERENCES equipamentos(id) ON DELETE CASCADE ON UPDATE CASCADE
    );
    ",
    
    base_equipamento = "
    CREATE TABLE  base_equipamento (
      base_id INTEGER NOT NULL,
      equipamento_id INTEGER NOT NULL,
      CONSTRAINT base_equipamento_FK FOREIGN KEY (base_id) REFERENCES bases_fixas(id) ON DELETE CASCADE ON UPDATE CASCADE,
      CONSTRAINT base_equipamento_FK_1 FOREIGN KEY (equipamento_id) REFERENCES equipamentos(id) ON DELETE CASCADE ON UPDATE CASCADE
    );
    "
  )
  
  query_list %>% purrr::map(~DBI::dbGetQuery(con, .x))
  
}
