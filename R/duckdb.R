init_duckdb <- function(dbname){
  tmpdir <- file.path(fs::path_home(), "helseprofil", "duck")
  if(!fs::dir_exists(tmpdir)) fs::dir_create(tmpdir)
  DBI::dbConnect(duckdb::duckdb(), dbdir = file.path(tmpdir, paste0(dbname, ".duckdb")))
}
