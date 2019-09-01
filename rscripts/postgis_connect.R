make_postgis_con <- function(mydb = NA, myuser = NA, host = NA, pwd = NA){
  require(RPostgreSQL)
  require(postGIStools)
  
  if(is.na(mydb)){mydb <- readline(prompt = 'dbname:')}
  if(is.na(myuser)){myuser <- readline(prompt = 'user:')}
  if(is.na(host)){host <- readline(prompt = 'host:')}
  if(is.na(pwd)){pwd <- readline(prompt = 'pwd:')}
  
  return(dbConnect(PostgreSQL(), dbname = mydb, user = myuser, host = host, password = pwd) ) 
}
