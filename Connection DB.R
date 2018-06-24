library(RMySQL)
library(DBI)

con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "web154_db19", 
                 host = "", 
                 port = 3306,
                 user = "web154_19",
                 password = "PJS2018!")

con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "tweater", 
                 host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                 port = 3306,
                 user = "xx",
                 password = "datacamp")

# List tables
dbListTables(con)

#Read from a table
#dbReadTable(con, "employees")

#Read many tables
# tables <- lapply(table_names, dbReadTable, conn = con)

#If you only need a fraction of the Data use SQL-Querys:
# elisabeth <- dbGetQuery(con, "SELECT tweat_id FROM comments WHERE user_id = 1")

#Wenn man nicht alle Ergebnisse einer Abfrage auf einmal will, sondern stückchenweise, dann
#kann man das folgendermaßen tun:
# 1. Send query to the DB:
  # res <- dbSendQuery(con, "SELECT * FROM comments WHERE user_id > 4")
# 2. Use dbFetch() twice 
  # 

#Always disconnect to DB when work is finished
#dbDisconnect(con)
