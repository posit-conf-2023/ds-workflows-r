# If data.cityofchicago.org is down for the workshop, we can retrieve the raw data from the database


library(tidyverse)
library(RPostgres)
library(DBI)

con <- dbConnect(RPostgres::Postgres(), 
                 host = Sys.getenv("CONF23_DB_HOST"), 
                 port = "5432", 
                 dbname = "conf23_r", 
                 user = Sys.getenv("CONF23_DB_USER"), 
                 password = Sys.getenv("CONF23_DB_PASSWORD"))


inspections_raw <- tbl(con, "inspections_raw") |> collect()
