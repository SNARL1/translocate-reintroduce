# Connect to PostgreSQL database

con = dbConnect(dbDriver("Postgres"),
                host = Sys.getenv("host"), 
                port = Sys.getenv("port"), 
                dbname = Sys.getenv("dbname"),
                user = rstudioapi::askForPassword("user name"),
                password = rstudioapi::askForPassword("password")
                )

