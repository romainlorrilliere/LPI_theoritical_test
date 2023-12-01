library(RPostgreSQL)



 openDB.PSQL <- function(user="postgres",pw="postgres",DBname=NULL){
    ## --- initializing parameters for debugging ----
                                        #DBname=NULL;
                                        #user="romain" # windows
                                        #user = NULL # linux
                                        #  pw=NULL
    ## ---

    library(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")

    if(is.null(DBname)) {
        DBname <- "stoc_eps"
    }

    cat("\n",DBname,user,ifelse(is.null(pw),"","****"),"\n")
                                         # about when I use windows a have to define the user
      if(is.null(user)) {
         con <- dbConnect(drv, dbname=DBname)
      } else {
          con <- dbConnect(drv, dbname=DBname,user=user, password=pw)
    }

    return(con)
}


con <- openDB.PSQL()
