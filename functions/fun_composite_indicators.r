###
### some function to assess coposite biodivsersity indicators
###



### pop size

get_total <- function(X) sum(X)

get_median <- function(X) median(X)

get_quantile <- function(X,q) quantile(X,q)

get_braycurtis_tot <- function(N,N0) {
    DT <- data.table(N,N0)
    BC <- sum(apply(DT, 1, function(x) max(x)-min(x))) / sum(DT)
    return(BC)
}

get_a_mean <- function(DT,var = c("N","sp")){
    DTloc <- copy(DT)
    setnames(DTloc,var,c("x","sp"))
    return(mean(DTloc[,.(sum = sum(x)),by=sp][,sum],na.rm = TRUE))
}



get_g_mean <- function(DT,var = c("N","sp")){
    DTloc <- copy(DT)
    setnames(DTloc,var,c("x","sp"))
    return(mean(log(DTloc[,.(sum = sum(x)),by=sp][,sum]),na.rm = TRUE))
}







