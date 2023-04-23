
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title f_recursive_dyn_pop recursive function to generate the temporal variation of abundance of population according to paramters
##' @param l the list with the abundance and the parameters
##' @param r_temp_var
##' @param demo_var ["poisson", "trunc","none"] method to modelling the demographic stochasticity
##' @param t starting time
##' @return
##' @author
f_recursive_dyn_pop_2 <- function(l,r_temp_var,demo_var = "poisson",t=2) {
    l$r[t,] <- ifelse(l$N[t-1,] >2,rnorm(ncol(l$r),l$r[1,],r_temp_var),0)

    if(demo_var == "poisson") l$N[t,] <- rpois(ncol(l$N),exp(l$r[t,]) * l$N[t-1,])
     else l$N[t,] <- trunc(exp(l$r[t,]) * l$N[t-1,])

    if(t == nrow(l$N)) return(l) else t  <-  f_recursive_dyn_pop(l,r_temp_var, demo_var,t+1)
}




f_loop_dyn_pop <- function(N0,r,K,nb_t = 10,model_name = "ricker",demo_var="poisson",fig=TRUE) {

    vecN <- rep(NA,nb_t)
    vecN[1] <- N0

    for(t in 2:nb_t) {
        vecN[t] <-  do.call(model_name,list(r =r,K = K,N=vecN[t-1]))
        if(demo_var == "poisson") vecN[t] <- rpois(1,vecN[t])
        else if(demo_var == "real") vecN[t] <- rpois(1,vecN[t])
        else vecN[t] <- trunc(vecN[t])
    }

    if(fig) {
        require(ggplot2)
        vecN <- data.frame(year = 1:nb_t,N=vecN)
        gg <- ggplot(vecN,aes(x=year,y=N))+ geom_line()+ geom_point()
        print(gg)
    }

}

