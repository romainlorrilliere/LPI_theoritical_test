
################################################################
#### script to produce temporal series of population dynamics
################################################################




##
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title f_make_pop_dyn function to produce population temporal series according to several paramters
##' @param N0_mean [num] mean of initial abundance
##' @param N0_var [num] variance of initial abundance
##' @param r_mean [num] mean of replacement rate
##' @param r_init_var [num] initial variance of replacement rate
##' @param r_temporal_var [num] temporal variance of replacement rate
##' @param K_mean [num] mean of initial carrying capacity
##' @param K_init_var [num] initial variance of the carryng capacity
##' @param nb_pop [num] number of population
##' @param nb_year [num] number of years of the temporal series
##' @param fig ["print","save","both", ""] choose if you want produce a figure and if you want print it, save it or both.
##' @param demo_var ["poisson", ""] method to modelling the demographic stochasticity
##' @return a list with the populations abundance and their paramter that change in time.
##' @author Romain Lorrilliere
f_make_pop_dyn <- function(N0_mean=NULL, N0_var = 0,K=100,K_method="exact",r_mean = 0.01,r_init_var = 0, r_temporal_var_mean = 0, nb_pop = 3, nb_year = 50, model_name = "ricker",stocha_demo = "poisson", fig = "print") {


    r0 <- rnorm(nb_pop,r_mean,r_init_var)
    rvar <- rnorm(nb_pop,r_temporal_var_mean,r_temporal_var_mean/10)
    pops_r <-  array(rnorm(nb_year * nb_pop,rep(r0,each=nb_year),rep(rvar,each=nb_year)),dim=c(nb_year,nb_pop))
    pops_r[1,] <- r0
    pops_r <- round(pops_r,4)

    if(K_method == "exact"){
        pops_K <-  array(rep(K,each=nb_year),dim=c(nb_year,nb_pop))
    } else{
        pops_Inf <- array(rep(ifelse(K != Inf,0,Inf),each=nb_year),dim=c(nb_year,nb_pop))
        K[K == Inf] <- 99999
        pops_K  <- array(rep(round(runif(nb_pop,K/10,K)),each=nb_year),dim=c(nb_year,nb_pop))
        pops_K <- pops_K + pops_Inf
    }

    if(is.null(N0_mean) & all(K != Inf))
        pops_N0 <- round(runif(nb_pop,1,pops_K[1,]/2)) else pops_N0 <- round(rnorm(nb_pop,N0_mean,N0_var))
    pops_N0[pops_N0 < 0]  <- 0

    pops_N <- array(NA,dim=c(nb_year,nb_pop))
    pops_N[1,] <- pops_N0

    l_pops <- list(N=pops_N,r=pops_r,K = pops_K)

    l_pops <- f_recursive_dyn_pop(l_pops,model_name,stocha_demo)

    if(fig %in% c("save","print","both")) {

        require(reshape2)
        require(data.table)
        require(ggplot2)

        gg_pops <- NULL

        for(i in 1:length(l_pops)) {

            gg_pops_i <- reshape2::melt(l_pops[[i]])
            setDT(gg_pops_i)
            colnames(gg_pops_i) <- c("year","pop","val")
            gg_pops_i[,var := names(l_pops)[i]]

            gg_pops <- rbind(gg_pops,gg_pops_i)
        }

        gg_pops[,pop := as.factor(pop)]
        if(length(levels(gg_pops[,pop]))> 10) gg_pops[,pop := as.numeric(pop)]
        gg <- ggplot(data = gg_pops,mapping=aes(x=year,y=val,colour=pop,group=pop,shape = (val==0)))
     ##   gg <- gg + geom_hline(yintercept = 0,size=2,colour="white")
        gg <- gg + geom_point() + geom_line()
        gg <- gg + scale_shape_manual(values=c(19,21))
        gg <- gg + facet_grid(var~.,scales="free_y")

        if(fig %in% c("save","both")) {
            gg_file <- paste0("output/dyn_pops",format(Sys.time(),'_%Y%m%d_%H%M%S'),".png")
            ggsave(gg_file,gg)
        }
        if(fig %in% c("print","both") )
            print(gg)

    }
    return(l_pops)
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title f_recursive_dyn_pop recursive function to generate the temporal variation of abundance of population according to paramters
##' @param l the list with the abundance and the parameters
##' @param r_temp_var
##' @param demo_var ["poisson", ""] method to modelling the demographic stochasticity
##' @param t starting time
##' @return
##' @author
f_recursive_dyn_pop_2 <- function(l,r_temp_var,demo_var = "poisson",t=2) {
    l$r[t,] <- ifelse(l$N[t-1,] >2,rnorm(ncol(l$r),l$r[1,],r_temp_var),0)

    if(demo_var == "poisson") l$N[t,] <- rpois(ncol(l$N),exp(l$r[t,]) * l$N[t-1,])
     else l$N[t,] <- trunc(exp(l$r[t,]) * l$N[t-1,])

    if(t == nrow(l$N)) return(l) else t  <-  f_recursive_dyn_pop(l,r_temp_var, demo_var,t+1)
}



f_recursive_dyn_pop <- function(l,model_name = "ricker",demo_var = "poisson",t=2) {

  l$N[t,] <-  do.call(model_name,list(r = l$r[t,],K = l$K[t,],N=l$N[t-1,]))

    if(demo_var == "poisson") l$N[t,] <- rpois(ncol(l$N),l$N[t,])
    else if(demo_var == "real")l$N[t,] <-l$N[t,]
    else l$N[t,] <- trunc(l$N[t,])

    if(t == nrow(l$N)) return(l) else l  <-  f_recursive_dyn_pop(l,model_name, demo_var,t+1)
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


# r proliferation rate
beverton_holt <- function(N,r,K) return((r*N)/(1+((r-1)/K)*N))
# r intrinsic growth rate
ricker <- function(N,r,K)  return(N*exp(r*(1-N/K)))

logistic <- function(N,r,K) return(N + (r*N*(1-N/K)))




