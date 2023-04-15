
################################################################
#### script to produce temporal series of population dynamics
################################################################





f_make_dyn_species <- function(nb_sp=1,N0_mean=NULL, N0_var = 0,K=100,K_method="exact",r_mean = 0.01,r_init_var = 0, r_temporal_var_mean = 0, nb_pop = 3, nb_year = 50, model_name = "ricker",demographic_stocha = "poisson", fig = "print",save_data=TRUE) {

    require(reshape2)
    require(data.table)

    for(sp in 1:nb_sp){
        if(is.list(N0_mean)) N0_mean_sp  <-  N0_mean[[sp]] else N0_mean_sp <- N0_mean
        if(is.list(N0_var)) N0_var_sp  <-  N0_var[[sp]] else N0_var_sp <- N0_var
        if(is.list(K)) K_sp <- K[[sp]] else K_sp  <- K
        if(is.list(K_method)) K_method_sp <- K_method[[sp]] else K_method_sp  <- K_method
        if(is.list(r_mean)) r_mean_sp <- r_mean[[sp]] else r_mean_sp  <- r_mean
        if(is.list(r_init_var)) r_init_var_sp <- r_init_var[[sp]] else r_init_var_sp  <- r_init_var
       if(is.list(r_temporal_var_mean)) r_temporal_var_mean_sp <- r_temporal_var_mean[[sp]] else r_temporal_var_mean_sp  <- r_temporal_var_mean
        if(is.list(nb_pop)) nb_pop_sp <- nb_pop[[sp]] else nb_pop_sp  <- nb_pop
        if(is.list(nb_year)) nb_year_sp <- nb_year[[sp]] else nb_year_sp  <- nb_year
        if(is.list(model_name)) model_name_sp <- model_name[[sp]] else model_name_sp  <- model_name
        if(is.list(demographic_stocha)) demographic_stocha_sp <- demographic_stocha[[sp]] else demographic_stocha_sp  <- demographic_stocha


        l_sp <-  f_make_pop_dyn(nb_pop = nb_pop_sp, N0_mean = N0_mean_sp,N0_var = N0_var_sp,K=K_sp,K_method=K_method_sp,r_mean = r_mean_sp, r_init_var = r_init_var_sp,r_temporal_var_mean = r_temporal_var_mean_sp,nb_year = nb_year_sp,model_name = model_name_sp,demographic_stocha = demographic_stocha_sp, fig="")

        pop_sp <- l_sp$N
        d_pop_sp <- melt(pop_sp)
        colnames(d_pop_sp) <- c("year","pop","N")
        setDT(d_pop_sp)
        d_pop_sp[,sp := paste0(sp)]
        d_pop_sp[,pop := as.factor(paste0(sp,"_",pop))]


        if(sp == 1) d_pop <- d_pop_sp else d_pop <- rbind(d_pop,d_pop_sp)


}

    if(fig %in% c("save","print","both")) {

        require(reshape2)
        require(data.table)
        require(ggplot2)



        if(nb_sp> 10) d_pop[,sp := as.numeric(sp)]
        gg <- ggplot(data = d_pop,mapping=aes(x=year,y=N,colour=sp,group=pop,shape = (N==0)))
     ##   gg <- gg + geom_hline(yintercept = 0,size=2,colour="white")
        gg <- gg + geom_point() + geom_line()
        gg <- gg + scale_shape_manual(values=c(19,21))

        if(fig %in% c("save","both")) {
            gg_file <- paste0("output/dyn_sps",format(Sys.time(),'_%Y%m%d_%H%M%S'),".png")
            ggsave(gg_file,gg)
        }
        if(fig %in% c("print","both") )
            print(gg)

    }


    if(save_data) fwrite(d_pop,paste0("output/dyn_sps",format(Sys.time(),'_%Y%m%d_%H%M%S'),".csv"))
    return(d_pop)




}




##
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title f_make_pop_dyn function to produce population temporal series according to several paramters
##' @param N0_mean [num] mean of initial abundance
##' @param N0_var [num] variance of initial abundance
##' @param K [num] carryinf capacity
##' @param K_method ["runif"] if the K should be picked in uniform distribution between K/10 and K
##' @param r_mean [num] mean of replacement rate
##' @param r_init_var [num] initial between population variance of replacement rate
##' @param r_temporal_var_mean [num] temporal variance of remplacement rate
##' @param nb_pop [num] number of population
##' @param nb_year [num] number of years of the temporal series
##' @param model_name [string %in% "beverton_holt","ricker","logistic"] name of the function used to simulate the population dynamics
##' @param demographic_stocha [string in "poisson", "round","none"] method to modelling the demographic stochasticity
##' @param fig ["print","save","both", ""] choose if you want produce a figure and if you want print it, save it or both.
##' @return a list with the populations abundance and their paramter that change in time.
##' @author Romain Lorrilliere
f_make_pop_dyn <- function(N0_mean=NULL, N0_var = 0,K=100,K_method="exact",r_mean = 0.01,r_init_var = 0, r_temporal_var_mean = 0, nb_pop = 3, nb_year = 50, model_name = "ricker",demographic_stocha = "poisson", fig = "print") {


    r0 <- rnorm(nb_pop,r_mean,r_init_var)
    rvar <- rnorm(nb_pop,r_temporal_var_mean,r_temporal_var_mean/10)
    pops_r <-  array(rnorm(nb_year * nb_pop,rep(r0,each=nb_year),rep(rvar,each=nb_year)),dim=c(nb_year,nb_pop))
    pops_r[1,] <- r0
    pops_r <- round(pops_r,4)
    if(model_name == "beverton_holt") pops_r[pops_r < 0]  <-  0

    if(K_method == "runif"){
      pops_Inf <- array(rep(ifelse(K != Inf,0,Inf),each=nb_year),dim=c(nb_year,nb_pop))
        K[K == Inf] <- 99999
        pops_K  <- array(rep(round(runif(nb_pop,K/10,K)),each=nb_year),dim=c(nb_year,nb_pop))
        pops_K <- pops_K + pops_Inf
    } else{
          pops_K <-  array(rep(K,each=nb_year),dim=c(nb_year,nb_pop))
    }

    if(is.null(N0_mean) & all(K != Inf))
        pops_N0 <- round(runif(nb_pop,1,pops_K[1,]/2)) else pops_N0 <- round(rnorm(nb_pop,N0_mean,N0_var))
    pops_N0[pops_N0 < 0]  <- 0

    pops_N <- array(NA,dim=c(nb_year,nb_pop))
    pops_N[1,] <- pops_N0

    l_pops <- list(N=pops_N,r=pops_r,K = pops_K)

    l_pops <- f_recursive_dyn_pop(l_pops,model_name,demographic_stocha)

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
##' @param demographic_stocha ["poisson", "round","none"] method to modelling the demographic stochasticity
##' @param t starting time
##' @return
##' @author Romain Lorrilliere
f_recursive_dyn_pop <- function(l,model_name = "ricker",demographic_stocha = "poisson",t=2) {

    ## solution de debogage pour les cas ou r déclin et N > K
  if(model_name == "beverton_holt") l$r[t,]<- ifelse(l$N[t-1,] > l$K[t-1,],abs(l$r[t,]-1)+1.1,l$r[t,]) else l$r[t,]<- ifelse(l$N[t-1,] > l$K[t-1,],abs(l$r[t,])+.1,l$r[t,])

  l$N[t,] <-  do.call(model_name,list(r = l$r[t,],K = l$K[t,],N=l$N[t-1,]))
  l$N[t,] <- ifelse(l$N[t,] < 0 , 0, l$N[t,])

    if(demographic_stocha == "poisson") l$N[t,] <- rpois(ncol(l$N),l$N[t,])
    else if(demographic_stocha == "round") l$N[t,] <- round(l$N[t,])
    else if(demographic_stocha == "none")l$N[t,] <-l$N[t,]

#  if(any(l$N[t,] > 3*l$K[t,]) & model_name == "beverton_holt") browser()

    if(t == nrow(l$N)) return(l) else l  <-  f_recursive_dyn_pop(l,model_name,demographic_stocha,t+1)
}




# r proliferation rate
beverton_holt <- function(N,r,K) return((r*N)/(1+(N*(r-1)/K)))
# r intrinsic growth rate
ricker <- function(N,r,K)  return(N*exp(r*(1-N/K)))

logistic <- function(N,r,K) return(N + (r*N*(1-N/K)))




