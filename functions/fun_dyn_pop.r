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
##' @param nb_pop [num] number of population
##' @param nb_year [num] number of years of the temporal series
##' @param fig ["print","save","both", ""] choose if you want produce a figure and if you want print it, save it or both.
##' @param demo_var ["poisson", ""] method to modelling the demographic stochasticity
##' @return a list with the populations abundance and their paramter that change in time.
##' @author Romain Lorrilliere
f_make_pop_dyn <- function(N0_mean=100, N0_var = 0,r_mean = 0.01,r_init_var = 0, r_temporal_var = 0, nb_pop = 3, nb_year = 50, demo_var = "poisson", fig = "print") {

    pops_N0 <- round(rnorm(nb_pop,N0_mean,N0_var))
    pops_N0[pops_N0 < 0]  <- 0

    pops_N <- array(NA,dim=c(nb_year,nb_pop))
    pops_N[1,] <- pops_N0

    pops_r <-  array(NA,dim=c(nb_year,nb_pop))
    pops_r[1,] <- rnorm(nb_pop,r_mean,r_init_var)

    l_pops <- list(N=pops_N,r=pops_r)

    l_pops <- f_recursive_dyn_pop(l_pops,r_temp_var = r_temporal_var,demo_var)

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
f_recursive_dyn_pop <- function(l,r_temp_var,demo_var = "poisson",t=2) {
    l$r[t,] <- ifelse(l$N[t-1,] >2,rnorm(ncol(l$r),l$r[1,],r_temp_var),0)

    if(demo_var == "poisson") l$N[t,] <- rpois(ncol(l$N),exp(l$r[t,]) * l$N[t-1,])
     else l$N[t,] <- trunc(exp(l$r[t,]) * l$N[t-1,])

    if(t == nrow(l$N)) return(l) else t  <-  f_recursive_dyn_pop(l,r_temp_var, demo_var,t+1)
}

