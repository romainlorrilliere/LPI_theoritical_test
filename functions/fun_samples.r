f_samples_pop <- function(pops,
                           start_mean = 10, start_var = 10,
                           duration_mean = 20, duration_var = 10,
                           completude_mean = 0.5, completude_var = 0.5,
                           proba_obs_mean = 0.5, proba_obs_var = 0.2,
                           fig = "print") {

    ## start_mean <- 10;start_var <- 10;duration_mean <- 20;duration_var <- 10;completude_mean <- 0.5
    ## completude_var <- 0.5;proba_obs_mean <- 0.5;proba_obs_var <- 0.2

    yearmax <- max(pops[,year])
    npop <- length(unique(pops$pop))
    start_sample <- round(rnorm(npop,start_mean,start_var))
    start_sample[start_sample<0] <- 0
    start_sample[start_sample > yearmax] <- yearmax

    duration <- round(rnorm(npop,duration_mean,duration_var))
    duration[duration<0] <- 0
    end_sample <- start_sample + duration - 1
    end_sample[end_sample > yearmax] <- yearmax
    duration <- end_sample - start_sample + 1

    completude_distrib <- rnorm(npop*100,completude_mean,completude_var)
    completude_distrib <- completude_distrib[completude_distrib >= 0 & completude_distrib <= 1]
    completude <- round(sample(completude_distrib,npop),2)

    proba_obs_distrib <- rnorm(npop*100,proba_obs_mean,proba_obs_var)
    proba_obs_distrib <- proba_obs_distrib[proba_obs_distrib >= 0 & proba_obs_distrib <= 1]
    proba_obs <- round(sample(proba_obs_distrib,npop),2)



    sample <- data.table(pop=unique(pops[,pop]),start_sample,end_sample,duration,completude,proba_obs)


    pops <- merge(pops,sample,by="pop")


    pops[,`:=`(sample = ifelse(year == start_sample | year == end_sample,1,
                        ifelse(year > start_sample & year < end_sample,
                               rbinom(nrow(pops),1,completude),0))
             , Nobs = ifelse(proba_obs == 1 ,N,rbinom(nrow(pops),N,proba_obs)))]
    pops[sample == 0, Nobs:= NA]

    if(fig %in% c("save","print","both")) {
        require(ggplot2)


        ggpop <- pops[,.(pop,year,N)]
        ggpop[,var:= "N"]

        ggobs <- pops[,.(pop,year,Nobs)]
        ggobs[,var:= "Nobs"]
        setnames(ggobs,"Nobs","N")

        gg_samples <- rbind(ggpop,ggobs)

        gg_samples <- gg_samples[!is.na(N),]



        xmin <- max(0,min(gg_samples[var=="Nobs",year])-2)
        xmax <- min(yearmax,max(gg_samples[var=="Nobs",year])+2)
        gg_samples <- gg_samples[year >= xmin-2 & year <= xmax+2,]
        gg_samples[,pop := as.factor(pop)]
        if(length(levels(gg_samples[,pop]))> 10) gg_samples[,pop := as.numeric(pop)]
        gg <- ggplot(data= gg_samples,mapping=aes(x=year,y=N,group=pop,colour = pop,shape = (N==0)))
        gg <- gg + facet_grid(var~.,scales="free_y")
       ## gg <- gg + geom_hline(yintercept = 0,size=2,colour="white")
        gg <- gg + geom_line(size=1,alpha=0.7)+ geom_point(fill = "white", size=2,alpha=0.9)
    ##    gg <- gg + geom_point(data = gg_samples[N==0,],colour="white",size=0.8)
        gg <- gg + coord_cartesian(xlim = c(xmin, xmax))
        gg <- gg + scale_shape_manual(values=c(19,21))

        if(fig %in% c("save","both")) {
            gg_file <- paste0("output/dyn_pops",format(Sys.time(),'_%Y%m%d_%H%M%S'),".png")
            ggsave(gg_file,gg)
        }
        if(fig %in% c("print","both") )
            print(gg)

    }
    return(pops)
}
