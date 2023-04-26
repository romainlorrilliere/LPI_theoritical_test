library(data.table)
library(ggplot2)
library(ggeffects)

source("c:/git/LPI_theoritical_test/functions/fun_dyn_pop.r")
source("c:/git/LPI_theoritical_test/functions/fun_samples.r")
source("c:/git/LPI_theoritical_test/functions/fun_lpi.r")


# dyn 10 pop
r_tempvar <- 0
demographic_stocha <- "poisson"

d_pop  <- f_make_dyn_species(nb_sp = nb_sp,nb_pop = 10, N0_mean = N_mean,N0_var = N_sd,K=K,K_method="exact",r_mean = r, r_init_var = r_var,r_temporal_var_mean = r_tempvar,nb_year = nbyear,model_name = model_dyn,demographic_stocha = demographic_stocha, fig="print")

d_pop[,sp := as.character(sp)]
tab_param[,sp := as.character(sp)]
d_pop <- merge(d_pop,tab_param[,.(sp,K,r,N0,N0K)],by = c("sp"))

d_pop[,Nobs := N]


# info extinction local


d_extinction_pop <- d_pop[year == 30 ,.(extinct =  N == 0),by = .(pop)]

d_extinction <- d_pop[year == 30 ,.(extinct =  as.numeric(N == 0)),by = .(pop,sp)][,.(extinct =sum(extinct)/10),by = sp]
d_extinction_year <- d_pop[,.(extinct_year =  as.numeric(N == 0)),by = .(pop,sp,year)][,.(extinct_year =sum(extinct_year)/10),by = .(sp,year)]
d_extinction[,sp := as.character(sp)]
d_extinction_year[,sp := as.character(sp)]

d_pop <- merge(d_pop,d_extinction_pop,by="pop")
