
###------------

library(data.table)
library(ggplot2)
library(glmmTMB)
library(ggeffects)
library(scales)

source("c:/git/LPI_theoritical_test/functions/fun_dyn_pop.r")
source("c:/git/LPI_theoritical_test/functions/fun_samples.r")
source("c:/git/LPI_theoritical_test/functions/fun_lpi.r")
source("c:/git/LPI_theoritical_test/functions/fun_composite_indicators.r")



###---------------

ny <- 50
nb_pop_sync <- 10
nb_rep_random <- 30





###-----------


tparam <- expand.grid(model_name = "uniform",N0 = c(10,50,100), K = Inf,r_mean = c(5,0,-5),demographic_stocha = "none",nb_pop = 1)
setDT(tparam)
tparam[,`:=`(id_sc = paste0("1_",1:.N),nb_sp = 1)]

d1 <- f_make_dyn_species(model_name = "uniform",demographic_stocha = "none",r_mean = tparam$r_mean,N0_mean = tparam$N0,K = tparam$K,nb_pop = nrow(tparam),nb_year = ny,fig="save")


setnames(d1,c("r","K"),c("r_pop","K_pop"))
d1[,`:=`(id_sc = pop, iteration = 1, id_iteration = paste0(pop,"_i1"))]

setkey(tparam,id_sc)
setkey(d1,id_sc)
d1 <- d1[tparam]
d1[,`:=`(sp = 1,pop = 1, id_sp = paste0(id_iteration,"_s",1),id_pop = paste0(id_iteration,"_s",1,"_p",1),sc_name = "uniforme",K_sp = K_pop,r_sp = r_pop,
         sync_cos_div = NA, sync_cos_time =  NA, sync_cos_power = NA , sync_sin_div = NA, sync_sin_time = NA, sync_sin_power = NA)]

theCol <- c("id_sc","sc_name","model_name","demographic_stocha","sync_cos_div", "sync_cos_time" , "sync_cos_power"  , "sync_sin_div" , "sync_sin_time" , "sync_sin_power", "nb_sp","nb_pop" ,"N0","K","r_mean","iteration","id_iteration","id_sp", "sp", "K_sp","r_sp", "id_pop" ,"pop" ,"K_pop","r_pop" ,"year","N")

d1 <- d1[,theCol,with=FALSE]

d <- copy(d1)


###----------------

tparam <- expand.grid(model_name = "logistic",N0 = c(10,50,100), K = Inf,r_mean = c(0.1,0,-0.1),demographic_stocha = "none",nb_pop = 1)
setDT(tparam)
tparam[,K := N0 * 10]
tparam[,`:=`(id_sc = paste0("1_",1:.N),nb_sp = 1)]


d1 <- f_make_dyn_species(model_name = "logistic",demographic_stocha = "none",r_mean = tparam$r_mean,N0_mean = tparam$N0,K = tparam$K,nb_pop = nrow(tparam),nb_year = ny,fig="save")


setnames(d1,c("r","K"),c("r_pop","K_pop"))
d1[,`:=`(id_sc = pop)]


setkey(tparam,id_sc)
setkey(d1,id_sc)
d1 <- d1[tparam]
d1[,id_sc := paste0(2,substr(id_sc,2,nchar(id_sc)))]
d1[,`:=`(iteration = 1, id_iteration = paste0(id_sc,"_i1"))]


d1[,`:=`(sp = 1,pop = 1, id_sp = paste0(id_iteration,"_s",1),id_pop = paste0(id_iteration,"_s",1,"_p",1),sc_name = "logistic",K_sp = K_pop,r_sp = r_pop,
         sync_cos_div = NA, sync_cos_time =  NA, sync_cos_power = NA , sync_sin_div = NA, sync_sin_time = NA, sync_sin_power = NA)]


theCol <- c("id_sc","sc_name","model_name","demographic_stocha","sync_cos_div", "sync_cos_time" , "sync_cos_power"  , "sync_sin_div" , "sync_sin_time" , "sync_sin_power", "nb_sp","nb_pop" ,"N0","K","r_mean","iteration","id_iteration","id_sp", "sp", "K_sp","r_sp", "id_pop" ,"pop" ,"K_pop","r_pop" ,"year","N")

d1 <- d1[,theCol,with=FALSE]


d <- rbind(d,d1)

###----------------



tparam <- expand.grid(model_name = "logistic",N0 = c(10,50,100), K = Inf,r_mean = c(0.1,0,-0.1),demographic_stocha = "none",nb_pop = nb_pop_sync)
setDT(tparam)
tparam[,K := N0 * 10]
tparam[,`:=`(id_sc = paste0("1_",1:.N),nb_sp = 1)]

sc <- 1
it <- 1


idsc <- paste0("3_",sc)
idit <- paste0(idsc,"_i",it)
idsp <- paste0(idit,"_s1")

###-----------------


d_sync <- data.table(year = seq(from=1,to= ny, by = 1))

d_sync[,`:=`(id_iteration = idit, p_cos_div = round( runif(1,0,1),2), p_cos_time = round( runif(1,1,4),1), p_cos_power = round(runif(1,1,3),2),
             p_sin_div = round( runif(1,0,1),2), p_sin_time = round( runif(1,1,4),1), p_sin_power = round(runif(1,1,3),2))]


d_sync[, r_periodic := round(rescale(cos(year *  p_cos_div) +
                                       cos(year *p_cos_time) +
                                       cos(year ^ p_cos_power) +
                                       sin(year *  p_sin_div) +
                                       sin(year *p_sin_time) +
                                       sin(year^p_sin_power),
                                     to=c(-0.25,0.25)),
                             3)]
print(head(d_sync))
r_sync<- d_sync[,r_periodic]

d_sync_all <- copy(d_sync)

###--------------

print(summary(r_sync))

gg <- ggplot(data= d_sync,aes(x=r_periodic)) + geom_histogram() + labs(x = "periodic additionnal effect on growht rate")
print(gg)
ggsave("output/sc_sync_1_hist.png",gg)



gg <- ggplot(data=d_sync, aes(x = year, y = r_periodic))+ geom_line() + geom_point()
eq_txt <- paste0("cos(",d_sync$p_cos_div[1]," * t) + cos(",d_sync$p_cos_time[1]," * t) + cos(t^",d_sync$p_cos_power[1],") + sin(",d_sync$p_sin_div[1]," * t) + sin(",d_sync$p_sin_time[1]," * t) + sin(t^",d_sync$p_sin_power[1],")")
gg <- gg +  labs(x = "year", y = "periodic additionnal effect on growth rate",title = idit, subtitle = eq_txt)

print(gg)
ggsave("output/sc_sync_1.png",gg)

###------------


di <-  f_make_pop_dyn(nb_pop = nb_pop_sync, N0_mean = tparam$N0[sc],N0_var = 0,K=tparam$K[sc],K_method="exact",r_mean = tparam$r_mean[sc], r_init_var = 0,r_temporal_var_mean = 0,r_synchro = r_sync,nb_year = ny,model_name = "logistic",demographic_stocha = "poisson", fig="save",format_output = "table")

setDT(di)
setnames(di,c("r","K"),c("r_pop","K_pop"))


di[,`:=`(id_sc =idsc,sc_name = "logistic_sync",model_name = "logistic",demographic_stocha = "poisson",
             sync_cos_div = d_sync$p_cos_div[1], sync_cos_time =  d_sync$p_cos_time[1], sync_cos_power = d_sync$p_cos_power[1] ,
             sync_sin_div = d_sync$p_sin_div[1], sync_sin_time = d_sync$p_sin_time[1], sync_sin_power = d_sync$p_sin_power[1],
             nb_sp = 1, nb_pop = tparam$nb_pop[sc], N0 = tparam$N0[sc],K = tparam$N0[sc],r_mean = tparam$r_mean[sc],
             iteration = it,id_iteration = idit, id_sp = idsp, sp = 1, id_pop = paste0(idsp,"_p",pop),K_sp = K_pop,r_sp = r_pop)]

theCol <- c("id_sc","sc_name","model_name","demographic_stocha","sync_cos_div", "sync_cos_time" , "sync_cos_power"  , "sync_sin_div" , "sync_sin_time" , "sync_sin_power", "nb_sp","nb_pop" ,"N0","K","r_mean","iteration","id_iteration","id_sp", "sp", "K_sp","r_sp", "id_pop" ,"pop" ,"K_pop","r_pop" ,"year","N")

di <- di[,theCol,with=FALSE]


d <- rbind(d,di)

for(sc in 2:nrow(tparam)) {

  di <-  f_make_pop_dyn(nb_pop = nb_pop_sync, N0_mean = tparam$N0[sc],N0_var = 0,K=tparam$K[sc],K_method="exact",r_mean = tparam$r_mean[sc], r_init_var = 0,r_temporal_var_mean = 0,r_synchro = r_sync,nb_year = ny,model_name = "logistic",demographic_stocha = "poisson", fig="",format_output = "table")

setDT(di)
setnames(di,c("r","K"),c("r_pop","K_pop"))

idsc <- paste0("3_",sc)
idit <- paste0(idsc,"_i",it)
idsp <- paste0(idit,"_s1")
di[,`:=`(id_sc =idsc,sc_name = "logistic_sync",model_name = "logistic",demographic_stocha = "poisson",
             sync_cos_div = d_sync$p_cos_div[1], sync_cos_time =  d_sync$p_cos_time[1], sync_cos_power = d_sync$p_cos_power[1] ,
             sync_sin_div = d_sync$p_sin_div[1], sync_sin_time = d_sync$p_sin_time[1], sync_sin_power = d_sync$p_sin_power[1],
             nb_sp = 1, nb_pop = tparam$nb_pop[sc], N0 = tparam$N0[sc],K = tparam$K[sc],r_mean = tparam$r_mean[sc],
             iteration = it,id_iteration = idit, id_sp = idsp, sp = 1, id_pop = paste0(idsp,"_p",pop),K_sp = K_pop,r_sp = r_pop)]

theCol <- c("id_sc","sc_name","model_name","demographic_stocha","sync_cos_div", "sync_cos_time" , "sync_cos_power"  , "sync_sin_div" , "sync_sin_time" , "sync_sin_power", "nb_sp","nb_pop" ,"N0","K","r_mean","iteration","id_iteration","id_sp", "sp", "K_sp","r_sp", "id_pop" ,"pop" ,"K_pop","r_pop" ,"year","N")

di <- di[,theCol,with=FALSE]


d <- rbind(d,di)


}



###--------------




for(it in 2:nb_rep_random) {

  idit <- paste0(idsc,"_i",it)
idsp <- paste0(idit,"_s1")

d_sync <- data.table(year = seq(from=1,to= ny, by = 1))

d_sync[,`:=`(id_iteration = idit,p_cos_div = round( runif(1,0,1),2), p_cos_time = round( runif(1,1,4),1), p_cos_power = round(runif(1,1,3),2),
             p_sin_div = round( runif(1,0,1),2), p_sin_time = round( runif(1,1,4),1), p_sin_power = round(runif(1,1,3),2))]


d_sync[, r_periodic := round(rescale(cos(year *  p_cos_div) +
                                       cos(year *p_cos_time) +
                                       cos(year ^ p_cos_power) +
                                       sin(year*  p_sin_div) +
                                       sin(year *p_sin_time) +
                                       sin(year ^p_sin_power),
                                     to=c(-0.25,0.25)),
                             3)]

r_sync<- d_sync[,r_periodic]

d_sync_all <- rbind(d_sync_all,d_sync)


for(sc in 1:nrow(tparam)) {

  di <-  f_make_pop_dyn(nb_pop = nb_pop_sync, N0_mean = tparam$N0[sc],N0_var = 0,K=tparam$K[sc],K_method="exact",r_mean = tparam$r_mean[sc], r_init_var = 0,r_temporal_var_mean = 0,r_synchro = r_sync,nb_year = ny,model_name = "logistic",demographic_stocha = "poisson", fig="",format_output = "table")

setDT(di)
setnames(di,c("r","K"),c("r_pop","K_pop"))

idsc <- paste0("3_",sc)
idit <- paste0(idsc,"_i",it)
idsp <- paste0(idit,"_s1")
di[,`:=`(id_sc =idsc,sc_name = "logistic_sync",model_name = "logistic",demographic_stocha = "poisson",
             sync_cos_div = d_sync$p_cos_div[1], sync_cos_time =  d_sync$p_cos_time[1], sync_cos_power = d_sync$p_cos_power[1] ,
             sync_sin_div = d_sync$p_sin_div[1], sync_sin_time = d_sync$p_sin_time[1], sync_sin_power = d_sync$p_sin_power[1],
             nb_sp = 1, nb_pop = tparam$nb_pop[sc], N0 = tparam$N0[sc],K = tparam$K[sc],r_mean = tparam$r_mean[sc],
             iteration = it,id_iteration = idit, id_sp = idsp, sp = 1, id_pop = paste0(idsp,"_p",pop),K_sp = K_pop,r_sp = r_pop)]

theCol <- c("id_sc","sc_name","model_name","demographic_stocha","sync_cos_div", "sync_cos_time" , "sync_cos_power"  , "sync_sin_div" , "sync_sin_time" , "sync_sin_power", "nb_sp","nb_pop" ,"N0","K","r_mean","iteration","id_iteration","id_sp", "sp", "K_sp","r_sp", "id_pop" ,"pop" ,"K_pop","r_pop" ,"year","N")

di <- di[,theCol,with=FALSE]


d <- rbind(d,di)


}


}

fwrite(d,"data/data_dyn.csv")
fwrite(d_sync,"data/data_sync.csv")

###--------------


gg <- ggplot(d_sync_all,aes(x=year,y=r_periodic))+ facet_wrap(.~id_iteration)
gg <- gg + geom_line()
gg
ggsave("output/sc_dyn_sync.png",gg)

###--------------

nbsc <- length(unique(d$id_sc))
nbit <- length(unique(d$id_iteration))

###-------------

gg <- ggplot(data = d, aes(x =year, y = N, group = id_pop)) + geom_line(alpha = 0.6) + facet_wrap(.~id_sc,scale="free_y")
gg
ggsave("output/sc_dyn_scenario.png",gg)

###-----------


