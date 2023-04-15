## script to test the cluster



if(!("functions" %in% dir())) setwd("..")

source("functions/fun_dyn_pop.r")
source("functions/fun_samples.r")
source("functions/fun_lpi.r")


library(data.table)
library(ggplot2)
#library(ggeffects)



K <- 1000
r <- 1
N_mean <- 999
N_sd <- 0
npop <- 50
demographic_stocha <- "none"
nbyear <- 30
r_var <- 0
r_tempvar <- 0.01
model_dyn  <- "beverton_holt"

d_pop  <- f_make_dyn_species(nb_sp = 10,nb_pop = npop, N0_mean = N_mean,N0_var = N_sd,K=K,K_method="exact",r_mean = r, r_init_var = r_var,r_temporal_var_mean = r_tempvar,nb_year = nbyear,model_name = model_dyn,demographic_stocha = demographic_stocha, fig="")

cat("\n\n population data: \n\n")
print(head(d_pop))



sample_start_mean <- 1
sample_start_var <- 0
sample_duration_mean <- 30
sample_duration_var <- 0
sample_completude_mean <- 1
sample_completude_var <- 0
sample_proba_obs_mean <-1
sample_proba_obs_var <- 0

popsamples <- f_samples_pop(d_pop,start_mean = sample_start_mean, start_var = sample_start_var,
                           duration_mean = sample_duration_mean, duration_var = sample_duration_var,
                           completude_mean = sample_completude_mean, completude_var = sample_completude_var ,
                           proba_obs_mean = sample_proba_obs_mean , proba_obs_var = sample_proba_obs_var,
                           fig = "")
## mod <- glm(N~year ,data=d_pop,family="poisson")
## pred <- ggpredict(mod)
## pred

cat("\n\n sample data: \n\n")
print(head(popsamples))

d_lpi <- assess_lpi(popsamples,fig="")


filename <- paste0("output/lpi_test_cluster_",format(Sys.time(),'%Y%m%d_%H%M%S'),".csv")

cat("save: ",filename)
fwrite(d_lpi,filename)
cat("   DONE!! \n")
