lpi_simulation <- function(nb_sp=1,N0_mean=NULL, N0_var = 0,K=100,K_method="exact",r_mean = 0.01,r_init_var = 0, r_temporal_var_mean = 0, nb_pop = 3, nb_year = 50, model_name = "ricker",demographic_stocha = "poisson",
start_mean = sample_start_mean, start_var = sample_start_var,
                           duration_mean = sample_duration_mean, duration_var = sample_duration_var,
                           completude_mean = sample_completude_var, completude_var = sample_completude_var ,
                           proba_obs_mean = sample_proba_obs_mean , proba_obs_var = sample_proba_obs_var,
                           fig = "print",save_data=TRUE
