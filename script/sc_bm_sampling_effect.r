require(data.table)

run <- FALSE

source("../functions/fun_samples.r")


f_sample_bm <- function(d,nb_rep=10,output=FALSE ) {



    d_sc <- expand.grid(duration = c(round(seq(3,50,5)),50),completude = c(.5,1), proba = c(seq(.1,1,0.25),1))

    setDT(d_sc)

    d_sc[,id_sc_sample := 1:.N]
    fwrite(d_sc,"data/sc_sample.csv")


    veccol <- c("pop","year","Nobs")
    for(i in 1:nrow(d_sc)) {
        cat("\n",i,": ")
        for(r in 1:nb_rep) {
            cat(r," ")
            di <- d[,.(id_pop,year,N)]
            setnames(di,"id_pop","pop")
            di <- f_samples_pop(di,start_mean = 10,start_var = 20, duration_mean = d_sc$duration[i], duration_var=0,completude_mean = d_sc$completude[i],completude_var = 0, proba_obs_mean = d_sc$proba[i],proba_obs_var=0,fig="")
            di <- di[,veccol,with=FALSE]
            di <- di[!is.na(Nobs),]
            di[,`:=`(id_sc_sample = i,rep_sample = r,id_rep_sample = paste0(i,"_rs",r))]

            if(i == 1 & r == 1) dsample <- di else dsample <- rbind(dsample,di)
        }
    }

    setnames(dsample,"pop","id_pop")

    dsp <- unique(d[,.(id_sc,id_iteration,id_sp,id_pop)])

    dsample <- merge(dsample,dsp,by="id_pop")

    head(dsample)

    fwrite(dsample,"data/data_sample.csv")

    if(output) return(dsample)
}


if(run) {

    d <- fread("data/data_dyn.csv")
    f_sample_bm(d)

}
