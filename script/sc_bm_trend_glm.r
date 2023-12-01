

library(glmmTMB)
library(data.table)


run <- FALSE



f_assess_trend_sp <- function(d,output=FALSE,fileout="ref", the_ids=NULL) {
    vec_serie <- c("all","start_10","mid_10","end_10")

    d_glm_trend <- expand.grid(id_iteration = unique(d[,id_iteration]), periode = vec_serie)

    d_glm_trend$trend = 99.99
    d_glm_trend$sd = 99.99
    d_glm_trend$ICinf = 99.99
    d_glm_trend$ICsup = 99.99

    setDT(d_glm_trend)
    if(is.null(the_ids))
        the_ids <- unique(d_glm_trend[,id_iteration])

    nb_id <- length(the_id)

    for(ii in 1:nb_id) {
        i <- the_ids[ii]
        cat("\n",ii,"/",nb_id,"|",i,": ",sep="")
        for(s in vec_serie) {
            cat(s," ")
            if(s == "all") {
                di <- d[id_iteration == i,]
            } else if(s == "start_10") {
                di <- d[id_iteration == i & year < 11,]
            } else if(s =="mid_10") {
                di <- d[id_iteration == i & year >= (mean(year)-5 & year <= mean(year) + 5),]
            } else {
                di <- d[id_iteration == i & year >= max(year) - 10,]
            }

            if(nrow(di)>2) {

                nbpop <- length(unique(di[,id_pop]))

                if(nbpop == 1 ){
                                        #md for only one pop
                    cat("glm ")
                    md <- try(glm(N~year, data = di, family= "quasipoisson"),silent = TRUE)
                } else {
                    cat("glmmTMB ")
                    md <- try(glmmTMB(N~year + (1|id_pop), data = di, family = "nbinom2"),silent=TRUE)
                }

                trend_i <- NA
                sd_i <- NA
                ICinf_i <- NA
                ICsup_i <- NA

                if(class(md)[1] != "try-error") {
                    if(nbpop == 1 ){
                        smd <- as.data.frame(summary(md)$coefficients)
                    } else {
                        smd <- as.data.frame(coef(summary(md))$cond)
                    }

                    trend_i <- exp(smd[2,1])
                    sd_i <- exp(smd[2,2])
                    ci <- try(confint(md),silent = TRUE)
                    if(class(ci)[1] != "try-error") {
                        ICinf_i <- exp(ci[2,1])
                        ICsup_i <- exp(ci[2,2])
                    }
                }
                d_glm_trend[id_iteration == i & periode == s, trend := trend_i]
                d_glm_trend[id_iteration == i & periode == s, sd := sd_i]
                d_glm_trend[id_iteration == i & periode == s, ICinf := ICinf_i]
                d_glm_trend[id_iteration == i & periode == s, ICsup := ICsup_i]

            }
        }
        cat("\n\nSave\n")
        fwrite(d_glm_trend,paste0("output/trend_",fileout,".csv"))
    }


    cat("\n\nSave\n")
    fwrite(d_glm_trend,paste0("output/trend_",fileout,".csv"))

    print(head(d_glm_trend))

    if(output) return(d_glm_trend)
}





if(run) {
    d <- fread("data/data_dyn.csv")
    f_assess_trend_sp(d)
}
