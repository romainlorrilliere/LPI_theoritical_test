
if(getwd() == "c:/git/LPI_theoritical_test") setwd("script/")

source("sc_bm_sampling_effect.r")
source("sc_bm_trend_glm.r")


do_dyn <- FALSE
do_ref <- FALSE
do_sample <- FALSE
do_trend <- FALSE
id_preparation <- FALSE
import <- TRUE
do_analysis <- TRUE

## dyn
if(do_dyn) source("sc_bm_dyn.r") else if(import) d <- fread("data/data_dyn.csv")

## glm reference
if(do_ref) f_assess_trend_sp(d) else  if(import) dref <- fread("output/trend_ref.csv")

## sample
if(do_sample) dsample <- f_sample_bm(d,output=TRUE) else  if(import) dsample <- fread("data/data_sample.csv")


## glm sample



if(do_trend) {
    if(import) {
        setnames(dsample,c("id_iteration","Nobs"),c("id_it","N"))
        dsample[,id_iteration := paste0(id_it,"_smp",id_rep_sample)]
        all_ids <- unique(dsample[,id_iteration])
        length(all_ids)
    }
    if(id_preparation) {
         all_ids <- copy(dsample)
         all_ids[,`:=`(id_pop = NULL,year = NULL, N = NULL)]
         all_ids <- unique(all_ids)
         all_ids[,seq := sample(1:500,.N,replace=TRUE)]
         setorder(all_ids,seq)
         dim(all_ids)
         fwrite(all_ids,"data/ids_sample_trend.csv")
    } else {
        if(import) all_ids <- fread("data/ids_sample_trend.csv")
    }


    for(s in 1:10){
        cat("\n\n================\n\n     ",s,"/10\n\n======================\n\n",sep="")
        ids_s <- all_ids[seq==s,id_iteration]
        dsample_s <- dsample[id_iteration %in% ids_s]
        sample_trend <- f_assess_trend_sp(dsample_s,output = TRUE, fileout = paste0("sample_",s),the_ids = ids_s)
    }
}



if(do_analysis){
    dsample_trend <- NULL
    for(i in 1:10){
        ds <- fread(paste0("output/trend_sample_",s,".csv"))
        dsample_trend <- rbind(dsample_trend,ds)
    }
    dsample_trend <- merge(dsample_trend,all_ids,by = "id_iteration")
    dim(dsample_trend)
    head(dsample_trend)


    setnames(dsample_trend,c("id_iteration","id_it"),c("id_iteration_sr","id_iteration"))
    setnames(dref,colnames(dref)[3:6],paste0(colnames(dref)[3:6],"_ref"))
    dsample_trend <- merge(dsample_trend,dref,by = c("id_iteration","periode"))


    dparam_it <- unique(d[,.(id_iteration,sc_name,model_name,demographic_stocha,nb_sp,nb_pop,N0,K,r_mean)])

    dsample_trend <- merge(dsample_trend,dparam_it,by="id_iteration")
       dim(dsample_trend)
    head(dsample_trend)

    dscs <- fread("data/sc_sample.csv")
    dsample_trend <- merge(dsample_trend,dscs,by = "id_sc_sample")

    dsample_trend[trend > 90,`:=`(trend = NA, sd= NA, ICinf = NA, ICsup = NA)]

    dsample_trend[,assessed := !is.na(trend)]

    dsample_trend[assessed==TRUE ,`:=`(incert = abs(trend - trend_ref),
                                       biais = trend / trend_ref,
                                       biais_diff = trend - trend_ref,
                        insideIC = ICinf <= trend_ref & ICsup >= trend_ref)]

    head(dsample_trend)

    library(partykit)

    tree <-  ctree(biais ~  N0 + K + duration + completude + proba, data = dsample_trend[assessed == TRUE,])


    tree.file <- paste0("output/ctree_biais.tiff")
    tiff(tree.file,width = 1200, height = 600)
    plot(tree ,type="simple",main="biais")
    dev.off()


    tree <-  ctree(insideIC ~  N0 + K + duration + completude + proba, data = dsample_trend[assessed == TRUE,])


    tree.file <- paste0("output/ctree_insideIC.tiff")
    tiff(tree.file,width = 1200, height = 600)
    plot(tree ,type="simple",main="Inside IC")
    dev.off()


library(ggplot2)
    gg <- ggplot(data = dsample_trend[assessed == TRUE & biais < 3,], aes(x = duration , y = biais, colour = insideIC)) + geom_smooth()  +geom_point(size = 2) + facet_grid(periode~sc_name)
    gg

    ggsave("output/gg_biais.png",gg)

}
