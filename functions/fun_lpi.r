assess_lpi <- function(popsamples) {


## replace zero
popsamples[, one_percent := .01* mean(Nobs,na.rm=TRUE),by=pop]

popsamples[,Nobs := as.numeric(Nobs)]
popsamples[Nobs == 0, Nobs := one_percent]

## interpolation


sample_prev <- popsamples[,.(pop,year,Nobs)]
sample_prev[,year := year + 1]
setnames(sample_prev,"Nobs","Nobs_prev")

popsamples <- merge(popsamples,sample_prev,by=c("pop","year"),all.x=TRUE)
setorder(popsamples,pop,year)


popsamples[,group := cumsum(!as.numeric(is.na(Nobs) & is.na(Nobs_prev))) ,by = pop]


group_prev <- popsamples[,.(pop,group,Nobs,year)]
group_prev[,year := max(year),by=.(pop,group)]
group_prev[,group := group + 1]
setnames(group_prev,c("Nobs","year"),c("Nobs_prev_group","year_prev"))
group_prev <- unique(group_prev)

group_post <- popsamples[,.(pop,group,Nobs,year)]
group_post[,year := min(year),by=.(pop,group)]
group_post[,group := group - 1]
setnames(group_post,c("Nobs","year"),c("Nobs_post_group","year_post"))
group_post <- unique(group_post)

popsamples <- merge(popsamples,group_prev,by=c("pop","group"),all.x = TRUE)
popsamples <- merge(popsamples,group_post,by=c("pop","group"),all.x = TRUE)

interpoLPI <- function (i,Np,Ns,p,s)Np * (Ns/Np)^((i -p)/(s-p))


popsamples[,Nobs_LPI := ifelse(is.na(Nobs),interpoLPI(i=year,Np = Nobs_prev_group,Ns = Nobs_post_group,p = year_prev,s = year_post), Nobs)]
popsamples[,modification:=ifelse(is.na(Nobs) & !is.na(Nobs_LPI), "interpolated",ifelse(Nobs_LPI == one_percent,"one percent","no"))]

## assess d

N_prev <- popsamples[,.(pop,year,Nobs_LPI)]
N_prev[,year := year + 1]
setnames(N_prev,"Nobs_LPI","Nprev")
popsamples <- merge(popsamples,N_prev,by=c("pop","year"))
popsamples[,d := log(Nobs_LPI/Nprev)]


## species aggregation

sp_samples <- popsamples[,.(d = mean(d,na.rm=TRUE)),by = .(sp,year)]


## assess index value

## source("functions/index_value.cpp")

sp_samples[!is.na(d),i := 10^(cumsum(d)),by=sp]


lpi <- sp_samples[!is.na(i),.(value = exp(mean(log(i)))),by=year]



gg1 <- melt(popsamples[,.(pop,sp,year,modification,N,Nobs_LPI)],id.vars = c("pop","sp","year","modification"), mesure.vars = c("N","Nobs_LPI"))
setDT(gg1)
gg1[variable == "N", modification := "no"]
gg2 <- melt(sp_samples[,.(year,sp,d,i)],id.vars = c("sp","year"),nesure.vars = c("d","i"))
setDT(gg2)
gg2[,`:=`(pop = paste0(sp,"_all"),modification = "no" )]
setcolorder(gg2,colnames(gg1))
gg3 <- lpi
gg3[,`:=`(pop = "all",sp="all",modification = "no",variable = "LPI" )]
setcolorder(gg3,colnames(gg1))


gg_samples <- rbind(rbind(gg1,gg2),gg3)
gg_samples <- gg_samples[!is.na(value),]


vecshape = c("no" = 19,"interpolated" = 21 , "one percent" = 25 )
gg <- ggplot(gg_samples,aes(x=year,y=value,colour=sp,group=pop,shape=modification)) + facet_grid(variable~.,scales = "free_y")
gg <- gg+ geom_line() + geom_point(fill = "white",size=2)
gg <- gg + scale_shape_manual(values = vecshape)
print(gg)
ggsave(paste0("output/lpi_sim",format(Sys.time(),'_%Y%m%d_%H%M%S'),".png"),gg,width=10,height=12)

return(gg_samples)

}
