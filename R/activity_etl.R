source('engage_common.R')

serialize.perc.data <- function(file){
    ifile = read.csv(file, sep='\t', head=F)
    colnames(ifile) <- c('category.cn', 'volume', 'size',
                         'ADur', 'MWTime', 'MEdur',
                         'VMR', 'CI', 'PABw')
    ifile$PABw <- ifile$PABw/1024 # KiloByte/s
    ## add english service names, totally 32 categories
    category <- data.frame(category.cn = service.cn,
                           category = service.en)
    ifile <- merge(ifile, category, by=c("category.cn"))
    ## filter category with small amount of sessions
    ifile <- ifile[which(ifile$category != "Others"),]
    ## update factors
    ifile$category.cn <- factor(ifile$category.cn)
    ifile$category <- factor(ifile$category)
    ## add volume bins
    ifile <- transform(ifile,
                       class=cut(volume, c(0,1,5,10,20,max(volume)),
                       labels=c('tiny','small','median','large','huge')))
    ifile
}

act.mob <- serialize.perc.data("../data/activity/activity_perc.mob")
saveRDS(act.mob, "rdata/act.perc.mob.rds")
act.nmob <- serialize.perc.data("../data/activity/activity_perc.nmob")
saveRDS(act.nmob, "rdata/act.perc.nmob.rds")
