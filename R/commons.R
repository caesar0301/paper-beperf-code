#!/usr/bin/env R
# author: chenxm
library(depmixS4)
library(ggplot2)
library(GGally)
library(plyr)
library(cluster)
library(reshape2)
library(scales)
# for parallel processing
library(parallel)
library(doParallel)
library(foreach)

## Global variables
service.cn <- c("CDN服务","下载更新","主页博客",
                "休闲娱乐", "体育","健康","即时通讯",
                "图片服务","女性","广告","微博",
                "房产","搜索导航","政府机构","教育",
                "数码科技","新闻","旅游","求职",
                "汽车", "游戏","生活工具","电子邮箱",
                "社交网络","移动软件","网上购物","视频",
                "论坛社区","读书","金融投资","门户","音乐")
service.en <- c("Others","Downloads","SN",
                "Leisure","Leisure","Life","IM",
                "Others","Shopping","Others","Microblog",
                "Life","Search","Others","Education",
                "News","News","Life","Life",
                "News","Games","Downloads","Email",
                "SN","Downloads","Shopping","Video",
                "SN","Reading","Life","News","Music")
service.en.all <- c("CDN","Update","Blog",
                    "Leisure","Sports","Health","IM",
                    "Pictures","Women","Ads","Microblog",
                    "HouseRent","Search","Gov.","Education",
                    "Tech.","News","Travel","Job",
                    "Auto","Games","Utilities","Email",
                    "Social","Others","Shopping","Video",
                    "Community","Reading","Finance","Portal","Music")

## Calculate entropy of engagement metrics
library(entropy)
cal.entropies <- function(en.macro){
    ddply(en.macro, c("UID"), function(df){
        cov.e <- sapply(
          df[,c("service.en","building","TOW","TOD")],
          function(y){
            freq <- aggregate(df$SDur, by=list(y), sum)
            entropy(freq$x, method="ML")
            })
        eng.e <- sapply(df[,c("SDur","VF")], function(y){
          freq <- discretize(y, 100)
          entropy(freq, method="ML") })
        c(cov.e, eng.e)})}

################# Data ETL ####################
# Save text data as RDS object
serialize.mydata <- function(engage_micro_file){
    library(data.table)
    library(bit64)
    print("Reading data ... ")
    mydata <- fread(engage_micro_file, head=F, sep='\t')
    setnames(mydata, colnames(mydata),
             c("UID","service","STime","volume","size",
               "SDur","IR","VF","ADur","MWTime","PABw","building"))
    print("Add service english names ... ")
    mydata$UID <- as.integer(factor(mydata$UID))
    service.en <- data.frame(service = service.cn,
                             service.en = service.en)
    mydata <- merge(mydata, service.en, by=c("service"))
    mydata <- mydata[which(mydata$service.en != "Others"),]
    ## update factors of filtered data
    mydata$service <- factor(mydata$service)
    mydata$service.en <- factor(mydata$service.en)
    ## rank building
    print("Add building rank ...")
    mydata <- rank.building(mydata)
    ## rank services
    print("Add service rank ...")
    mydata <- rank.service(mydata)
    ## transform fields
    mydata <- transform2(mydata)
    mydata
}

rank.building <- function(en.mob){
    library(plyr)
    ddply(en.mob, c("UID"),
          function(x){
              sd.sum <- ddply(x, c("building"), function(y)(sum(y$SDur)))
              colnames(sd.sum) <- c("building","BDur")
              bdur.unique <- unique(sd.sum$BDur)
              if(length(bdur.unique) == length(sd.sum$BDur)){
                  brank <- rank(sd.sum$BDur)
                  sd.sum$BRank <- max(brank) - brank + 1
              }else{
                  brank <- rank(bdur.unique)
                  sd.sum.rk <- data.frame(BDur=bdur.unique,
                                          BRank=max(brank) - brank + 1)
                  sd.sum <- merge(sd.sum, sd.sum.rk, by=c("BDur"))
              }
              merge(x, sd.sum[,c("building","BRank")], by="building")
          })
}

rank.service <- function(en.mob){
    library(plyr)
    ddply(en.mob, c("UID"), function(x){
        sd.sum <- ddply(x, c("service.en"), function(y)(sum(y$SDur)))
        colnames(sd.sum) <- c("service.en","SrvDur")
        srvdur.unique <- unique(sd.sum$SrvDur)
        if(length(srvdur.unique) == length(sd.sum$SrvDur)){
            srank <- rank(sd.sum$SrvDur)
            sd.sum$SrvRank <- max(srank) - srank + 1
        }else{
            srank <- rank(srvdur.unique)
            sd.sum.rk <- data.frame(SrvDur=srvdur.unique,
                                    SrvRank=max(srank) - srank + 1)
            sd.sum <- merge(sd.sum, sd.sum.rk, by=c("SrvDur"))
        }
        merge(x, sd.sum[,c("service.en","SrvRank")], by="service.en")
    })
}

transform2 <- function(en.mob){
    ddply(en.mob, ~ UID, function(x){
        ## scale fields
        x$SDur <- x$SDur/60 # minutes
        x$PABw <- x$PABw/1024 # KB
        x$size <- x$size/1024 # KB
        ## temporal info
        pts <- as.POSIXlt(x$STime, origin="1970-01-01")
        x["TOW"] <- pts$wday
        x["TOD"] <- pts$hour
        x["SPM"] <- x$volume/x$SDur
        x <- x[order(x$UID, x$STime),]
        return(x)
    })
}


############### SCA ######################
## control instance number regarding struct vars
scar.filter.by.freq <- function(df, struct, min=20){
    ## sum up instance frequency
    df.stat <- aggregate(df[,1], by=lapply(struct, function(x)(df[[x]])),
                         FUN=length)
    colnames(df.stat) <- c(struct, "scar.filter.freq")
    df.tmp <- merge(df, df.stat, by.x=struct, by.y=struct)
    ## filter out keys whose frequencies are less than `min'
    df.tmp <- df.tmp[which(df.tmp$scar.filter.freq > min),]
    ## delete unused key
    subset(df.tmp, select=-c(scar.filter.freq))
}

## format cor. resulted list as data frame
scar.deps <- c("SDur","IR","VF")
scar.inds <- c("ADur","MWTime","PABw")
scar.target <- c(scar.deps, scar.inds)
scar.fmt.ld <- function(dlist, struct){
    library(plyr)
    ldply(dlist, function(x){
        ## format struct
        sv <- sapply(struct, function(y)(x[[y]]))
        sv <- data.frame(t(sv))
        colnames(sv) <- struct
        ## format cor. values
        deps <- scar.deps
        inds <- scar.inds
        cor <- x[["r"]]
        prob <- x[["p"]]
        ncase <- x[["n"]]
        ## double lapply to replace nested loop
        cv <- ldply(deps, function(y){
            ldply(inds, function(z){
                data.frame(dep=y, ind=z, cor=cor[z,y],
                           prob=prob[z,y], n=ncase)})
        })
        ## merge two data frames with different rows
        merge(sv, cv)
    })
}

## structured correlation analysis
scar.cor <- function(df, struct, target){
    # calculate correlation
    cal.cor <- function(df){
        library(psych)
        res <- corr.test(df, method="spearman")
        list(r=res$r, p=res$p, n=res$n)
    }
    cor.ml <- by(df,
                 lapply(struct, function(x)(factor(df[[x]]))),
                 function(x){ # a list returned
                     cor <- cal.cor(x[,target])
                     for ( s in struct)
                         cor[[s]] <- unique(x[,s])
                     cor},
                 simplify=F)
    ## filter out NA element in list
    cor.ml.flt <- Filter(Negate(is.null), cor.ml)
    ## in another way
    ## cor.ml[!sapply(cor.ml, is.null)]
    scar.fmt.ld(cor.ml.flt, struct)
}

## obtain aggregated stat for significant instances
scar.sig.odds <- function(df, measure.vars, FUN=median, sig.p=0.05){
    ddply(df, measure.vars, function(x){
        selector <- (x$prob < sig.p)
        sig.cnt <- length(x$prob[selector])
        odds <- 1.0 * sig.cnt / (length(x$prob) - sig.cnt)
        stat <- FUN(na.omit(x$cor[selector]))
        c(cor=stat, odds=odds)})
}

# rank unique
rank.unique <- function(x, na.last=TRUE){
    x.unique <- unique(x)
    x.unique.df <- data.frame(x=x.unique, r=rank(x.unique, na.last))
    x.df <- data.frame(x=x)
    ranked <- merge(x.df, x.unique.df, by=c("x"), all=TRUE, sort=FALSE)
    ranked
}


############# HMMs #################
## Create an HMM object without parameters fitted
hmm.object <- function(en.data, ns){
    depmix(response=list(SDur ~ 1, VF ~ 1, ADur ~ 1,
           MWTime ~ 1, PABw ~ 1),
           data = en.data, nstates = ns,
           transition = ~ TOD + service.en + BRank,
           family=list(gaussian(), gaussian(), gaussian(),
           gaussian(), gaussian()))
}

## learn HMM with variant states
hmm.learn <- function(en.data, plot=FALSE){
    stat <- data.frame()
    models <- list()
    for(sn in seq(2,6)){
        print(paste("state #: ", sn))
        en.data$IR <- en.data$IR * 100
        mod <- hmm.object(en.data, sn)
        fm <- NA
        tryCatch({
            set.seed(1);
            fm <- fit(mod, verbose=F)
        }, error= function(e){
            ## pass
        }, finally = {
            models <- c(models, fm)
            if(is.na(fm))
                stat <- rbind(stat, c(sn, rep(NA, 5)))
            else
                stat <- rbind(stat, c(sn,logLik(fm),AIC(fm),
                                      BIC(fm),AICC(fm),npar(fm)))
        }) # END tryCatch
    }
    ## Select optimal model
    colnames(stat) <- c("state", "ll","AIC","BIC","AICC", "npar")
    print(stat)
    opt.model <- hmm.model.select(stat, models, plot)
    return(opt.model)
}

## calculate AICC
AICC <- function(fm){
    np <- npar(fm)
    no <- nobs(fm)
    aic <- AIC(fm)
    return(aic + 2*np*(np+1)/(no-np-1))
}

hmm.model.select <- function(stat, models, plot=FALSE){
    opt.model <- NA
    stat <- stat[complete.cases(stat),]
    if(nrow(stat)==0) # return NA if there are no valid models
        return(opt.model)
    if(plot){ ## Plot model statistics
        stat$penalty <- stat$AICC + 2*stat$ll
        melted <- melt(stat,id.vars="state",
                       measure.vars=c("ll","penalty","AICC"))
        p <- ggplot(melted, aes(state, value, group=variable, color=variable)) +
          geom_line() + geom_point() +
          theme_bw() + xlab("States")+ylab("Value")+
            scale_color_discrete(name="", breaks=c("ll","penalty","AICC"),
                                 labels=c("logLik","Penalty","AICC"))
        p <- p + theme(legend.position=c(0.85,0.75),
                       axis.title.x=element_text(size=15),
                       axis.title.y=element_text(size=15))
        ggsave("figures/hmm-model-selection.pdf", p, width=4, height=3.5)
    }
    state <- stat$state[order(stat$AICC)][1]
    ## Select optimal model
    opt.index <- order(stat$AICC)[1]
    print(paste("Opt. state #: ", state)) # states
    opt.model <- models[[state-1]]
    return(opt.model)
}

## calculate distance of two HMMs
hmm.hd <- function(fm1, fm2, obs, type="Hellinger", plot=FALSE){
    ds <- ddply(obs, .(UID), function(ob){
        cal.viterbi <- function(o, fm){
            hmm2 <- hmm.object(o, nstates(fm))
            hmm2 <- setpars(hmm2, getpars(fm))
            viterbi(hmm2)} # End of Viterbi
        d <- NA
        tryCatch({
            v1 <- cal.viterbi(ob, fm1)
            v2 <- cal.viterbi(ob, fm2)
            p1 <- prod(apply(subset(v1,select=-state),1,max))
            p2 <- prod(apply(subset(v2,select=-state),1,max))
            d <- 0
            if (type=="Hellinger"){
                d <- 0.5*(sqrt(p1)-sqrt(p2))^2
            } else if(type=="KL"){
                d <- 0.5*(p1*log(p1/p2) + p2*log(p2/p1))
            }
        }, error=function(e) {
            #print(e)
        })
        return(c(d, 1, nrow(ob)))})
    colnames(ds) <- c("UID","d","t","obs")
    ## Replace NA with mean of valid element
    ds$d[is.na(ds$d)] <- mean(ds$d, na.rm=T)
    ## saveRDS(ds, "data/engage.hmm.dist.2m.rds") # debugging
    ## Plot dm converge line
    if(plot){
        pdf("hmm-model-converge.pdf", width=6, height=4)
        par(mar=c(4,5,1,1), mgp=c(2.2,0.5,0), cex.lab=1.7)
        dmm <- apply(as.matrix(seq(1:length(ds$d))),1,
                     function(x) mean(ds$d[1:x]))
        plot(dmm, type="l", xlab="N", ylab=expression(H["i,j"]))
        abline(v=100, lty=2, col="red"); points(dmm, pch=20)
        grid()
        dev.off()}
    ## Return final distance
    if(type=="Hellinger")
        return(sqrt(sum(ds$d)))
    if(type=="KL")
        return(sum(ds$d))
}

## samplse users
sample.trajs <- function(en.mob, num){
  return(subset(en.mob, UID %in% sample(
    unique(en.mob$UID), num, replace=F)))
}

## Generate distance matrix
hmm.dm <- function(models, trajs, N){
  M = length(models)
  dm = foreach(i=1:M, .combine='rbind') %:%
    foreach(j=1:M, .combine='c') %dopar% {
      dist = 0
      if( i > j ) {
        S = sample.trajs(trajs, N)
        dist = hmm.hd(models[[i]], models[[j]], S, "Hellinger", TRUE)
        print(paste(i, j, dist))
      }
      dist
    }
  return(as.matrix(dm))
}

plot.dm.tile <- function(dm){
    data <- as.matrix(dm)
    hc<-hclust(dist(data))
    rowInd<-hc$order
    hc<-hclust(dist(t(data)))
    colInd<-hc$order
    data.m<-data[rowInd,colInd]
    data.m<-apply(data.m,1, rescale)
    data.m<-t(data.m)
    coln<-colnames(data.m)
    rown<-rownames(data.m)
    colnames(data.m)<-1:ncol(data.m)
    rownames(data.m)<-1:nrow(data.m)
    data.m<-melt(data.m)
    base_size<-1
    p <- ggplot(data.m, aes(Var2, Var1)) +
        geom_tile(aes(fill = value), colour = "white") +
            scale_fill_gradient(low = "yellow", high = "red")
    p <- p + theme_grey(base_size = base_size) + labs(x = "", y = "") +
        scale_x_continuous(expand = c(0, 0),labels=coln,breaks=1:length(coln)) +
            scale_y_continuous(expand = c(0, 0),labels=rown,breaks=1:length(rown))
    p <- p + theme(axis.ticks = element_blank(),
                   axis.text.x = element_text(size = base_size *0.8,
                   angle = 90, hjust = 0, colour = "grey50"),
                   axis.text.y = element_text(size = base_size * 0.8,
                   hjust=1, colour="grey50"))
    plot(p)
}

plot.dm.ord <- function(dm){
    source("http://ichthyology.usm.edu/courses/multivariate/coldiss.R")
    coldiss(dm)
}

plot.dm.hm <- function(dm){
    par(mfrow=c(2,1))
    heatmap(as.matrix(dm))
    ##library(gplots)
    ##heatmap.2(as.matrix(dm))
}

plot.dm.hc <- function(dm, k=3){
    clust <- hclust(dm, method="average")
    ck <- cutree(clust, k = k)
    ord <- order(ck)
    coph <- cophenetic(clust)
    layout(matrix(1:4, ncol = 2, byrow=T))
    par(mar=c(3,3,3,2), mfrow=c(1,4))
    image(as.matrix(dm)[ord, ord], main = "Model dist.")
    image(as.matrix(coph)[ord, ord], main = "Cophenetic dist.")
##    image((as.matrix(coph) - as.matrix(dm))[ord, ord],
##          main = "Cophenetic - Model dist.")
    par(mar=c(4,4,3,2), mgp=c(1.5,0.5,0), cex.lab=1.5)
    plot(as.dendrogram(clust), leaflab="none", main="Dendrogram",
         xlab="User trajectories", ylab="Height")
    par(mar=c(4,4,3,2), mgp=c(2,0.5,0), cex.lab=1.5)
    plot(coph ~ dm, ylab = "Cophenetic dist.", xlab = "Model dist.",
         main = "Shepard diagram")
    rs.lab <- sprintf("%.3f", cor(coph, dm, method="spearman"))
    text(0.8*max(dm), 0.5*max(coph), cex=1.5,
         substitute(paste(r[s], " = ", rs), list(rs=rs.lab)))
    abline(0,1, col = "red")
    box()
    layout(1)
    return(ck)
}
