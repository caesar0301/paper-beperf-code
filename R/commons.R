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

## calculate AICC
AICC <- function(fm){
  np <- npar(fm)
  no <- nobs(fm)
  aic <- AIC(fm)
  return(aic + 2*np*(np+1)/(no-np-1))
}

## select optimal model from AICC stat
hmm.model.select <- function(stat, models){
  opt.model <- NULL
  stat <- stat[complete.cases(stat),]
  if(nrow(stat)==0)
    return(opt.model)
  
#   ## Plot model statistics
#   stat$penalty <- stat$AICC + 2*stat$ll
#   melted <- melt(stat,id.vars="state",
#                  measure.vars=c("ll","penalty","AICC"))
#   p <- ggplot(melted, aes(state, value, group=variable, color=variable)) +
#     geom_line() + geom_point() +
#     theme_bw() + xlab("States")+ylab("Value")+
#     scale_color_discrete(name="", breaks=c("ll","penalty","AICC"),
#                          labels=c("logLik","Penalty","AICC"))
#   p <- p + theme(legend.position=c(0.85,0.75),
#                  axis.title.x=element_text(size=15),
#                  axis.title.y=element_text(size=15))
#   ggsave("figures/hmm-model-selection.pdf", p, width=4, height=3.5)
  
  state <- stat$state[order(stat$AICC)][1]
  ## Select optimal model
  opt.index <- order(stat$AICC)[1]
  print(paste("Opt. state #: ", state)) # states
  opt.model <- models[[state-1]]
  return(opt.model)
}

## learn HMM with variant states
hmm.learn <- function(en.data){
    stat <- data.frame()
    models <- list()
    for(sn in seq(2,6)){
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
            stat <- if(is.na(fm)) { rbind(stat, c(sn, rep(NA, 5)))
              } else { rbind(stat, c(sn,logLik(fm),AIC(fm), BIC(fm),AICC(fm),npar(fm))) }
        }) # END tryCatch
    }
    ## Select optimal model
    colnames(stat) <- c("state", "ll","AIC","BIC","AICC", "npar")
    # print(stat)
    opt.model <- hmm.model.select(stat, models)
    ## Store light-weight model parameters
    opt.model2 <- if(is.null(opt.model)) { NULL
      } else { list(nstates=nstates(opt.model), pars=getpars(opt.model)) }
    return(opt.model2)
}

# predict with Viterbi
hmm.pred <- function(traj, fm){
  hmm2 <- hmm.object(traj, fm[["nstates"]])
  hmm2 <- setpars(hmm2, fm[["pars"]])
  viterbi(hmm2)
}

## samplse users
sample.trajs <- function(en.mob, num){
  return(subset(en.mob, UID %in% sample(
    unique(en.mob$UID), num, replace=F)))
}

## calculate distance of two HMMs based on one observation trajectory
hmm.model.dist <- function(traj, model1, model2, type="Hellinger") {
  dist <- NA
  tryCatch({
    v1 <- hmm.pred(traj, model1)
    v2 <- hmm.pred(traj, model2)
    p1 <- prod(apply(subset(v1,select=-state),1,max))
    p2 <- prod(apply(subset(v2,select=-state),1,max))
    dist <- if (type=="Hellinger"){
      0.5*(sqrt(p1)-sqrt(p2))^2
    } else if(type=="KL"){
      0.5*(p1*log(p1/p2) + p2*log(p2/p1))
    } else {
      0
    }
  }, error=function(e) {
    # print(e)
  })
  c(dist, 1, nrow(traj))
}

## calculate distance of two HMMs based on a set of trajectories
hmm.dist <- function(fm1, fm2, obs, type="Hellinger"){
  ds <- ddply(obs, .(UID), hmm.model.dist, fm1, fm2, type)
  colnames(ds) <- c("UID","d","t","obs")
  ds$d[is.na(ds$d)] <- mean(ds$d, na.rm=T)
#   ## Plot dm converge line
#   pdf("hmm-model-converge.pdf", width=6, height=4)
#   par(mar=c(4,5,1,1), mgp=c(2.2,0.5,0), cex.lab=1.7)
#   dmm <- apply(as.matrix(seq(1:length(ds$d))),1,
#                function(x) mean(ds$d[1:x]))
#   plot(dmm, type="l", xlab="N", ylab=expression(H["i,j"]))
#   abline(v=100, lty=2, col="red"); points(dmm, pch=20)
#   grid()
#   dev.off()
  ## Return final distance
  if(type=="Hellinger")
    sqrt(sum(ds$d))
  else if(type=="KL")
    sum(ds$d)
  else
    NA
}

## calculate distance of two HMMs based on a set of trajectories
hmm.dist2 <- function(optm1, optm2, obs, type="Hellinger"){
  # do calculation on the sample space parallely
  obs = split(obs, obs$UID)
  ds  = mclapply(obs, hmm.model.dist, optm1, optm2, type, mc.cores=detectCores() %/% 2)
  # change data form
  uids = names(ds)
  ds = as.data.frame(t(unstack(stack(ds))))
  colnames(ds) <- c("d","t","obs")
  ds$UID = uids
  ds$d[is.na(ds$d)] <- mean(ds$d, na.rm=T)
  ## Return final distance
  if(type=="Hellinger")
    sqrt(sum(ds$d))
  else if(type=="KL")
    sum(ds$d)
  else
    NA
}

## Generate distance matrix
hmm.dm <- function(models, trajs, N, cores=2L){
  M = length(models)
  outter = rep(1:M, each=M)
  inner = rep(1:M, times=M)
  start.time <- Sys.time()
  dm <- mcmapply(function(i, j) {
    dist = 0
    if( i < j ) {
      S = sample.trajs(trajs, N)
      dist = hmm.dist2(models[[i]], models[[j]], S, "Hellinger")
      print(paste(i, j, dist))
    }
    dist
  }, outter, inner, mc.cores=cores)
  dur <- Sys.time() - start.time
  print(paste("total us time:", dur))
  matrix(dm, nrow=M, byrow=T)
}