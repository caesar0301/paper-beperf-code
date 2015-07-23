library(ggplot2)
library(scales)

# Plot user engagement statistics
# "../data/engage/user_engagement_service_stat.nmob"
input.m <- read.csv("../data/engage/user_engagement_service_stat.mob",
                    sep="\t", header=FALSE)
colnames(input.m) <- c("service", "sessionCntPerUser",
                       "sessionVolMedPerUser","sessionDurMedPerUser")

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Session duration distribution
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

summary <- summaryBy(sessionDurMedPerUser ~ service , data=input.m, FUN=median)
input.m$service <- factor(
  input.m$service, levels=
    summary$service[order(-summary$sessionDurMedPerUser.median)],ordered=TRUE)
(p <- ggplot(input.m, aes(x=service, y=sessionDurMedPerUser/60)) +
    geom_boxplot(outlier.shape = NA) +
    theme(legend.justification=c(1,0), legend.position=c(1,0)) +
    scale_y_continuous(name="Session Duration (minutes)",
                       limits=c(0, 60), oob=squish) +
    theme(axis.text.x=element_text(angle=90, hjust=1)))
ggsave("figures/service-dur-per-user.pdf", p, width=9, height=8, family="GB1")

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Session count distribution
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

summary <- summaryBy(sessionCntPerUser ~ service , data=input.m, FUN=median)
input.m$service <- factor(
  input.m$service, levels=
    summary$service[order(-summary$sessionCntPerUser.median)], ordered=TRUE)
(p <- ggplot(input.m, aes(x=service, y=sessionCntPerUser)) +
    geom_boxplot(outlier.shape = NA) +
    theme(legend.justification=c(1,0), legend.position=c(1,0)) +
    scale_y_continuous(name="Session Count Per User",
                       limits=c(1,20), oob=squish) +
    theme(axis.text.x=element_text(angle=90, hjust=1)))
ggsave("figures/service-cnt-per-user.pdf", p, width=9, height=8, family="GB1")

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Session volume distribution
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

summary <- summaryBy(sessionVolMedPerUser ~ service , data=input.m, FUN=median)
input.m$service <- factor(
  input.m$service,levels=
    summary$service[order(-summary$sessionVolMedPerUser.median)], ordered=TRUE)
(p <- ggplot(input.m, aes(x=service, y=sessionVolMedPerUser)) +
    geom_boxplot(outlier.shape = NA) +
    theme(legend.justification=c(1,0), legend.position=c(1,0)) +
    scale_y_continuous(name="Session Volume Per User",
                       limits=c(1,100), oob=squish) +
    theme(axis.text.x=element_text(angle=90, hjust=1)))
ggsave("figures/service-vol-per-user.pdf", p, width=9, height=8, family="GB1")
