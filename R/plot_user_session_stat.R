library(ggplot2)
library(scales)

# Plot user engagement statistics
input.m <- read.csv("../data/engage/user_engagement_session_stat.mob", sep="\t", header=FALSE)
input.nm <- read.csv("../data/engage/user_engagement_session_stat.nmob", sep="\t", header=FALSE)
input.m$type <- "m"
input.nm$type <- "nm"

input <- rbind(input.m, input.nm)

fields <- c("sessionCnt", "sessionVolMed", "sessionDurMed", "type")
colnames(input) <- fields

(p <- ggplot(input, aes(sessionCnt, colour=type)) + stat_ecdf() +
    scale_x_continuous(name="Session Count", limits=c(0,200),oob=squish) +
    scale_y_continuous(name="Percentage") +
    scale_color_discrete(name="Devices",
                         breaks=c("m", "nm"),
                         labels=c("Mobile", "NMobile")) +
    theme(legend.justification=c(1,0), legend.position=c(1,0)))
ggsave("figures/session-cnt-per-user.pdf", p, width=3.5, height=3.5)

(p <- ggplot(input, aes(sessionVolMed, colour=type)) + stat_ecdf() +
    scale_x_continuous(name="#Activities/Session", limits=c(10, 1000), oob=squish) +
    scale_y_continuous(name="Percentage") +
    scale_color_discrete(name="Devices",
                         breaks=c("m", "nm"),
                         labels=c("Mobile", "NMobile")) +
    theme(legend.justification=c(1,0), legend.position=c(1,0)))
ggsave("figures/session-vol-per-user.pdf", p, width=3.5, height=3.5)

(p <- ggplot(input, aes(sessionDurMed/60, colour=type)) + stat_ecdf() +
    scale_x_continuous(name="Session Duration (minutes)", limits=c(0, 120), oob=squish) +
    scale_y_continuous(name="Percentage") +
    scale_color_discrete(name="Devices",
                         breaks=c("m", "nm"),
                         labels=c("Mobile", "NMobile")) +
    theme(legend.justification=c(1,0), legend.position=c(1,0)))
ggsave("figures/session-dur-per-user.pdf", p, width=3.5, height=3.5)
