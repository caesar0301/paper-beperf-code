library(ggplot2)

data <- read.csv('../data/engage/interrupt.txt', header=F, sep='\t')
colnames(data) <- c("category","volume","size","adur","mwtime","pabw","ir")
data$pabw <- data$pabw/1024 # byte
data <- data[which(data$category %in% c("im", 'music','social','weather')), ]
data <- data[which(data$volume>1), ]
data <- data[which(data$adur>1), ]
data <- data[sample(nrow(data), size=10000, replace=F), ]

p <- ggplot(data, aes(x=mwtime, color=category)) + geom_density() +
  scale_x_continuous(limits=c(0,1))
plot(p)