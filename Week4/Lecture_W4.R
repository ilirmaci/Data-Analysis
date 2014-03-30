### Data Analysis course
### Week 4 lecture code

setwd("/data/Dropbox/Data Analysis/Week4/")

load("samsungData.rda")

#convert to data.table
require(data.table)
s <- data.table(samsungData)
s$subject <- as.character(s$subject)
setkey(s, subject)
rm(samsungData)

require(ggplot2) 
require(reshape2)

# plotting average acceleration for first subject

# first 2 data cols and activity name
d12 <- melt(s["1", c(1, 2, 563), with=FALSE], id.vars="activity")
xvals <- rep(1:(nrow(d12) %/% 2), 2)
p1 <- ggplot(d12, aes(x=xvals, y=value, color=activity))
p1 + geom_point(size=3) + facet_wrap(~variable)

# hiearachical clustering based on acceleration
distanceMatrix <- dist(s["1", 1:3, with=FALSE])
hc <- hclust(distanceMatrix)
plot(hc)

# single value decomposition
svd1 <- svd(scale(s["1", 1:561, with=FALSE]))
svd1.flat <- melt(svd1$u[,1:2])
names(svd1.flat) <- c("r", "c", "value")
x <- rep(s["1",]$activity, 2)  ## activity names vector x2
p2 <- ggplot(svd1.flat, aes(x=r, y=value, color=x))
p2 + geom_point(size=3) + facet_wrap(~c)

