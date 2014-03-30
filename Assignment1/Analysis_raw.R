### Data Analysis course
### Assignment 1

setwd("/data/Dropbox/Data Analysis/Assignment1/")

# Load the dataset
cc <- rep("character", 15) ## colClasses vector
cc[c(2, 3, 10)] <- "numeric"
cc[12:14] <- "integer"
cc[c(5, 6, 8, 9, 11, 15)] <- "factor"

names <- c("amount.requested", "amount.funded", "interest", 
           "length", "purpose", "di.ratio", "state", "home", 
           "income", "fico", "open.lines", "credit.balance", 
           "inquiries", "empl.length")

ld <- read.csv("loansData.csv", colClasses=cc, row.names=1, col.names=names)

# read state names and regions
states <- read.delim("states.txt", row.names=1)  ## code as row names


### Retouching formats

# changing Interest.Rate to numeric
ld$interest <- gsub("%", "", x=ld$interest, fixed=TRUE)
ld$interest <- as.numeric(ld$interest)

# changing Debt.To.Income.Ratio to numeric
ld$di.ratio <- gsub("%", "", x=ld$di.ratio, fixed=TRUE)
ld$di.ratio <- as.numeric(ld$di.ratio)/100 

# reordering Home.Ownership levels
ll <- c("NONE", "RENT", "MORTGAGE", "OWN", "OTHER")
ld$home <- factor(ld$home, levels=ll)

# correcting order of Employment.Length factor
ll <- c("< 1 year", "1 year", "2 years", "3 years", "4 years", "5 years", 
        "6 years", "7 years", "8 years", "9 years", "10+ years", "n/a")
ld$empl.length <- factor(ld$empl.length, levels=ll)

# rearranging state so NY is reference
ld$state <- relevel(ld$state, "NY")

# creating region variable
# source: http://www.bea.gov/regional/docs/regions.cfm
ld$region <- states[ld$state, "region"]
ld$region <- relevel(ld$region, "Mideast")  ## use Mideast as reference

# creating income quartiles
q.breaks <- quantile(ld$income, na.rm=T)
q.breaks[1] <- q.breaks[1] - 1
ld$income.quartile <- cut(ld$income, breaks=q.breaks, 
                       labels=c("4th quartile", "3rd quartile", 
                                "2nd quartile", "1st quartile"))

# creating 3 equally-sized groups of amount.funded
ld$amount.funded.group <- cut(ld$amount.funded, breaks=c(-1, 7500, 15000, Inf), 
                              labels=c("less than 7.5K", "7.5-15K", "more than 15K"))

### Plots
require(ggplot2)
require(reshape2)

# dataset of medians by amount.funded.group and length
medians <- melt(tapply(ld$interest, list(ld$amount.funded.group,
                                         ld$length), median))
names(medians) <- c("amount.funded.group", "length", "median.interest")


# basic boxplot of Interest.Rate by FICO.Range
p1 <- ggplot(ld, aes(x=fico, y=interest, color=income.quartile)) + geom_jitter(size=2, alpha=0.5) 
p1 <- p1 + facet_grid(length~amount.funded.group) + scale_color_brewer(type="seq", palette="YlOrRd") 
p1 + theme_bw() + geom_smooth(aes(group=1), method="lm") + geom_hline(data=medians, aes(yintercept=median.interest))

# income vs. amount funded
p2 <- ggplot(ld, aes(x=income, y=amount.requested, color=length)) 
p2 <- p2 + geom_point(size=1, pch=16) + scale_x_continuous(limits=c(0, 30000)) 
p2 + geom_abline(intercept=0, slope=6, size=1) + facet_wrap(~fico)

### Regression analysis

m1 <- lm(interest ~ fico + amount.requested + length + open.lines 
         + inquiries, data=ld)
summary(m1)
anova(m1)

m2 <- lm(interest ~ fico + purpose + income + di.ratio + region + home 
         + amount.funded*length, data=ld)
summary(m2)
anova(m2)


### Notes:
#   - Two types of variables: 
#       i) decided in conjunction with the interest rate
#       ii) independent of the interest rate
# 
# must be careful when using variables from i in regressions 
# (they will be significant, but heavily biased)

# Seems like home does not matter (from the regression), 
# but looking at ANOVA we see that jointly they are significant

# maybe try 2sls?



