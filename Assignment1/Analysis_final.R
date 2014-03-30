### Data Analysis course
### Assignment 1
### Code used in assignment

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
ll <- c("RENT", "MORTGAGE", "OWN", "NONE", "OTHER")
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

# creating dummy for amount.funded < amount.requested
ld$portion.funded <- factor(ld$amount.requested > ld$amount.funded, 
                            levels=c(FALSE, TRUE), labels=c("Full", "Partial"))

### Plot
require(ggplot2)
require(reshape2)

# dataset of medians by amount.funded.group and length
medians <- melt(tapply(ld$interest, list(ld$amount.funded.group,
                                         ld$length), median))
names(medians) <- c("amount.funded.group", "length", "median.interest")

# list of labels for fico variable
fico.labels <- levels(ld$fico)
matches <- regexpr(fico.labels, pattern="[0-9]{3}") 
fico.labels <- regmatches(x=fico.labels, m=matches) 
fico.labels[-seq(2, 37, by=5)] <- ""  ## remove every label except 3, 6, 9,...


# Interest.Rate by FICO.Range split by 
p1 <- ggplot(ld, aes(x=fico, y=interest, color=income.quartile)) 
p1 <- p1 + geom_jitter(size=2, alpha=0.5) 
p1 <- p1 + facet_grid(length~amount.funded.group) + scale_color_brewer(type="seq", palette="YlOrRd") 
p1 <- p1 + geom_smooth(aes(group=1), method="lm") 
p1 <- p1 + geom_hline(data=medians, aes(yintercept=median.interest))
p1 <- p1 + scale_x_discrete(labels=fico.labels) + theme_bw()
p1 <- p1 + labs(title="Interest rate vs. FICO rating",
                x="FICO credit score", y="Interest rate (%)",
                color="Income quartile")
ggsave("A1_plot.pdf", plot=p1)

### Regression analysis

m1 <- lm(interest ~ fico + amount.funded + length + open.lines + portion.funded
         + credit.balance + di.ratio + inquiries, data=ld)
summary(m1)
anova(m1)

m2 <- lm(interest ~ fico + purpose + income + state + home + portion.funded
         + amount.funded*length + empl.length, data=ld)
summary(m2)
anova(m2)

m3 <- lm(interest ~ fico + purpose + income + region + home + portion.funded
         + amount.funded*length + empl.length, data=ld)
summary(m3)
anova(m3)





