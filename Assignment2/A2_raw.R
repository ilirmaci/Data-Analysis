# to try: hierarchical clustering
#         SVD
#         step-wise selection
#         trees

setwd('/data/Dropbox/Data Analysis/Assignment2/')
load('samsungData.rda')

s <-  samsungData
rm(samsungData)

# create unique variable names
names.backup <- names(s)  ## in case we want to recover them
names(s) = make.names(names(s), unique=TRUE)

# remove trailing dots from names
names(s) <- gsub('[[:punct:]]+$', replacement='', x=names(s))

# remove multiple contiguous dots
names(s) <- gsub('[[:punct:]]+', replacement='.', x=names(s))

# change activity into a factor variable
s$activity <- factor(s$activity)


# define training, cross-validation, and test sets
# set to 60%, 20%, and 20% respectively
s$set <- cut(s$subject, breaks=c(0, 21, 26, 32), 
             labels=c('training', 'validation', 'test'))
table(s$set, s$subject)





## Hierarchical clustering ##
#############################

# distance measure of all columns except subject and activity
d <- dist(s[, -c(562, 563)])
hc <- hclust(d)

## Singular Value Decomposition ###

# check for all-zero rows or colums
which(rowSums(s[, -(562:564)]) == 0)

# apply SVD on training set (except last three columns)
svd.training <- svd(scale(s[s$set=='training', -(562:564)]))


# variation explained by principal components
expl.variation <- svd.training$d^2/sum(svd.training$d^2) 
plot(expl.variation)

# variation explained by first 60 and 100 variables
sum(expl.variation[1:60])
sum(expl.variation[1:100])


# create reduced data matrix with first 60 columns
s60 <- as.data.frame(scale(s[s$set=='training', -(562:564)]) %*% svd.training$v[, 1:60])
s60$activity <- s$activity[s$set=='training']

# create reduced data matrix with first 100 columns
s100 <- as.data.frame(scale(s[s$set=='training', -(562:564)]) %*% svd.training$v[, 1:100])
s100$activity <- s$activity[s$set=='training']
                                          
## Multinomial LOGIT regression ###

mnl60 <- multinom(activity ~ ., data=s60, maxit=1000)
mnl100 <- multinom(activity ~ ., data=s100, maxit=1000)

# get prediction error rates on training and validation data
table(predict(mnl60), s60$activity) -> t60
sum(diag(t60))/sum(t60) ## training set error


# transform validation data
s60.val <- as.data.frame(scale(s[s$set=='validation', -(562:564)]) 
                         %*% svd.training$v[, 1:60])

table(predict(mnl60, newdata=s60.val), s$activity[s$set=='validation']) -> v60
sum(diag(v60))/sum(v60) ## validation set error

# get prediction error rates on training and validation data
table(predict(mnl100), s100$activity) -> t100
sum(diag(t100))/sum(t100) ## training set error

# transform validation data
s100.val <- as.data.frame(scale(s[s$set=='validation', -(562:564)]) 
                               %*% svd.training$v[, 1:100])
      
table(predict(mnl100, newdata=s100.val), s$activity[s$set=='validation']) -> v100
sum(diag(v100))/sum(v100) ## validation set error


## NEURAL NETWORKS ###
######################

# vector of regularization parameters to try
lambda <- c(0.01, 0.03, 0.1, 0.3, 0.5, 1, 1.5, 3, 5, 10)

# with s60 data

# data table to record prediction errors
p60 <- data.frame(lambda=lambda, training=NA, validation=NA)

for(i in seq_along(lambda)){
    nn60 <- nnet(activity~., data=s60, size=14, maxit=2500, decay=lambda[i])
    
    activity.hat <- predict(nn60, type='class')
    table(activity.hat, s60$activity) -> t60.nn
    activity.hat <- predict(nn60, newdata=s60.val, type='class')
    table(activity.hat, s$activity[s$set=='validation']) -> v60.nn
    
    # training set precision
    p60$training[i] <- sum(diag(t60.nn))/sum(t60.nn) 
    # validation set precision
    p60$validation[i] <- sum(diag(v60.nn))/sum(v60.nn)
}

# lambda that gives best fit
p60$lambda[which.max(p60$validation)]

# with s100 data

# data table to record prediction errors
p100 <- data.frame(lambda=lambda, training=NA, validation=NA)

for(i in seq_along(lambda)){
    nn100 <- nnet(activity~., data=s100, size=14, maxit=2500, 
                  decay=10, MaxNWts=2000)
    
    activity.hat <- predict(nn100, type='class')
    table(activity.hat, s100$activity) -> t100.nn
    activity.hat <- predict(nn100, newdata=s100.val, type='class')
    table(activity.hat, s$activity[s$set=='validation']) -> v100.nn
    
    # training set precision
    p100$training[i] <- sum(diag(t100.nn))/sum(t100.nn) 
    # validation set precision
    p100$validation[i] <- sum(diag(v100.nn))/sum(v100.nn)
}

# lambda that gives best fit
p100$lambda[which.max(p100$validation)]

## with optimal lambdas
nn60 <- nnet(activity~., data=s60, size=14, maxit=2500, decay=0.3)

activity.hat <- predict(nn60, type='class')
table(activity.hat, s60$activity) -> t60.nn
activity.hat <- predict(nn60, newdata=s60.val, type='class')
table(activity.hat, s$activity[s$set=='validation']) -> v60.nn
sum(diag(t60.nn))/sum(t60.nn) 
sum(diag(v60.nn))/sum(v60.nn)


nn100 <- nnet(activity~., data=s100, size=14, maxit=2500, decay=0.3, MaxNWts=2000)

activity.hat <- predict(nn100, type='class')
table(activity.hat, s100$activity) -> t100.nn
activity.hat <- predict(nn100, newdata=s100.val, type='class')
table(activity.hat, s$activity[s$set=='validation']) -> v100.nn
sum(diag(t100.nn))/sum(t100.nn) 
sum(diag(v100.nn))/sum(v100.nn)

## PLOT ##
##########
png(file='A2_plot.png', width=800, height=400)
par(mfrow=c(1, 2), cex=1)
plot(p60$lambda, p60$training, type='l', ylim=c(0.85, 1), 
     xlab='Weight decay (regularization)',
     ylab='Success rate (correct predictions / total predictions)',
     main='Figure 1.a: 60-column data')
lines(p60$lambda, p60$validation, col='red', lwd=2)

plot(p100$lambda, p100$training, type='l', ylim=c(0.85, 1),
     xlab='Weight decay (regularization)',
     ylab='Success rate (correct predictions / total predictions)',
     main='Figure 1.b: 100-column data')
lines(p100$lambda, p100$validation, col='red', lwd=2)
legend('bottomright', legend=c('Training', 'Validation'), lwd=c(1,2), col=c(1,2))
par(mfrow=c(1, 1))
dev.off()

## Test data results ##
#######################

# transform data
s100.test <- as.data.frame(scale(s[s$set=='test', -(562:564)]) 
                          %*% svd.training$v[, 1:100])
# MNL
table(predict(mnl100, newdata=s100.test), s$activity[s$set=='test']) -> test100
sum(diag(test100))/sum(test100) ## validation set error

# Neural network
activity.hat <- predict(nn100, newdata=s100.test, type='class')
table(activity.hat, s$activity[s$set=='test']) -> test100.nn
sum(diag(test100.nn))/sum(test100.nn) 
