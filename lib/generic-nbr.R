### Lable Ranking prediciton function----
pred.cont <- function(rank.data, model, test.data) {
    prod <- -log(model$priors) + sapply(1:nrow(rank.data), function(r) {
        cond <- sapply(1:length(test.data), function(i) {
            dnorm(test.data[i], mean = model$cond$mean[r, i], 
                sd = model$cond$sdev[r, i])
        })
        # cond[is.infinite(cond)]<-NA cond<-round(cond,5)
        # cond[which(cond==0)]<-NA cond
        sum(-log(cond), na.rm = T)
    })
    rank.data[which.min(prod), ]
#   prod <-model$priors*
#   sapply(1:nrow(rank.data), function(r) {
#     cond <- sapply(1:length(test.data), function(i) {
#       pnorm(test.data[i], mean = model$cond$mean[r, i], 
#         sd = model$cond$sdev[r, i])
#     })
#     cond[is.infinite(cond)]<-NA 
#     cond<-round(cond,5)
#     cond[which(cond==0)]<-NA 
#     cond
#     prod(cond, na.rm = T)
#   })
#     rank.data[which.max(prod), ]
  
}

### Function to create NB model----
lr.model <- function(correlations, data, weights) {
    priors <- sapply(1:ncol(correlations), function(r) {
        weighted.mean(correlations[r, ], weights, na.rm = T)
    })
    
    attributes <- 1:ncol(data)
    mu <- weights * t(sapply(1:nrow(correlations), function(r) {
        sapply(attributes, function(x) {
            sum(data[, x] * correlations[r, ], na.rm = T)/sum(correlations[r, 
                ], na.rm = T)
        })
    }))
    sigma <- weights * t(sapply(1:nrow(correlations), function(r) {
        sapply(attributes, function(x) {
            sqrt(sum(correlations[r, ] * (data[, x] - mu[r, x])^2, 
                na.rm = T)/sum(correlations[r, ], na.rm = T))
        })
        
    }))
    conditionals <- list(mean = mu, sdev = sigma)
    list(priors = priors, cond = conditionals)
}

evaluation.simple <- function(tr, pr, method = "spearman", use = "p") {
    cor(tr, pr, method = method, use = use)
}

### Function wrap model and predictions-----
nbr.generic <- function(rank.data, data, test.data, weights) {
        correlations <- rescale(cor(t(rank.data), use = "p"),from=c(-1,1))
        model <- lr.model(correlations, data, weights)
        pred.cont(rank.data, model, as.numeric(test.data))
}

#### Discriminative Power functions: NB model only + discrim.
#### power-----
nbr.generic.model <- function(rank.data, data) {
    correlations <- (cor(t(rank.data), use = "pairwise.complete.obs") + 
        1)/2
    
    attributes <- 1:ncol(data)
    mu <- t(sapply(1:nrow(correlations), function(r) {
        sapply(attributes, function(x) {
            sum(data[, x] * correlations[r, ], na.rm = T)/sum(correlations[r, 
                ], na.rm = T)
        })
    }))
    sigma <- t(sapply(1:nrow(correlations), function(r) {
        sapply(attributes, function(x) {
            sqrt(sum(correlations[r, ] * (data[, x] - mu[r, x])^2, 
                na.rm = T)/sum(correlations[r, ], na.rm = T))
        })
        
    }))
    list(priors = correlations, mean = mu, sdev = sigma)
}

bayes.model <- function(rank.data, data = data.sym.a, n) {
    correlations <- (cor(t(rank.data), use = "pairwise.complete.obs") + 
        1)/2
    lapply(4:nrow(rank.data), function(i) {
        ### Weights: these are not case when give more weight to the
        ### last period rank (0...1), but the case that last period
        ### rank does not change a lot, i.e. 1....0
        weights <- n^((1:(i - 2))/(i - 2) - 1)
        model <- lr.model(correlations[2:(i - 1), 2:(i - 1), 
            drop = F], data[1:(i - 2), ], weights)
        discr.model(rank.data[2:(i - 1), ], model, data[i - 1, 
            ])
    })
}

discr.model <- function(sel.rank.data, model, test.data) {
    base.prob <- -log(model$priors)
    nomogr <- sapply(1:nrow(sel.rank.data), function(r) {
        cond <- sapply(1:length(test.data), function(i) {
            pnorm(test.data[i], mean = model$cond$mean[r, i], 
                sd = model$cond$sdev[r, i], lower.tail = F)
        })
        cond[is.infinite(cond)] <- NA
        cond <- round(cond, 5)
        cond[which(cond == 0)] <- NA
        -log(cond)
    })
    list(base = base.prob, nom = nomogr)
}


### Implementing generic funciton for growing-window-----
rankings.time.corrected.gw.cont <- function(rank.data, data, 
    n) {
    # correlations<-(
    # cor(t(rank.data),use='pairwise.complete.obs')+1)/2
    sapply(4:nrow(rank.data), function(i) {
        weights <- n^((1:(i - 2))/(i - 2) - 1)
        
        predict.rank <- nbr.generic(rank.data[2:(i - 1), ], data[1:(i - 
            2), ], as.numeric(data[i - 1, ]), weights)
        
        if (length(predict.rank) == 0) {
            rep(NA, length(rank.data[i - 1, ]))
        } else {
            predict.rank
        }
    })
   # na.m <- matrix(NA, ncol = 2, nrow = length(rank.data[1, ]))
  #  cbind(na.m, pred.rank.m)
}
rankings.time.corrected.gw.cont <- function(rank.data, data,n) {
        # correlations<-(
        # cor(t(rank.data),use='pairwise.complete.obs')+1)/2
        sapply(3:nrow(rank.data), function(i) {
                weights <- n^((1:(i - 1))/(i - 1) - 1)
                
                predict.rank <- nbr.generic(rank.data[1:(i - 1), ], data[1:(i-1), ], as.numeric(data[i, ]), weights)
                
                if (length(predict.rank) == 0) {
                        rep(NA, length(rank.data[i - 1, ]))
                } else {
                        predict.rank
                }
        })
        # na.m <- matrix(NA, ncol = 2, nrow = length(rank.data[1, ]))
        #  cbind(na.m, pred.rank.m)
}

#### n-fold cross validation----
rankings.n.fold.cv <- function(rank.data, data, fold, my.seed = 1) {
    set.seed <- my.seed
    folds <- rep(1:fold, length.out = nrow(rank.data))[order(runif(nrow(rank.data)))]
    
    mclapply(1:fold, function(i) {
        weights <- rep(1, nrow(rank.data[folds != i, ]))
        rows <- which(folds == i)
        predict.rank <- t(sapply(rows, function(f) {
            nbr.generic(rank.data[folds != i, ], data[folds != 
                i, ], as.numeric(data[f, ]), weights)
        }))
        
        rownames(predict.rank) <- rows
        predict.rank
    }, mc.cores = getOption("mc.cores", 2L))
}


# library(abind) cont.data <-
# read.csv('/Users/aiguzhinov/Documents/Dropbox/workspace/Naive.Bayes.separate.functions/cont.data.na.csv',
# header = T, sep = ',', row.names = 1) rank.data <-
# cont.data[, c(5:7)] data <- cont.data[, c(1:4)] #
# ###Prediciton exmaple---- i = 14 n = 100 weights <-
# n^((1:i)/i - 1)
# nbr.generic(rank.data=ranking[1:i,],data=features[1:i,],test.data=features[i+1,],weights=rep(1,i))
# rankings.n.fold.cv(ranking,features,10) example.gw <-
# rankings.time.corrected.gw.cont(ranking,features,5)
# example.array <-
# abind(true=ranking[4:14,],pred=t(example.gw),along=3)
# accuracy <-
# apply(example.array,1,function(s){apply(s,2,evaluation.simple,s[,1])})
# apply(accuracy,1,mean) 
