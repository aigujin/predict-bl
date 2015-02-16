#source(analysts.dir("/Functions/aux.functions.R"))
mean.pmafe <- function(actual,forecast){
  ###forecast:matrix of dim(q,b)
  ###actual: matrix of dim(q,s)
fe <- apply(forecast,2,function(b){ abs(b-coredata(actual)) })
  ##fe: matrix of dim(q,b)
mean.fe <- apply(fe, 1, mean, na.rm = T)
  ##mean.f: vector of length=nrow(mean.fe)
score <- apply(fe,2,function(f){mean(f/mean.fe,na.rm=T) })
  ##score: vector of length=ncol(fe)
rank(score, na.last = "keep")
}
target.ranking <- function(stocks, forecasts, actuals, coverage=12,
    min.brokers = 3) {
    ## stocks=stock tickers;
    #forecast=analysts' forecasts (eps or pt) an array of dim(q,b,s);
  #actual=actual values (eps of prices) - a matrix dim(q,s);
  #coverage=min.number of quarters an analyts covering a stock;

    sel.s.in.act <- intersect(stocks,colnames(actuals))


    core.brok <- sort(dimnames(forecasts)[[2]][unique(unlist(apply(apply(forecasts[,
        , sel.s.in.act], c(2, 3), function(i) {
        length(show.no.na(i)) >= coverage
    }), 2, which)))])

    ## inter.t=dim(b,q,s)
    intermediate.tr <- abind(lapply(sel.s.in.act, function(s) {
        fe <- abs(as.numeric(actuals[, s]) - forecasts[, core.brok,
            s])
        mean.fe <- apply(fe, 1, mean, na.rm = T)
        apply(fe/mean.fe, 1, rank, na.last = 'keep')
    }), along = 3, new.names = c(NULL, NULL, sel.s.in.act))

    ### Core stocks: stocks with 3 and more brokers
    core.stocks <- names(which(unlist(lapply(apply(apply(intermediate.tr,
        c(2, 3), function(i) {
            length(show.no.na(i)) >= min.brokers
        }), 2, which), length)) != 0))

    tr <- aperm(intermediate.tr[core.brok, , core.stocks], c(2,
        1, 3))

    q.less.min <- apply(tr, c(1, 3), function(i) {
        length(show.no.na(i)) < min.brokers
    })
    for (q in 1:nrow(q.less.min)) {
        s.less.min <- which(q.less.min[q, ])
        tr[q, , s.less.min] <- NA
    }
    tr
}

filter.tr <- function(int.tr, coverage=12, min.brokers = 3) {
    ## stocks=stock tickers;
    #forecast=analysts' forecasts (eps or pt) an array of dim(q,b,s);
  #actual=actual values (eps of prices) - a matrix dim(q,s);
  #coverage=min.number of quarters an analyts covering a stock;

    sel.s.in.act <- dimnames(int.tr)[[2]]


    core.brok <- sort(dimnames(int.tr)[[3]][unique(unlist(apply(apply(int.tr[,sel.s.in.act,], c(2, 3), function(i) {
        length(show.no.na(i)) >= coverage
    }), 2, which)))])

    ### Core stocks: stocks with 3 and more brokers
    core.stocks <- names(which(unlist(lapply(apply(apply(int.tr,
        c(1, 2), function(i) {
            length(show.no.na(i)) >= min.brokers
        }), 2, which), length)) != 0))

    tr <- int.tr[ , core.stocks, core.brok]

    q.less.min <- apply(tr, c(1, 3), function(i) {
        length(show.no.na(i)) < min.brokers
    })
    for (q in 1:nrow(q.less.min)) {
        s.less.min <- which(q.less.min[q, ])
        tr[q, , s.less.min] <- NA
    }
    tr
}
###Old version of making baslines: its wrong one, used once to reproduce old results
# baseline.rankings.f <- function(tr, start.q) {
#         ###Stocks in dim 3
#         default <- abind(mclapply((start.q+1):(dim(tr)[1]), function(q) {
#                 apply(apply(tr[1:(q), drop = F, , ], c(2, 3), mean, na.rm = T),
#                       2, rank, na.last = "keep")
#         }, mc.cores = 4), along = 3)
#         abind(
#                 true = tr[start.q:(dim(tr)[1]-1), , ],
#                 naive = tr[(start.q+1):(dim(tr)[1]), , ],
#                 default = aperm(default, c(3, 1, 2)), along = 4)
# }

baseline.rankings.f <- function(tr, start.q) {
        ###Stocks in dim 3
default <- abind(mclapply((start.q+1):(dim(tr)[1]), function(q) {
apply(apply(tr[1:(q-1), drop = F, , ], c(2, 3), mean, na.rm = T),2, rank, na.last = "keep")}, mc.cores = 4), along = 3)


        na.m <- array(NA,dim=c(1,dim(tr)[2],dim(tr)[3]))
        abind(
                true = tr[start.q:(dim(tr)[1]), , ],
                naive = abind(na.m,tr[(start.q):(dim(tr)[1]-1), , ],along=1),
                default = abind(na.m,aperm(default, c(3, 1, 2)),along=1), along = 4)
}


vvs.combine <- function(stocks, broker,stock.vvs,t)
  {

    comb.vvs <- stock.vvs[, stocks, ]
    period.vvs = c((dim(comb.vvs)[1] - t + 1):dim(comb.vvs)[1])
    period.brok = c((dim(broker)[1] - t + 1):dim(broker)[1])
    abind(broker[period.brok, stocks, ], comb.vvs[period.vvs, , ], along = 3)
}



ind.var.versions <- function(data, method, a, b) {
    switch(method, raw = as.matrix(data), random = random.ts(data),
        '1diff' = diff.data(data, a), roll.sd = roll.sd(data,
            b))
}

random.ts <- function(data) {
    ind.data.no.na <- na.locf(data)
    ts.ind.var <- ts(ind.data.no.na, frequency = 4)
    decom.ts.random <- sapply(1:ncol(ts.ind.var), function(v) {
        if (length(ts.ind.var[, v][!is.na(ts.ind.var[, v])]) <
            8) {
            ts.ind.var[, v] <- rep(NA, nrow(data))
        } else decompose(ts.ind.var[, v], type = c("additive"))$random

    })
    colnames(decom.ts.random) <- colnames(ts.ind.var)
  new.data <- na.locf(decom.ts.random,na.rm=F)
  a <- nrow(data) - nrow(new.data)
   na.locf(rbind(new.data[rep(1, a), ], new.data),na.rm=F,fromLast = T)
}

diff.data <- function(data, lag) {
    diff.data <- diff(as.matrix(data), lag = lag)
    a <- nrow(data) - nrow(diff.data)
    rbind(diff.data[rep(1, a), ], diff.data)
}
roll.sd <- function(data, lag) {
    roll.data <- rollapply(data, lag, sd, na.rm = T)
    a <- nrow(data) - nrow(roll.data)
    rbind(roll.data[rep(1, a), ], roll.data)

}
state.vvs.f <- function(data, lag,lag.sd) {
  data[is.infinite(data)] <- NA
  list('raw'=data,
    '1diff'=c(NA,diff(data, lag = lag)),
    'roll.sd'=rollapplyr(data, lag.sd, sd, na.rm = T,fill=NA,partial=T),
    'random'=if(length(data[!is.na(data)])<=8){as.numeric(rep(NA,length(data)))} else {
      #decompose(ts(data,frequency=4))$random
      equal.length.f(data,coredata(stl(ts(data,frequency=4),s.window='periodic',na.action=na.approx)$time.series[,3]))
    }
  )
}


equal.length.f <- function(x,y)
{
  c(rep(NA,length(x)-length(y)),y)
}


# predirank.f <- function(methods, stock, tr, parameters) {
#     #period.m = c((dim(market)[1] - dim(tr)[1] + 1):dim(market)[1])
#     #period.s = c((dim(stock)[1] - dim(tr)[1] + 1):dim(stock)[1])
#     abind(mclapply(methods, function(meth) {
#         predict.ranking.sript.f(meth, stock,tr,parameters)
#       }, mc.cores = 2), along = 4, new.names = list(dimnames(tr)[[2]], NULL, dimnames(tr)[[3]], methods))
# }

predict.ranking.script.f <- function(methods, stock.names, stock,  tr, r.parameters) {
  n = as.numeric(r.parameters[1])
  diff.lag = as.numeric(r.parameters[2])
  sd.lag = as.numeric(r.parameters[3])
  x=as.numeric(r.parameters[4])

  abind(mclapply(stock.names, function(s) {

          mod.vvs <- acast(melt(setnames(data.table(melt(stock[,s,])),c('q.id','vvs','value'))[,state.vvs.f(value,diff.lag,sd.lag),by=vvs][,q.num:=1:.N,by=vvs],id.vars=c('vvs','q.num')),q.num~vvs~variable)

          abind(lapply(methods , function(meth) {

      rankings.time.corrected.gw.cont(tr[, , s], mod.vvs[,,meth], n)
      #analyst.prediction.f(tr[, , s], mod.vvs[,,meth], n)
      #roll.analyst.prediction.f(tr[, , s], mod.vvs[,,meth], n,x)
    }), along = 3)
  },mc.cores=4), along = 4, new.names = list(dimnames(tr)[[2]],NULL, methods,stock.names))
}

roll.ranking.f <- function(methods, stock.names, stock,  tr, r.parameters) {
        n = as.numeric(r.parameters[1])
        diff.lag = as.numeric(r.parameters[2])
        sd.lag = as.numeric(r.parameters[3])
        x=as.numeric(r.parameters[4])
        
        abind(mclapply(stock.names, function(s) {
                
                mod.vvs <- acast(melt(setnames(data.table(melt(stock[,s,])),c('q.id','vvs','value'))[,state.vvs.f(value,diff.lag,sd.lag),by=vvs][,q.num:=1:.N,by=vvs],id.vars=c('vvs','q.num')),q.num~vvs~variable)
                
                abind(lapply(methods , function(meth) {
                        roll.analyst.prediction.f(tr[, , s], mod.vvs[,,meth], n,x)
                }), along = 3)
        },mc.cores=4), along = 4, new.names = list(dimnames(tr)[[2]],NULL, methods,stock.names))
}
### Good results script
# predict.ranking.script.f <- function(methods, stock.names, stock, tr, r.parameters) {
#         n = as.numeric(r.parameters[1])
#         diff.lag = as.numeric(r.parameters[2])
#         sd.lag = as.numeric(r.parameters[3])
#
#         abind(mclapply(methods, function(meth) {
#                 abind(lapply(stock.names, function(s) {
#                         #per.stock.vvs <- stock[, s, ]
#                         #per.stock.vvs[is.infinite(as.matrix(per.stock.vvs))] <- NA
#                         #mod.vvs <- ind.var.versions(per.stock.vvs, meth, diff.lag,sd.lag)
#                         mod.vvs <- acast(setnames(data.table(melt(stock[,s,])),c('q.id','vvs','value'))[,state.vvs.f(value,diff.lag,sd.lag),by=vvs][,q.num:=1:.N,by=vvs],q.num~vvs,value.var = meth)
#
#                         #setnames(data.table(melt(stock[,s,])),c('q.id','vvs','value'))[,state.vvs.f(value,diff.lag,sd.lag),by=vvs][,q.num:=1:.N,by=vvs][vvs=='debt.to.eq']
#
#                         rankings.time.corrected.gw.cont(tr[, , s], mod.vvs, n)
#                 }), along = 3)
#         },mc.cores=4), along = 4, new.names = list(dimnames(tr)[[2]],NULL, dimnames(tr)[[3]], methods))
# }

analyst.prediction.f <- function(rank,features,n){
sapply(4:nrow(rank),function(t){
        model <- nbrModel(features[1:(t-2),drop=F,],rank[2:(t-1),drop=F,],round(timeWeights(t-2,n),4))
        predict.rank <- nbRank(model,y = rank[2:(t-1),drop=F,],test=features[t-1,drop=F,])
        if (length(predict.rank) == 0) {
                rep(NA, length(rank[t - 1, ]))
        } else {
                predict.rank
        }
# model <- nbrModel(features[1:(t-1),drop=F,],rank[1:(t-1),drop=F,],round(timeWeights(t-1,n),4))
#         predict.rank <- nbRank(model,y = rank[1:(t-1),drop=F,],test=features[t,drop=F,])
#         if (length(predict.rank) == 0) {
#                 rep(NA, length(rank[t, ]))
#         } else {
#                 predict.rank
#         }
})
}

roll.analyst.prediction.f <- function(rank,features,n,x){
        windowSize <- x+1
        windows <- embed(1:nrow(rank),windowSize)
        pred.t <- windows[,1]
        train.t <- windows[,2:windowSize]
        
        ### needs to calculate naive and default for each of the windows
        
        sapply(1:nrow(train.t),function(t){
                train <- rev(train.t[t,])
                weights <- n^((1:length(train))/length(train) - 1)
                predict.rank <- nbr.generic(rank[train,drop=F,],features[train,drop=F,],features[pred.t[t],drop=F,],weights)
                if (length(predict.rank) == 0) {
                        rep(NA, length(rank[pred.t[t], ]))
                } else {
                        predict.rank
                }
        })
}

roll.baselines.f <- function(y,x){
        windowSize <- x+1
        windows <- embed(1:nrow(y),windowSize)
        pred.t <- windows[,1]
        train.t <- windows[,2:windowSize]
        abind(mclapply(1:nrow(train.t),function(t){
                train <- rev(train.t[t,])
                naive <- y[tail(train,1),,]
                default <- apply(apply(y[train, drop = F, , ], c(2,3), mean, na.rm = T),2,rank,na.last = "keep")
                true <- y[pred.t[t],,]
                abind(true,naive,default,along=3)
        },mc.cores=4),along=4)
}


evaluation.simple <- function(tr,pr,method='spearman',use='p')
{
  cor(tr,pr,method=method,use=use)
}


