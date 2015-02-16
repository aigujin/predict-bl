rank.exp.ret.f <- function(tr.r,rec) {
        if(all(is.na(tr.r))|all(is.na(rec))){0}else{
  ones <- as.matrix(rep(1,length(tr.r)))
  tr.r.w <- 1-(tr.r-min(tr.r,na.rm=T))/max(tr.r,na.rm=T)
  tr.r.w[is.na(tr.r.w)] <- 0
  rec[is.na(rec)] <- 0
  #crossprod(rec,tr.r.w)/crossprod(ones,tr.r.w)
  sum(rec*tr.r.w)/sum(tr.r.w)
        }
  #ifelse(round(val,3)==0,NA,val)
}

buildQs <- function(stocks=stocks,rankings=rankings,ret=ret,...){
  require(plyr)
  sapply(stocks,function(s){
    mapply(rank.exp.ret.f,alply(rankings[,,s],1),alply(ret[,,s],1))
  },...)
}

buildOmega <- function(accuracy,step=4,weights=c(0.1,0.3,0.7,1)){
        ma <- apply(accuracy,c(1,3),rollapply,step,mean,na.rm=T,fill=NA,partial=T)
        w.ma<- apply(accuracy,c(1,3),rollapply,step,weighted.mean,weights,na.rm=T,by=1,fill=NA,)

  abind(aperm(accuracy,c(2,1,3)),ma,w.ma,along=4,new.names=list(NULL,NULL,NULL,c('last','ma','w.ma')))
}




###remove numbers; i+1...n for true, and i...n-1 all others
bl.script.f <- function(stocks,rankings,ret,t=42) {

  #true.Q <- buildQs(stocks,rankings[,,,'true'],ret[(dim(ret)[1]-t+1):dim(ret)[1],,])
  #all.Q<- aaply(rankings[,,,2:dim(rankings)[[4]]],4, buildQs,stocks=stocks,ret=ret[(dim(ret)[1]-t):(dim(ret)[1]-1),,],.drop=F)
  true.Q <- buildQs(stocks,rankings[,,,'true'],ret[(dim(ret)[1]-t):(dim(ret)[1]-1),,])
  all.Q<- aaply(rankings[,,,2:dim(rankings)[[4]]],4, buildQs,stocks=stocks,ret=ret[(dim(ret)[1]-t+1):dim(ret)[1],,],.drop=F)
  abind(true=true.Q,aperm(all.Q,c(2,3,1)),along=3)
}


# script.weights.env.f <- function(method,m=market.in,ret,conf,tau,delta) {
#   lapply(seq_along(m),function(q) {
#     list2env(m[[q]],envir=.viewsEnv)
#     assign('ret',ret[q,,method],pos=.viewsEnv)
#     assign('conf',conf[q,method,],pos=.viewsEnv)
#
#     with(.viewsEnv,{
#       makeViews(ret,conf,stock.names);
#       black.litterman(tau,impl.ret,confid,sigma,p,Q,rf);
#       opt.w <- optm.w.f(postMu,post.sigma,delta);
#       list(opt.w=opt.w,n.views=nrow(Q),rf=rf)
#       })
#   })
# }

# script.weights.env.f <- function(method,m=market.in,ret,conf,tau,delta) {
#   lapply(seq_along(m),function(q) {
#     list2env(m[[q]],envir=.GlobalEnv)
#     makeViews(ret[q,,method],conf[q,method,],stock.names)
#     black.litterman(tau,impl.ret,confid,sigma,p,Q,rf)
#     opt.w <- optm.w.f(postMu,post.sigma,delta);
#     #opt.w <- optm.w.constains.f(postMu,post.sigma,delta);
#     list(opt.w=opt.w,n.views=nrow(Q),rf=rf)
#   })
# }

script.weights.env.f <- function(method,m=market.in,ret,conf,tau,delta) {
  lapply(seq_along(m),function(q) {
    list2env(m[[q]],envir=.GlobalEnv)
    makeViews(ret[q,],conf[q,],stock.names)
    black.litterman(tau,impl.ret,confid,sigma,p,Q,rf)
    opt.w <- optm.w.f(postMu,post.sigma,delta);
    #opt.w <- optm.w.constains.f(postMu,post.sigma,delta);
    data.table(Stock=rownames(opt.w),opt.w=as.vector(opt.w),n.views=nrow(Q),q.id=q,ex.ret=ex.ret,check.names=T)[,':='(port.ret=sum(opt.w*ex.ret),Method=method,Quarters=Quarters)]
  })
}

non.rank.script.weights.env.f <- function(method,m,ret,conf,tau) {
        lapply(seq_along(m),function(q) {
                #list2env(m[[q]],envir=.GlobalEnv)
                views <- non.rank.makeViews(ret[q,],conf[q,],m[[q]]$stock.names)
                bl <- black.litterman(tau,m[[q]]$impl.ret,views$confid,m[[q]]$sigma,views$p,views$Q,m[[q]]$rf)
                n.views=nrow(views$Q)
                
        opt.w <- optm.w.f(bl$postMu,bl$post.sigma,delta)
        #ns.opt.w <- optm.w.constains.f(bl$postMu,bl$post.sigma,delta)
data.table(Stock=rownames(opt.w),opt.w=as.vector(opt.w[,1]),ns.opt.w=as.vector(opt.w[,2]),n.views=n.views,q.id=q,ex.ret=m[[q]]$ex.ret)[,':='(port.ret=sum(opt.w*ex.ret),ns.port.ret=sum(ns.opt.w*ex.ret),Method=method,Quarters=m[[q]]$Quarters)][,ann.port.ret:=(1+port.ret)^(1/4)-1]
                
        })
}

###Using BLCOP package
blcop.f <- function(method,m,ret,conf,tau) {
        require("BLCOP")
        lapply(seq_along(m),function(q) {
                #list2env(m[[q]],envir=.GlobalEnv)
                views <- non.rank.makeViews(ret[q,],conf[q,],m[[q]]$stock.names)
                m.views <- BLViews(P=views$p,q=as.numeric(views$Q),views$confid,colnames(views$p))
                bl <- posteriorEst(m.views,as.numeric(m[[q]]$impl.ret),tau,m[[q]]$sigma,0.01)
                n.views=nrow(views$Q)
opt.w <- getPortfolio(optimalPortfolios.fPort(bl)[[2]])$weights
ns.opt.w <- getPortfolio(optimalPortfolios.fPort(bl,constraints = "Short")[[2]])$weights
                data.table(Stock=names(opt.w),opt.w=as.vector(opt.w),ns.opt.w=as.vector(ns.opt.w),n.views=n.views,q.id=q,ex.ret=m[[q]]$ex.ret)[,':='(port.ret=sum(opt.w*ex.ret),ns.port.ret=sum(ns.opt.w*ex.ret),Method=method,Quarters=m[[q]]$Quarters)][,ann.port.ret:=(1+port.ret)^(1/4)-1]
                
        })
}




script.bl.model.f <- function(method,m=market.in,ret,conf,tau) {
  lapply(seq_along(m),function(q) {
    list2env(m[[q]],envir=.GlobalEnv)
    makeViews(ret[q,,method],conf[q,method,],stock.names)
    black.litterman(tau,impl.ret,confid,sigma,p,Q,rf)
    list(postMu=postMu,post.sigma=post.sigma,n.views=nrow(Q),rf=rf)

  })
}


#conf.last=views.omegas[2,bl.period[1],1,,'last']
#conf.ma=views.omegas[2,bl.period[1],1,,'ma']

#q.ret=views.Qs[2,bl.period[1],,1]
###Discuss with Ana Paula
makeViews <- function(q.ret,conf,sp.stocks){
  ### Set zeros and infinite to NAs:
  require(scales)
 cleanModel <- zeros2NA(q.ret)

  ###Find stocks with  no NA, (no zero in returns) and accuracy:
  valid.stocks.ret <- names(show.no.na(cleanModel))
  valid.stocks.conf <- names(show.no.na(conf))


  ###Match stocks in views with stocks in SP500:
  valid.s <- intersect(sp.stocks,intersect(valid.stocks.ret,valid.stocks.conf))
  #valid.s <- valid.stocks[which(valid.stocks%in%sp.stocks)]

  ###Obtaining Qs:
  Q <- as.matrix(q.ret[valid.s])/4

  ###Obtaining BL omegas:
  #scaled.accuracy <- round((conf[valid.s]+1)/2,4)
  scaled.accuracy<-round(rescale(conf[valid.s],from=c(-1,1)),4)
  scaled.accuracy[scaled.accuracy==0] <- 0.00001
  omegas <- (1-scaled.accuracy)/scaled.accuracy

  ###Building the p-matrix, put 1's to matched stocks:
  pickMatrix<-diag(0, length(sp.stocks), length(sp.stocks))
  dimnames(pickMatrix) <- list(sp.stocks, sp.stocks)
  diag(pickMatrix[valid.s,valid.s]) <- 1
  p.views<-pickMatrix[valid.s,]
  p.views <- if(nrow(Q)==1){
    t(p.views)
  }
  else {p.views}
  assign('Q',Q,pos=.GlobalEnv)
  assign('p',p.views,pos=.GlobalEnv)
  assign('confid',omegas,pos=.GlobalEnv)
}

non.rank.makeViews <- function(q.ret,conf,sp.stocks){
        ### Set zeros and infinite to NAs:
        require(scales)
        cleanModel <- zeros2NA(q.ret)
        
        ###Find stocks with  no NA, (no zero in returns) and accuracy:
        valid.stocks.ret <- names(show.no.na(cleanModel))
        valid.stocks.conf <- names(show.no.na(conf))
        
        
        ###Match stocks in views with stocks in SP500:
        valid.s <- intersect(sp.stocks,intersect(valid.stocks.ret,valid.stocks.conf))
        #valid.s <- valid.stocks[which(valid.stocks%in%sp.stocks)]
        
        ###Obtaining Qs:
        Q <- as.matrix(q.ret[valid.s])
        
        ###Obtaining BL omegas:
        omegas <- conf[valid.s]
        #scaled.accuracy<-round(rescale(conf[valid.s],from=c(-1,1)),4)
        #scaled.accuracy[scaled.accuracy==0] <- 0.00001
        #omegas <- (1-scaled.accuracy)/scaled.accuracy
        
        ###Building the p-matrix, put 1's to matched stocks:
        pickMatrix<-diag(0, length(sp.stocks), length(sp.stocks))
        dimnames(pickMatrix) <- list(sp.stocks, sp.stocks)
        diag(pickMatrix[valid.s,valid.s]) <- 1
        p.views<-pickMatrix[valid.s,]
        p.views <- if(nrow(Q)==1){
                t(p.views)
        }
        else {p.views}
        #assign('Q',Q,pos=.GlobalEnv)
        #assign('p',p.views,pos=.GlobalEnv)
        #assign('confid',omegas,pos=.GlobalEnv)
        list(Q=Q,p=p.views,confid=omegas)
}


black.litterman <- function(tau,mu,confid,covar,p,Q,rf){
  if(length(Q)==0){list(postMu=mu,post.sigma=covar*tau)}
  else{

#     temp<-tcrossprod(covar,p)
#      omega <- diag(diag(confid*p%*%temp))
#      postMu<-mu + tau * temp %*% ginv(tau * p %*% temp + omega)%*% ((Q-rf*4) - p %*% mu)
#      M <- tau*covar - tau*temp %*% ginv(tau * p %*% temp + omega)%*%p%*%(tau*covar)
    aux.1<-tcrossprod(covar,p)
    omega <- diag(diag(confid*p%*%aux.1))
    aux.2 <-  tau*aux.1 %*% ginv(tau * p %*% aux.1 + omega)
    postMu<-mu + aux.2 %*% ((Q-rf*4) - p %*% mu)
    M <- tau * covar - aux.2 %*% p %*% (tau*covar)

    post.sigma<-covar+M

    #assign('postMu',postMu,pos=.GlobalEnv)
    #assign('post.sigma',post.sigma,pos=.GlobalEnv)
    list(postMu=postMu,post.sigma=post.sigma)
  }
}


optm.w.f <- function(postMu,post.sigma,delta){
  #   ####Herlod 2005 weights with sum = 1
  # ones<-  as.matrix(rep(1,ncol(post.sigma)))
  #temp <- crossprod(ones,ginv(post.sigma))
  #a.her <- temp%*%ones
  #  a.her <- crossprod(ones,inv.sig)%*%ones
  # b.her <- temp%*%postMu
###Efficent set constants
  #a <- temp%*%postMu
  #b <- crossprod(postMu,ginv(post.sigma))%*%postMu
  #c <- temp%*%ones
  ###minimum variance portfolio
  #w <- crossprod(ginv(post.sigma),ones)%*%ginv(a.her )
  #h.mv <- crossprod(ginv(post.sigma),ones)%*%ginv(c)
  #h.var <- crossprod(ginv(post.sigma),postMu-tcrossprod(ones,a/c))
  #w <- h.mv+h.var
  ###tangent portfolio
  # full form
  #tan.p <- crossprod(ginv(post.sigma),postMu)%*%ginv(crossprod(ones,ginv(post.sigma))%*%postMu)
  #w <- crossprod(ginv(post.sigma*delta),postMu)%*%ginv(a)

 #h.w <- ginv(post.sigma)%*%ones%*%ginv(a.her*delta)%*%(delta-b.her)+ginv(post.sigma)%*%postMu%*%ginv(delta)

  post.w.temp <- ginv(delta*post.sigma)%*%postMu
  #w <- crossprod(ginv(post.sigma),ones)%*%ginv(crossprod(ones,ginv(post.sigma))%*%ones )

  s.w <- post.w.temp/sum(post.w.temp)
  require("tseries")
  ns.w <- as.matrix(portfolio.optim(t(postMu),covmat=post.sigma,shorts=F)$pw)
  w <- cbind(s.w,ns.w)
  rownames(w) <- rownames(postMu)
  w
}

optm.w.f.old <- function(postMu,post.sigma,delta){
        ####Herlod 2005 weights with sum = 1
        ones<-  as.matrix(rep(1,ncol(post.sigma)))
        temp <- crossprod(ones,ginv(post.sigma))
        a <- temp%*%postMu
        w <- crossprod(ginv(post.sigma*delta),postMu)%*%ginv(a)
        rownames(w) <- rownames(postMu)
        w
}

portfolio.return.f <- function(w,ret,rf){
  real.mu<-ret-rf
  real.mu[is.na(real.mu)]<-0
  real.mu[is.infinite(real.mu)]<-0
  crossprod(w,t(real.mu))
}

market.return <- function(market.weights,stock.ret,rf){
  portfolio.return.f(market.weights,stock.ret,rf)
}

turnover <- function(opt.w)
{
  sapply(2:length(opt.w),function(q)
  {
    activ.stocks.end <- rownames(opt.w[[q]]$opt.w)
    activ.stocks.beg <- rownames(opt.w[[q-1]]$opt.w)
    stocks <- intersect(activ.stocks.end,activ.stocks.beg)
    w.beg<-opt.w[[q-1]][[1]][stocks,]
    w.end<-opt.w[[q]][[1]][stocks,]
    turnover.f(w.beg,w.end)
  })
}

market.turnover <- function(w)
{
  sapply(2:length(w),function(q)
  {
    activ.stocks.end <- names(w[[q]])
    activ.stocks.beg <- names(w[[q-1]])
    stocks <- intersect(activ.stocks.end,activ.stocks.beg)
    w.beg<-w[[q-1]][stocks]
    w.end<-w[[q]][stocks]
    turnover.f(w.beg,w.end)
  })
}


turnover.f <- function(w.beg,w.end)
{
        purchases.TO<-ifelse(w.end-w.beg<0,0,w.end-w.beg)
        sales.TO<-ifelse(w.beg-w.end<0,0,w.beg-w.end)
        min(sum(purchases.TO,na.rm=T),sum(sales.TO,na.rm=T))
        
}


optm.w.constains.f <- function(postMu,post.sigma,delta){
  require(quadprog)
# post.w.temp <- ginv(delta*post.sigma)%*%postMu
#   #w <- post.w.temp/sum(post.w.temp)
#   #rownames(w) <- rownames(postMu)
#   #w
#   ###Constrains: budget (sum(w)==1)  and no leverging (-1<w<1)
#   ##solve.QP(Dmat,dvec,Amat,bvec,meq=0,factorized=F)
#  Dmat <- post.sigma
#  dvec <- postMu*delta
#  n <- nrow(Dmat) # number of assets

### tseries method portfolio.optim
require("tseries")
w <- as.matrix(portfolio.optim(t(postMu),covmat=post.sigma,shorts=F)$pw)
#post.w.temp <- ginv(delta*Dmat)%*%postMu
#w <- post.w.temp/sum(post.w.temp)
#   
#   # Constraints: sum(w) = 1
#   Amat <- matrix(1, nrow(Dmat))
#   bvec <- 1
#   meq <- 1
#   qp <- solve.QP(Dmat, dvec, Amat, bvec, meq)
#   # Constraints: sum(w) = 1 & no short-sell
#    Amat <- cbind(1, diag(nrow(Dmat)))
#  Amat <- cbind(                 # One constraint per column
#          matrix( rep(1,n), nr=n ), # The weights sum up to 1
#          diag(n)                   # No short-selling
#  )
#    bvec <- c(1,rep(0,n))
#     meq <- 1
#     qp <- solve.QP(Dmat, dvec, Amat, bvec, meq)
#     qp$solution[abs(qp$solution) <= 1e-7] <- 0
# ###Constrains: budget (sum(w)==1)  and no levereging (-1<w<1)
# Amat <- cbind(1,-diag(nrow(post.sigma)),diag(nrow(post.sigma)))
#  bvec <- c(1, rep(-1, nrow(post.sigma)),-rep(1, nrow(post.sigma)))
#  meq <- 0
#  qp <- solve.QP(post.sigma, postMu*delta, Amat, bvec, meq)
#  qp$solution[abs(qp$solution) <= 1e-7] <- 0
 #Amat <- cbind(matrix(rep(1,n),nr=n),diag(n),-1*diag(n))
 #bvec <- c(1,rep(0,n),rep(-1,n))
 #meq <- 1
 #qp <- solve.QP(Dmat, dvec, Amat, bvec, meq)
 #qp$solution[abs(qp$solution) <= 1e-7] <- 0
 #w <- as.matrix(qp$solution)
  rownames(w) <- rownames(postMu)
  w
}



opt.w.f <- function(views,conf,tau){
        
        #rbindlist(lapply(agg.names,function(agg){
                #list.conf <- alply(conf[,,,agg],2,.dims=T)
                rbindlist(mclapply(dimnames(views)[[3]],function(type){
                        rbindlist(non.rank.script.weights.env.f(type,market.list[m.period],views[bl.period,,type],conf[bl.period,,type],tau))
                },mc.cores=4)
                )
                #}))
}

script.blcop.f <- function(views,conf,tau){
        
        #rbindlist(lapply(agg.names,function(agg){
        #list.conf <- alply(conf[,,,agg],2,.dims=T)
        rbindlist(lapply(names(views),function(type){
                rbindlist(blcop.f(type,market.list[m.period],views[[type]][bl.period,],conf[bl.period,,type],tau))
        })
        )
        #}))
}



# operation.bl <- function(rankings,views,confidence)
# {
#   list.rank <- alply(bl.script.f(all.s,rankings[,all.b,all.s,],views,t=dim(rankings)[1]),3,.dims=T)
#   non.r.conf <- buildOmega(aperm(replicate(3,confidence),c(3,1,2)))
# non.r.conf[,1,,][!is.na(non.r.conf[,1,,])] <- 0
# dimnames(non.r.conf)[[2]] <- names(list.rank)
# case.1.non.r.conf <- non.r.conf[,,all.s,]
# dimnames(case.1.non.r.conf)[[2]] <- names(list.rank)
# opt.w.f(c('last','ma','w.ma'),list.rank,case.1.non.r.conf,0.02,delta)
# }

operation.bl <- function(list.views,confidence,conf.id)
{
  non.r.conf <- buildOmega(aperm(replicate(3,confidence),c(3,1,2)))
  
  dimnames(non.r.conf)[[2]] <- names(list.views)
  non.r.conf[,'true',,][!is.na(non.r.conf[,'true',,])] <- 0
  
  ##case.1.non.r.conf <- non.r.conf[,,all.s,]
  ##dimnames(case.1.non.r.conf)[[2]] <- names(list.views)
  opt.w.f(conf.id,list.views,non.r.conf,tau)
}

cum.ret.f <- function(Return,n.views,in.value=100){
        #ra <- (1+Return)^(1/4)-1
        ra <- Return
        cum.ret <- rollapplyr(ra,seq_len(length(ra)),function(i){prod(1+i)})
        list(
                #in.value+rollapplyr(Return,seq_len(length(Return)),Return.cumulative),
                in.value*cum.ret,
                prod(1+ra)^(4/length(ra))-1,
                #Return.cumulative(Return),
                #StdDev.annualized(Return,scale=4),
                sd(Return)*sqrt(4),
                mean(n.views)
        )
}






bl.results.f <- function(opt.w)
{
bl.results <- unique(opt.w,by=c('q.id','Method','Views'))[,list(Quarters=as.yearqtr(Quarters),Views,Method,port.ret,ns.port.ret,ann.port.ret,n.views,q.id)]
#risk.free <- setkey(rbindlist(lapply(seq_along(market.list),function(q){data.table(Quarters=as.yearqtr(market.list[[q]]$Quarters),rf=market.list[[q]]$rf)}))[m.period,],Quarters)
##Looks like these are the dates for SP500 from=1998-10-01 to 2009-09-30; shoud be from 1999-01-01 to 2009-12-31
#require(quantmod)
#prices = getSymbols("^GSPC", from = "1999-01-01", to = "2009-12-31")

#prices = getSymbols("SPY", from = "1999-01-01", to = "2009-12-31")
#prices = getSymbols("^GSPC", from = "1999-01-01", to = "2009-12-31")
#market.port.ret<- setnames(setkey(data.table(Quarters=index(quarterlyReturn(get(prices)[,6])),port.ret=coredata(quarterlyReturn(get(prices)[,6])))[,Quarters:=as.yearqtr(Quarters)],Quarters)[risk.free],2,'sp.ret')[,port.ret:=sp.ret-rf][,Views:='Market'][,n.views:=500][,ann.port.ret:=(1+port.ret)^(1/4)-1][,q.id:=.I][,.(Quarters,Views,port.ret,ann.port.ret,n.views,q.id)]

setkey(market.set,q.id)
market.set <- market.set[,':='(Views='Market',n.views=.N),by=q.id]
market.port.ret <- unique(market.set[,':='(port.ret=sum(mw*ex.ret),Views='Market',n.views=.N),by=q.id][,ann.port.ret:=(1+port.ret)^(1/4)-1][,ns.port.ret:=port.ret])[m.period[1]:44,list(Quarters=as.yearqtr(Quarters),Views,port.ret,ns.port.ret,ann.port.ret,n.views)][,q.id:=.I]


#market.data <- rbind(market.port.ret.spy,market.port.ret)

bl.results <- rbind(bl.results,bl.results[,as.list(market.port.ret),by=list(Method)],use.names=T)

bl.results[,c('cum.ret','ann.ret','ann.sd','meanViews'):=cum.ret.f(port.ret,n.views,100),by=list(Method,Views)][,ann.sr:=ann.ret/ann.sd]
setkey(bl.results,Method,Views)
to.bl <- rbindlist(lapply(2:length(bl.period),function(b){
  beg <- setkey(opt.w[q.id==b,list(Stock,opt.w),by=list(Method,Views)],Stock,Method,Views)
  end <- setkey(opt.w[q.id==b-1,list(Stock,opt.w),by=list(Method,Views)],Stock,Method,Views)
  end[beg][,turnover.f(opt.w,i.opt.w),by=list(Method,Views)]
  }))[,Ave.TO:=mean(V1),by=list(Method,Views)]

to.market <- rbindlist(lapply(2:length(m.period),function(b){
beg <- setkey(market.set[,q.num:=.GRP,by=q.id][q.num==m.period[b],list(Views,Stock,mw)],Stock,Views)
end <- setkey(market.set[,q.num:=.GRP,by=q.id][q.num==m.period[b-1],list(Views,Stock,mw)],Stock,Views)
end[beg,allow.cartesian=T][,turnover.f(mw,i.mw),by=Views]}))[,Ave.TO:=mean(V1),by=list(Views)]
setkey(bl.results,Views,Method)[setkey(rbind(to.bl[,list(Views,Method,Ave.TO)],to.bl[,as.list(unique(to.market,by='Ave.TO')[,list(Views,Method,Ave.TO)])],use.names=T),Views,Method),allow.cartesian=T]
}




pred.bl.results.f <- function(opt.w)
{
        bl.results <- unique(opt.w,by=c('q.id','Method','Views','confAgg'))[,list(Quarters=as.yearqtr(Quarters),Views,Method,port.ret,ns.port.ret,ann.port.ret,n.views,q.id,confAgg)]
        
        setkey(market.set,q.id)
        market.set <- market.set[,':='(Views='Market',n.views=.N),by=q.id]
        market.port.ret <- unique(market.set[,':='(port.ret=sum(mw*ex.ret),Method='Market',Views='Market',n.views=.N),by=q.id][,ann.port.ret:=(1+port.ret)^(1/4)-1][,ns.port.ret:=port.ret])[m.period[1]:44,list(Quarters=as.yearqtr(Quarters),Method,Views,port.ret,ns.port.ret,ann.port.ret,n.views)][,q.id:=.I]
        
        
        #market.data <- rbind(market.port.ret.spy,market.port.ret)
        
        bl.results <- rbind(bl.results,bl.results[,as.list(market.port.ret),by=list(confAgg)],use.names=T)
        
        bl.results[,c('cum.ret','ann.ret','ann.sd','meanViews'):=cum.ret.f(port.ret,n.views,100),by=list(Method,Views,confAgg)][,ann.sr:=ann.ret/ann.sd]
        setkey(bl.results,Method,confAgg,Views)
        to.bl <- rbindlist(lapply(2:length(m.period),function(b){
                beg <- setkey(opt.w[q.id==b,list(Stock,opt.w),by=list(Method,confAgg,Views)],Stock,Method,confAgg,Views)
                end <- setkey(opt.w[q.id==b-1,list(Stock,opt.w),by=list(Method,confAgg,Views)],Stock,Method,confAgg,Views)
                end[beg][,turnover.f(opt.w,i.opt.w),by=list(Method,confAgg,Views)]
        }))[,Ave.TO:=mean(V1),by=list(Method,confAgg,Views)]
        
        to.market <- rbindlist(lapply(2:length(m.period),function(b){
                beg <- setkey(market.set[,q.num:=.GRP,by=q.id][q.num==m.period[b],list(Views,Stock,mw)],Stock,Views)
                end <- setkey(market.set[,q.num:=.GRP,by=q.id][q.num==m.period[b-1],list(Views,Stock,mw)],Stock,Views)
                end[beg,allow.cartesian=T][,turnover.f(mw,i.mw),by=Views]}))[,':='(Ave.TO=mean(V1),Method='Market'),by=list(Views)]
        
        setkey(bl.results,Views,Method,confAgg)[setkey(rbind(to.bl[,list(Views,Method,confAgg,Ave.TO)],to.bl[,as.list(unique(to.market,by='Ave.TO')[,list(Views,Method,Ave.TO)]),by=confAgg],use.names=T),Views,Method,confAgg),allow.cartesian=T]
}

omega.f <- function(conf){
scaled.accuracy<-round(rescale(conf,from=c(-1,1)),4)
scaled.accuracy[scaled.accuracy==0] <- 0.00001
(1-scaled.accuracy)/scaled.accuracy
}

