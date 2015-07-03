print(sel.vvs)
core.dt <- q.data[,core.b:=.N>=12,by=list(Stock,Broker)][(core.b)][,true:=rank(score),by=list(q.id,Stock)][,core.s:=.N>=3,by=list(q.id,Stock)][(core.s)][,core.q:=length(unique(q.id))>=8,by=.(Stock)][(core.q)]

broker.vvs <- acast(melt(unique(core.dt[,broker.vvs.f(PT,priceAdj),by=list(q.id,Stock)],by=c('q.id','Stock')),id.vars = c('q.id','Stock'),measure.vars = c('uncertainty','assym','dispersion')),q.id~Stock~variable)

quarters <- data.table(q.id=sort(unique(core.dt[,q.id])))


#pt.new.dt <- setkey(melt(core.dt[,merge(setkey(quarters,q.id),.SD,all=T),by=list(Broker,Stock),.SDcols=c('q.id','Broker','Stock','true')][,.(q.id,Broker,Stock,true)][,naive:=c(NA,head(true,-1)),by=.(Broker,Stock)][,mean.rank:=grow.window.f(true,seq_len(length(true)),mean,na.rm=T),by=.(Broker,Stock)][,default:=rank(mean.rank,na.last = 'keep'),by=.(q.id,Stock)],id.vars = c('q.id','Stock','Broker'),measure.vars = baselines,value.name = 'rank',variable.name = 'Method'),q.id,Stock,Broker,Method)[!is.na(q.id)]

pt.new <- acast(core.dt,q.id~Broker~Stock,value.var='true')

stocks <- intersect(intersect(intersect(dimnames(pt.new)[[3]],dimnames(array.all.vvs)[[2]]),market.set[,Stock]),dimnames(broker.vvs)[[2]])
stock.vvs <- vvs.combine(stocks,broker.vvs,array.all.vvs,t=dim(pt.new)[1])

vvs.names <- dimnames(stock.vvs)[[3]]
#cache('vvs.names')
#baseline.rankings <- baseline.rankings.f(pt.new[,,stocks],3)
#dimnames(baseline.rankings)[[1]] <- dimnames(pt.new)[[1]][3:length(dimnames(pt.new)[[1]])]

#system.time(pred.r<- predict.ranking.script.f(methods,stocks,stock.vvs,pt.new[,,stocks],rank.parameters))


#all.rankings <- abind(baseline.rankings[(dim(baseline.rankings)[1]-dim(pred.r)[2]+1):dim(baseline.rankings)[1],,,],aperm(pred.r,c(2,1,4,3)),along=4)


#ranked.pt.dt <- setkey(setnames(data.table(reshape2::melt(all.rankings)),c('q.id','Broker','Stock','Method','rank'))[,q.id:=as.yearqtr(q.id)],q.id,Stock,Broker,Method)

#pt.accu <- melt(dcast.data.table(ranked.pt.dt,q.id+Stock+Broker~Method,value.var='rank')[,lapply(.SD,function(i){evaluation.simple(.SD[[1]],i) }),by=.(q.id,Stock),.SDcols=c(baselines,methods)],id.vars=c('q.id','Stock'),na.rm=T)

#rm('broker.vvs','quarters','pt.new','stocks','stock.vvs','pred.r','all.rankings')


###rolling funciions
roll.baselines <- roll.baselines.f(pt.new[,,stocks],rank.parameters[[4]])
system.time(pred.r <- roll.ranking.f(pred.id,stocks,stock.vvs[,,sel.vvs],pt.new[,,stocks],rank.parameters))

all.rankings <- abind(aperm(roll.baselines,c(4,1,2,3)),aperm(pred.r,c(2,1,4,3)),along=4)
dimnames(all.rankings)[[4]] <- c(baselines,pred.id)
dimnames(all.rankings)[[1]] <- rollapply(dimnames(pt.new)[[1]],rank.parameters[[4]]+1,last)


ranked.pt.dt <- setkey(setnames(data.table(reshape2::melt(all.rankings)),c('q.id','Broker','Stock','Method','rank'))[,q.id:=as.yearqtr(q.id)],q.id,Stock,Broker,Method)

pt.accu <- melt(dcast.data.table(ranked.pt.dt,q.id+Stock+Broker~Method,value.var='rank')[,lapply(.SD,function(i){evaluation.simple(.SD[[1]],i) }),by=.(q.id,Stock),.SDcols=c(baselines,pred.id)],id.vars=c('q.id','Stock'),na.rm=T)

rm('broker.vvs','quarters','pt.new','stocks','stock.vvs','pred.r','all.rankings')

### EPS ranking predicting----

eps.tr <- acast(setkey(na.omit(complete.dt)[,':='(true=rank,q.id=fis.q)],q.id)[quarters],q.id~Broker~Stock,value.var='true')

broker.vvs <- acast(melt(unique(setkey(na.omit(complete.dt)[,':='(true=rank,q.id=fis.q)],q.id)[quarters][,broker.vvs.f(Est,act.eps),by=list(q.id,Stock)],by=c('q.id','Stock')),id.vars = c('q.id','Stock'),measure.vars = c('uncertainty','assym','dispersion')),q.id~Stock~variable)


#stocks <- sort(intersect(intersect(dimnames(eps.tr)[[3]],dimnames(array.all.vvs)[[2]]),unique(unlist(lapply(market.list,function(m){m$stock.names})))))
stocks <- intersect(intersect(intersect(dimnames(eps.tr)[[3]],dimnames(array.all.vvs)[[2]]),market.set[,Stock]),dimnames(broker.vvs)[[2]])
stock.vvs <- vvs.combine(stocks,broker.vvs,array.all.vvs,t=dim(eps.tr)[1])

roll.baselines <- roll.baselines.f(eps.tr[,,stocks],rank.parameters[[4]])
system.time(pred.r <- roll.ranking.f(pred.id,stocks,stock.vvs[,,sel.vvs],eps.tr[,,stocks],rank.parameters))

all.rankings <- abind(aperm(roll.baselines,c(4,1,2,3)),aperm(pred.r,c(2,1,4,3)),along=4)
dimnames(all.rankings)[[4]] <- c(baselines,pred.id)
dimnames(all.rankings)[[1]] <- rollapply(dimnames(eps.tr)[[1]],rank.parameters[[4]]+1,last)


ranked.eps.dt <- setkey(setnames(data.table(reshape2::melt(all.rankings)),c('q.id','Broker','Stock','Method','rank'))[,q.id:=as.yearqtr(q.id)],q.id,Stock,Broker,Method)

eps.accu <- melt(dcast.data.table(ranked.eps.dt,q.id+Stock+Broker~Method,value.var='rank')[,lapply(.SD,function(i){evaluation.simple(.SD[[1]],i) }),by=.(q.id,Stock),.SDcols=c(baselines,pred.id)],id.vars=c('q.id','Stock'),na.rm=T)

rm('broker.vvs','quarters','eps.tr','stocks','stock.vvs','pred.r','all.rankings')
