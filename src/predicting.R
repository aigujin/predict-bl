print(sel.vvs)
core.dt <- q.data[,core.b:=.N>=12,by=list(Stock,Broker)][(core.b)][,true:=rank(score),by=list(q.id,Stock)][,core.s:=.N>=3,by=list(q.id,Stock)][(core.s)][,core.q:=length(unique(q.id))>=8,by=.(Stock)][(core.q)]

broker.vvs <- acast(melt(unique(core.dt[,broker.vvs.f(PT,priceAdj),by=list(q.id,Stock)],by=c('q.id','Stock')),id.vars = c('q.id','Stock'),measure.vars = c('uncertainty','assym','dispersion')),q.id~Stock~variable)

quarters <- data.table(q.id=sort(unique(core.dt[,q.id])))

pt.tr <- acast(core.dt,q.id~Broker~Stock,value.var='true')

stocks <- intersect(intersect(intersect(dimnames(pt.tr)[[3]],dimnames(array.all.vvs)[[2]]),market.set[,Stock]),dimnames(broker.vvs)[[2]])
stock.vvs <- vvs.combine(stocks,broker.vvs,array.all.vvs,t=dim(pt.tr)[1])


pt.rank <- rank.function.f(rank.method,pred.id,stocks,stock.vvs[,,sel.vvs],pt.tr[,,stocks],rank.parameters)

rm('broker.vvs','pt.new','stocks','stock.vvs')

### EPS ranking predicting----

eps.tr <- acast(setkey(na.omit(complete.dt)[,':='(true=rank,q.id=fis.q)],q.id)[quarters],q.id~Broker~Stock,value.var='true')

broker.vvs <- acast(melt(unique(setkey(na.omit(complete.dt)[,':='(true=rank,q.id=fis.q)],q.id)[quarters][,broker.vvs.f(Est,act.eps),by=list(q.id,Stock)],by=c('q.id','Stock')),id.vars = c('q.id','Stock'),measure.vars = c('uncertainty','assym','dispersion')),q.id~Stock~variable)

stocks <- intersect(intersect(intersect(dimnames(eps.tr)[[3]],dimnames(array.all.vvs)[[2]]),market.set[,Stock]),dimnames(broker.vvs)[[2]])
stock.vvs <- vvs.combine(stocks,broker.vvs,array.all.vvs,t=dim(eps.tr)[1])

eps.rank <- rank.function.f(rank.method,pred.id,stocks,stock.vvs[,,sel.vvs],eps.tr[,,stocks],rank.parameters)

rm('broker.vvs','quarters','eps.tr','stocks','stock.vvs')


ranked.pt.dt <- pt.rank[[1]]
pt.accu <- pt.rank[[2]]
ranked.eps.dt <- eps.rank[[1]]
eps.accu <- eps.rank[[2]]
