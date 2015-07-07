
quarters <- setnames(data.table(unique(ranked.pt.dt[,q.id])),'q.id')[,q.id:=as.yearqtr(q.id)]

exp.ret <- setkey(melt(core.dt[,merge(setkey(quarters,q.id),.SD,all=T),by=list(Broker,Stock),.SDcols=c('q.id','Broker','Stock','b.view')][,.(q.id,Broker,Stock,b.view)][,true:=truncate.f(b.view,percentile)][,naive:=c(NA,head(true,-1)),by=.(Broker,Stock)][,':='(true=naive,default=naive),by=.(Broker,Stock)][,eval(pred.id):=naive,by=.(Broker,Stock)],id.vars = c('q.id','Stock','Broker'),measure.vars = c(baselines,pred.id),value.name = 'exp.ret',variable.name = 'Method'),q.id,Stock,Broker,Method)


pt.set <- setkey(ranked.pt.dt,q.id,Stock,Broker,Method)[exp.ret]

pt.ret <- pt.set[,rank.exp.ret.f(rank,exp.ret),by=.(q.id,Stock,Method)][V1!=0,][q.id!='1999 Q2',]

pt.list.rank <- acast(pt.ret,q.id~Stock~Method,value.var='V1')

pt.rank.views <- setnames(pt.ret[,year:=format(as.yearqtr(q.id),'%Y')],'V1','View')

###EPS case----

eps.set <- setkey(ranked.eps.dt,q.id,Stock,Broker,Method)[exp.ret]
eps.exp.ret <- eps.set[,rank.exp.ret.f(rank,exp.ret),by=.(q.id,Stock,Method)][V1!=0,][q.id!='1999 Q2',]
eps.list.rank <- acast(eps.exp.ret,q.id~Stock~Method,value.var='V1')
eps.rank.views <- setnames(eps.exp.ret[,year:=format(as.yearqtr(q.id),'%Y')],'V1','View')


###CONS case----
meanTper <- na.omit(melt(unique(core.dt[,merge(setkey(quarters,q.id),.SD,all=T),by=list(Broker,Stock),.SDcols=c('q.id','Broker','Stock','b.view')][,.(q.id,Broker,Stock,b.view)][,true:=truncate.f(b.view,percentile)][,true:=median(true,na.rm=T),by=list(q.id,Stock)],by=c('q.id','Stock'))[,.(q.id,Stock,true)][,naive:=c(NA,head(true,-1)),by=.(Stock)][,':='(true=naive,default=naive),by=.(Stock)][,eval(pred.id):=naive,by=.(Stock)],id.vars = c('q.id','Stock'),measure.vars = c(baselines,pred.id),variable.name = 'Method')[q.id!='1999 Q2',])


nr.views <- setnames(meanTper[,year:=format(as.yearqtr(q.id),'%Y')],'value','View')

cons.list.rank <- acast(meanTper,q.id~Stock~Method,value.var='View')

### for CONS strategy true confidnece = naive confidence
conf.dt <- melt(unique(core.dt,by=c('q.id','Stock'),fromLast = T)[,.(q.id,Stock,s.coefVar)][,true:=0L][,naive:=c(NA,head(s.coefVar,-1)),by=Stock][,default:=grow.window.f(s.coefVar,seq_len(length(s.coefVar)),mean,na.rm=T),by=Stock][,true:=naive][,eval(pred.id):=naive,by=Stock],id.vars = c('q.id','Stock'),measure.vars=c(baselines,pred.id),value.name='cons')


cons.conf.coef <- acast(conf.dt,q.id~Stock~variable,value.var='cons')[dimnames(conf.coef)[[1]],pt.stocks,drop=F,]


eps.stocks <- intersect(dimnames(eps.list.rank)[[2]],dimnames(conf.coef)[[2]])

pt.stocks <- intersect(dimnames(pt.list.rank)[[2]],dimnames(conf.coef)[[2]])

bl.period <- 1:dim(pt.list.rank)[[1]]
m.period <-(length(market.list)-length(bl.period)+1) : (length(market.list))

pt.opt.w<- opt.w.f(pt.list.rank,cons.conf.coef[,pt.stocks,],tau)[,Views:='TP']
eps.opt.w<- opt.w.f(eps.list.rank,cons.conf.coef[,eps.stocks,],tau)[,Views:='EPS']
cons.opt.w<- opt.w.f(cons.list.rank,cons.conf.coef,tau)[,Views:='CONS']


opt.w<- rbind(pt.opt.w,eps.opt.w,cons.opt.w)[,confAgg:='cons']


final.bl <- setkey(unique(pred.bl.results.f(opt.w),by=c('Method','q.id','Views','confAgg')),Method)
#cache('final.bl')
#final.bl$Method <- factor(final.bl$Method,levels=unique(final.bl$Method)[c(8,4,3,6,1,5,7,2)])
final.bl$Method <- factor(final.bl$Method,levels=c(baselines,pred.id,'Market'))
final.bl$Views <- factor(final.bl$Views,levels=unique(final.bl$Views)[c(1,3,2)])
colourCount = length(unique(final.bl$Method))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(colourCount, "Set1"))
