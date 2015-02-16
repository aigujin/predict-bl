
core.dt <- q.data[,core.b:=.N>=12,by=list(Stock,Broker)][(core.b)][,true:=rank(score),by=list(q.id,Stock)][,core.s:=.N>=3,by=list(q.id,Stock)][(core.s)][,core.q:=length(unique(q.id))>=8,by=.(Stock)][(core.q)]

broker.vvs <- acast(melt(unique(core.dt[,broker.vvs.f(PT,priceAdj),by=list(q.id,Stock)],by=c('q.id','Stock')),id.vars = c('q.id','Stock'),measure.vars = c('uncertainty','assym','dispersion')),q.id~Stock~variable)

#quarters <- data.table(q.id=sort(unique(core.dt[,q.id])))


#pt.new.dt <- setkey(melt(core.dt[,merge(setkey(quarters,q.id),.SD,all=T),by=list(Broker,Stock),.SDcols=c('q.id','Broker','Stock','true')][,.(q.id,Broker,Stock,true)][,naive:=c(NA,head(true,-1)),by=.(Broker,Stock)][,mean.rank:=grow.window.f(true,seq_len(length(true)),mean,na.rm=T),by=.(Broker,Stock)][,default:=rank(mean.rank,na.last = 'keep'),by=.(q.id,Stock)],id.vars = c('q.id','Stock','Broker'),measure.vars = baselines,value.name = 'rank',variable.name = 'Method'),q.id,Stock,Broker,Method)[!is.na(q.id)]

pt.new <- acast(core.dt,q.id~Broker~Stock,value.var='true')

stocks <- intersect(intersect(intersect(dimnames(pt.new)[[3]],dimnames(array.all.vvs)[[2]]),market.set[,Stock]),dimnames(broker.vvs)[[2]])
stock.vvs <- vvs.combine(stocks,broker.vvs,array.all.vvs,t=dim(pt.new)[1])

#baseline.rankings <- baseline.rankings.f(pt.new[,,stocks],3)
#dimnames(baseline.rankings)[[1]] <- dimnames(pt.new)[[1]][3:length(dimnames(pt.new)[[1]])]

#system.time(pred.r<- predict.ranking.script.f(methods,stocks,stock.vvs,pt.new[,,stocks],rank.parameters))


#all.rankings <- abind(baseline.rankings[(dim(baseline.rankings)[1]-dim(pred.r)[2]+1):dim(baseline.rankings)[1],,,],aperm(pred.r,c(2,1,4,3)),along=4)


#ranked.pt.dt <- setkey(setnames(data.table(reshape2::melt(all.rankings)),c('q.id','Broker','Stock','Method','rank'))[,q.id:=as.yearqtr(q.id)],q.id,Stock,Broker,Method)

#pt.accu <- melt(dcast.data.table(ranked.pt.dt,q.id+Stock+Broker~Method,value.var='rank')[,lapply(.SD,function(i){evaluation.simple(.SD[[1]],i) }),by=.(q.id,Stock),.SDcols=c(baselines,methods)],id.vars=c('q.id','Stock'),na.rm=T)

#rm('broker.vvs','quarters','pt.new','stocks','stock.vvs','pred.r','all.rankings')


###rolling funciions
roll.baselines <- roll.baselines.f(pt.new[,,stocks],rank.parameters[[4]])
system.time(pred.r <- roll.ranking.f(methods,stocks,stock.vvs,pt.new[,,stocks],rank.parameters))

all.rankings <- abind(aperm(roll.baselines,c(4,1,2,3)),aperm(pred.r,c(2,1,4,3)),along=4)
dimnames(all.rankings)[[4]] <- c(baselines,methods)
dimnames(all.rankings)[[1]] <- rollapply(dimnames(pt.new)[[1]],rank.parameters[[4]]+1,last)


ranked.pt.dt <- setkey(setnames(data.table(reshape2::melt(all.rankings)),c('q.id','Broker','Stock','Method','rank'))[,q.id:=as.yearqtr(q.id)],q.id,Stock,Broker,Method)

pt.accu <- melt(dcast.data.table(ranked.pt.dt,q.id+Stock+Broker~Method,value.var='rank')[,lapply(.SD,function(i){evaluation.simple(.SD[[1]],i) }),by=.(q.id,Stock),.SDcols=c(baselines,methods)],id.vars=c('q.id','Stock'),na.rm=T)

rm('broker.vvs','quarters','pt.new','stocks','stock.vvs','pred.r','all.rankings')



#accuracy <- apply(all.rankings,c(1,3),function(s){apply(s,2,evaluation.simple,s[,'true'],'s','p' ) })
#apply(accuracy,1,mean,na.rm=T)
# 
# #cache('all.rankings')
# load('cache/all.rankings.RData')
# #------
# ##load EPS rankings
# #load('~/Dropbox/workspace/Projects/EPS/cache/bl.all.rankings.RData')
# #all.rankings <- bl.all.rankings
# ###Ranking: making brokers view (extended TPER for 4 quarters)
# brok.tper <- acast(q.data,q.id~Broker~Stock,value.var='b.view')
# 
# 
# 
# ###an alternaive to measure accuracy: coef. of variation. Coefficient of variation: lower values means less variable,thus, more confidence. The higher the CV, the greater the dispersion in the variable [http://www.ats.ucla.edu/stat/mult_pkg/faq/general/coefficient_of_variation.htm]
# 
# #load('cache/all.rankings.RData')
# all.s <- findIntersect(baseline.rankings,brok.tper,3)
# all.b <- findIntersect(baseline.rankings,brok.tper,2)
# bl.period <- 4:40
# m.period <-(length(market.list)-length(bl.period)+1) : (length(market.list))
# 
# 
# percentile <- 0.05
# ### Timing of brokers tper should be exactly as for ranking(pt.new)
# trunk.brok.tper <- truncate.f(brok.tper[2:44,all.b,all.s],percentile)
# ### pt.new: 1999 Q2 - 2009 Q4 (1:43)
# ### baseline rankings: 1999 Q3- 2009 Q4 (3:44,of the market)
# ### true 1999 Q2 - 2009 Q3
# ### naive 1999 Q3 - 2009 Q4
# ### Market 1999 Q1 - 2009 Q4 (1:44)
# ### all.rankings 2000 Q1 - 2009 Q4
# 
# list.rank <- alply(bl.script.f(all.s,all.rankings[,all.b,all.s,],trunk.brok.tper,t=dim(all.rankings)[1]),3,.dims=T)
# rank.conf <- buildOmega(apply(all.rankings[,all.b,all.s,],c(1,3),function(s){apply(s,2,evaluation.simple,s[,'true'],'s','p' ) }))
# 
# 
# 
# ##BL inputs for non-rank strategy:meanTper and s.coefVar
# meanTper <-acast(unique(setkey(q.data,Stock)[all.s],by=c('Stock','q.id')),q.id~Stock,value.var='meanTper')
# trunk.meanTper <- truncate.f(meanTper,percentile)
# list.non.rank <- alply(abind(true.nr=trunk.meanTper[4:43,],naive.nr=trunk.meanTper[5:44,],default.nr=rollapplyr(trunk.meanTper[5:44,],seq_len(nrow(trunk.meanTper[5:44,])),mean,na.rm=T),along=3),3,.dims=T)
# 
# 
# require(scales)
# conf.coef <- acast(unique(setkey(q.data,Stock)[all.s],by=c('Stock','q.id')),q.id~Stock,value.var='s.coefVar')
# 
# ### Aggregation of non.r.conf
# non.r.conf <- buildOmega(aperm(replicate(3,conf.coef[5:44,]),c(3,1,2)))
# ###True confidence for non.rank
# non.r.conf[,1,,][!is.na(non.r.conf[,1,,])] <- 0
# dimnames(non.r.conf)[[2]] <- names(list.non.rank)
# 
# 
# #ggplot(data.table(melt(non.r.conf,na.rm=T)),aes(x=Var1,y=value,group=Var1))+geom_point()+facet_wrap(Var2~Var4)
# 
# #ggplot(data.table(melt(list.non.rank,na.rm=T)),aes(x=Var1,y=value,group=Var1))+geom_boxplot()+facet_grid(.~L1)
# 
# #(3:44)[4:42]
# bl.period <- 4:40
# m.period <-(length(market.list)-length(bl.period)+1) : (length(market.list))
# 
# ###market timing: market.list[rankings][views]
# #(1:44)[2:44][4:43]
# ###4:43
# #m.period <- (5:44)
# #m.period <- (4:43)
# #m.period <- 3:44
# #ggplot(data.table(melt(true.Q,na.rm=T)),aes(x=as.Date(as.yearqtr(Var1)),y=value,group=Var1))+geom_boxplot()+theme_bw()
# #ggplot(data.table(melt(norm.conf,na.rm=T)),aes(x=as.Date(as.yearqtr(Var1)),y=value,group=Var1))+geom_point()
# 
# 
# ### case 1: views={rank,non.rank}; conf={non-rank}
# 
# case.1.non.r.conf <- non.r.conf[,,all.s,]
# dimnames(case.1.non.r.conf)[[2]] <- names(list.rank)[1:3]
# opt.w <- opt.w.f(c('last','ma','w.ma'),list.rank,c(list.rank[1:3],list.non.rank),case.1.non.r.conf,non.r.conf,case='1',0.02,delta)
# cache('opt.w')
# 
# ### case 2: views={rank,non-rank} and conf={rank,non-rank}
# #opt.w <- opt.w.f(c('last','ma','w.ma'),list.rank,list.non.rank,rank.conf,non.r.conf,case='2',tau=0.02,delta)
# 
# opt.w<- rbind(opt.w[Method=='true'|Method=='naive'|Method=='default'][,Views:='TP'],opt.w[Method=='true.nr'|Method=='naive.nr'|Method=='default.nr'][,Views:='CONS'])[Views=='CONS',Method:=ifelse(Method=='true.nr'&Views=='CONS','true',ifelse(Method=='naive.nr'&Views=='CONS','naive','default'))]
# 
# pt.eps.opt.w<- pt.eps.opt.w[Method=='true'|Method=='naive'|Method=='default'][,Views:='EPS']
# 
# opt.w <- rbind(opt.w,pt.eps.opt.w)
# 
# 
# 
# 
# #all.final.bl <- rbind(final.bl,eps.final.bl[Method=='true'|Method=='naive'|Method=='default'][,Method:=ifelse(Method=='true','eps.true',ifelse(Method=='naive','eps.naive','eps.default'))],pt.eps.final.bl[Method=='true'|Method=='naive'|Method=='default'][,Method:=ifelse(Method=='true','pt.eps.true',ifelse(Method=='naive','pt.eps.naive','pt.eps.default'))])
# cache('all.final.bl')
# 
# 
# final.bl <- setkey(unique(bl.results.f(opt.w),by=c('Method','Aggregation','q.id','Views')),Method)
# # order for plotting c('true','naive','default','true.nr','naive.nr','default.nr','Market')
# final.bl$Method <- factor(final.bl$Method,levels=unique(final.bl$Method)[c(3,2,1)])
# final.bl$Views <- factor(final.bl$Views,levels=unique(final.bl$Views)[c(4,1,2,3)])
# cache('final.bl')
# colourCount = length(unique(final.bl$Views))
# getPalette = colorRampPalette(RColorBrewer::brewer.pal(colourCount, "Set1"))
# 
# p.cum.ret <- ggplot(final.bl[Aggregation=='ma'],aes(x=as.Date(Quarters),y=cum.ret,group=Views,color=Views))+geom_line(size=0.5)+geom_hline(yintercept=100)+facet_grid(~Method,scale='free_x')+ylab('Portfolio wealth (initial=$100)')+xlab('Quarters')+ggtitle('Portfolio performance with $100 initial investment')+theme_bw(base_family='Avenir')+theme(plot.title = element_text(colour = "Blue"),legend.position='top')+scale_color_manual(values=getPalette(colourCount))+guides(color=guide_legend(nrow=1))
# 
# 
# 
# #+scale_x_date(breaks='year',labels=date_format('%Y'))
# 
# 
# p.ann.ret <- ggplot(unique(melt(final.bl[Aggregation=='ma',list(Views,Method,ann.ret,ann.sd,ann.sr)],id.vars=c('Method','Views'))),aes(x=Views,y=value))+geom_bar(aes(fill=Views),stat='identity',alpha=0.7)+facet_grid(variable~Method,scale='free_y')+theme_bw(base_family='Avenir')+ggtitle('Annualized portfolio measures (return, st.dev, and the Sharpe ratio) \n conditional on confidence aggregation')+theme(plot.title = element_text(colour = "Blue"),legend.position='top',axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(labels=percent)+ylab('Percent')+geom_text(aes(label=round(value,3)*100),angle=90,size=3,hjust=1.2)+scale_fill_manual(values=getPalette(colourCount))#+guides(fill=guide_legend(nrow=1))
# 
# 
# 
# p.TO <- ggplot(unique(melt(final.bl[Aggregation=='ma',list(Views,Method,meanViews,Ave.TO)],id.vars=c('Method','Views'))),aes(x=Views,y=value,fill=Views))+theme_bw(base_family='Avenir')+geom_bar(stat='identity')+facet_grid(variable~Method,scale='free_y')+ggtitle('Trading statistics conditional confidence aggregation \n (averages of number of views and turnover ratio)')+theme(legend.position='none',axis.title.x=element_blank(),plot.title = element_text(colour = "Blue"),axis.title.y=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_fill_manual(values=getPalette(colourCount))+guides(fill=guide_legend(nrow=1))
# #+geom_text(aes(label=round(value,2)),angle=90,size=3,hjust=1.2)
# 
# require(gridExtra)
# cairo_pdf("graphs/temp.ma.results.pdf", family="Avenir",h = 11.7, w = 8.3)
# grid.arrange(p.cum.ret,p.ann.ret,p.TO)
# dev.off()
# 
