rm(list=ls())
setwd('~/Dropbox/workspace/Projects/Black-Litterman/')

library(ProjectTemplate)
load.project()
delta<-1L;tau=1/50;baselines <- c('true','naive','default');pred.id<-c('raw','diff','random','roll.sd');confid.id <- c('cons','last','ma');percentile <- 0.05;rank.parameters <- c(n=100L,diff.lag=1L,sd.lag=8L,roll.p=15L)
##rolling rankings: 20 qtrs - no winning; 16 qtrs - win roll.sd (last); 12
#require(labelRank)
### market data: ~ 113 sec (mcapply: ~ 52 sec)
system.time(source('~/Dropbox/workspace/Projects/BL-strategies/munge/01-data.q.ret.R'))

### State var. data: ~118 sec
system.time(source('munge/state-variables.R'))
### Analysts ranking data: ~ 280 sec
system.time(source('~/Dropbox/workspace/Projects/BL-strategies/munge/03-analysts.process.R'))
### Predicting ~ 347 sec
sel.vvs <- vvs.names[-c(6,9)]
system.time(source('src/predicting.R'))

pt.accu[,mean(value),by=.(variable)]

ggplot(pt.accu[,mean(value),by=.(q.id,variable)],aes(x=as.Date(q.id),y=V1,group=variable,color=variable))+geom_line(size=0.5,alpha=0.7)+geom_smooth(method='loess',se=F,size=1L)+theme_bw()

### trading: ~ 322 sec
system.time(source('src/trading.R'))

#ggplot(res.accu[,mean(value,na.rm=T),by=.(q.id,variable,conf)],aes(x=as.Date(q.id),y=V1,color=variable,group=variable))+geom_line()+facet_grid(conf~.,scale='free_y')+theme_bw()


ggplot(final.bl,aes(x=as.Date(Quarters),y=cum.ret,group=Method,color=Method))+geom_line(size=0.5)+ylab('Portfolio wealth (initial=$100)')+xlab('Quarters')+ggtitle('Portfolio performance with $100 initial investment')+theme_bw(base_family='Avenir')+theme(plot.title = element_text(colour = "Blue"),legend.position='top')+scale_color_manual(values=getPalette(colourCount))+guides(color=guide_legend(nrow=1))+geom_hline(yintercept=100L)+facet_grid(Views~confAgg,scale='free_x')

ggplot(unique(melt(final.bl[,list(Views,Method,confAgg,ann.ret,ann.sd,ann.sr)],id.vars=c('Method','Views','confAgg'))),aes(x=Method,y=value))+geom_bar(aes(fill=Method),stat='identity',alpha=0.7)+facet_grid(variable~confAgg,scale='free_y')+theme_bw(base_family='Avenir')+ggtitle('Annualized portfolio measures (return, st.dev, and the Sharpe ratio) \n conditional on confidence aggregation')+theme(plot.title = element_text(colour = "Blue"),legend.position='top',axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(labels=percent)+ylab('Percent')+geom_text(aes(label=round(value,3)*100),angle=90,size=3,hjust=1.2)+scale_fill_manual(values=getPalette(colourCount))

require(knitr)
setwd('~/Dropbox/workspace/Projects/Black-Litterman/doc/paper/')
knit2pdf('chapter-4-knitr.Rnw',quiet=T)
setwd('~/Dropbox/workspace/Projects/Black-Litterman/')
