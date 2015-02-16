# TODO: Add comment
#
# Author: aiguzhinov
##########################

#####################################################
### A new rewrite of the script with base on data.table package
### 27/06/14

### Variables:
### btm=btm,
### debt.to.eq=,
### size=
### accruals=
### ret.vol =


#source('~/Dropbox/workspace/Projects/Black-Litterman/BL-strategies/lib/my.paths.R')
sectors<-c('Consumer Discretionary','Consumer Staples','Energy','Financials','Health Care','Industrials','IT','Materials','Telecom Services','Utilities')

vvs.data <- setkey(rbindlist(mclapply(sectors,function(sec){
  vvs <- list.files(paste(data.stock.dir(sec),'Acc.Variables',sep='/'),pattern='*.csv')
  dt <- do.call(cbind,lapply(vvs,function(i){
    vvs.dt <- data.table(read.csv2(paste(data.stock.dir(sec),'Acc.Variables',i,sep='/'),sep=';',nrow=84))[,q.id:=as.yearqtr(seq.Date(as.Date('1989-04-01'),as.Date('2010-01-01'),by = '3 month')-1)]
  setkey(melt(vvs.dt,id.vars='q.id',variable.name = 'Stock',value.name = strsplit(i,'.csv')[[1]]),q.id,Stock)
    }))
dt[,unique(colnames(dt)),with=F][,Sector:=sec]
},mc.cores=4))[,':='(btm=(tot.assets-tot.liab)/as.numeric(market.cap),size=log(as.numeric(market.cap)),debt.to.eq=st.debt/(tot.assets-tot.liab),accruals=total.accruals(curr.assets,cash,curr.liab,st.debt,taxes,depr,tot.assets)),by=list(Stock,Sector)],q.id,Stock)


###Broker specific variables load q.data from cache. To be calculated immediately before the ranking process
# load('cache/q.data.RData')
# core.dt <- q.data[,core.b:=.N>=12,by=list(Stock,Broker)][(core.b)][,rank:=rank(score),by=list(q.id,Stock)][,core.s:=.N>=3,by=list(q.id,Stock)][(core.s)]
# 
# broker.dt <- melt(unique(core.dt[,broker.vvs.f(PT,priceAdj),by=list(q.id,Stock)],by=c('q.id','Stock')),id.vars = c('q.id','Stock'),measure.vars = c('uncertainty','assym','dispersion'))
# 
# broker.vvs <- acast(broker.dt,q.id~Stock~variable)


#saveCache(broker.vvs)


stock.ref <- setkey(unique(rbindlist(lapply(list.files('~/Dropbox/Datastream/AllStocks/names/',pattern='name.ds.*.csv'),function(i){fread(paste('~/Dropbox/Datastream/AllStocks/names/',i,sep=''),header=T,na.strings = '')})),by='DSCD')[,Stock:=WC05601],DSCD,Stock)

load('~/Dropbox/workspace/Projects/BL-strategies/data/ref.matrix.RData')
#make DT of it
ref.dt <- setnames(setkey(data.table(Stock=ref.matrix[,1],s.id=rownames(ref.matrix)),s.id),1:2,c('Stock','DSCD'))
sp.id <- setkey(setnames(fread('~/Dropbox/workspace/Projects/BL-strategies/data/sp.ids.new.csv',header=F),'DSCD'),DSCD)
miss.tkt <- c('AMP','ANV','ATCH','ACS','ASND','BT','HPH','MEYR','MWI','MII','RN','UCC')
miss.dt <- data.table(Stock=miss.tkt,DSCD=ref.dt[sp.id][which(is.na(Stock))][,DSCD])
ref.dt <- setkey(rbind(ref.dt,miss.dt)[,Stock:=as.character(Stock)][which(duplicated(rbind(ref.dt,miss.dt)[,Stock])),Stock:=paste(Stock,'1',sep='.')],DSCD,Stock)
full.stock.ref <- setkey(unique(merge(stock.ref,ref.dt,all=T),by=c('DSCD','Stock')),DSCD)


#ds.ret <- full.stock.ref[setkey(unique(data.collect('ret',3),by=c('Date','DSCD')),DSCD),allow.cartesian=T][,list(Stock,DSCD,Date,ret)]
#load('data/adj.prices.v5.RData')
all.stock.prices <- na.omit(setkey(setnames(unique(data.collect('p',1),by=c('Date','DSCD')),'p','priceAdj'),DSCD)[setkey(full.stock.ref,DSCD),allow.cartesian=T])[,list(Date,Stock,DSCD,NAME,priceAdj)][,q.id:=as.yearqtr(Date)]

require("quantmod")
q.ret.sp<-all.stock.prices[,q.ret:=(log(last(priceAdj))-log(first(priceAdj))),by=.(Stock,q.id)][,.(q.id,DSCD,Stock,q.ret)]
ds.ret <- all.stock.prices[,ret:=c(NA,diff(log(priceAdj))),by=Stock][,.(Date,DSCD,Stock,ret)]

setkey(vvs.data,Stock)
#stock.prices <- setkey(unique(vvs.data[,list(Stock,Sector)])[setkey(melt(data.table(Date=index(p),coredata(p)),id.vars = 'Date',variable.name = 'Stock',value.name = 'Price'),Stock)[,s.ret:=c(NA,diff(log(Price))),by=Stock]],Date)
stock.ret <- unique(vvs.data[,list(Stock,Sector)])[setkey(ds.ret,Stock)]
#yahoo.prices <- merge(merge(merge(full.price.v2,full.price.v3,by=c('Date','Stock'),all=TRUE),full.price.v4,by=c('Date','Stock'),all=TRUE),full.price.v5,by=c('Date','Stock'),all=TRUE)

require(Quandl)
Quandl.auth("X5ws5JEoYdP6VFefbPmQ")

tbil <- data.table(Quandl('FRED/DTB3',start_date = '1988-12-31',end_date = '2009-12-31',sort='asc'))
sp500 <- setnames(data.table(Quandl("YAHOO/INDEX_GSPC", trim_start='1988-12-30', trim_end='2009-12-31',column=6,sort='asc')),2,'Price')

stock.returns <- na.omit(setkey(setkey(sp500[,sp.ret:=c(NA,diff(log(Price)))][,list(Date,sp.ret)],Date)[setkey(tbil[,rf:=Value/100/252][,list(Date,rf)],Date)][stock.ret[,list(Date,Stock,Sector,ret)]][,q.id:=as.yearqtr(Date)][,s.vol:=sum((sp.ret-mean(sp.ret))^2,na.rm=T)+sum((ret-rf-sp.ret)^2,na.rm=T),by=list(q.id,Stock,Sector)],q.id,Stock))

### macro data infl,gnp,interst,vix----

macro.vvs <- setkey(data.table(Quandl(c('FRED/DTB3','FRED/GNP','FRED/CPIAUCSL',"YAHOO/INDEX_VIX.6"),start_date = '1988-12-31',end_date = '2009-12-31',collapse=c('quarterly'),sort='asc'),check.names=T)[,':='(ret.vix=c(NA,diff(log(YAHOO.INDEX_VIX...Adjusted.Close))),inf.rate=c(NA,diff(log(FRED.CPIAUCSL...Value/100))),growth.gnp=c(NA,diff(log(FRED.GNP...Value))),t.bill=FRED.DTB3...Value/100,q.id=as.yearqtr(Date))][,list(ret.vix,growth.gnp,inf.rate,t.bill,q.id)],q.id)

#vix.index <- setkey(rbind(data.table(read.csv2(data.stock.dir('Market/vxocurrent.csv'),sep=';',header=T))[,Date:=as.Date(Date,format='%d-%m-%Y')],data.table(read.csv2(data.stock.dir('Market/vxoarchive.csv'),header=T,sep=";",dec=','))[,Date:=as.Date(Date,format='%d-%m-%Y')])[,q.id:=as.yearqtr(Date)],q.id)[,ret.vix:={log.p <- log(Close/100/sqrt(4)); last(log.p)-first(log.p)},by=q.id]


sp.sec.index<-setkey(melt(setnames(data.table(read.csv2(data.stock.dir('Market/sectors.indecies.csv'),header=T,sep=";",dec=',')),2:11,sectors)[,q.id:=as.yearqtr(as.character(Name),format='Q%q %Y')][,Name:=NULL],id.vars = 'q.id',variable.name = 'Sector',value.name = 'Price')[,sec.ret:=c(NA,diff(log(Price))),by=Sector],q.id)

all.macro <- setkey(macro.vvs[sp.sec.index],q.id,Sector)
#infl<-setkey(data.table(read.csv2(data.stock.dir('Market/inflation.csv'),sep=';',header=F))[,inf.rate:=V2/100][,q.id:=as.yearqtr(seq.Date(as.Date('1989-04-01'),as.Date('2010-01-01'),by = '3 month')-1)],q.id)

#gnp<-setkey(data.table(read.csv2(data.stock.dir('Market/gnp.quarterly.csv'),sep=';',header=T))[,':='(log.gnp=log(GNP),q.id=as.yearqtr(seq.Date(as.Date('1989-04-01'),as.Date('2010-01-01'),by = '3 month')-1))],q.id)

#t.bill<-setkey(data.table(read.csv(data.stock.dir('Market/t-bill.csv'),sep=',',header=T))[,':='(Date=as.Date(DATE,format='%m/%d/%Y'),DATE=NULL,interest=VALUE/100)][,q.id:=as.yearqtr(Date)],q.id)



#all.macro <- setkey(sp.sec.index[,list(q.id,Sector,sec.ret)][setkey(unique(na.omit(gnp[,list(q.id,log.gnp)][unique(infl[,list(q.id,inf.rate)])][unique(vix.index[,list(q.id,ret.vix)])][t.bill[,list(q.id,interest)]]),by='q.id'),q.id),allow.cartesian=T][,q.num:=.GRP,by=q.id][,Sector:=as.character(Sector)],q.num,Sector)

setkey(vvs.data,q.id,Stock)
all.vvs <- unique(na.omit(melt(all.macro[setkey(vvs.data[,list(q.id,Stock,Sector,btm,size,debt.to.eq,accruals)][unique(stock.returns[,list(q.id,Stock,s.vol)],by=c('q.id','Stock')),allow.cartesian=T],q.id,Sector)],id.vars = c('q.id','Stock'),measure.vars =  c('btm','size','debt.to.eq','accruals','s.vol','sec.ret','growth.gnp','inf.rate','ret.vix','t.bill'))),by=c('q.id','Stock','variable'))

array.all.vvs <- acast(all.vvs,q.id~Stock~variable)

#cache('array.all.vvs')
#cache('all.vvs')
rm('ref.matrix','stock.ref','sp.id','miss.tkt','miss.dt','ref.dt','full.stock.ref','all.vvs','all.macro','sp.sec.index','macro.vvs','stock.returns','sp500','tbill','stock.ret','q.ret.sp','ds.ret','all.stock.prices','sectors','vvs.data','tbil')


### Building variables states
#
# vvs.dt <- setnames(data.table(melt(array.all.vvs)),c('q.id','Stock','vvs','value'))
# [,state.vvs.f(value,1,8),by=list(Stock,vvs)]
#
# ex.vvs.dt <- setkey(vvs.dt,Stock)[c('AAPL','AMZN')]
#
# data=ex.vvs.dt[Stock=='AAPL'&vvs=='debt.to.eq'][,value]
#
#
# test <- setnames(data.table(melt(stock.vvs[,'AAPL',])),c('q.id','vvs','value'))[,state.vvs.f(value,1,8),by=vvs][,q.num:=1:.N,by=vvs]



#load('cache/vvs.RData')
### END====
#
# #source('/home/artur/workspace/MetaRanks/my.paths.linux.R')
# source('/Users/aiguzhinov/Documents/Dropbox/workspace/MetaRanks/my.paths.R')
#
#
# #help.start(browser='/Applications/Safari.app')
# delete.all <- function()
# 	rm(list=ls(pos=.GlobalEnv), pos=.GlobalEnv)
# delete.all()
#
# library(date)
# library(Hmisc)
# library(pastecs)
# library(abind)
# library(zoo)
# library(cluster)
# library(ade4)
# library(WGCNA)
# allowWGCNAThreads()
# library(xtable)
# library(IDPmisc)
# library(TTR)
# library(PerformanceAnalytics)
#
#
# #source(analysts.dir('ranking.function.global.R'))
# source(analysts.dir('main.functions.R'))
# source(analysts.dir('updated.functions.R'))
# source(analysts.dir('/Functions/ind.var.array.R'))
# sectors<-c('Consumer Discretionary',
# 		'Consumer Staples',
# 		'Energy',
# 		'Financials',
# 		'Health Care',
# 		'Industrials',
# 		'IT',
# 		'Materials',
# 		'Telecom Services',
# 		'Utilities')
#
#
#
#
#
# sector<-'Energy'
# 		#sectors[1]
# sec<-1
#
# # 'Industrials'
# #  'IT'
# #		'Materials'
# load(file=data.stock.dir('Market/market.return.in.quarter.RData'))
# quarters.est<-seq(as.Date('1989/4/1'), as.Date('2010/1/1'), by = "3 months")
#
# sapply(7
# 		#c(4:8,10),
# 		function(sec)
# {
# #load(data.stock.dir(paste(sectors[sec],'Acc.Variables/stock.prices.daily.RData',sep='/')))
# #load(analysts.dir(paste('/Experiments/Pre-processing/sel.data.list.',sectors[sec],'.RData',sep='')))
# #load(analysts.dir(paste('/Sectors/',sectors[sec],'/stock.data.array.RData',sep='')))
#
#
# setwd(analysts.dir(paste('/Sectors/',sectors[sec],sep='')))
# load('stock.data.sel.s.RData')
# #act.eps.values<-read.csv2('Acc.Variables/act.eps.csv',sep=';',header=T)
# #act.eps.date<-read.csv2('Acc.Variables/act.eps.date.csv',sep=';',header=T)
# #act.eps.date<-act.eps.date[,-2][-c(1,86),]
# #fq.date<-read.csv2('Acc.Variables/fq-dates.csv',sep=';',header=T)
# #fq.dates<-fq.date[1:84,]
# setwd(data.stock.dir(paste(sectors[sec])))
# stock.prices<-read.csv2('Acc.Variables/stock.prices.csv',sep=';',header=T)
# #sales<-read.csv2(('Acc.Variables/sales.csv'),sep=';',header=T)
# #depr<-read.csv2(('Acc.Variables/depr.csv'),sep=';',header=T)
# #taxes<-read.csv2(('Acc.Variables/taxes.csv'),sep=';',header=T)
# #debt<-read.csv2(('Acc.Variables/st.debt.csv'),sep=';',header=T)
# #tot.assets<-read.csv2(('Acc.Variables/tot.assets.csv'),sep=';',header=T)
# #tot.liab<-read.csv2(('Acc.Variables/tot.liab.csv'),sep=';',header=T)
# ##tot.liab<-tot.liab[,-2]
# #cash<-read.csv2(('Acc.Variables/cash.csv'),sep=';',header=T)
# #liab<-read.csv2(('Acc.Variables/curr.liab.csv'),sep=';',header=T)
# #assets<-read.csv2(('Acc.Variables/curr.assets.csv'),sep=';',header=T)
# #revenues<-read.csv2('Acc.Variables/revenues.csv',sep=';',header=T)
# ##revenues<-revenues[,-2]
# #net.income<-read.csv2('Acc.Variables/net.income.csv',sep=';',header=T)
# ##net.income<-net.income[,-2]
# shares<-read.csv2('Acc.Variables/shares.outstand.csv',sep=';',header=T)
# shares<-shares[,-2]
# #market.cap<-read.csv2('Acc.Variables/market.cap.csv',sep=';',header=T)
# ##market.cap<-market.cap[,-2]
# #equity<-tot.assets-tot.liab
# adj<-1:84
#
#
# #quarters.ibes<-read.csv2(data.stock.dir('Market/quarters.ibes.csv'),sep=';',header=F)
# #quarters<-sapply(strsplit(sub("FQ", ",", quarters.ibes[,1]), ","), function(v) {paste(v[2],v[1])})
#
# #out<--filter[[sec]]
# #sel.s<-c(1:ncol(stock.prices))
# #
# stock.data.array<-abind(stock.prices=stock.prices[adj,],shares=shares[adj,],
# #		equity=equity[adj,],assets=assets[adj,],
# #		cash=cash[adj,],liab=liab[adj,],
# #		debt=debt[adj,],taxes=taxes[adj,],
# #		depr=depr[adj,],tot.assets=tot.assets[adj,],
# #		sales=sales[adj,],revenues=revenues[adj,],
# #		net.income=net.income[adj,],tot.liab=tot.liab[adj,],
# #		market.cap=market.cap[adj,]
# 		along=3)
# #save(stock.data.array,file=analysts.dir(paste('/Sectors/',sectors[sec],'/stock.data.array.RData',sep='')))
# #
#
# #setwd(data.stock.dir(sectors[sec]))
# #save(stock.prices.daily,file='Acc.Variables/stock.prices.daily.RData')
# ##Return volatilty Serra----
# library('quantmod')
# library("PerformanceAnalytics")
# load(analysts.dir('/Sectors/Market/market.price.zoo.RData'))
# ret.m<-diff(log(market.price.zoo))
# end.points<-endpoints(ret.m,on='quarters')
#
# sec<-10
# load(analysts.dir(paste('/Sectors/prices.daily.',sectors[sec],'.RData',sep='')))
#
# load(analysts.dir(paste('/Sectors/',sectors[sec],'/raw.all.stock.vvs.RData',sep='')))
# load(analysts.dir(paste('/Sectors/',sectors[sec],'/stock.data.array.RData',sep='')))
#
# ret.i<-diff(log(prices.daily))
# ret.vol<-t(sapply(2:(length(end.points)),function(q)
# {
#   ##ret.i[end.points[q]:end.points[q-1],]-t.bill.zoo[end.points.rf[q]:end.points.rf[q-1],]
# market.var<-sum((ret.m[end.points[q]:end.points[q-1]]-mean(ret.m[end.points[q]:end.points[q-1]],na.rm=T))^2,na.rm=T)
#   #
# stock.ret<-ret.i[end.points[q]:end.points[q-1],]-ret.m[end.points[q]:end.points[q-1]]
# stock.var<-apply(stock.ret,2,function(i){sum(i^2,na.rm=T)})
#   #
#   stock.var+market.var
# }))
#
# load(analysts.dir(paste('/Experiments/Pre-processing/sel.data.',sectors[sec],'.RData',sep='')))
#
#
# #ret.vol.miss.stock<-firm.ret.vol(stock.prices.daily,market.return.in.quarter,quarters.est)
#
#
# #save(ret.vol,file='ret.vol.RData')
# #load(paste(data.stock.dir(sectors[sec],'ret.vol.RData',sep='/')))
# ###Cons. Discr c(7)
# ###Cons. Staples -NONE
# ###Energy c(71,118)
# ###Finacials AGII,IBCP  (10,116)
# ###Health Care - NONE
# ###Industrials: AAI, CAR CREG UAUA (1,36,56,220)
# ### IT: ACTL DTPI SMT  (6, 107,317)
# ### Materials: SSCC (68)
# ###Telecom.- NONE
# ### Utilities: AYE, PEG (4,28)
#
#
# #filter<-list(c(7),c(0),c(71,118),c(10,116),c(0),c(1,36,56,220),c(6,107,317),c(68),c(0),c(4,28))
# #stock.out<-c('ALOY','ME','SWSI','AGII','IBCP','AAI','CAR','CREG','UAUA','ACTL',
# #		'DTPI','SMT','SSCC','AYE','PEG')
#
# market.cap<-coredata(prices.daily[end.points,])*stock.data.array[,sel.s,'shares']
#
# ###Telecom
# #
# #market.cap<-stock.data.array[,sel.s,'market.cap']
# #save(market.cap,file=analysts.dir(paste('/Sectors/',sectors[sec],'/market.cap.RData',sep='')))
# })
#
# size<-log(market.cap)
#
# btm<-stock.data.array[,sel.s,'equity']/market.cap
#
# #size<-#market.cap[adj,sel.s]
# #		log(market.cap)
# #var.size<-variance(size,8)
#
# #accruals<-total.accruals(stock.data.array[,,'assets'],
# #		stock.data.array[,,'cash'],stock.data.array[,,'liab'],
# #		stock.data.array[,,'debt'],stock.data.array[,,'taxes'],
# #		stock.data.array[,,'depr'],stock.data.array[,,'tot.assets'])
# #var.accruals<-variance(accruals,8)
#
# #debt.to.eq<-stock.data.array[,,'debt']/stock.data.array[,,'equity']
# #var.debt.to.eq<-variance(debt.to.eq,8)
# #stock.ret<-stock.return(stock.prices[adj,sel.s])
# #var.btm<-variance(btm,8)
#
# #stock.vvs<-abind(ret=var.btm,debt.to.eq=var.debt.to.eq,size=var.size,accruals=var.accruals,along=3)
#
# raw.sel.stock.vvs<-abind(btm=btm,debt.to.eq=raw.all.stock.vvs[,sel.s,'debt.to.eq'],size=size,accruals=raw.all.stock.vvs[,sel.s,'accruals'],ret.vol=ret.vol,along=3)
# save(raw.sel.stock.vvs,file=analysts.dir(paste('/Sectors/',sectors[sec],'/raw.sel.stock.vvs.RData',sep='')))
#
# #raw.all.stock.vvs<-abind(debt.to.eq=debt.to.eq,size=size,accruals=accruals,along=3)
#
# #save(stock.vvs,raw.stock.vvs,file=analysts.dir(paste('/Sectors/',sectors[sec],'/stock.vvs.RData',sep='')))
#
# ###END----
#
# #save(ret.vol,file=analysts.dir(paste('/Sectors/',sectors[sec],'/ret.vol.RData',sep='')))
#
#
# #})
#
#
# m<-array(NA,c(84,length(all.stocks),4))
# for(s in 1:length(sectors))
# {
#   load(analysts.dir(paste('/Sectors/',sectors[s],'/raw.all.stock.vvs.RData',sep='')))
#
#   m[,all.stocks.id[[s]],]<-raw.all.stock.vvs
# }
# raw.stocks.vvs<-m
#
# sel.core.stocks.vvs<-raw.stocks.vvs[,core.stocks,]
# save(sel.core.stocks.vvs,file='sel.core.stocks.vvs.RData')
#
# load(data.stock.dir('New stocks/ret.vol.RData'))
# new.stocks.ret.vol<-ret.vol
#
# m<-array(NA,c(84,length(all.stocks)))
# for(s in 1:length(sectors))
# {
#   load(analysts.dir(paste('/Sectors/',sectors[s],'/ret.vol.RData',sep='')))
#
#   m[,sel.stocks.list[[s]]]<-ret.vol
# }
# sel.stocks.ret.vol.vvs<-m
#
#
# m<-array(NA,c(84,length(all.stocks)))
# for(s in 1:length(sectors))
# {
#   sel.stocks.ret.vol.vvs[,in.stocks.list[[s]]]<-new.stocks.ret.vol[,in.stocks.id.list[[s]]]
# }
#
#
# sel.stocks.ret.vol.vvs<-m
#
# raw.stocks.vvs<-abind(raw.stocks.vvs,sel.stocks.ret.vol.vvs)
# save(raw.stocks.vvs,file=analysts.dir('Sectors/All Sectors/raw.stocks.vvs.RData'))
#
#
#
# # sapply(sectors,function (sector)
# # 		{
# # setwd(analysts.dir(paste('/Sectors/',sectors[sec],sep='')))
# # load('broker.consensus.stats.RData')
# # load('all.forecasts.all.brokers.Rdata')
# #
# # setwd(analysts.dir(paste('/Sectors/Stats data/',sectors[sec],sep='')))
# # load('AAPL.stats.RData')
#
# ####Creating consensus vvs ----
# sapply(1:10,function (sec)
# {
# load(analysts.dir(paste('/Experiments/Pre-processing/sel.data.',sectors[sec],'.RData',sep="")))
# ####Calculating EPS forecasts data----
# setwd(analysts.dir(paste('Sectors/Stats data/',sectors[sec],sep='')))
# stocks <- list.files()
# names.stocks<-sapply(strsplit(sub(".stats.RData", ",", stocks), ","), function(v) {paste(v[1])})
#
# all.b.moments<-abind(lapply(1:length(stocks),function(s)
# {
#   load(stocks[order(names.stocks)][s],envir=.GlobalEnv)
#   sapply(1:84,function(q)
# {
#     data.all <- unlist(sapply(names(stats),function(b)
#     {
#       stats[[b]][,2][stats[[b]][,3]==q]
#     }),use.names=F)
#
#     means<-ifelse(length(data.all)==0,NA,mean(data.all,na.rm=T))
#     st.dev<-ifelse(length(data.all)==0,NA,sd(data.all,na.rm=T))
#
#   c(means,st.dev)
#     })
# }),along=3,new.names=list(c('in.mean.all','st.dev'),NULL,NULL))
#
# core.b.moments<-abind(lapply(1:length(stocks),function(s)
# {
#   load(stocks[order(names.stocks)][s],envir=.GlobalEnv)
#   sapply(1:84,function(q)
#   {
#     data.all <- unlist(sapply(names(core),function(b)
#     {
#       stats[[b]][,2][stats[[b]][,3]==q]
#     }),use.names=F)
#
#     core.means<-ifelse(length(data.all)==0,NA,mean(data.all,na.rm=T))
#     core.st.dev<-ifelse(length(data.all)==0,NA,sd(data.all,na.rm=T))
#
#     c(core.means,core.st.dev)
#   })
# }),along=3,new.names=list(c('in.mean.core','core.st.dev'),NULL,NULL))
#
#
# consensus.vvs<-abind(aperm(all.b.moments,c(2,3,1)),aperm(core.b.moments,c(2,3,1)),along=3)
#
# save(consensus.vvs,file=analysts.dir(paste('/Sectors/',sectors[sec],'/consensus.all.s.vvs.RData',sep='')))
# })
#
# ###Calculate revisions of forecasts moments--------
# sapply(1:10,function (sec)
# {
#   load(analysts.dir(paste('/Experiments/Pre-processing/sel.data.',sectors[sec],'.RData',sep="")))
#
#   setwd(analysts.dir(paste('Sectors/Stats data/',sectors[sec],sep='')))
#   stocks <- list.files()
#   names.stocks<-sapply(strsplit(sub(".stats.RData", ",", stocks), ","), function(v) {paste(v[1])})
#
#   all.b.moments<-sapply(1:length(stocks),function(s)
#   {
#     load(stocks[order(names.stocks)][s],envir=.GlobalEnv)
#     sapply(1:84,function(q)
#     {
#       data.all <- unlist(sapply(names(stats),function(b)
#       {
#         length(stats[[b]][,2][stats[[b]][,3]==q])
#       }),use.names=F)
#
#       mean(data.all[data.all!=0])
#       #st.dev<-ifelse(length(data.all)==0,NA,sd(data.all,na.rm=T))
#       })
#   })
#
#   core.b.moments<-sapply(1:length(stocks),function(s)
#   {
#     load(stocks[order(names.stocks)][s],envir=.GlobalEnv)
#     sapply(1:84,function(q)
#     {
#       data.all <- unlist(sapply(names(core),function(b)
#       {
#         length(stats[[b]][,2][stats[[b]][,3]==q])
#       }),use.names=F)
#
#       mean(data.all[data.all!=0])
#     })
#   })
#
#
#   revisions.vvs<-abind(t(all.b.moments),t(core.b.moments),along=3,new.names=list(NULL,NULL,c('mean.rev.all','mean.rev.core')))
#   revisions.vvs<-aperm(revisions.vvs,c(2,1,3))
#
#   save(revisions.vvs,file=analysts.dir(paste('/Sectors/',sectors[sec],'/revisions.all.s.vvs.RData',sep='')))
# })
#
#
# #var.revisions<-apply(broker.stats[,1,,],c(1,3),function(i){sd(i[i!=0]) })
# #mean.revisions<-apply(broker.stats[,1,,],c(1,3),function(i){mean(i[i!=0]) })
#
# in.q.consensus.core<-apply(broker.stats[,3,,],c(1,3),function(i){mean(i,na.rm=T) })
# in.q.sd.consensus.core<-apply(broker.stats[,3,,],c(1,3),function(i){sd(i,na.rm=T) })
#
# in.q.consensus<-consensus.stats[,3,]
# #roll.rev<-rolling.stats(broker.stats,1,8,'mean')
# #roll.rev.sd<-rolling.stats(broker.stats,1,8,'sd')
#
# roll.cons.mean<-rolling.stats(broker.stats,3,8,'mean')
# roll.cons.sd<-rolling.stats(broker.stats,3,8,'sd')
#
#
# #revision.vvs<-abind(mean=mean.revisions,roll.mean=roll.rev,sd=var.revisions,roll.mean=roll.rev.sd,along=3)
#
# consensus.core.vvs<-abind(in.mean.core=in.q.consensus.core,in.mean.all=in.q.consensus,in.q.sd=in.q.sd.consensus.core,
# 						roll.mean=roll.cons.mean,roll.sd=roll.cons.sd,along=3)
#
#
# #save(revision.vvs,file=analysts.dir(paste('/Sectors/',sector,'/revision.vvs.RData',sep='')))
# save(consensus.core.vvs,file=analysts.dir(paste('/Sectors/',sector,'/consensus.core.vvs.RData',sep='')))
#
# })
#
#
# var.consensus<-consensus.stats[,5,]
# var.consensus<-consensus.stats[,3,]
#
# test<-apply(broker.stats[,3,,51],2,function(i){SMA(i,n=8) })
#
# #last.forecast<-last.forecast.all
# #
# #####*******Market data
# #market.ret<-read.csv(data.stock.dir('Market/market.data.csv'))
# #market.price<-read.csv(data.stock.dir('Market/sp.price.csv'))
#
# vix.price<-read.csv2(data.stock.dir('Market/vxocurrent.csv'),sep=';',header=T)
# vix.old.price<-read.csv2(data.stock.dir('Market/vxoarchive.csv'),header=T,sep=";",dec=',')
# #vix<-rbind(vix.old.price,vix.price)
# #vix.months<-as.Date(as.date(as.character(vix[,1]), order='dmy'))
#
# #vix.xts <- xts(vix[,5],order.by=as.Date(as.character(vix[,1]),format='%d-%m-%Y'))
# #end.q <- endpoints(index(vix.xts),on='quarters')
# #vix.xts.q <- vix.xts[end.q[13:96],]/100/sqrt(4)
#
# #vix.xts.q/100/sqrt(4)
#
# #vix.ret <- Return.calculate(vix.xts.q)
# #vix.prices.quarter<-c(sapply(2:length(quarters.est), function(q)
# #		{
# #head(vix[which(vix.months<=quarters.est[q] & vix.months>quarters.est[q-1]),5],1)
# #		}), tail(vix[which(vix.months<=quarters.est[84]),5],1))
#
# #var.vix<-variance(as.matrix(vix.prices.quarter),8)
# sp.sector.index<-read.csv2(data.stock.dir('Market/sectors.indecies.csv'),header=T,sep=";",dec=',')
# #sector.xts <- xts(sp.sector.index[,2:11],order.by=as.yearqtr(as.character(sp.sector.index[,1]),format='Q%q %Y'))
#
# sector.ret <- Return.calculate(sector.xts)
# #
# #quarters.m<-as.Date(as.Date(as.character(market.ret[1:85,1]), order='mdy'))
# #days.m<-as.Date(as.date(as.character(market.price[,1]), order='mdy'))
# #
# ####Market volatility:S&P500
# #
# #daily.return<-sapply(2:length(days.m),function(i)
# #		{
# #			#log(market.price[i,2])-log(market.price[i-1,2])
# # diff(log(market.price[,2]))
# #		})
#
# market.return.in.quarter<-sapply(2:length(quarters.est), function(q)
# 		{
# 			daily.return[which(days.m<=quarters.est[q] & days.m>quarters.est[q-1])]
#
# 		})
# #save(market.return.in.quarter,file=data.stock.dir('Market/market.return.in.quarter.RData'))
#
# daily.ret.in.quarter<-function(price,quarters.est)
# {
# 	#days.m<-as.Date(as.date(as.character(index(price)), order='mdy'))
#
# 	daily.return<-diff(log(price),1)
# 	sapply(2:length(quarters.est), function(q)
# 			{
# 				daily.return[which(index(daily.return)<=quarters.est[q] & index(daily.return)>quarters.est[q-1])]
#
# 			})
# }
# #daily.prices.in.quarter<-sapply(2:length(quarters.est), function(q)
# #		{
# #			market.price[which(days.m<=quarters.est[q] & days.m>quarters.est[q-1]),2]
# #
# #		})
# #
# #market.variance<- sapply(1:length(daily.prices.in.quarter),function(i)
# #				{
# #					quarter.ret<-log(tail(daily.prices.in.quarter[[i]],1))-log(head(daily.prices.in.quarter[[i]],1))
# #
# #					#(quarter.ret-mean(daily.return.in.quarter[[i]]))^2*length(daily.prices.in.quarter[[i]])
# #				})
# #
# #
# #
# #load(data.stock.dir('Market/market.var.Rdata'))
# infl<-read.csv2(data.stock.dir('Market/inflation.monthly.csv'),sep=';',header=T)
# #inflation<-infl[-c(1:12),][seq(3,253,3),3]/100
# gnp<-read.csv2(data.stock.dir('Market/gnp.quarterly.csv'),sep=';',header=T)
# t.bill<-read.csv(data.stock.dir('Market/t-bill.csv'),sep=',',header=T)
# #t.bill.months<-as.Date(as.date(as.character(t.bill[,1]), order='mdy'))
# interest.rate<-sapply(1:length(quarters.est), function(q)
# 		{
# 			t.bill[t.bill.months==quarters.est[q],2]/100
# 		})
# #
# #raw.macro.vvs<-cbind(interest=interest.rate,gnp=gnp,inflation=inflation,market=vix.prices.quarter)
# #
# #
# var.gnp<-variance(as.matrix(gnp),8)
# #var.gnp.sc<-variance(scale(as.matrix(gnp)),8)
# #
# var.inflation<-variance(as.matrix(inflation),8)
# #var.sp<-variance(scale(sp.sector.index[,-1]),8)
# var.interest.rate<-variance(as.matrix(interest.rate),8)
# var.vix<-variance(as.matrix(vix.prices.quarter),8)
# ##
# ##mean.forecasts<-apply(energy.broker.stats[,1,,],c(1,2),function(i){mean(i[i!=0]) })
# ##mean.stocks<-apply(energy.broker.stats[,1,,],c(1,3),function(i){sum(i[i!=0]) })
# #
# #
# ##forecast.error<-fe(last.forecast.all,act.eps.values[-85,],stock.prices[-85,])
# ##fe.firm<-fe.firm.component(forecast.error,1)
# ##relat.optimism<-relative.optimism(last.forecast.all,stock.prices[-85,])
# #
# ##consensus.sd<-consensus.var(consensus.est)
# ##consensus.delta<-consensus.change(consensus.est)
# ##fe<-creamer.fe(consensus.est,act.eps.values[-c(1,86),])
# ##sue.values<-sue(act.eps.values[-c(1,86),])
# ##ep<-earnings.to.price(act.eps.values[-c(1,86),],stock.prices[-c(1,86),])
# ##sg<-sales.growth(sales[-c(1,86),])
# #
# #
# #
# macro.vvs<-(cbind(interest=var.interest.rate,gnp=var.gnp,inflation=var.inflation,market=var.vix))
# #sector.vvs<-var.sp
# #
# #raw.macro.vvs<-cbind(interest=interest.rate,gnp=gnp,inflation=inflation,market=vix.prices.quarter)
# #raw.sector.vvs<-sp.sector.index[,-1]
# #
# save(macro.vvs,sector.ret,raw.macro.vvs,raw.sector.vvs,file=data.stock.dir('Market/macro.sector.vvs.RData'))
#
#
# #####ALL VARIABLES ANALYSIS: TIME SERIES ANALYSIS
# load(analysts.dir(paste('/Sectors/',sectors[sec],'/raw.stock.vvs.RData',sep='')))
# load(analysts.dir(paste('/Sectors/',sectors[sec],'/consensus.core.vvs.RData',sep='')))
# load(analysts.dir(paste('/Sectors/',sectors[sec],'/revision.vvs.RData',sep='')))
# load(data.stock.dir('Market/macro.sector.vvs.RData'))
#
# revision.vvs
# raw.stock.vvs
# consensus.core.vvs
# raw.macro.vvs
# raw.sector.vvs
#
#
# x<-ts(raw.macro.vvs[,4],frequency=4)
#
# x<-ts(na.locf(raw.stock.vvs[,7,4],fromLast=T),frequency=4)
#
#
# decom.macro.vvs<-decompose(x,type=c('multiplicative'))
#
#
# plot(decom.macro.vvs)
#
# ts(scale(raw.macro.vvs[,2]),frequency=4)
#
# test<-abind(raw.stock.vvs,consensus.core.vvs,revision.vvs,along=3)
# test<-test[,,-c('roll.mean','roll.sd')]
# ###Testing autocorrelation with growing window of max lag
#
# test.2<-apply(test,2:3,function(i)
# 		{ acf(i,plot=F,na.action=na.pass,lag=84)$acf[,,1]})
# ###variance
#
# test.3<-apply(test,2:3,function(i)
# 		{ variance.array(i,8)})
#
# test.4<-apply(test,2:3,function(i)
# 		{ diff(log(i),1) })
#
#
# semi.var<-function(data)
# {
# mean(
# 		((mean(data,na.rm=T)-data)[which(data<mean(data,na.rm=T))])^2
# )
#
# }
#
# #load(data.stock.dir('Market/macro.sector.vvs.RData'))
#
# #ind.var<-abind(lapply(1:ncol(predictors[,,1]),function(s)
# #				{
# #					cbind(predictors[,s,],market.var=market.variance[1:84],inflation=inflation,gnp=gnp)
# #				}),along=3)
#
# #save(ind.var,file='ind.var.RData')
# #save(relat.optimism,file='relative.optimism.RData')
#
# ##load('out.RData')
# #load(analysts.dir(paste('/Experiments/Pre-processing/sel.data.list.',sectors[sec],'.RData',sep='')))
# ##setwd(analysts.dir('/Sectors/All Sectors'))
# ##load('latest.est.all.sectors.RData')
# ##load('distrib.forecasts.all.sectors.RData')
# #
# sel.s.list<-all.stocks[mis.stocks.in]#names(stock.prices[sel.s])#7,310
# symbols<-all.stocks[mis.stocks.in]
# #		#sel.s.list[-c(6,107,317)]
# ####Getting daily stock prices-------
# library(quantmod)
# tickers<-sort(names(unlist(which.from.all)))
#
# getSymbols(tickers[-c(82,258,268)],from='1989-01-01',enddate='2009-12-31',src='google')
#
# AdClose <- do.call(merge, lapply(sel.s.in.sp[1:74], function(x) Ad(get(x))))
# which(sel.s.in.sp=='CEG')#75
#
# getSymbols(sel.s.in.sp[76:425],from=ustart,end=uend)
# AdClose.2<- do.call(merge, lapply(sel.s.in.sp[76:425], function(x) Ad(get(x))))
#
#
#
#
# miss.id <- which(tckk=='MBR')
# prev.id <- miss.id-1
# miss.id
# #
# #
# price.1<-all_dat
# names(price.1)<-miss.tkt[1:prev.id]#next.id:length(miss.tkt)]
# stock.1<-dummy
# names(stock.1)<-tckk[miss.id]
# next.id <- miss.id+1
# price.2<-all_dat
# names(price.2)<-c('TRUE.','TSH')
#
# adj.stock.2 <- c(price.1,stock.61,price.2,stock.302,price.3)
#
# #stock.48*
# #price.3<-all_dat
# #stock.52*
# #price.4<-all_dat
# #stock.59*
# #price.5<-all_dat
# #stock.63*
# #price.6<-all_dat
# #stock.65*
# #price.7<-all_dat
# #stock.73*
# #price.8<-all_dat
# #stock.77*
# #price.9<-all_dat
# #87
# #price.10<-all_dat
# #95
# #price.11<-all_dat
# #99
# #price.12<-all_dat
# #101
# #price.13<-all_dat
# #140
# #stock.142
#
# #Dummy stock
#
#
# #rm(price.1,price.2,price.3,price.4)
# #
# #stock.25<-get.hist.quote('ARTG', start=ustart, end=uend,quote = c("Close"),provider = "yahoo", compression = "d")
# #stock.48<-get.hist.quote('car', start=ustart, end='2011-11-01',quote = c("Close"),provider = "yahoo", compression = "d")
# #stock.52<-get.hist.quote('ccg', start=ustart, end=uend,quote = c("Close"),provider = "yahoo", compression = "d")
# #stock.59<-get.hist.quote('chp', start=ustart, end=uend,quote = c("Close"),provider = "yahoo", compression = "d")
# #stock.63<-get.hist.quote('cnxt', start=ustart, end=uend,quote = c("Close"),provider = "yahoo", compression = "d")
# #stock.73<-get.hist.quote('cru', start=ustart, end=uend,quote = c("Close"),provider = "yahoo", compression = "d")
# stock.142<-get.hist.quote('gsicx', start=ustart, end=uend,quote = c("Close"),provider = "yahoo", compression = "d")
# #
# ##stock.prices.daily<-c(stock.prices.daily[1:309],as.list(stock.310),
# ##						stock.prices.daily[310:329])
# #
# #
# stocks.combined<-c(price.1,price.2)
# sapply(1:7,function(sec)
#   {
#   new.sel.s.prices<-stocks.combined[names(new.sel.sec.adj[[sec]][[1]])]
#   save(new.sel.s.prices,file=analysts.dir(paste('/Sectors/',sectors[sec],'/new.sel.s.prices.RData',sep='')))
# })
# sel.s.in.sp.adj.prices<-stocks.combined
# save(sel.s.in.sp.adj.prices,file=analysts.dir('Sectors/All Sectors/sel.s.in.sp.adj.prices.RData'))
#
#
# ####Stock prices daily adjustments for all sectors---
# ####load old.sel stock prices daily file----
# sec<-5
# load(analysts.dir(paste('/Sectors/',sectors[sec],'/stock.prices.daily.RData',sep='')))
# load(analysts.dir(paste('/Experiments/Pre-processing/sel.data.list.',sectors[sec],'.RData',sep='')))
# load(analysts.dir(paste('/Sectors/',sectors[sec],'/new.sel.s.prices.RData',sep='')))
# load(analysts.dir(paste('/Sectors/',sectors[sec],'/tickers.RData',sep='')))
#
# names(stock.prices.daily)<- tickers[order(tickers)][sel.s]
# old.sel<-sel.s
# load(analysts.dir(paste('/Experiments/Pre-processing/sel.data.',sectors[sec],'.RData',sep='')))
#
# new.sel<-sel.s
#
# new.tickers<-new.sel[which(is.na(match(names(new.sel),names(stock.prices.daily))))]
#
# out.tickers.id<-which(is.na(match(names(stock.prices.daily),names(new.sel))))
#
#
# tckk <- names(new.tkt)# ticker names defined
# #
# all_dat<-list()#empty list to fill in the data
# numtk <-length(tckk);
# for(i in 1:numtk)
# {
#   all_dat[[i]]<-xxx<-get.hist.quote(instrument = tckk[i], start=ustart, end=uend,quote = c("Close"),provider = "yahoo", compression = "d")
#   }
#
# names(dummy)<-'NCL'
#
# price.1<-all_dat
# names(price.1)<-names(new.tickers)
#
# price.2<-all_dat
# names(price.2)<-names(new.tickers[9:20])
#
# price.3<-all_dat
# names(price.3)<-'CBC'
#
# new.stock.prices.daily<-c(stock.prices.daily[-(out.tickers.id)],price.1)
#
# stock.prices.daily<-new.stock.prices.daily[order(names(new.stock.prices.daily))]
#
# save(stock.prices.daily,file=analysts.dir(paste('/Sectors/',sectors[sec],'/new.stock.prices.daily.RData',sep='')))
#
# ####-end----
# ####Maing a daily prices matrix for each sectors---
# #days.m<-as.Date(as.date(as.character(market.price[-1,1]), order='mdy'))
# ###S&P 500 data set---
# sapply(1:10,function(sec)
# {
# load(analysts.dir(paste('/Sectors/',sectors[sec],'/new.stock.prices.daily.RData',sep='')))
# d<-zoo(matrix(NA,nrow=5289,ncol=number.sel.stocks),order.by=index.days)
# colnames(d)<-sel.s.in.sp
#
# for( s in 1:length(sel.s.in.sp.adj.prices)  )
#    {
#      d[index(sel.s.in.sp.adj.prices[[s]]),s ]<-sel.s.in.sp.adj.prices[[s]]
#      }
#
# prices.adj.sp.daily<-d
# save(prices.adj.sp.daily,file=analysts.dir('/Sectors/All Sectors/prices.adj.sp.daily.RData'))
# })
# tickers <- as.character(new.tkt[,1])
# ######Making matrix----
# a<-zoo(matrix(NA,nrow=1:length((dummy[[1]])),ncol=length(new.tkt),dimnames=list(NULL,new.tkt)),order.by=index(dummy[[1]]))
# for( s in 1:length(price.1)  )
# {
#   a[index(price.1[[s]]),s ]<-price.1[[s]]
# }
# colnames(d)<-tickers
# new.price.adj.daily<-merge.zoo(prices.adj.sp.daily,d)
# new.price.adj.daily<-new.price.adj.daily[,sort(names(new.price.adj.daily))]
# save(new.price.adj.daily,file=analysts.dir('/Sectors/All Sectors/new.price.adj.daily.RData'))
# #######
# #new.prices.daily<-merge.zoo(prices.daily.old,d)
# new.prices.daily<-merge.zoo(new.prices.daily,d)
# new.prices.daily<-new.prices.daily[,sort(names(new.prices.daily))]
# save(new.prices.daily,file=analysts.dir('/Sectors/All Sectors/new.prices.daily.RData'))
#
# })
# stock.prices.daily<-c(
# 	price.1,
# 		dummy,
# 		price.2),
#    dummy, #48
# 		price.3,
# dummy,#		#as.list(stock.52),
# 	price.4,
# dummy,#		as.list(stock.59),
#  	price.5,
# dummy,#		#as.list(stock.63),
# 		price.6,
# dummy,#		as.list(stock.65),
# 		price.7,
#   dummy,#73
#   price.8,
#   dummy, #77
# 	price.9,
#   dummy,#87
# 	price.10,
#   dummy, #95
# 	price.11,
#   dummy, #99
# 	price.12,
#   dummy, #101
# 	price.13,
#   dummy, #140
# 	price.14,
# as.list(stock.142)
# 		)
# #		#as.list(stock.226),all_dat)
# ##stock.prices.daily<-c(price.1,price.2,price.3)
# missning.stock.prices<-sapply(1:length(stock.prices.daily),function(s)
#          {
#            dimnames(stock.prices.daily[[s]])[[1]]<-missin.stocks[s]
#          })
#
# ####Merging all sel stocks into one matrix----
# sec=9
# load(analysts.dir(paste('/Sectors/',sectors[sec],'/new.stock.prices.daily.RData',sep='')))
# index.days <- index(stock.prices.daily[['S']])
#
# all.sel.tickers <- sort(unique(unlist(sapply(1:10,function(sec){
# load(analysts.dir(paste('Sectors/','sel.all.data.',sectors[sec],'.RData',sep='')))
# names(sel.s)
# }))))
#
# d<-zoo(matrix(NA,nrow=5289,ncol=length(sp.tickers)),order.by=index.days)
# colnames(d) <- sp.tickers
#
#   for( s in 1:length(sp.tickers)  )
#   {
#     stock <- names(adj.stock.2[s])
#     d[index(adj.stock.2[[s]]),stock ]<-adj.stock.2[[s]]
#   }
#
# prices.adj.sel.daily.part.2<-d
#
# sp.tickers <- all.sel.tickers[which(is.na(pmatch(all.sel.tickers,sel.not.sp.tikers)))]
#
# temp.adj.prices <- cbind(prices.adj.sel.daily.part.1,prices.adj.sel.daily.part.2)
# adj.prices.sel <- temp.adj.prices[,sort(colnames(temp.adj.prices))]
# adj.prices.sel.no.sp500 <- prices.adj.sel.daily.part.1[,sort(colnames(prices.adj.sel.daily.part.1))]
# adj.prices.sp500 <- prices.adj.sel.daily.part.2[,sort(colnames(prices.adj.sel.daily.part.2))]
#
# adj.prices.temp <- cbind(adj.prices.sel,a)
# adj.prices.sel <- adj.prices.temp[,sort(colnames(adj.prices.temp))]
# save(adj.prices.sel,file=analysts.dir('/Sectors/All Sectors/adj.prices.sel.RData'))
#
#
#
