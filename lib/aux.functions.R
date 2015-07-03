roll.ret.f <- function(Return){
        cbind(ann.ret=Return.annualized(Return,scale=4),
              ann.sd=sd(Return)*sqrt(4),
              cum.ret=Return.cumulative(Return))
}

grow.window.f <- function(x,len,...){
        rollapplyr(x,len,...,partial=T)
}

show.no.na <- function(data) {
    data[!is.na(data)]
}


zeros2NA <- function(x){
        x[is.infinite(x)] <- NA
        x[x==0] <- NA
        x
}

# ### create boxplot---
# boxplot.f <- function(data, name) {
#     load(analysts.dir("Sectors/Market/quarters.zoo.RData"))
#
#     require(zoo)
#     require(plyr)
#     if (is.list(data)) {
#         data.list <- data
#     } else {
#         data.list <- alply(data, 1, function(i) {
#             i
#         })
#     }
#
#     boxplot(data.list, xaxt = "n", main = name)
#     abline(h = 0)
#     axis(1, at = c(seq(1, length(data.list), 4), length(data.list)),
#         labels = as.yearqtr(quarters.zoo[c(seq(length(quarters.zoo) -
#             length(data.list) + 1, length(quarters.zoo), 4),
#             length(quarters.zoo))]))
# }
#
# boxplot.gplot.f <- function(data,varnames,title,breaks="12 months"){
#   load(analysts.dir("Sectors/Market/quarters.zoo.RData"))
#   df <- melt(data,na.rm=T,varnames=varnames)
#   df.dates <- data.frame(Dates=quarters.zoo[df[,1]],df)
#   ggplot(df.dates,aes(x=Dates,y=value,group=round_any(Quarters,1,floor)))+geom_boxplot()+ scale_x_date(breaks = date_breaks(breaks),labels = date_format("%Y"))+ggtitle(title)
# }


findIntersect <- function(x,y,dimension){
        intersect(dimnames(x)[[dimension]],dimnames(y)[[dimension]])
}

descriptive.f <- function(columns,...)
{
        require(pastecs)
        data.table(rownames(stat.desc(as.matrix(columns),...)),round(stat.desc(as.matrix(columns),...),3))
}

truncate.f <- function(data,percentile)
{
        cuts<- quantile(data[!is.infinite(data)],c(percentile,1-percentile),na.rm=T)
        data[data<cuts[[1]]|data>cuts[[2]] ] <- NA
        data
}


split.rank <- function(r,n){
        if(all(is.na(r))){NA_character_}else{
                if(max(r,na.rm=T)==3|max(r,na.rm=T)==4){n=1}else{if(max(r,na.rm=T)==5|max(r,na.rm=T)==6){n=2}else{if(max(r,na.rm=T)==7|max(r,na.rm=T)==8){n=3}}}#else{if(max(r,na.rm=T)==9|max(r,na.rm=T)==10){n=4}}}}
                #### remove r with <8
                t <- head(order(r,na.last=NA),n)
                b <- tail(order(r,na.last=NA),n)
                replace(replace(replace(replace(r,t,'top'),b,'bottom'),setdiff(order(r,na.last=T),c(t,b)),'middle'),which(is.na(r)),NA_character_)
        }
}

terciles.rank.f <- function(r,n)
{
  rel.r <- r/max(r,na.rm=T)
  cuts<- quantile(rel.r,seq(0,1,1/n)[2:4],na.rm=T)
  replace(replace(replace(r,rel.r<=cuts[[1]],'top'),rel.r>cuts[[1]]&rel.r<=cuts[[2]],'middle'),rel.r>cuts[[2]]&rel.r<=cuts[[3]],'bottom')
}

summary.stat <- function(dt){
  rbind(setnames(cbind(dt[,.N,by=list(year,Stock)][,.N,by=year],dt[,.N,by=list(year,Broker)][,.N,by=year][,N],dt[,.N,by=year][,N]),c('Years','Firms','Brokers','Observations')),data.table(cbind(Years='All years','Firms'=dt[,.N,by=Stock][,.N],'Brokers'=dt[,.N,by=Broker][,.N],'Observations'=prettyNum(dt[,.N],big.mark=' '))))
}




summary.mean.stat <- function(dt,value){
        rbind(setnames(cbind(dt[,.N,by=list(year,Stock)][,descriptive.f(N),by=year][V1==value][,c(1,3),with=F],dt[,.N,by=list(year,Broker)][,descriptive.f(N),by=year][V1==value][,3,with=F],dt[,.N,by=list(year,Stock,Broker)][,descriptive.f(N),by=year][V1==value][,3,with=F]),c('Years','Firms','Brokers','Observations')),setnames(data.table(cbind(Years='All years','Firms'=dt[,.N,by=Stock][,descriptive.f(N)][V1==value][,2,with=F],'Brokers'=dt[,.N,by=Broker][,descriptive.f(N)][V1==value][,2,with=F],'Observations'=dt[,.N,by=list(Stock,Broker)][,descriptive.f(N)][V1==value][,2,with=F])),c('Years','Firms','Brokers','Observations')))
}
cont.tab.f <- function(dt,t,n.b)
{

    rank.split <- na.omit(dt)[,valid.b:=.N>n.b,by=.(q.id,Stock)][(valid.b)][,valid:=.N>4,by=.(Broker,Stock)][(valid)][,next.per:=terciles.rank.f(rank,n),by=list(q.id,Stock)][,next.q:=c(rep(NA,t),as.numeric(diff(q.id,t))==t/4),by=.(Broker,Stock)][,cur.per:=c(rep(NA,t),head(next.per,-t)),by=list(Broker,Stock)][,cur.per:=ifelse(is.na(cur.per),NA,ifelse(next.q==TRUE,cur.per,NA))]

    require(descr)
    pt.cont.dt <- na.omit(rank.split)[,valid.s:=.N>n,by=.(Stock)][(valid.s)][,as.data.table(crosstab(cur.per,next.per,prop.r=T,prop.c=F,prop.t=F,prop.chisq = F,plot=F,missing.include=F,chisq=F)[2]),by=.(Stock)][,value:=letters[1:9],by=.(Stock)][,mean(prop.row),by=.(value)]

    matrix(as.matrix(pt.cont.dt[,3,with=F]),nrow=3,ncol=3)[c(3,2,1),c(3,2,1)]

}

data.collect<- function(data.id,n)
{
        rbindlist(mclapply(list.files('~/Dropbox/Datastream/AllStocks/it3/',pattern=paste('^',data.id,'.*.csv',sep='')),function(i){
                id=strsplit(i,split = '\\.');
                names.tmp <- sapply(read.csv2(paste('~/Dropbox/Datastream/AllStocks/it3/',i,sep=''),nrow=1,sep=',',na.strings='#ERROR',header=F,skip=1)
                                    ,function(i){strsplit(as.character(i),split = '[[:punct:]]')[[1]][n]})
                melt(setnames(fread(paste('~/Dropbox/Datastream/AllStocks/it3/',i,sep=''),sep=',',na.strings='')[,V1:=as.Date(V1,format = "%m/%d/%Y")],c('Date',names.tmp[2:length(names.tmp)])),id.vars = 'Date',variable.name = 'DSCD',value.name = id[[1]][1],na.rm=T)},mc.cores=4))
}

af.data.collect<- function(data.id,n)
{
        rbindlist(mclapply(list.files('~/Dropbox/Datastream/AllStocks/it3/',pattern=paste('^',data.id,'.*.csv',sep='')),function(i){
                id=strsplit(i,split = '\\.');
                names.tmp <- sapply(read.csv2(paste('~/Dropbox/Datastream/AllStocks/it3/',i,sep=''),nrow=1,sep=',',na.strings='#ERROR',header=F,skip=1)
                                    ,function(i){strsplit(as.character(i),split = '[[:punct:]]')[[1]][n]})
                melt(setnames(fread(paste('~/Dropbox/Datastream/AllStocks/it3/',i,sep=''),sep=',',na.strings='',skip=2)[,V1:=as.Date(V1,format = "%m/%d/%Y")],c('Date',names.tmp[2:length(names.tmp)])),id.vars = 'Date',variable.name = 'DSCD',value.name = id[[1]][1],na.rm=T)},mc.cores=4))
}

total.accruals<-function(cur.assets,cash,cur.liab,debt,tax,depr,tot.assets)
{
        variables<-cbind(
                cur.assets,
                cash,
                cur.liab,
                debt,
                tax,
                tot.assets)
        variables[is.na(variables)]<-0
        variables.diff<-diff(variables,1)
        num<-variables.diff[,1]-variables.diff[,2]-
                (variables.diff[,3]-variables.diff[,4])-
                variables.diff[,5]-depr

        denom<-diff(variables[,6],lag=4)/2
        ta<-num/denom
}
broker.vvs.f <- function(forecasts, actual)
{
        sq.error <- (mean(actual) - mean(forecasts))^2
        dispersion <- var(forecasts)
        num.forcst <- length(forecasts)
        sq.error[is.na(sq.error)] <- 0
        uncertainty <- sq.error+dispersion
        inf.assym <- 1-(sq.error - dispersion/num.forcst )/((1-1/num.forcst)*dispersion+sq.error)
        list(uncertainty=uncertainty,assym=inf.assym,dispersion=dispersion)
}
