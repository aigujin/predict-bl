# TODO: Add comment
# 
# Author: aiguzhinov
###############################################################################

###Analyst.rankings
analysts.dir<-function(filename)
{
paste('~/Dropbox/workspace/AnalystRanking/',filename,sep='')
}	

###Metaranks
meta.dir<-function(filename)
{
	paste('~/Dropbox/workspace/MetaRanks/',filename,sep='')
}

###Database path
data.stock.dir<-function(filename)
{
	paste('~/Dropbox/AnalystDB/Sectors/',filename,sep='')
}

###Projets
project.dir<-function(filename)
{
  paste('~/Dropbox/workspace/Projects/',filename,sep='')
}