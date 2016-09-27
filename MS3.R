###
###     LOAD ALL SENTIMENT FOR DJIA STOCKS
###


#read all tweet data
tweets = read.csv("C:/Users/hp-pc/Documents/R/alldata/alltweets.csv")

#read the tickers under DJIA
djia = read.csv("C:/Users/hp-pc/Documents/R/alldata/djia.csv")


### AXP WORKSPACE  
###
### READ SENTIMENT DATA FOR AXP (change stock ticker to write new csv file at every run)

#qry = sprintf("select * from tweets where symbol = '%s'", com)
#axp_sent_all = sqldf(qry)
axp_sent_all = sqldf("select * from tweets where symbol = 'AXP'")

#get stock data for AXP
axp_stock_all = read.csv("C:/Users/hp-pc/Documents/R/alldata/axp.csv")

#filter if required
axp_stock_fin = sqldf("select *
                      from axp_stock_all")

#date format conversion
axp_stock_fin$fsdate <- as.Date(as.character(axp_stock_fin$Date),'%Y-%m-%d')

#convert sentiment timestamp to date string
axp_sent_fin = sqldf("select *,substr(timestamp_utc, 1, 10) as vdate
                     from axp_sent_all")

#date format conversion
axp_sent_fin$sdate <- as.Date(axp_sent_fin$vdate,'%Y-%m-%d')

#left outer join to get sentiment data for corresponding stock days
axp_total = sqldf("select f.*,s.*
                  from axp_stock_fin f  
                  left join axp_sent_fin s on f.fsdate = s.sdate")


#test for autocorrelation
acf(axp_total$Adj.Close,type = "correlation",na.action = na.pass)

#
# ANALYSIS FOR STOCK TRADING VOLUMES
#

#difference in volumes
volume_diff = c(diff((axp_total$Volume)),1)
volume_diff[is.na(volume_diff)] = 0
axp_total$volume_diff = -(volume_diff)

#test for autocorrelation
acf(axp_total$volume_diff,type = "correlation",na.action = na.pass)

#difference in tweets
tweet_diff = c(diff((axp_total$TOTAL_SCANNED_MESSAGES)),1)
tweet_diff[is.na(tweet_diff)] = 0
axp_total$tweet_diff = -(tweet_diff)

#test for autocorrelation
acf(axp_total$tweet_diff,type = "correlation",na.action = na.pass)

#bin calculation as per sturges rule
log2(length(axp_total$volume_diff))

res=NULL
for (j in 1:10)
{
  #MUTUAL INFORMATION CALCULATION (stock volume vs tweet volume)
  tdiff=axp_total$tweet_diff[j:(length(axp_total$tweet_diff))]
  vdiff=axp_total$volume_diff[1:(length(axp_total$volume_diff)-(j-1))]
  y2d = discretize2d(vdiff,tdiff, numBins1=11, numBins2=11)
  res = c(res,mi.empirical(y2d))
}
result_frame = c('DIS','Stock Volume','All Tweet Volume',res)

FF <- as.matrix(t(result_frame))
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldata/MSresult.csv", sep = ",", 
            col.names = FALSE, append=TRUE)



#difference in sentimentful tweets
axp_total$total_sentiment = axp_total$BULL_SCORED_MESSAGES + axp_total$BEAR_SCORED_MESSAGES
sentiment_diff = c(diff((axp_total$total_sentiment)),1)
sentiment_diff[is.na(sentiment_diff)] = 0
axp_total$sentiment_diff = -(sentiment_diff)

#test for autocorrelation
acf(axp_total$sentiment_diff,type = "correlation",na.action = na.pass)


res=NULL
#j=1
for (j in 1:10)
{
  #MUTUAL INFORMATION CALCULATION (stock volume vs sentiment volume)
  sdiff=axp_total$sentiment_diff[j:(length(axp_total$sentiment_diff))]
  vdiff=axp_total$volume_diff[1:(length(axp_total$volume_diff)-(j-1))]
  y2d = discretize2d(sdiff,vdiff, numBins1=11, numBins2=11)
  mi.empirical(y2d)
  res = c(res,mi.empirical(y2d))
}

result_frame = c('DIS','Stock Volume','Sentiment Tweet Volume',res)

FF <- as.matrix(t(result_frame))
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldata/MSresult.csv", sep = ",", 
            col.names = FALSE, append=TRUE)

#difference in bullish intensity
bull_int_diff = c(diff((axp_total$BULLISH_INTENSITY)),1)
bull_int_diff[is.na(bull_int_diff)] = 0
axp_total$bull_int_diff = -(bull_int_diff)

#test for autocorrelation
acf(axp_total$bull_int_diff,type = "correlation",na.action = na.pass)

res=NULL
for (j in 1:10)
{
  #MUTUAL INFORMATION CALCULATION (stock volume vs bullish intensity)
  bdiff=axp_total$bull_int_diff[j:(length(axp_total$bull_int_diff))]
  vdiff=axp_total$volume_diff[1:(length(axp_total$volume_diff)-(j-1))]
  y2d = discretize2d(vdiff,bdiff, numBins1=11, numBins2=11)
  mi.empirical(y2d)
  res = c(res,mi.empirical(y2d))
}
result_frame = c('DIS','Stock Volume','Bullish Intensity',res)

FF <- as.matrix(t(result_frame))
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldata/MSresult.csv", sep = ",", 
            col.names = FALSE, append=TRUE)

#difference in bearish intensity
bear_int_diff = c(diff((axp_total$BEARISH_INTENSITY)),1)
bear_int_diff[is.na(bear_int_diff)] = 0
axp_total$bear_int_diff = -(bear_int_diff)

#test for autocorrelation
acf(axp_total$bear_int_diff,type = "correlation",na.action = na.pass)

res=NULL
for (j in 1:10)
{
  #MUTUAL INFORMATION CALCULATION (stock volume vs bearish intensity)
  bdiff=axp_total$bear_int_diff[j:(length(axp_total$bear_int_diff))]
  vdiff=axp_total$volume_diff[1:(length(axp_total$volume_diff)-(j-1))]
  y2d = discretize2d(vdiff,bdiff, numBins1=11, numBins2=11)
  mi.empirical(y2d)
  res = c(res,mi.empirical(y2d))
}

result_frame = c('DIS','Stock Volume','Bearish Intensity',res)

FF <- as.matrix(t(result_frame))
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldata/MSresult.csv", sep = ",", 
            col.names = FALSE, append=TRUE)




#difference in bullish volume
bull_vol_diff = c(diff((axp_total$BULL_SCORED_MESSAGES)),1)
bull_vol_diff[is.na(bull_vol_diff)] = 0
axp_total$bull_vol_diff = -(bull_vol_diff)

#test for autocorrelation
acf(axp_total$bull_vol_diff,type = "correlation",na.action = na.pass)


res=NULL
for (j in 1:10)
{
  
  
  #MUTUAL INFORMATION CALCULATION (stock volume vs bullish volume)
  bdiff=axp_total$bull_vol_diff[j:(length(axp_total$bull_vol_diff))]
  vdiff=axp_total$volume_diff[1:(length(axp_total$volume_diff)-(j-1))]
  y2d = discretize2d(vdiff,bdiff, numBins1=11, numBins2=11)
  mi.empirical(y2d)
  res = c(res,mi.empirical(y2d))
}

result_frame = c('DIS','Stock Volume','Bullish Volume',res)

FF <- as.matrix(t(result_frame))
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldata/MSresult.csv", sep = ",", 
            col.names = FALSE, append=TRUE)




#difference in bearish volume
bear_vol_diff = c(diff((axp_total$BEAR_SCORED_MESSAGES)),1)
bear_vol_diff[is.na(bear_vol_diff)] = 0
axp_total$bear_vol_diff = -(bear_vol_diff)

#test for autocorrelation
acf(axp_total$bear_vol_diff,type = "correlation",na.action = na.pass)


res=NULL
for (j in 1:10)
{
  
  #MUTUAL INFORMATION CALCULATION (stock volume vs bearish volume)
  bdiff=axp_total$bear_vol_diff[j:(length(axp_total$bear_vol_diff))]
  vdiff=axp_total$volume_diff[1:(length(axp_total$volume_diff)-(j-1))]
  y2d = discretize2d(vdiff,bdiff, numBins1=11, numBins2=11)
  mi.empirical(y2d)
  res = c(res,mi.empirical(y2d))
}

result_frame = c('DIS','Stock Volume','Bearish Volume',res)

FF <- as.matrix(t(result_frame))
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldata/MSresult.csv", sep = ",", 
            col.names = FALSE, append=TRUE)




#
# ANALYSIS FOR STOCK RETURNS
#

#difference in returns
return_diff = c(diff((axp_total$Adj.Close)),1)
return_diff[is.na(return_diff)] = 0
axp_total$return_diff = -(return_diff)

#test for autocorrelation
acf(axp_total$return_diff,type = "correlation",na.action = na.pass)

#difference in tweets
tweet_diff = c(diff((axp_total$TOTAL_SCANNED_MESSAGES)),1)
tweet_diff[is.na(tweet_diff)] = 0
axp_total$tweet_diff = -(tweet_diff)

#test for autocorrelation
acf(axp_total$tweet_diff,type = "correlation",na.action = na.pass)

#bin calculation as per sturges rule
log2(length(axp_total$return_diff))


res=NULL
for (j in 1:10)
{
  
  #MUTUAL INFORMATION CALCULATION (stock retuen vs tweet volume)
  tdiff=axp_total$tweet_diff[j:(length(axp_total$tweet_diff))]
  rdiff=axp_total$return_diff[1:(length(axp_total$return_diff)-(j-1))]
  y2d = discretize2d(rdiff,tdiff, numBins1=11, numBins2=11)
  mi.empirical(y2d)
  
  res = c(res,mi.empirical(y2d))
}

result_frame = c('DIS','Stock Return','All Tweet Volume',res)

FF <- as.matrix(t(result_frame))
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldata/MSresult.csv", sep = ",", 
            col.names = FALSE, append=TRUE)


#difference in sentimentful tweets
axp_total$total_sentiment = axp_total$BULL_SCORED_MESSAGES + axp_total$BEAR_SCORED_MESSAGES
sentiment_diff = c(diff((axp_total$total_sentiment)),1)
sentiment_diff[is.na(sentiment_diff)] = 0
axp_total$sentiment_diff = -(sentiment_diff)

#test for autocorrelation
acf(axp_total$sentiment_diff,type = "correlation",na.action = na.pass)


res=NULL
for (j in 1:10)
{
  
  #MUTUAL INFORMATION CALCULATION (stock volume vs sentiment volume)
  sdiff=axp_total$sentiment_diff[j:(length(axp_total$sentiment_diff))]
  rdiff=axp_total$return_diff[1:(length(axp_total$return_diff)-(j-1))]
  y2d = discretize2d(sdiff,rdiff, numBins1=11, numBins2=11)
  mi.empirical(y2d)
  res = c(res,mi.empirical(y2d))
}


result_frame = c('DIS','Stock Return','Sentiment Tweet Volume',res)

FF <- as.matrix(t(result_frame))
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldata/MSresult.csv", sep = ",", 
            col.names = FALSE, append=TRUE)




#difference in bullish intensity
bull_int_diff = c(diff((axp_total$BULLISH_INTENSITY)),1)
bull_int_diff[is.na(bull_int_diff)] = 0
axp_total$bull_int_diff = -(bull_int_diff)

#test for autocorrelation
acf(axp_total$bull_int_diff,type = "correlation",na.action = na.pass)


res=NULL
for (j in 1:10)
{
  
  #MUTUAL INFORMATION CALCULATION (stock volume vs bullish intensity)
  bdiff=axp_total$bull_int_diff[j:(length(axp_total$bull_int_diff))]
  rdiff=axp_total$return_diff[1:(length(axp_total$return_diff)-(j-1))]
  y2d = discretize2d(rdiff,bdiff, numBins1=11, numBins2=11)
  mi.empirical(y2d)
  res = c(res,mi.empirical(y2d))
}

result_frame = c('DIS','Stock Return','Bullish Intensity',res)

FF <- as.matrix(t(result_frame))
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldata/MSresult.csv", sep = ",", 
            col.names = FALSE, append=TRUE)



#difference in bearish intensity
bear_int_diff = c(diff((axp_total$BEARISH_INTENSITY)),1)
bear_int_diff[is.na(bear_int_diff)] = 0
axp_total$bear_int_diff = -(bear_int_diff)

#test for autocorrelation
acf(axp_total$bear_int_diff,type = "correlation",na.action = na.pass)

res=NULL
for (j in 1:10)
{
  #MUTUAL INFORMATION CALCULATION (stock volume vs bearish intensity)
  bdiff=axp_total$bear_int_diff[j:(length(axp_total$bear_int_diff))]
  rdiff=axp_total$return_diff[1:(length(axp_total$return_diff)-(j-1))]
  y2d = discretize2d(rdiff,bdiff, numBins1=11, numBins2=11)
  mi.empirical(y2d)
  res = c(res,mi.empirical(y2d))
}

result_frame = c('DIS','Stock Return','Bearish Intensity',res)

FF <- as.matrix(t(result_frame))
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldata/MSresult.csv", sep = ",", 
            col.names = FALSE, append=TRUE)

#difference in bullish volume
bull_vol_diff = c(diff((axp_total$BULL_SCORED_MESSAGES)),1)
bull_vol_diff[is.na(bull_vol_diff)] = 0
axp_total$bull_vol_diff = -(bull_vol_diff)

#test for autocorrelation
acf(axp_total$bull_vol_diff,type = "correlation",na.action = na.pass)


res=NULL
for (j in 1:10)
{
  #MUTUAL INFORMATION CALCULATION (stock volume vs bullish volume)
  bdiff=axp_total$bull_vol_diff[j:(length(axp_total$bull_vol_diff))]
  rdiff=axp_total$return_diff[1:(length(axp_total$return_diff)-(j-1))]
  y2d = discretize2d(rdiff,bdiff, numBins1=11, numBins2=11)
  mi.empirical(y2d)
  res = c(res,mi.empirical(y2d))
}

result_frame = c('DIS','Stock Return','Bullish Volume',res)

FF <- as.matrix(t(result_frame))
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldata/MSresult.csv", sep = ",", 
            col.names = FALSE, append=TRUE)

#difference in bearish volume
bear_vol_diff = c(diff((axp_total$BEAR_SCORED_MESSAGES)),1)
bear_vol_diff[is.na(bear_vol_diff)] = 0
axp_total$bear_vol_diff = -(bear_vol_diff)

#test for autocorrelation
acf(axp_total$bear_vol_diff,type = "correlation",na.action = na.pass)


res=NULL
for (j in 1:10)
{
  #MUTUAL INFORMATION CALCULATION (stock volume vs bearish volume)
  bdiff=axp_total$bear_vol_diff[j:(length(axp_total$bear_vol_diff))]
  rdiff=axp_total$return_diff[1:(length(axp_total$return_diff)-(j-1))]
  y2d = discretize2d(rdiff,bdiff, numBins1=11, numBins2=11)
  mi.empirical(y2d)
  
  res = c(res,mi.empirical(y2d))
}

result_frame = c('DIS','Stock Return','Bearish Volume',res)

FF <- as.matrix(t(result_frame))
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldata/MSresult.csv", sep = ",", 
            col.names = FALSE, append=TRUE)