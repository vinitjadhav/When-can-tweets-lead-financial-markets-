
###
###     LOAD ALL SENTIMENT FOR DJIA STOCKS
###


#read all tweet data
#tweets = read.csv("C:/Users/hp-pc/Documents/R/alldatanew/alltweets.csv")

### AXP WORKSPACE  
###
### READ SENTIMENT DATA FOR AXP

#qry = sprintf("select * from tweets where symbol = '%s'", com)
#axp_sent_all = sqldf(qry)

stock_name = 'ZNGA'
axp_sent_all = sqldf("select * from tweets where symbol = 'ZNGA'")           #####change stock symbol

#get stock data for AXP
axp_stock_all = read.csv("C:/Users/hp-pc/Documents/R/alldatanew/znga.csv")      #####change stock symbol

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
# GET FEATURES ABOUT STOCK.
#
average_daily_Volume = mean(axp_total$Volume)
variance_volume = var(axp_total$Volume)

average_tweet = mean(na.omit(axp_total$TOTAL_SCANNED_MESSAGES))
var_tweet = var(na.omit(axp_total$TOTAL_SCANNED_MESSAGES))
no_day_tweet = length(na.omit(axp_total$TOTAL_SCANNED_MESSAGES))

average_tweet_bull = mean(na.omit(axp_total$BULL_SCORED_MESSAGES))
var_tweet_bull = var(na.omit(axp_total$BULL_SCORED_MESSAGES))

average_tweet_bear = mean(na.omit(axp_total$BEAR_SCORED_MESSAGES))
var_tweet_bear = var(na.omit(axp_total$BEAR_SCORED_MESSAGES))

bull_of_total = average_tweet_bull/(average_tweet_bear+average_tweet_bull)
bear_of_total = average_tweet_bear/(average_tweet_bear+average_tweet_bull)

sent_of_tweet = (average_tweet_bear+average_tweet_bull)/average_tweet

res_PCA=c(average_daily_Volume,variance_volume,average_tweet,var_tweet,no_day_tweet,
          average_tweet_bull,var_tweet_bull,average_tweet_bear,var_tweet_bear,
          bull_of_total,bear_of_total,sent_of_tweet)

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

# total tweet volume - 1
res=NULL
max_MI=0
opt_lag=-1
for (j in 1:10)
{
  #MUTUAL INFORMATION CALCULATION (stock volume vs tweet volume)
  tdiff=axp_total$tweet_diff[j:(length(axp_total$tweet_diff))]
  vdiff=axp_total$volume_diff[1:(length(axp_total$volume_diff)-(j-1))]
  y2d = discretize2d(vdiff,tdiff, numBins1=11, numBins2=11)
  if(mi.empirical(y2d)>=max_MI)
  {
    res = c(stock_name,1,j-1,mi.empirical(y2d),res_PCA)                          #####change stock symbol
    max_MI = mi.empirical(y2d)
  }
}
result_frame = res

FF <- as.matrix(t(result_frame))
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldatanew/MSresult.csv", sep = ",", 
            col.names = FALSE, append=TRUE)



#difference in sentimentful tweets
axp_total$total_sentiment = axp_total$BULL_SCORED_MESSAGES + axp_total$BEAR_SCORED_MESSAGES
sentiment_diff = c(diff((axp_total$total_sentiment)),1)
sentiment_diff[is.na(sentiment_diff)] = 0
axp_total$sentiment_diff = -(sentiment_diff)

#test for autocorrelation
acf(axp_total$sentiment_diff,type = "correlation",na.action = na.pass)


#total tweet with sentiment volume - 2
res=NULL
max_MI=0
opt_lag=-1
for (j in 1:10)
{
  #MUTUAL INFORMATION CALCULATION (stock volume vs sentiment volume)
  sdiff=axp_total$sentiment_diff[j:(length(axp_total$sentiment_diff))]
  vdiff=axp_total$volume_diff[1:(length(axp_total$volume_diff)-(j-1))]
  y2d = discretize2d(sdiff,vdiff, numBins1=11, numBins2=11)
  if(mi.empirical(y2d)>=max_MI)
  {
    res = c(stock_name,2,j-1,mi.empirical(y2d),res_PCA)                        #####change stock symbol
    max_MI = mi.empirical(y2d)
  }
}
result_frame = res

FF <- as.matrix(t(result_frame))
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldatanew/MSresult.csv", sep = ",", 
            col.names = FALSE, append=TRUE)


#difference in bullish volume
bull_vol_diff = c(diff((axp_total$BULL_SCORED_MESSAGES)),1)
bull_vol_diff[is.na(bull_vol_diff)] = 0
axp_total$bull_vol_diff = -(bull_vol_diff)

#test for autocorrelation
acf(axp_total$bull_vol_diff,type = "correlation",na.action = na.pass)


#total bullish volume - 3
res=NULL
max_MI=0
opt_lag=-1
for (j in 1:10)
{
  
  
  #MUTUAL INFORMATION CALCULATION (stock volume vs bullish volume)
  bdiff=axp_total$bull_vol_diff[j:(length(axp_total$bull_vol_diff))]
  vdiff=axp_total$volume_diff[1:(length(axp_total$volume_diff)-(j-1))]
  y2d = discretize2d(vdiff,bdiff, numBins1=11, numBins2=11)
  if(mi.empirical(y2d)>=max_MI)
  {
    res = c(stock_name,3,j-1,mi.empirical(y2d),res_PCA)                          #####change stock symbol
    max_MI = mi.empirical(y2d)
  }
}
result_frame = res

FF <- as.matrix(t(result_frame))
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldatanew/MSresult.csv", sep = ",", 
            col.names = FALSE, append=TRUE)




#difference in bearish volume
bear_vol_diff = c(diff((axp_total$BEAR_SCORED_MESSAGES)),1)
bear_vol_diff[is.na(bear_vol_diff)] = 0
axp_total$bear_vol_diff = -(bear_vol_diff)

#test for autocorrelation
acf(axp_total$bear_vol_diff,type = "correlation",na.action = na.pass)


#total bearish volume - 4
res=NULL
max_MI=0
opt_lag=-1
for (j in 1:10)
{
  
  #MUTUAL INFORMATION CALCULATION (stock volume vs bearish volume)
  bdiff=axp_total$bear_vol_diff[j:(length(axp_total$bear_vol_diff))]
  vdiff=axp_total$volume_diff[1:(length(axp_total$volume_diff)-(j-1))]
  y2d = discretize2d(vdiff,bdiff, numBins1=11, numBins2=11)
  if(mi.empirical(y2d)>=max_MI)
  {
    res = c(stock_name,4,j-1,mi.empirical(y2d),res_PCA)                          #####change stock symbol
    max_MI = mi.empirical(y2d)
  }
}
result_frame = res

FF <- as.matrix(t(result_frame))
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldatanew/MSresult.csv", sep = ",", 
            col.names = FALSE, append=TRUE)

print('end')

