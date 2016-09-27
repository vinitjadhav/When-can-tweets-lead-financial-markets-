#read PCA data
pca_data = read.csv("C:/Users/hp-pc/Documents/R/alldatanew/MSResult.csv")

sector = read.csv("C:/Users/hp-pc/Documents/R/alldatanew/Industry.csv")

##
## All tweet volume(senti_param=1) vs stock volume
##

pca_all_tweet = sqldf("select * from pca_data where senti_param = 1")

## all features values for 100 stocks
pca_all_tweet_df = sqldf("select optimal_lag,
                           avg_daily_tweet,
                           daily_tweet_var,
                           no_days_tweet,
                           avg_bull_tweet,
                           var_bull_tweet,
                           avg_bear_tweet,
                           var_bear_tweet,
                           per_bull,
                           per_bear,
                           per_senti from pca_all_tweet")

## write result to csv for cluster analysis
FF <- as.matrix(pca_all_tweet_df)
write.table(FF, file = "C:/Users/hp-pc/Documents/MATLAB/alltweetfeat.csv", sep = ",", 
            col.names = FALSE, append=FALSE)

# check correlation between all features
# pairs(pca_all_tweet_df)

## perform PCA 
pca = princomp(pca_all_tweet_df, scores = TRUE, cor=TRUE)
summary(pca)
plot(pca)

## imp components after PCA
plot(pca,type='l')

## get PC values
pca_vec = prcomp(pca_all_tweet_df)
comp = data.frame(pca_vec$x[,1:3])
comp_log = log(abs(comp))


## perform k means across all feature data
k = kmeans(pca_all_tweet_df, 7, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

## plot clustering
plot3d(comp_log$PC1,comp_log$PC2,comp_log$PC3,col=k$clust)

## add cluster and sector to orginal dataframe
pca_all_tweet$cluster = k$clust
pca_all_tweet$sector = sector$sector


## result table showing stock, PC values, sector and maxmi for plotting 
result = sqldf("select ticker,max_mi from pca_all_tweet")
result$PC1 = comp_log$PC1
result$PC2 = comp_log$PC2
result$PC3 = comp_log$PC3
result$sector = sector$sector
result$cluster = k$cluster

## write result to csv for plotting in Matlab
FF <- as.matrix(result)
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldatanew/finalresult/alltweetpc.csv", sep = ",", 
            col.names = FALSE, append=FALSE)

## result grouped by clusters for analysis
result_clusterwise = sqldf("select cluster,avg(max_mi),count(1),
                            avg(avg_daily_tweet),
                            avg(optimal_lag),
                            avg(no_days_tweet),
                            avg(avg_bull_tweet),
                            avg(avg_bear_tweet),
                            avg(per_bull),
                            avg(per_bear),
                            avg(per_senti)
                            from pca_all_tweet group by cluster")


## write result to csv for cluster analysis
FF <- as.matrix(result_clusterwise)
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldatanew/finalresult/alltweetcluster.csv", sep = ",", 
            col.names = FALSE, append=FALSE)

## result grouped by sector for analysis
result_sectorwise = sqldf("select sector,avg(max_mi),count(1),
                           avg(avg_daily_tweet),
                           avg(optimal_lag),
                           avg(no_days_tweet),
                           avg(avg_bull_tweet),
                           avg(avg_bear_tweet),
                           avg(per_bull),
                           avg(per_bear),
                           avg(per_senti)
                           from pca_all_tweet group by sector")

## write result to csv for sector analysis
FF <- as.matrix(result_sectorwise)
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldatanew/finalresult/alltweetsector.csv", sep = ",", 
            col.names = FALSE, append=FALSE)





##
## Senti tweet volume(senti_param=2) vs stock volume
##

pca_all_tweet = sqldf("select * from pca_data where senti_param = 2")

## all features values for 100 stocks
pca_all_tweet_df = sqldf("select optimal_lag,
                         avg_daily_tweet,
                         daily_tweet_var,
                         no_days_tweet,
                         avg_bull_tweet,
                         var_bull_tweet,
                         avg_bear_tweet,
                         var_bear_tweet,
                         per_bull,
                         per_bear,
                         per_senti from pca_all_tweet")

## write result to csv for cluster analysis
FF <- as.matrix(pca_all_tweet_df)
write.table(FF, file = "C:/Users/hp-pc/Documents/MATLAB/sentitweetfeat.csv", sep = ",", 
            col.names = FALSE, append=FALSE)

# check correlation between all features
# pairs(pca_all_tweet_df)

## perform PCA 
pca = princomp(pca_all_tweet_df, scores = TRUE, cor=TRUE)
summary(pca)
plot(pca)

## imp components after PCA
plot(pca,type='l')

## get PC values
pca_vec = prcomp(pca_all_tweet_df)
comp = data.frame(pca_vec$x[,1:3])
comp_log = log(abs(comp))


## perform k means across all feature data
k = kmeans(pca_all_tweet_df, 7, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

## plot clustering
plot3d(comp_log$PC1,comp_log$PC2,comp_log$PC3,col=k$clust)

## add cluster and sector to orginal dataframe
pca_all_tweet$cluster = k$clust
pca_all_tweet$sector = sector$sector


## result table showing stock, PC values, sector and maxmi for plotting 
result = sqldf("select ticker,max_mi from pca_all_tweet")
result$PC1 = comp_log$PC1
result$PC2 = comp_log$PC2
result$PC3 = comp_log$PC3
result$sector = sector$sector
result$cluster = k$cluster

## write result to csv for plotting in Matlab
FF <- as.matrix(result)
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldatanew/finalresult/sentitweetpc.csv", sep = ",", 
            col.names = FALSE, append=FALSE)

## result grouped by clusters for analysis
result_clusterwise = sqldf("select cluster,avg(max_mi),count(1),
                           avg(avg_daily_tweet),
                           avg(optimal_lag),
                           avg(no_days_tweet),
                           avg(avg_bull_tweet),
                           avg(avg_bear_tweet),
                           avg(per_bull),
                           avg(per_bear),
                           avg(per_senti)
                           from pca_all_tweet group by cluster")


## write result to csv for cluster analysis
FF <- as.matrix(result_clusterwise)
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldatanew/finalresult/sentitweetcluster.csv", sep = ",", 
            col.names = FALSE, append=FALSE)

## result grouped by sector for analysis
result_sectorwise = sqldf("select sector,avg(max_mi),count(1),
                          avg(avg_daily_tweet),
                          avg(optimal_lag),
                          avg(no_days_tweet),
                          avg(avg_bull_tweet),
                          avg(avg_bear_tweet),
                          avg(per_bull),
                          avg(per_bear),
                          avg(per_senti)
                          from pca_all_tweet group by sector")

## write result to csv for sector analysis
FF <- as.matrix(result_sectorwise)
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldatanew/finalresult/sentitweetsector.csv", sep = ",", 
            col.names = FALSE, append=FALSE)




##
## Bull tweet volume(senti_param=3) vs stock volume
##

pca_all_tweet = sqldf("select * from pca_data where senti_param = 3")

## all features values for 100 stocks
pca_all_tweet_df = sqldf("select optimal_lag,
                         avg_daily_tweet,
                         daily_tweet_var,
                         no_days_tweet,
                         avg_bull_tweet,
                         var_bull_tweet,
                         avg_bear_tweet,
                         var_bear_tweet,
                         per_bull,
                         per_bear,
                         per_senti from pca_all_tweet")

## write result to csv for cluster analysis
FF <- as.matrix(pca_all_tweet_df)
write.table(FF, file = "C:/Users/hp-pc/Documents/MATLAB/bulltweetfeat.csv", sep = ",", 
            col.names = FALSE, append=FALSE)

# check correlation between all features
# pairs(pca_all_tweet_df)

## perform PCA 
pca = princomp(pca_all_tweet_df, scores = TRUE, cor=TRUE)
summary(pca)
plot(pca)

## imp components after PCA
plot(pca,type='l')

## get PC values
pca_vec = prcomp(pca_all_tweet_df)
comp = data.frame(pca_vec$x[,1:3])
comp_log = log(abs(comp))


## perform k means across all feature data
k = kmeans(pca_all_tweet_df, 7, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

## plot clustering
plot3d(comp_log$PC1,comp_log$PC2,comp_log$PC3,col=k$clust)

## add cluster and sector to orginal dataframe
pca_all_tweet$cluster = k$clust
pca_all_tweet$sector = sector$sector


## result table showing stock, PC values, sector and maxmi for plotting 
result = sqldf("select ticker,max_mi from pca_all_tweet")
result$PC1 = comp_log$PC1
result$PC2 = comp_log$PC2
result$PC3 = comp_log$PC3
result$sector = sector$sector
result$cluster = k$cluster

## write result to csv for plotting in Matlab
FF <- as.matrix(result)
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldatanew/finalresult/bulltweetpc.csv", sep = ",", 
            col.names = FALSE, append=FALSE)

## result grouped by clusters for analysis
result_clusterwise = sqldf("select cluster,avg(max_mi),count(1),
                           avg(avg_daily_tweet),
                           avg(optimal_lag),
                           avg(no_days_tweet),
                           avg(avg_bull_tweet),
                           avg(avg_bear_tweet),
                           avg(per_bull),
                           avg(per_bear),
                           avg(per_senti)
                           from pca_all_tweet group by cluster")


## write result to csv for cluster analysis
FF <- as.matrix(result_clusterwise)
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldatanew/finalresult/bulltweetcluster.csv", sep = ",", 
            col.names = FALSE, append=FALSE)

## result grouped by sector for analysis
result_sectorwise = sqldf("select sector,avg(max_mi),count(1),
                          avg(avg_daily_tweet),
                          avg(optimal_lag),
                          avg(no_days_tweet),
                          avg(avg_bull_tweet),
                          avg(avg_bear_tweet),
                          avg(per_bull),
                          avg(per_bear),
                          avg(per_senti)
                          from pca_all_tweet group by sector")

## write result to csv for sector analysis
FF <- as.matrix(result_sectorwise)
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldatanew/finalresult/bulltweetsector.csv", sep = ",", 
            col.names = FALSE, append=FALSE)



##
## Bear tweet volume(senti_param=4) vs stock volume
##

pca_all_tweet = sqldf("select * from pca_data where senti_param = 4")

## all features values for 100 stocks
pca_all_tweet_df = sqldf("select optimal_lag,
                         avg_daily_tweet,
                         daily_tweet_var,
                         no_days_tweet,
                         avg_bull_tweet,
                         var_bull_tweet,
                         avg_bear_tweet,
                         var_bear_tweet,
                         per_bull,
                         per_bear,
                         per_senti from pca_all_tweet")

## write result to csv for cluster analysis
FF <- as.matrix(pca_all_tweet_df)
write.table(FF, file = "C:/Users/hp-pc/Documents/MATLAB/beartweetfeat.csv", sep = ",", 
            col.names = FALSE, append=FALSE)

# check correlation between all features
# pairs(pca_all_tweet_df)

## perform PCA 
pca = princomp(pca_all_tweet_df, scores = TRUE, cor=TRUE)
summary(pca)
plot(pca)

## imp components after PCA
plot(pca,type='l')

## get PC values
pca_vec = prcomp(pca_all_tweet_df)
comp = data.frame(pca_vec$x[,1:3])
comp_log = log(abs(comp))


## perform k means across all feature data
k = kmeans(pca_all_tweet_df, 7, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

## plot clustering
plot3d(comp_log$PC1,comp_log$PC2,comp_log$PC3,col=k$clust)

## add cluster and sector to orginal dataframe
pca_all_tweet$cluster = k$clust
pca_all_tweet$sector = sector$sector


## result table showing stock, PC values, sector and maxmi for plotting 
result = sqldf("select ticker,max_mi from pca_all_tweet")
result$PC1 = comp_log$PC1
result$PC2 = comp_log$PC2
result$PC3 = comp_log$PC3
result$sector = sector$sector
result$cluster = k$cluster

## write result to csv for plotting in Matlab
FF <- as.matrix(result)
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldatanew/finalresult/beartweetpc.csv", sep = ",", 
            col.names = FALSE, append=FALSE)

## result grouped by clusters for analysis
result_clusterwise = sqldf("select cluster,avg(max_mi),count(1),
                           avg(avg_daily_tweet),
                           avg(optimal_lag),
                           avg(no_days_tweet),
                           avg(avg_bull_tweet),
                           avg(avg_bear_tweet),
                           avg(per_bull),
                           avg(per_bear),
                           avg(per_senti)
                           from pca_all_tweet group by cluster")


## write result to csv for cluster analysis
FF <- as.matrix(result_clusterwise)
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldatanew/finalresult/beartweetcluster.csv", sep = ",", 
            col.names = FALSE, append=FALSE)

## result grouped by sector for analysis
result_sectorwise = sqldf("select sector,avg(max_mi),count(1),
                          avg(avg_daily_tweet),
                          avg(optimal_lag),
                          avg(no_days_tweet),
                          avg(avg_bull_tweet),
                          avg(avg_bear_tweet),
                          avg(per_bull),
                          avg(per_bear),
                          avg(per_senti)
                          from pca_all_tweet group by sector")

## write result to csv for sector analysis
FF <- as.matrix(result_sectorwise)
write.table(FF, file = "C:/Users/hp-pc/Documents/R/alldatanew/finalresult/beartweetsector.csv", sep = ",", 
            col.names = FALSE, append=FALSE)