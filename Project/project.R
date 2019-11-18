library(tidyverse)
library(ggplot2)
library(factoextra)
library(dplyr)

# data %>%
#   filter(is_weekend == 1) %>%
#   summarise(lifestyle = sum(data_channel_is_lifestyle),
#             entertainment = sum(data_channel_is_entertainment),
#             tech=sum(data_channel_is_tech),
#             world=sum(data_channel_is_world),
#             socmed=sum(data_channel_is_socmed),
#             business=sum(data_channel_is_bus))


get_tidy_data <- function(df) {
  # extracting date and title from url string
  clustering_df <- df %>% 
    mutate(date = as.Date(sub(".*?\\b(\\d{4}/\\d{1,2}/\\d{1,2})\\b.*", "\\1", url)),
           article_title = word(url, -2, sep = '/'))
  
  # gather article categories and weekdays
  other_df <- clustering_df %>% gather(key='category', value='is_category',
                            data_channel_is_lifestyle, data_channel_is_entertainment,
                            data_channel_is_bus, data_channel_is_socmed, data_channel_is_tech,
                            data_channel_is_world) %>%
    filter(is_category == 1) %>%
    select(-is_category) %>%
    mutate(category = case_when(category == 'data_channel_is_lifestyle' ~ 'Lifestyle',
                                category == 'data_channel_is_entertainment' ~ 'Entertainment',
                                category == 'data_channel_is_bus' ~ 'Business',
                                category == 'data_channel_is_socmed' ~ 'Social Media',
                                category == 'data_channel_is_tech' ~ 'Technology',
                                category == 'data_channel_is_world' ~ 'World')) %>%
    gather(key='day', value='what_day',
           weekday_is_monday, weekday_is_tuesday,
           weekday_is_wednesday, weekday_is_thursday, weekday_is_friday,
           weekday_is_saturday, weekday_is_sunday) %>%
    filter(what_day == 1) %>%
    select(-what_day) %>%
    mutate(day = case_when(day == 'weekday_is_monday' ~ 'Monday',
                           day == 'weekday_is_tuesday' ~ 'Tuesday',
                           day == 'weekday_is_wednesday' ~ 'Wednesday',
                           day == 'weekday_is_thursday' ~ 'Thursday',
                           day == 'weekday_is_friday' ~ 'Friday',
                           day == 'weekday_is_saturday' ~ 'Saturday',
                           day == 'weekday_is_sunday' ~ 'Sunday')) %>%
    arrange(url)
  
  clustering_df <- clustering_df %>% filter(url %in% other_df$url )
  
  numericDF<- other_df %>%
    select(-url,-article_title,-date,-timedelta,-is_weekend, -day, -category)
  
  corrMatrix <- as_tibble(cor(numericDF, method = c("pearson", "kendall", "spearman")))
  rownames(corrMatrix) <- colnames(corrMatrix)
  
  return(list(clustering_df, other_df,numericDF,corrMatrix))
}


data = read_csv("/Users/sagarsingh/Downloads/OnlineNewsPopularity/OnlineNewsPopularity.csv")

listOfDFs= get_tidy_data(data)

clustering_df <- listOfDFs[[1]]
other_df <- listOfDFs[[2]]
numericDF <- listOfDFs[[3]]

corrMatrix <- listOfDFs[[4]]

summary(numericDF)

#count(other_df %>% group_by(url) %>% summarise(n = sum(is_category)) %>% filter(n==0))


#Principle Component Analysis


prcompResult <- prcomp(numericDF,scale.=TRUE)
summary(prcompResult)
PCA_results <- prcompResult[[2]]
fviz_eig(prcompResult)
plot(prcompResult, type="l")

#PC25 explains 91% variance in the data

#Important Features by PC components
# PC1 = global_sentiment_polarity, rate_positive_words
# PC2 = min_negative_polarity, global_rate_negative_words
# PC3 = kw_avg_avg, 
# PC4 = n_non_stop_words,n_non_stop_unique_tokens,n_unique_tokens,
# PC5 = kw_avg_min, kw_max_min,kw_avg_max
# PC6 = abs_title_sentiment_polarity, title_subjectivity,self_reference_avg_sharess
# PC7 = self_reference_avg_sharess, self_reference_max_shares, self_reference_min_shares
# PC8 = title_subjectivity, abs_title_sentiment_polarity
# PC9 = kw_min_min, kw_max_max
# PC10 = LDA_03
# PC11 = max_negative_polarity, avg_negative_polarity
# PC12 = LDA_04
# PC13 = LDA_02
# PC14 = LDA_01
# PC15 = LDA_00
# PC16 = shares 
# PC17 = n_tokens_title
# PC18 = num_videos, n_tokens_title
# PC19 = title_sentiment_polarity
# PC20 = num_keywords
# PC21 = title_sentiment_polarity
# PC22 = kw_min_max
# PC23 = num_self_hrefs
# PC24 = abs_title_subjectivity, num_hrefs, num_imgs
# PC25 = kw_min_avg

  
  
importantFeatures <- numericDF %>% select(global_sentiment_polarity, rate_positive_words, min_negative_polarity, 
                                          global_rate_negative_words,
                                          n_unique_tokens, kw_avg_avg, kw_avg_max,
                                          n_non_stop_words,n_non_stop_unique_tokens,
                                          kw_avg_min, kw_max_min,abs_title_sentiment_polarity, 
                                          title_subjectivity, self_reference_avg_sharess, 
                                          self_reference_min_shares,self_reference_max_shares,kw_min_min, kw_max_max, 
                                          LDA_00,LDA_03,
                                          max_negative_polarity, avg_negative_polarity,LDA_04,LDA_02,
                                          LDA_01,num_imgs, num_videos,shares,n_tokens_title,title_sentiment_polarity,
                                          kw_min_max,num_self_hrefs,abs_title_subjectivity, num_hrefs,
                                          kw_min_avg)


summary(importantFeatures)

importantFeatures$shares <- scale(importantFeatures$shares)
importantFeatures$num_hrefs <- scale(importantFeatures$num_hrefs)
importantFeatures$num_imgs <- scale(importantFeatures$num_imgs)
importantFeatures$num_videos <- scale(importantFeatures$num_videos)
importantFeatures$num_self_hrefs <- scale(importantFeatures$num_self_hrefs)

importantFeatures$self_reference_avg_sharess <- scale(importantFeatures$self_reference_avg_sharess)
importantFeatures$self_reference_max_shares <- scale(importantFeatures$self_reference_max_shares)
importantFeatures$self_reference_min_shares <- scale(importantFeatures$self_reference_min_shares)


#Exploratory Clustering Analysis

set.seed(20)
elbowPlot <- function(df,max_iter){
  wss <- data.frame(i = integer(), error = numeric())
  for (i in 2:max_iter){
    km1 <- kmeans(df, centers = i)
    wss <- rbind(wss, data.frame(i,error=sum(km1$withinss)))
  }
  g <- ggplot(data=wss, aes(x=i, y = error))+
    geom_line() + 
    geom_smooth() +
    scale_x_continuous(breaks = c(1:16))
  
  print(g)
  
  return(wss)
}

apply_KMEANS = function(df,k){
  
  set.seed(20)
  dfCluster <- kmeans(df, k, nstart = 20)
  print(dfCluster)
  return(dfCluster) 
}


elbowPlot(importantFeatures,15)

dfCluster1 <- apply_KMEANS(importantFeatures,6)
dfCluster1$size

fviz_cluster(dfCluster1, importantFeatures,  geom = "point")

other_df_withClusterAssignment <- other_df
other_df_withClusterAssignment$cluster1 <- dfCluster1$cluster
other_df_withClusterAssignment$cluster1 <- as.factor(other_df_withClusterAssignment$cluster1)

ggplot(other_df_withClusterAssignment) + 
  geom_point(aes(num_imgs,shares,color=cluster1)) +
  facet_wrap(~category+day)

library(dbscan)

kNNdistplot(scale(numericDF), k=15)

res.db <- dbscan::dbscan(scale(numericDF),  eps = 7, minPts = 15)
res.db
fviz_cluster(res.db, scale(numericDF),  geom = "point")




other_df_withClusterAssignment$cluster2 <- res.db$cluster
other_df_withClusterAssignment$cluster2 <- as.factor(other_df_withClusterAssignment$cluster2)


#If visualization reveals that your data has no amount of separation or distinct groups, 
#then clustering may not be appropriate, which can be seen here




##Omit for now, as just random test runs
#----------------------------------------------------------------------------------------

numericDF_AllScaled <- numericDF %>% select(data_channel_is_bus,data_channel_is_entertainment,data_channel_is_lifestyle,
                               data_channel_is_socmed,data_channel_is_tech,data_channel_is_world,weekday_is_monday,
                               weekday_is_tuesday,weekday_is_wednesday,weekday_is_thursday,weekday_is_friday,
                               weekday_is_saturday,weekday_is_sunday)

nfields <- as_tibble(scale(numericDF %>% select(-data_channel_is_bus,-data_channel_is_entertainment,-data_channel_is_lifestyle,
                                               -data_channel_is_socmed,-data_channel_is_tech,-data_channel_is_world,-weekday_is_monday,
                                               -weekday_is_tuesday,-weekday_is_wednesday,-weekday_is_thursday,-weekday_is_friday,
                                               -weekday_is_saturday,-weekday_is_sunday,shares)))

numericDF_AllScaled <- cbind(numericDF_AllScaled,nfields)

numericDF_ShareScaled <- numericDF                               
numericDF_ShareScaled$shares <- scale(numericDF$shares)

test1 <- numericDF_ShareScaled
elbowPlot(test1,15)
test2 <- numericDF_ShareScaled %>% select(-timedelta)
elbowPlot(test2,15)
test3 <- numericDF_AllScaled
elbowPlot(numericDF_AllScaled,15)

test4 <- numericDF_AllScaled %>% select(num_imgs,num_videos,shares,num_hrefs,num_self_hrefs,num_keywords,shares)
test5 <- numericDF_ShareScaled %>% select(num_imgs,num_videos,shares,num_hrefs,num_self_hrefs,num_keywords,shares)

elbowPlot(test4,15)

test6 <- numericDF_ShareScaled %>% select(n_tokens_title,n_tokens_content,n_unique_tokens,
                              n_non_stop_unique_tokens,num_keywords,num_imgs,num_videos, shares)
elbowPlot(test6,15)

test7 <- numericDF_ShareScaled

other_df_withClusterAssignment <- other_df

dfCluster1 <- apply_KMEANS(test2,8)
dfCluster2 <- apply_KMEANS(test6,6)
dfCluster3 <- apply_KMEANS(test6,8)


library(factoextra)
fviz_cluster(dfCluster1, numericDF_ShareScaled[,c("shares","num_imgs","num_videos")],  geom = "point")
fviz_cluster(dfCluster2, numericDF_ShareScaled,  geom = "point")
fviz_cluster(dfCluster3, numericDF_ShareScaled,  geom = "point")

library(dbscan)


test8 <- numericDF_AllScaled %>% select(timedelta,num_hrefs,num_imgs,num_videos,
                                        data_channel_is_bus,data_channel_is_entertainment,data_channel_is_lifestyle,
                                        data_channel_is_socmed,data_channel_is_tech,data_channel_is_world,weekday_is_monday,
                                        weekday_is_tuesday,weekday_is_wednesday,weekday_is_thursday,weekday_is_friday,
                                        weekday_is_saturday,weekday_is_sunday,global_sentiment_polarity,title_subjectivity,
                                        abs_title_sentiment_polarity,shares)

test9 <- numericDF_AllScaled %>% select(timedelta,num_hrefs,num_imgs,num_videos,
                                               data_channel_is_bus,data_channel_is_entertainment,data_channel_is_lifestyle,
                                               data_channel_is_socmed,data_channel_is_tech,data_channel_is_world,weekday_is_monday,
                                               weekday_is_tuesday,weekday_is_wednesday,weekday_is_thursday,weekday_is_friday,
                                               weekday_is_saturday,weekday_is_sunday,shares)

kNNdistplot(numericDF_AllScaled, k=5)
k = kNNdist(numericDF_AllScaled, k=5)

kNNdistplot(test8, k=5)

kNNdistplot(test9, k=5)

res.db <- dbscan::dbscan(numericDF_AllScaled,  eps = 3, minPts = 5)
res.db
fviz_cluster(res.db, numericDF_AllScaled,  geom = "point")

res.db1 <- dbscan::dbscan(test8,  eps = 3, minPts = 5)
res.db1
fviz_cluster(res.db1, test8,  geom = "point")

res.db2 <- dbscan::dbscan(test9,  eps = 2.8, minPts = 5)
res.db2
fviz_cluster(res.db2, test9,  geom = "point")

elbowPlot(test8,20)
dfCluster4 <- apply_KMEANS(test8,8)

elbowPlot(test9,15)
dfCluster4 <- apply_KMEANS(test9,)

#kNNdistplot(iris.scaled, k=4)
# iris.scaled <- scale(iris[, -5])
#pam.res <- pam(iris.scaled, 3)
#fviz_cluster(pam.res, geom = "point", ellipse.type = "norm")
#hc.cut <- hcut(numericDF_AllScaled, k = 3, hc_method = "complete")
#fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
#fviz_cluster(hc.cut, ellipse.type = "convex")
# elbowPlot(as_tibble(iris.scaled))
# km.res <- kmeans(iris.scaled, 3, nstart = 10)
# fviz_cluster(km.res, iris[, -5], geom = "point")

other_df_withClusterAssignment$cluster1 <- dfCluster1$cluster
other_df_withClusterAssignment$cluster1 <- as.factor(other_df_withClusterAssignment$cluster1)

other_df_withClusterAssignment$cluster2 <- dfCluster2$cluster
other_df_withClusterAssignment$cluster2 <- as.factor(other_df_withClusterAssignment$cluster2)

other_df_withClusterAssignment$cluster2 <- dfCluster2$cluster
other_df_withClusterAssignment$cluster2 <- as.factor(other_df_withClusterAssignment$cluster2)

summary(other_df_withClusterAssignment[other_df_withClusterAssignment$cluster1 == 1,]$global_sentiment_polarity)
summary(other_df_withClusterAssignment[other_df_withClusterAssignment$cluster1 == 2,]$global_sentiment_polarity)
summary(other_df_withClusterAssignment[other_df_withClusterAssignment$cluster1 == 3,]$global_sentiment_polarity)

ggplot(other_df_withClusterAssignment) + 
  geom_point(aes(global_sentiment_polarity,shares, color=cluster2)) 

ggplot(other_df_withClusterAssignment) + 
  geom_point(aes(num_imgs,num_videos, color=day)) +
  facet_grid(~cluster1)

ggplot(other_df_withClusterAssignment) + 
  geom_point(aes(num_videos,shares, color=cluster1)) +
  facet_wrap(~day+category)

ggplot(other_df_withClusterAssignment) + 
  geom_point(aes(num_imgs,num_videos, color=category)) +
  facet_grid(~cluster1)

ggplot(other_df_withClusterAssignment) + 
  geom_point(aes(num_imgs,shares, color=cluster1)) +
  facet_grid(~day)



ggplot(other_df_withClusterAssignment) + 
  geom_point(aes(n_tokens_title,shares)) + 
  facet_grid(~cluster1) 

ggplot(other_df_withClusterAssignment) + 
  geom_point(aes(n_tokens_title,shares)) + 
  facet_grid(~is_weekend)
