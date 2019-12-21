library(purrr)
library(caret)
library(BaylorEdPsych)
library(ggplot2)
library(dplyr)
solar_dataset <- 
  read.table("C:/School_SYR/TERM3/MAR-653/PROJECT_DATASET/deep-solar-dataset/deepsolar_tract.csv",
             header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

solar_dataset <- solar_dataset[complete.cases(solar_dataset),]

solar_dataset$X <- NULL


factor_cols <- c('state', 'county')

solar_kmeans <- solar_dataset[,!(names(solar_dataset) %in% factor_cols)]

solar_kmeans$tile_count <- NULL
solar_kmeans$solar_system_count <- NULL
solar_kmeans$total_panel_area <- NULL
solar_kmeans$number_of_solar_system_per_household <- NULL
solar_kmeans$total_panel_area_residential <- NULL
solar_kmeans$total_panel_area_nonresidential <- NULL
solar_kmeans$fips <- NULL
solar_kmeans$heating_fuel_solar <- NULL
solar_kmeans$solar_panel_area_per_capita <- NULL
solar_kmeans$solar_panel_area_divided_by_area <- NULL
solar_kmeans$tile_count_residential <- NULL
solar_kmeans$tile_count_nonresidential <- NULL
solar_kmeans$solar_system_count_residential <- NULL
solar_kmeans$solar_system_count_nonresidential <- NULL



wss <- function(k) {
  kmeans(solar_kmeans, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


kMeans <- kmeans(solar_kmeans, 4)

kMeans_results <- data.frame(solar_kmeans, kMeans$cluster)

grouped_clusters <- kMeans_results %>% 
  group_by(kMeans.cluster) %>% 
  summarise_all(funs(mean))

kMeans_results %>%
  select(panel, kMeans.cluster) %>%
  group_by(kMeans.cluster) %>%
  summarise_each(funs(sum), panel)

# Create list of customer names for each cluster
#clusterNames <- list()
#clusterList <- list()
#for (clustr in 1:4) {
#  clusterNames[clustr] <- paste0("X", clustr)
#  clusterList[clustr] <- list(
#    names(
#      kMeans_results$kMeans.cluster[kMeans_results$kMeans.cluster == clustr]
#    )
#  )
#}
#names(clusterList) <- clusterNames

#print(clusterList)



solar_dataset$panel <- ifelse((solar_dataset$tile_count > 0), 1, 0)
solar_lm <- solar_dataset
solar_lm$tile_count <- NULL
solar_lm$solar_system_count <- NULL
solar_lm$total_panel_area <- NULL
solar_lm$number_of_solar_system_per_household <- NULL
solar_lm$total_panel_area_residential <- NULL
solar_lm$total_panel_area_nonresidential <- NULL
solar_lm$fips <- NULL
solar_lm$heating_fuel_solar <- NULL
solar_lm$solar_panel_area_per_capita <- NULL
solar_lm$solar_panel_area_divided_by_area <- NULL
solar_lm$tile_count_residential <- NULL
solar_lm$tile_count_nonresidential <- NULL
solar_lm$solar_system_count_residential <- NULL
solar_lm$solar_system_count_nonresidential <- NULL




floor <- createDataPartition(solar_lm$panel, p = 0.7, list = FALSE)

df_train <- solar_lm[floor, ]
df_test <- solar_lm[-floor,]

glm0 <- glm(panel ~ ., family = binomial("logit"), data = df_train)

results_df <-summary.glm(glm0)$coefficients
results_df


PseudoR2(glm0)

solar_lm2 <- solar_lm
solar_lm2$state <- NULL
solar_lm2$county <- NULL

#floor <- createDataPartition(solar_lm2$panel, p = 0.7, list = FALSE)

#df_train <- solar_lm[floor, ]
#df_test <- solar_lm[-floor,]

glm1 <- glm(panel ~ ., family = binomial("logit"), data = solar_lm2)
summary(glm1)
PseudoR2(glm1)


results_df <- as.data.frame(results_df)

results_df$names <- row.names(results_df)

colnames(results_df)[4] <- 'Pvalue'

new_results <- results_df %>%
  dplyr::filter(Estimate > 0) %>%
  dplyr::filter(Pvalue < 0.05)


results_df2 <-summary.glm(glm1)$coefficients

results_df2 <- as.data.frame(results_df2)

results_df2$names <- row.names(results_df2)

colnames(results_df2)[4] <- 'Pvalue'

new_results2 <- results_df2 %>%
  dplyr::filter(Estimate > 0) %>%
  dplyr::filter(Pvalue < 0.05)


#justin's code start

trainset<-solar_lm2
trainset$state<-solar_lm$state
trainset$panel<-as.factor(trainset$panel)
(every4_rows<-seq(1,nrow(trainset),3))
(newtest=trainset[every4_rows, ])
(newtrain=trainset[-every4_rows, ])
#str(newtest)
testsetpanevals<-data.frame(newtest$panel)





library(randomForest)

rfm <- randomForest(panel~., data=newtrain, ntree=10)
panelmemory<-newtest$panel
newtrain<-newtrain[,-153] #take out panel

predRF <- predict(rfm, newtest, type=c("class"))
predRF
predprob <-predict(rfm, newtest, type=("prob"))
predprob

newtest$panel<-testsetpanevals$newtest.panel

newtest$predicted<-predRF
#str(newtest)
denom<-nrow(newtest[newtest$predicted==newtest$panel,])
numerator<-nrow(newtest)
##Accuracy Calculation
denom/numerator
table(newtest$predicted, newtest$panel)

for (i in 1:nrow(newtest)){
  if(as.numeric(newtest$panel[i])==as.numeric(newtest$predicted[i])){newtest$result[i]<-TRUE} else {newtest$result[i]<-FALSE}
}
data.frame(newtest$panel, newtest$predicted, newtest$result)
P <- ggplot(newtest, aes(x=panel, fill=result, color=result)) +
  geom_histogram(binwidth = 1, stat="count" )+ labs(title="Testset vs Predicted Result")
P + theme_bw()


predprob<-data.frame(predprob)
colnames(predprob)<-c('pfalse','ptrue')
newtest$predprob<-predprob$ptrue
newtest
predictedtrueds<-newtest[newtest$predicted==TRUE,]
preictedtrueovereighty<-predictedtrueds[predictedtrueds$predprob>0.79,]


abb2state <- function(name, convert = F, strict = F){
  data(state)
  # state data doesn't include DC
  state = list()
  state[['name']] = c(state.name,"District Of Columbia")
  state[['abb']] = c(state.abb,"DC")
  
  if(convert) state[c(1,2)] = state[c(2,1)]
  
  single.a2s <- function(s){
    if(strict){
      is.in = tolower(state[['abb']]) %in% tolower(s)
      ifelse(any(is.in), state[['name']][is.in], NA)
    }else{
      # To check if input is in state full name or abb
      is.in = rapply(state, function(x) tolower(x) %in% tolower(s), how="list")
      state[['name']][is.in[[ifelse(any(is.in[['name']]), 'name', 'abb')]]]
    }
  }
  sapply(name, single.a2s)
}
preictedtrueovereighty
solar_panel_map<-preictedtrueovereighty
solar_panel_map$region<-solar_panel_map$state
abb2state(solar_panel_map$state)

library(ggplot2)
library(usmap)
testData <- data.frame(solar_panel_map$lat, solar_panel_map$lon)
testData
p <- plot_usmap( regions = "state") 
p + geom_point(data = testData, aes(x = solar_panel_map.lon, y = solar_panel_map.lat), color = "red")

solar_panel_map


us<-ggplot2::map_data("state")

map.popColor<-ggplot(solar_panel_map, aes(map_id=region))
map.popColor<-map.popColor+geom_map(map=us,aes(fill=predprob), color="white")
map.popColor<-map.popColor+expand_limits(x=solar_panel_map$lon,y=solar_panel_map$lat)
map.popColor<-map.popColor+coord_map()+ggtitle("Solar Panel Distribution")
#map.popColor<- map.popColor + geom_point(aes(x=solar_panel_rad$long, y=solar_panel_rad$lat), color="yellow", size=1)
map.popColor<- map.popColor + geom_point(aes(x=solar_panel_map$lon, y=solar_panel_map$lat), color="orange", size=10)
map.popColor

