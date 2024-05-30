#Install packages
install.packages("corrplot")
install.packages("factoextra")
install.packages("caret")
install.packages("ROCit")
#Use required libraries for analysis
library(dplyr)
library(tidyr)
library(stringr)
library(corrplot)
library(cluster)
library(factoextra)
library(caret)
library(rpart)
library(nnet)
library(e1071)
library(ROCit)
#Read csv file
usedcar<- read.csv("used_cars.csv")
#Check dimensions of the data
dim(usedcar)
#Data cleaning process
usedcar <- na.omit(usedcar) # Remove NA values
usedcar[usedcar == "-"] <- NA
usedcar[usedcar == " "] <- NA
usedcar[usedcar == ""] <- NA
usedcar[usedcar == "-"] <- NA
usedcar<- na.omit(usedcar)
usedcar <- subset(usedcar,fuel_type != "????") # Remove fuel_type column with symbol
usedcar<- subset(usedcar, engine != "????") # Remove engine with symbol
usedcar$fuel_type[usedcar$engine == 'Electric'] <- "Electric" #Change  fuel_type fields to electric for the engine type "Electric"
usedcar$fuel_type[usedcar$fuel_type == 'not supported'] <- "Electric" #Change fuel_type: non-supported to electric
usedcar<- subset(usedcar, fuel_type != "") #Remove blank fuel_type values
usedcar<- subset(usedcar, transmission !="????") # Remove transmission column with symbol
usedcar<- subset(usedcar,int_col !="????") # Remove int_col column with symbol
usedcar<- subset(usedcar, ext_col != "????") # Remove ext_col column with symbol
usedcar<- subset(usedcar,accident !="") #Remove accident column with blank values
usedcar <- subset(usedcar, price <= 749950) #Consider prices within range 749950
usedcar$milage <- gsub("mi.","",as.character(usedcar$milage)) #Remove symbols from milage col
usedcar$milage <- gsub(",","", as.character(usedcar$milage)) # Remove symbols from milage col
usedcar$milage <- as.numeric(usedcar$milage) #Convert milage column to numeric
usedcar$price <- as.numeric(usedcar$price) #Convert price column to numeric
#Convert brand column as factors for analysis
usedcar$fuel_type <- as.factor(usedcar$fuel_type) #Convert fuel type column as factors for analysis 
usedcar$accident<- as.factor(usedcar$accident) #Convert accident column as factors for analysis
usedcar$clean_title <- as.factor(usedcar$clean_title) #Convert clean title as factors for analysis
#calculate relationship of the variables
aircorr<- usedcar[,c("model_year","milage","price")]
corr<-round(cor(aircorr),2)
corrplot(corr, method = "number")
#Calculate top 5 brands sold
library(data.table)
brands<- data.table(usedcar$brand)
countbrands<-brands[, .N, by = brands$V1]
dfcountbrands<-countbrands[order(countbrands$N,decreasing = TRUE), ]
dfcountbrands <- dfcountbrands[1:5,]
pie(table(dfcountbrands$brands),labels= dfcountbrands$N,main = "Top 5 cars sold", col = rainbow(length(dfcountbrands$brands)))
legend("topright", c("BMW", "Ford", "Mercedes-Benz", "Chevrolet","Audi"),
       cex = 0.5, fill = rainbow(length(dfcountbrands$brands)))
#Calculate what model_year of the brand which were the most sold
library(ggplot2)
usedcar$model_year <- as.character(usedcar$model_year)
count<- usedcar %>% count(brand, model_year)
count<-count[order(count$n,decreasing = TRUE), ]
countmodel_year <- count[1:6,]
pl<- ggplot(data = countmodel_year, aes(x=model_year,fill=brand)) 
pl<- pl + geom_bar(stat="count")
pl
#Price classification
quantiles <- quantile(usedcar$price, probs = seq(0, 1, by = (1/3)))
quantiles
breaks<-c(-Inf, 21000, 41500, Inf)
price_categories <- c("low", "medium", "high")
usedcar$pricerange <- cut(usedcar$price, breaks = breaks, labels = price_categories, include.lowest = TRUE)
summary(usedcar$pricerange)
usedcar$brand<- as.factor(usedcar$brand)
usedcar$model<- as.factor(usedcar$model)
usedcar$model_year<- as.factor(usedcar$model_year)
usedcar$engine<- as.factor(usedcar$engine)
usedcar$transmission<- as.factor(usedcar$transmission)
usedcar$ext_col<- as.factor(usedcar$ext_col)
usedcar$int_col<- as.factor(usedcar$int_col)
usedcar$brand1<- as.numeric(usedcar$brand)
usedcar$brand1<- scale(usedcar$brand1)
usedcar$model1<- as.numeric(usedcar$model)
usedcar$model1<- scale(usedcar$model1)
#usedcar$pricerange1<- as.numeric(usedcar$pricerange)
#usedcar$pricerange1<- scale(usedcar$pricerange1)
usedcar$model_year1<- as.numeric(usedcar$model_year)
usedcar$model_year1<- scale(usedcar$model_year1)
usedcar$milage1<- scale(usedcar$milage)
usedcar$fuel_type1<- as.numeric(usedcar$fuel_type)
usedcar$fuel_type1<- scale(usedcar$fuel_type1)
usedcar$engine1<- as.numeric(usedcar$engine)
usedcar$engine1<- scale(usedcar$engine1)
usedcar$transmission1<- as.numeric(usedcar$transmission)
usedcar$transmission1<- scale(usedcar$transmission1)
usedcar$ext_col1<- as.numeric(usedcar$ext_col)
usedcar$ext_col1 <- scale(usedcar$ext_col1)
usedcar$int_col1<- as.numeric(usedcar$int_col)
usedcar$int_col1<- scale(usedcar$int_col1)
usedcar$accident1 <- as.numeric(usedcar$accident)
usedcar$accident1<- scale(usedcar$accident1)
usedcar$clean_title1<- as.numeric(usedcar$clean_title)
usedcar$clean_title1 <- scale(usedcar$clean_title1)
#Principal Component Analysis
usedcarpca<- usedcar[c('brand1','fuel_type1','engine1','transmission1','ext_col1','int_col1','accident1','milage1','model_year1','model1','clean_title1')]
usedcarlabel<- usedcar['pricerange']
#For replication
set.seed(123)
pca.out<- prcomp(usedcarpca, center = TRUE, scale = TRUE)
summary(pca.out)
#calculate total variance explained by each principal component
var_explained = pca.out$sdev^2 / sum(pca.out$sdev^2)
#Plot scree plot
library(ggplot2)

qplot(c(1:11), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

#Build biplot
fviz_pca_biplot(pca.out, label = "var", habillage = usedcarlabel$pricerange)
#Build cluster
clus.out <- kmeans(usedcarpca, centers = 4, nstart = 10)
clus.out$size  # number of obs. in each cluster
# alternatively, table(clus.out$cluster)
clus.out$betweenss/clus.out$tot.withinss  # ratio of between-cluster variation to within-cluster variation
# check centroids for each cluster 
clus.out$centers
#Check cluster
choosek <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(choosek) <- c("numClusters", "totWithinSS", "avg_silhouette")
for (k in 1:10) {
  set.seed(123)
  tempkm <- kmeans(usedcarpca,  centers = k, nstart = 10)
  if (k==1) {
    ss <- 0
  } else {
    # sil_width for column 3 in silhouette() output
    ss <- silhouette(tempkm$cluster, dist(usedcarpca))[, 3]
  }
  
  # append statistics
  tempdf <- data.frame(numClusters = k, totWithinSS = tempkm$tot.withinss, avg_silhouette = mean(ss))
  choosek <- rbind(choosek, tempdf)
}
g <- ggplot(choosek, aes(numClusters, totWithinSS))
g <- g + geom_line() + geom_point() + labs(x="Number of Clusters (k)", y="Total Within-Cluster Squared Distance") 
g
g + geom_text(aes(label=round(totWithinSS, 2)), vjust=-0.3) + scale_x_continuous(breaks = seq(1:10))
g <- ggplot(choosek, aes(numClusters, avg_silhouette))
g <- g + geom_line() + geom_point() + labs(x="Number of Clusters (k)", y="Average Silhouette")
g
g + geom_text(aes(label=round(avg_silhouette, 3)), vjust=-0.3) + scale_x_continuous(breaks = seq(1:10))
#Visualize cluster with pca scores
scores <- as.data.frame(pca.out$x)
set.seed(123)
clus.out2 <- kmeans(scores[,1:5],  centers = 4, nstart = 10)
clus.out2$betweenss/clus.out2$tot.withinss
km.clusters<- clus.out2$cluster
scores$cluster <- as.character(clus.out2$cluster)
#K means model

#Build KNN model
kdata<- usedcar[c('brand1','fuel_type1','engine1','transmission1','ext_col1','int_col1','accident1','milage1','model_year1','model1','clean_title1','pricerange')]
train <- sample(1:nrow(kdata), nrow(kdata)*(2/3)) # replace=FALSE by default

# Use the train index set to split the dataset
#  range.train for building the model
#  range.test for testing the model
range.train <- kdata[train,]   # 2371 rows
range.test <- kdata[-train,]   # the other 1186 rows
trainControl <- trainControl(method="repeatedcv", number=10, repeats=2)
metric <- "Accuracy"
#KNN model
set.seed(7)
fit.knn <- train(pricerange~., data=range.train, method="knn",
                 metric=metric ,trControl=trainControl)
print(fit.knn)
#Prediction 
set.seed(7)
prediction <- predict(fit.knn, newdata = range.test)
cf <- confusionMatrix(prediction, range.test$pricerange)
print(cf)
# Second KNN Model: Best predictors
kdata1<- usedcar[c('brand1','fuel_type1','engine1','transmission1','pricerange')]
train <- sample(1:nrow(kdata1), nrow(kdata1)*(2/3)) # replace=FALSE by default

# Use the train index set to split the dataset
#  churn.train for building the model
#  churn.test for testing the model
churn.train <- kdata1[train,]   # 2371 rows
churn.test <- kdata1[-train,]   # the other 1186 rows
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
#KNN model
set.seed(7)
fit.knn <- train(pricerange~., data=churn.train, method="knn",
                 metric=metric ,trControl=trainControl)
print(fit.knn)
#Prediction 
set.seed(7)
prediction <- predict(fit.knn, newdata = churn.test)
cf <- confusionMatrix(prediction, churn.test$pricerange)
print(cf)

#Third KNN model
kdata2<- usedcar[c('brand1','fuel_type1','engine1','transmission1','ext_col1','int_col1','accident1','milage','pricerange')]
train <- sample(1:nrow(kdata2), nrow(kdata2)*(2/3)) # replace=FALSE by default
churn.train <- kdata2[train,]
churn.test <- kdata2[-train,]
trainControl <- trainControl(method="repeatedcv", number=10, repeats=4)
metric <- "Accuracy"
set.seed(7)
fit.knn <- train(pricerange~., data=churn.train, method="knn",
                 metric=metric ,trControl=trainControl)
print(fit.knn)
#Prediction 
set.seed(7)
prediction <- predict(fit.knn, newdata = churn.test)
cf <- confusionMatrix(prediction, churn.test$pricerange)
print(cf)

colors<- c('green','red','orange')
model_year<- c('2015','2018','2019','2020','2021')
brand<- c('Ford','BMW','Mercedes-Benz')
#Built barplot
barplot(countmodel_year$n, main = "Total Revenue", names.arg = countmodel_year$model_year,
        xlab = "Month", ylab = "Revenue", col = colors)
legend("topright", brand, cex = 0.7, fill = colors)
pl<- ggplot(countmodel_year, aes(x = model_year, y = n, fill = brand)) + geom_col()
pl
#Find total count of fuel type of cars
fueltypetable <- data.table(usedcar$fuel_type)
pl<- ggplot(data = fueltypetable, aes(x=fueltypetable$V1))
pl<- pl + geom_bar(stat="count")
pl

datatable1 <- as.data.table(usedcar)
countfuel_type<-datatable1[, .(Percentage = .N / nrow(datatable1) * 100), by = fuel_type]
barplot(countfuel_type$Percentage, main = "Fuel_type", names.arg = countfuel_type$fuel_type, xlab = "Fuel_type", ylab = "Percentage")

#Logistic regression model
multinom_model <- multinom(pricerange ~ brand1 + model_year1 + transmission1 + milage1 + fuel_type1 + accident1 + clean_title1,
                           data = range.train)

predictions <- predict(multinom_model, newdata = range.test)

# Evaluate the model (e.g., confusion matrix)
cf2 <- confusionMatrix(predictions, range.test$pricerange)
print(cf2)

#Naive Bayes Model
fit.nb <- naiveBayes(pricerange ~.,
                     data = range.train)
#Predict class probability
prediction1 <- predict(fit.nb, newdata = range.test)
cf1 <- confusionMatrix(prediction1, range.test$pricerange)
print(cf1)
