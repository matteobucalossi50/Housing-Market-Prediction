# Import series of libraries
library("dplyr")
library("tidyr")
library("tidyverse")
library("ggplot2")
library('ggmap')
library('rjson')
library('jsonlite')
library('leaflet')
library('grDevices')
library('scales')
library('RCurl')
library('sp')
library('geojsonio')
library('lmtest')
library("faraway")
library("corrplot")
library("modelr")
library('DT')
library('plotly')
library('rmdformats')
library("readr")
library('class')
library("rpart") 
library("caret") 
library("rpart.plot")
library("rattle") # For fancyRpartPlot (Trees) Answer "no" on installing from binary source
library("tree") 
library('ISLR')
library('randomForest')
library('leaps')
library("Rmisc")
library("MASS")
library("factoextra")
library("pls")
library("magrittr")
library("glmnet")


# Import and clean data
kc_house_data <- read.csv("/Users/Matteo/Desktop/GW/DATS 6101/DATS 6101/project/DATS6101_Project/Project2/kc_house_data.csv")

kc_house_data <- subset(kc_house_data, select = -c(9, 10))

kc_house_data <- subset(kc_house_data, kc_house_data$bedrooms != 0)

kc_house_data <- subset(kc_house_data, kc_house_data$bathrooms != 0)

kc_house_data <- subset(kc_house_data, kc_house_data$bedrooms < 30)

kc_house_data <-  drop_na(kc_house_data)

kc_house_data$condition <- as.factor(kc_house_data$condition)

kc_house_data$grade <- as.factor(kc_house_data$grade)

price.ln = log(kc_house_data$price)


# Create price histogram - Leaflet
hist(kc_house_data$price[kc_house_data$price<=2000000], xaxt="n", ylim = c(0,5000), col = heat.colors(20), main = "Housing Price Histogram, $0-$2M Only", xlab = "Housing Price ($)", cex.axis = .75)
axis(side=1, at=axTicks(1), 
     labels=formatC(axTicks(1), format="d", big.mark=','))

# Create price distribution Map
price.bins <- c(0, 250000, 500000, 750000, 1000000, 1250000, 1500000, 1750000, 2000000, 8000000)
qpal <-  colorBin(palette = 'GnBu', kc_house_data$price, bins= price.bins, n = 9)
house.map1 <- leaflet(kc_house_data) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addCircleMarkers(lng = ~long, lat = ~lat, 
                   stroke = FALSE, 
                   fillOpacity = 5, 
                   color = ~qpal(price), 
                   radius = 2,
                   label = ~as.character(paste0('Price: $', price, ", ", 'condition: ', condition, ", ", 'year built: ', yr_built, 'Sqft living: ', sqft_living))) %>%
  addLegend('bottomright', pal = qpal, values = ~price, opacity = 1, title = 'Price', labFormat = labelFormat(prefix = '$', between = ' - $'))

house.map1


# Scatterplot price-sqft
plot(kc_house_data$sqft_living, kc_house_data$price, xaxt="n", yaxt="n", pch = 20, cex = .1, xlab = "Square Feet of Living Space", ylab = "Housing Price ($)", cex.axis = .75, main = "Housing Price vs. Living Space")
abline(lm(kc_house_data$price ~ kc_house_data$sqft_living, data = kc_house_data), col = "orange")
legend(x='topright', legend=paste('Correlation =',round(cor(kc_house_data$sqft_living, kc_house_data$price),5)))
axis(side=1, at=axTicks(1), 
     labels=formatC(axTicks(1), format="d", big.mark=','))
axis(side=2, at=axTicks(2), 
     labels=formatC(axTicks(2), format="d", big.mark=','))


# Boxplot price-grade
ggplotly(ggplot(kc_house_data, aes(x=grade, y=price, fill=grade)) + geom_boxplot() + scale_fill_brewer(palette="Spectral") + ggtitle("Housing Price vs. Apartment Grade (log)") + ylab("Housing Price ($)") + xlab("Apartment Grade") +  theme(plot.title= element_text(hjust=0.5, size = 14)) + scale_y_log10(labels = function(x) format(x,nsmall = 2,scientific = FALSE, big.mark = ',')))



# preprocessing for KNN
kc_house_data$pricefact <- cut(kc_house_data$price, breaks = 3, labels = c("Low", "Medium", "High")) # categorization of house prices
summary(kc_house_data$pricefact) # distribution of data points according to their categories

# split training/test data
set.seed(1)
kc_house_data_train_rows = sample(1:nrow(kc_house_data),     #<- from 1 to the number of rows in the data set
                                  round(0.8 * nrow(kc_house_data), 0),  #<- multiply the number of rows by 0.8 and round the decimals
                                  replace = FALSE)       #<- don't replace the numbers


length(kc_house_data_train_rows) / nrow(kc_house_data) # Let's check to make sure we have 80% of the rows. 

kc_house_data_train = kc_house_data[kc_house_data_train_rows, ]  #<- select the rows identified in the kc_house_data_train_rows
kc_house_data_test = kc_house_data[-kc_house_data_train_rows, ]  #<- select the rows that weren't identified in the kc_house_data_train_rows


# function to choose best K value
chooseK = function(k, train_set, val_set, train_class, val_class){
  
  # Build knn with k neighbors considered.
  set.seed(1)
  class_knn = knn(train = train_set,    #<- training set cases
                  test = val_set,       #<- test set cases
                  cl = train_class,     #<- category for classification
                  k = k,                #<- number of neighbors considered
                  use.all = TRUE)       #<- control ties between class assignments
  #   If true, all distances equal to the kth 
  #   largest are included
  
  tab = table(class_knn, val_class)
  
  # Calculate the accuracy. 
  accu = sum(tab[row(tab) == col(tab)]) / sum(tab)                         
  cbind(k = k, accuracy = accu)
}

# Test few values to choose K
knn_different_k = sapply(seq(1, 25, by = 2),  #<- set k to be odd number from 1 to 25
                         function(x) chooseK(x, 
                                             train_set = kc_house_data_train[, c("bedrooms", "bathrooms", "sqft_living", "sqft_lot", "floors", "sqft_above", "sqft_basement", "sqft_living15", "sqft_lot15")],
                                             val_set = kc_house_data_test[, c("bedrooms", "bathrooms", "sqft_living", "sqft_lot", "floors", "sqft_above", "sqft_basement", "sqft_living15", "sqft_lot15")],
                                             train_class = kc_house_data_train[, "pricefact"],
                                             val_class = kc_house_data_test[, "pricefact"]))

# plot accuracy for each k value
knn_different_k = data.frame(k = knn_different_k[1,],
                             accuracy = knn_different_k[2,])

ggplot(knn_different_k,
        aes(x = k, y = accuracy)) +
        geom_line(color = "orange", size = 1.5) +
        geom_point(size = 3)


# KNN classification model
set.seed(1)
price_predict = knn(train = kc_house_data_train[, c("bedrooms", "bathrooms", "sqft_living", "sqft_lot", "floors", "sqft_above", "sqft_basement", "sqft_living15", "sqft_lot15")],  #<- training set cases
                    test = kc_house_data_test[, c("bedrooms", "bathrooms", "sqft_living", "sqft_lot", "floors", "sqft_above", "sqft_basement", "sqft_living15", "sqft_lot15")],    #<- test set cases
                    cl = kc_house_data_train[, "pricefact"],  #<- category for true classification
                    k = 12)                               #<- number of neighbors considered

table(price_predict)

# evaluation KNN
kNN_res = table(price_predict, kc_house_data_test$`pricefact`)
kNN_acc = sum(kNN_res[row(kNN_res) == col(kNN_res)]) / sum(kNN_res)
print(kNN_acc)



# regression Tree

tree1 <- tree(log(price) ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + condition + grade + sqft_above + sqft_basement+yr_built+yr_renovated, data=kc_house_data)
summary(tree1)
plot(tree1) 
title("Simple Regression Tree")
text(tree1,cex=0.75, digits=3)

# using rpart for fancier viz
tree2 <- rpart(log(price) ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + condition + grade + sqft_above + sqft_basement+yr_built+yr_renovated, data=kc_house_data, cp=.02)
summary(tree2)
par(xpd = NA, mar = rep(0.7, 4)) 
plot(tree2, compress = TRUE)
text(tree2, cex = 0.7, use.n = TRUE, fancy = TRUE, all = TRUE)

fancyRpartPlot(tree2, main = "Fancy Regression Tree")
rpart.plot(tree2, box.palette="RdBu", shadow.col="gray", nn=TRUE, main = "Fancier Regression Tree")


# tree for geographic areas
treefit = tree(log(price) ~ long+lat,data=kc_house_data)
price.deciles = quantile(kc_house_data$price,0:10/10)
cut.prices = cut(kc_house_data$price,price.deciles,include.lowest=TRUE)
plot(kc_house_data$long,kc_house_data$lat,col=grey(10:2/11)[cut.prices],pch=20,xlab="Longitude",ylab="Latitude")
title('Regression Tree Map')
partition.tree(treefit,ordvars=c("long","lat"),add=TRUE, col = 'red')

# Now prune the original tree
tree1.seq <- prune.tree(tree1) # Sequence of pruned tree sizes/errors
plot(tree1.seq)  # error versus plot size
opt.trees = which(tree1.seq$dev == min(tree1.seq$dev)) # Positions of optimal (with respect to error) trees

plot(prune.tree(tree1,best=5))
title('Pruned Regression Tree')
text(prune.tree(tree1,best=5), cex=0.75, digits = 3)


# Regression Tree model
fold <- floor(runif(nrow(kc_house_data),1,11)) 
kc_house_data$fold <- fold
test.set <- kc_house_data[kc_house_data$fold == 1,] 
train.set <- kc_house_data[kc_house_data$fold != 1,] 

tree.pred <- rpart(log(price)~bedrooms + bathrooms + sqft_living + sqft_lot + floors + condition + grade + sqft_above + sqft_basement+yr_built+yr_renovated, data=train.set)
printcp(fit) # display the results
plotcp(tree.pred) # visualize cross-validation results
# additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(tree.pred) # visualize cross-validation results

# using 'tree' package
tree.pred <- tree(log(price)~bedrooms + bathrooms + sqft_living + sqft_lot + floors + condition + grade + sqft_above + sqft_basement+yr_built+yr_renovated, data=train.set, mindev=0.001)
treepr <- prune.tree(tree.pred, best = 5) # Return best pruned tree with 5 leaves, evaluating error on training data
plot(treepr) 
title('Training Regression Tree')
text(treepr,cex=0.75, digits=3)
tree.pred.seq <- prune.tree(tree.pred)
plot(tree.pred.seq)
tree.pred.seq$dev
opt.trees = which(tree.pred.seq$dev == min(tree.pred.seq$dev)) 
#evaluation on test data
treepr.pred <- prune.tree(tree.pred, best = 5, newdata = test.set) #evaluates on test data
plot(treepr.pred) 
title('Test Regression Tree')
text(treepr.pred,cex=0.75, digits=3)


# Random Forest - slow training (>10 minutes)
ff1 <- randomForest(log(price)~bedrooms + bathrooms + sqft_living + sqft_lot + floors + condition + grade + sqft_above + sqft_basement+yr_built+yr_renovated, data = train.set, importance = TRUE)
print(ff1)



# data cleaning for PCA
house <- data.frame(read.csv("kc_house_data.csv", header = TRUE))
house2 <- subset(house,select = -c(id,date,waterfront,view,zipcode,lat,long,sqft_living15,sqft_lot15,yr_renovated))
house2 <- drop_na(house2)
corx=cor(house2[,-1])
corx

# PComponents
prin_comp <- prcomp(scale(house2[,-1]))
summary(prin_comp)
prin_comp$center ##output the mean of variables
prin_comp$scale ##output the sd of variables
prin_comp$rotation ##variables in each component

# plotting PCA
fviz_eig(prin_comp)
biplot(prin_comp, scale = 0)
biplot(prin_comp,6:7, scale =0)

pr.var <- (prin_comp$sdev^2)
pve <- pr.var/sum(pr.var)
plot(cumsum(pve), xlab="Principal Component (standardized)", ylab ="Cumulative Proportion of Variance Explained",ylim=c(0,1),type="b")

# PCR model
houseprice <- prin_comp$x
modHouses <- lm(house2$price ~ houseprice[,1:7])
summary(modHouses)

# a full linear model seems to perform still better though
fullmodel=lm(price~.-sqft_basement,data=house2)
summary(fullmodel)

par(mfrow = c(1,2))
plot(house2$price, predict(modHouses), xlab = "actual price", ylab = "Predicted price", main = "PCR", abline(a = 0, b = 1, col = "red"))
plot(house2$price, predict(fullmodel), xlab = "actual price", ylab = "Predicted price", main = "Full model", abline(a = 0, b = 1, col = "red"))



# Using Ridge and Lasso regularizations

# function 
uzscale <- function(df, append=0, excl=NULL) { # Standardize dataframe to z scores, safe for non-numeric variables. 
  append = ifelse(append==TRUE || append=="true" || append=="True" || append=="T" || append=="t" || append==1 || append=="1", TRUE, FALSE) 
  nmax = length(df)
  if (nmax < 1 || !is.numeric(nmax) ) { return(df) }
  df1 = df
  onames = colnames(df)  # the original column names
  cnames = onames  # the new column names, if needed start with the original ones
  znames = paste("z",cnames, sep="")     # new column names added prefix 'z'. Those are non-numeric will not be used.
  nadd = ifelse(append, nmax, 0) # add to the column index or replace the orig columns
  j=1  # counting index
  for( i in 1:nmax ) {
    if ( is.numeric(df[,i]) && !( i %in% excl || onames[i] %in% excl ) ) { 
      df1[,j+nadd] = scale(df[,i])
      cnames = c(cnames, znames[i])
      j=j+1
    } else if ( !append ) { j=j+1
    } # if append == 1 and (colunm non-numeric or excluded), do not advance j.
  }
  if (append) { colnames(df1) <- cnames }
  return(df1)
}

# preparing data for regularizations
kc_house_data <- read.csv('kc_house_data.csv')
kc_house_data <- subset(kc_house_data, select = -c(9, 10))
kc_house_data <- subset(kc_house_data, kc_house_data$bedrooms != 0)
kc_house_data <- subset(kc_house_data, kc_house_data$bathrooms != 0)
kc_house_data <- subset(kc_house_data, kc_house_data$bedrooms < 30)
kc_house_data <-  drop_na(kc_house_data)


# Ridge 
kc_house_data$bedrooms <- as.factor(kc_house_data$bedrooms)
kc_house_data$bathrooms <- as.factor(kc_house_data$bathrooms)
kc_house_data$floors <- as.factor(kc_house_data$floors)
kc_house_data$condition <- as.factor(kc_house_data$condition)
kc_house_data$grade <- as.factor(kc_house_data$grade)

house_unscale = uzscale(kc_house_data)
x=model.matrix(price~.,house_unscale)[,-1]
y=house_unscale$price
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
print(dim(coef(ridge.mod)))
plot(ridge.mod)

# predicting with Ridge L2
coef(ridge.mod)[,50]
coef(ridge.mod)[,60] 
predict(ridge.mod,s=50,type="coefficients")[1:55,]

# training/test split
set.seed(1)
train = house_unscale %>% sample_frac(0.5)
test = house_unscale %>% setdiff(train)

x_train = model.matrix(price~., train)[,-1]
x_test = model.matrix(price~., test)[,-1]

y_train = train %>% dplyr::select(price) %>% unlist() # %>% as.numeric()
y_test = test %>% dplyr::select(price) %>% unlist() # %>% as.numeric()

# ridge model
ridge.mod=glmnet(x_train,y_train,alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x_test)
mean((mean(y_train)-y_test)^2) # the test set MSE

ridge.pred=predict(ridge.mod,s=1e10,newx=x_test)
ridge.pred = predict(ridge.mod, s = 0, newx = x_test)
predict(ridge.mod, s = 0, type="coefficients")[1:55,]


print(mean((ridge.pred - y_test)^2)) #MSE

ols.mod = lm(price~., data = train) # old model
summary(ols.mod)
print(mean(residuals(ols.mod)^2)) #MSE

# cross-validation
set.seed(1)
cv.out=cv.glmnet(x_train,y_train,alpha=0)  # Fit ridge regression model on training data
plot(cv.out)
bestlam = cv.out$lambda.min


ridge.pred=predict(ridge.mod,s=bestlam,newx=x_test)
mean((ridge.pred-y_test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:64,]
sst <- sum((y_test - mean(y_test))^2)
sse <- sum((ridge.pred - y_test)^2)
rsq <- 1 - sse / sst
print(rsq)


# Lasso regularization

lasso.mod=glmnet(x_train,y_train,alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x_train,y_train,alpha=1)
plot(cv.out)


bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x_test)
mean((lasso.pred-y_test)^2)
out = glmnet(x, y, alpha = 1, lambda = grid) # Fit lasso model on full dataset
lasso_coef = predict(out, type = "coefficients", s = bestlam)[1:64,] # Display coefficients using λ chosen by CV
lasso_coef
lasso_coef[lasso_coef!=0]

sst1 <- sum((y_test - mean(y_test))^2)
sse1 <- sum((lasso.pred - y_test)^2)
rsq1 <- 1 - sse1 / sst1
print(rsq1)



