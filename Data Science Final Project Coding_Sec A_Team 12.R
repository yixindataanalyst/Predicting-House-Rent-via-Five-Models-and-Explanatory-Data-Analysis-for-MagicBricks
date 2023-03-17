#################################
###Function Section#############
installpkg <- function(x){
  if(x %in% rownames(installed.packages())==FALSE) {
    if(x %in% rownames(available.packages())==FALSE) {
      paste(x,"is not a valid package - please check again...")
    } else {
      install.packages(x)           
    }
    
  } else {
    paste(x,"package already installed...")
  }
}

## deviance calculations
## pred must be probabilities (0<pred<1) for binomial
deviance <- function(y, pred, family=c("gaussian","binomial")){
  family <- match.arg(family)
  if(family=="gaussian"){
    return( sum( (y-pred)^2 ) )
  }else{
    if(is.factor(y)) y <- as.numeric(y)>1
    return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
  }
}
devianceQR <- function(y, pred, tau){
  return( sum(  tau*max(0, y-pred ) + (1-tau)*max(0, pred-y ) ) )
}

## get null devaince too, and return R2
R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)
  if(fam=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  dev <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  return(1-dev/dev0)
}

### Returns the indices for which |x[i]| > tr
support<- function(x, tr = 10e-6) {
  m<- rep(0, length(x))
  for (i in 1:length(x)) if( abs(x[i])> tr ) m[i]<- i
  m <- m[m>0]
  m
}

### Penalty choice for Quantile Regression
lambda.BC<- function(X, R = 1000, tau = 0.5, c = 1, alpha = .05){
  n <- nrow(X)
  norm2n<-function(z){sqrt(mean(z^2))}
  sigs <- apply(X,2,norm2n)
  U <- matrix(runif(n * R),n)
  R <- (t(X) %*% (tau - (U < tau)))/(sigs*sqrt(tau*(1-tau)))
  r <- apply(abs(R),2,max)
  c * quantile(r, 1 - alpha) * sqrt(tau*(1-tau))*c(1,sigs)
}

#########End of Function Section#############
####Loading Required Packages
library(readxl)
install.packages("usmap")
install.packages("farver")
install.packages("tidyverse")
library(tidyverse)
library(usmap)
library(farver)
library("dplyr")
library(glmnet)
installpkg("plfm")
library(plfm)
library(randomForest)
library(datasets)
library(caret)
data<-read.csv(file.choose())
View(data)

#Removing "." in Variables names 
library(dplyr)
data <- rename(data, PostedOn = Posted.On)
data <- rename(data, AreaType = Area.Type)
data <- rename(data, AreaLocality = Area.Locality)
data <- rename(data, FurnishingStatus = Furnishing.Status)
data <- rename(data, TenantPreferred = Tenant.Preferred)
data <- rename(data, PointOfContact = Point.of.Contact)

#Creating dummy variables
library("fastDummies")
data$PostedOn<-as.Date(data$PostedOn)
data<-dummy_cols(data,select_columns = c('AreaType','City','FurnishingStatus','TenantPreferred','PointOfContact'))
data <- rename(data,  AreaType_BuiltArea=`AreaType_Built Area`)
data <- rename(data,  AreaType_CarpetArea=`AreaType_Carpet Area`)
data <- rename(data,  AreaType_SuperArea=`AreaType_Super Area`)
data <- rename(data,  FurnishingStatus_SemiFurnished=`FurnishingStatus_Semi-Furnished`)
data <- rename(data,  TenantPreferred_BachelorsFamily=`TenantPreferred_Bachelors/Family`)
data <- rename(data,  PointOfContact_ContactAgent=`PointOfContact_Contact Agent`)
data <- rename(data,  PointOfContact_ContactBuilder=`PointOfContact_Contact Builder`)
data <- rename(data,  PointOfContact_ContactOwner=`PointOfContact_Contact Owner`)
#Variable "Floor" Cleaning
library(stringr)
# Split name column into firstname and last name
df<- as.data.frame(str_split_fixed(data$Floor," out of ", 2))
colnames(df)<-c("Floor_Number", "Total_Floor")
data <- cbind(data, df)
data$Floor_Number <- replace(data$Floor_Number, data$Floor_Number == "Ground", 0)
data$Floor_Number <- as.numeric(data$Floor_Number)+1
data$Total_Floor <-as.numeric(data$Total_Floor)+1
#Removing NAs
data <- na.omit(data)

#Remove Significant Outlier
data <- subset(data, Rent != 3500000)

#Remove AreaLocality Variable and Floor
data <- subset(data, select = -AreaLocality)

#Final Results
View(data)

library("ggplot2")
library("scales")

#The first linear regression model with all variables
model1<-lm(formula = Rent ~ . - (AreaType + City + FurnishingStatus +
                                   TenantPreferred + PointOfContact + Floor),
           data = data)
summary(model1)

#The second linear regression model excluding the variables with NA linear regression results
model2<-lm(formula = Rent ~ . - (AreaType + City + FurnishingStatus +
                                   TenantPreferred + PointOfContact + Floor + PostedOn + `AreaType_SuperArea` + `City_Mumbai` + `FurnishingStatus_Unfurnished` + `TenantPreferred_Family` + `PointOfContact_ContactOwner`),
           data = data)
summary(model2)

#The third linear regression model excluding the variables with NA linear regression results and the variables that are insignificant
model3<-lm(formula = Rent ~ . - (AreaType + City + FurnishingStatus +
                                   TenantPreferred + PointOfContact + Floor + PostedOn + `AreaType_SuperArea` + `AreaType_BuiltArea` + `City_Mumbai` + `FurnishingStatus_Unfurnished` + `FurnishingStatus_SemiFurnished` + `TenantPreferred_Family` + `PointOfContact_ContactOwner` + `PointOfContact_ContactBuilder`),
           data = data)
summary(model3)

##Build up the model
##Set up the Lasso Model and find out lambda.min and lambda.1se
##Since we have generated the dummy variables through "fastdummies" package，
##the following code is just to remove the repetitive columns.
data_Lasso<-subset(data, select=-c(Floor,AreaType,City,FurnishingStatus,TenantPreferred,PointOfContact))
### matrix the variables 
Mx<- model.matrix(Rent~ ., data=data_Lasso)[,-1]
My<- data_Lasso$Rent
##run the lasso and calculate the lamdba.min and lambda.1se
set.seed(2)
lassoCV <- cv.glmnet(Mx,My, family="gaussian")
lassoCV$lambda.min
lassoCV$lambda.1se
###Put the lanbda.min and lambda.1se back to the models to set up two models
lassomin <- glmnet(Mx, My, alpha = 1, lambda = lassoCV$lambda.min)
lasso1se <- glmnet(Mx, My, alpha = 1, lambda = lassoCV$lambda.1se)
##Build up a K-fold structure
nfold <- 10
n <- nrow(data_Lasso)
set.seed(1)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
PL.OOS <- data.frame(PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold)) 
L.OOS <- data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold)) 
OOS <- data.frame(simple.linear=rep(NA,nfold)) 
RandomForest.OOS<-data.frame(Random.Forest=rep(NA,nfold)) 
###Extarct the min features calculated by Lasso 
features.min<- coef(lassomin, s="lambda.min")
features.min<-features.min@Dimnames[[1]][which(features.min != 0 ) ][-1]
####Extract the 1se features calculated by Lasso
features.1se<- coef(lasso1se, s="lambda.1se")
features.1se<-features.1se@Dimnames[[1]][which(features.1se != 0 ) ][-1]
#### Count how many features are extracted
length(features.min)
length(features.1se) 
####Trim the dataset used for Post Lasso
data.min <- data.frame(Mx[,features.min],My)
data.1se <- data.frame(Mx[,features.1se],My)

##Set up the K-fold Loop

for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ### This is the CV for the Post Lasso Estimates
  ###setting the model for PL.min
  rmin <- glm(My~., data=data.min, subset=train, family="gaussian")
  ###Setting the model for PL.1se
  if ( length(features.1se) == 0){  r1se <- glm(Rent~1, data=data_Lasso, subset=train, family="gaussian") 
  } else {r1se <- glm(My~., data=data.1se, subset=train, family="gaussian")
  }
  ###Train the multiple linear regression with all variables kept
  Linear.Model =glm(Rent~., data =data_Lasso, subset = train, family="gaussian")
  ###Get the out-of-sample prediction for multiple linear regression
  predlinear <- predict(Linear.Model, newdata=data_Lasso[-train,], type="response")
  ###Train the random forest model
  rf <- randomForest(Rent~., data=data_Lasso,subset = train,proximity=TRUE)
  ### Get the out-of-sample prediction for random forest
  predrf<-predict(rf,newdata=data_Lasso[-train,],type="response")
  ### Calculate the R square for out-of-sample random forest forecasting. 
  RandomForest.OOS$Random.Forest[k] <- 1 - (sum((data_Lasso$Rent[-train]-predrf)^2)/sum((data_Lasso$Rent[-train]-mean(data_Lasso$Rent[-train]))^2))
  ### Get the out-of-sample prediction for PL.min
  predmin <- predict(rmin, newdata=data.min[-train,], type="response")
  ### Get the out-of-sample prediction for PL.1se
  pred1se  <- predict(r1se, newdata=data.1se[-train,], type="response")
  ### Get the R square for the out-of-sample PL.min 
  PL.OOS$PL.min[k] <- R2(y=My[-train], pred=predmin, family="gaussian")
  ### Get the R square for the out-of-sample PL.1se
  PL.OOS$PL.1se[k] <- R2(y=My[-train], pred=pred1se, family="gaussian")
  ### Get the R square for the out-of-sample multiple linear regression
  OOS$simple.linear[k] <- R2(y=My[-train], pred=predlinear, family="gaussian")
  ### Train the Lassomin and Lasso1se with the k-fold traning set
  lassomin  <- glmnet(Mx[train,],My[train], family="gaussian",lambda = lassoCV$lambda.min)
  lasso1se  <- glmnet(Mx[train,],My[train], family="gaussian",lambda = lassoCV$lambda.1se)
  ### Get the out-of-sample prediction for Lasoo.min and Lasoo.1se 
  predlassomin <- predict(lassomin, newx=Mx[-train,], type="response")
  predlasso1se  <- predict(lasso1se, newx=Mx[-train,], type="response")
  ### Calculate the R square for out-of-sample Lasso.min and Lasso.1se predictions. 
  L.OOS$L.min[k] <- R2(y=My[-train], pred=predlassomin, family="gaussian")
  L.OOS$L.1se[k] <- R2(y=My[-train], pred=predlasso1se, family="gaussian")
  
  print(paste("Iteration",k,"of",nfold,"completed"))
}

R2performance <- cbind(PL.OOS,L.OOS,OOS,RandomForest.OOS)
par( mar=  c(8, 4, 4, 2) + 0.6 )
names(OOS)[1] <-"simple linear"
barplot(colMeans(R2performance), las=2,xpd=FALSE, ylim=c(0,1) , xlab="", ylab = bquote( "Average Out of Sample " ~ R^2))

m.OOS <- as.matrix(R2performance)
rownames(m.OOS) <- c(1:nfold)
####Visualize the performance of different models and calculate the average performance
barplot(t(as.matrix(m.OOS)), beside=TRUE, ylim=c(0,1.15) ,xlim=c(0,80),legend.text=TRUE, 
        args.legend=list(x="topright",bty="n",x.intersp=0.2,y.intersp=0.3,cex=0.8),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))
colMeans(R2performance) 
##The Random Forest preforms way more better than other models with an average
##R2 of 0.72

### Plot for the IncNodePurity for different variables
varImpPlot(rf)
ImpData<-as.data.frame(importance(rf))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=IncNodePurity)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names,y=0,yend=max(IncNodePurity)), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

####Figure 1 plot
ggplot(data=data,aes(x=Size,y=Rent))+geom_point()+ylab("Rent (in Indian Rupee)")+ylab("Rent (in Indian Rupee)")+xlab("Size (in Square Feet)")
###Figure 2 plot
ggplot(data=data,aes(y=Rent))+geom_boxplot()+ylim(0,50000)+ylab("Rent (in Indian Rupee)")

###### Plot for the correlation matrix###
library(corrplot)
data_Lasso$PostedOn<-as.numeric(data_Lasso$PostedOn)
M<-cor(data_Lasso)
corrplot(M,method='color',order="alphabet")