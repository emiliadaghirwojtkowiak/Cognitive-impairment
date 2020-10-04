# Random Forrest Model ##
# Model 1 , comparison 2 #

## Analysis steps ##

#1. Impute missing 
#2. Balance training and test set
#3. RF model to select variables
#4. Variables selected incorporated to logistic regression
#   -assess OR, uci, lci, p-val

library("caret", lib.loc="~/R/win-library/3.5");library(ROSE);library("randomForest")
rm(list=ls())

##################################
## RF model with data balance ###
##################################
set.seed(42)
setwd("C:/dane/socha_edyta/dane przygotowane_ES/matryca danych modele")
mydata <- read.csv("MAT1.all.cov.forMODEL0.csv");
data.adj <- subset(mydata, select=c(7,8,9,16, c(17:32),92,133 ))
mydata <- data.adj
sum(is.na(mydata))

# 1.Impute missing values
imp <- impute(mydata)
dim(imp)
sum(is.na(imp))
imp<-as.matrix(imp)

# split the data
set.seed(101)  
imp<-as.data.frame(imp)
index <- createDataPartition(imp$casecont, p = 0.7, list = FALSE)
train_data <- imp[index, ]; dim(train_data);table(train_data$casecont)
test_data  <- imp[-index, ]; dim(test_data);table(test_data$casecont)

# 2.Balance the data using rose package
data.rose.train <- ROSE(as.factor(casecont) ~ ., data = train_data, seed = 1)$data
table(data.rose.train$casecont); dim(data.rose.train)

data.rose.test <- ROSE(as.factor(casecont) ~ ., data = test_data, seed = 1)$data
table(data.rose.test$casecont); dim(data.rose.test)

# 3.RF model to select variables
set.seed(101)
model_rf1 <- randomForest(as.factor(casecont) ~ .,importance=T,
                          data = data.rose.train
);model_rf1 

setwd("C:/dane/socha_edyta/dane przygotowane_ES/plots/plotsM1")
tiff("Opt_ntree_M1.tiff", width = 7, height = 5, units = 'in', res = 300)
plot(model_rf1, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3,lwd=2, col=1:3) 
title(main="Error Rates Random Forest Training data")


# How many variables include in RF model
# Tuning Random Forest

set.seed(101)
tRF<- tuneRF(x = data.rose.train[,c(1,2,3,c(5:22))],
             y=as.factor(data.rose.train$casecont),
             mtryStart = 4, #Aprox, Sqrt of Total no. of variables
             ntreeTry = 150,
             stepFactor = 1.5,
             improve = 0.0001,
             trace = TRUE,
             plot = TRUE,
             doBest = TRUE,
             nodesize = 10,
             importance = TRUE
)
data.rose.test$predicted.response <- predict(model_rf1,data.rose.test)

# Create Confusion Matrix
print(confusionMatrix(data=as.factor(data.rose.test$predicted.response),  
                      reference= as.factor(data.rose.test$casecont)))

require(pROC)


pred <- predict(model_rf1, newdata=test_data)
roc.curve(test_data$casecont, pred,
          main="ROC curve") 

################################
## Variable importance plot ###
################################

importance<- importance(model_rf1)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

var.rank <- subset(rankImportance, rankImportance$Rank=="#1" |
                     rankImportance$Rank=="#2"  |
                     rankImportance$Rank=="#3"  |
                     rankImportance$Rank=="#4" )

Variables <- as.matrix(c("Age", "GLU","HIS", "Tea-5/6 p.week"))
rankImportance <- cbind(var.rank[-1],Variables )

setwd("C:/dane/socha_edyta/dane przygotowane_ES/plots/plotsM1")
tiff("var_imp_Gini_M1.tiff", width = 7, height = 5, units = 'in', res = 300)

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = '') +
  coord_flip() 


# 4.Logistic regresion with selected variables ###

x <- rbind(data.rose.test[-23],data.rose.train)
LR <- glm(as.factor(casecont) ~  age+GLU +HIS+ X5.6.tydz..6
          ,family=binomial(link='logit'),data=x );
summary(LR)

#age
exp(summary(LR)$coefficients["age",1] + 
      qnorm(c(0.025,0.5,0.975)) * summary(LR)$coefficients["age",2])

#GLU
exp(summary(LR)$coefficients["GLU",1] + 
      qnorm(c(0.025,0.5,0.975)) * summary(LR)$coefficients["GLU",2])
# HIS
exp(summary(LR)$coefficients["HIS",1] + 
      qnorm(c(0.025,0.5,0.975)) * summary(LR)$coefficients["HIS",2])
# diet
exp(summary(LR)$coefficients["X5.6.tydz..6",1] + 
      qnorm(c(0.025,0.5,0.975)) * summary(LR)$coefficients["X5.6.tydz..6",2])

