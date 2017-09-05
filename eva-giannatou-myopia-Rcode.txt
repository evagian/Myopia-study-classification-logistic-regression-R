#print first 6 rows with kable function from knitr
knitr::kable(head(data)) 

#use SPHEQ to estimate myopia
library(ggplot2)
ggplot(myopia, aes(x = SPHEQ, y = MYOPIC)) 
+ geom_jitter(shape = "O", position = position_jitter(height = 0)) 
+theme_bw()

#plot correlation between variables
library(corrplot)
corrplot(cor(data))  

#plot correlation between attributes and myopia
correlations <- cor(myopia)
pcor <- correlations[,3]
library(corrplot)
corrplot(cor(myopia),method='e')

#Lasso: Attribute selection
library(glmnet)
myopia_mat <- model.matrix(MYOPIC~.-ID-STUDYYEAR, myopia)[,-3]
lambdas <- 10 ^ seq(8,-4,length=250)
myopia_models_lasso <- glmnet(myopia_mat,myopia$MYOPIC,alpha=1, 
                              lambda=lambdas, family="binomial")
plot(myopia_models_lasso, xvar = "lambda", label = TRUE)
lasso.cv <- cv.glmnet(myopia_mat,myopia$MYOPIC, alpha=1, 
                      lambda=lambdas, family="binomial")
lasso.cv <- cv.glmnet(myopia_mat,myopia$MYOPIC,alpha=1,
                      lambda=lambdas, family="binomial", type.measure = "auc")
coef(lasso.cv , s = c(1,0.1,0.01,0.001))
lasso.cv$lambda.min
#[1] 0.004861239

#Lasso: Prediction
library(glmnet)
myopia_mat <- model.matrix(MYOPIC~.-ID-STUDYYEAR, data)[,-3]
lambdas <- 10 ^ seq(8,-4,length=250)
myopia_models_lasso <- glmnet(myopia_mat,myopia$MYOPIC,alpha=1, 
                              lambda=lambdas, family="binomial")
predict(myopia_models_lasso, type="coefficients", 
        s = lasso.cv$lambda.min)
preds <- predict(myopia_models_lasso, myopia_mat, type = "response", s = c(lasso.cv$lambda.min, lasso.cv$lambda.1se))
head(preds)
preds <- predict(myopia_models_lasso, myopia_mat, type = "class", s = lasso.cv$lambda.min)

#Evaluate lasso model performance
table(predicted = preds, actual = myopia$MYOPIC)
#actual vs predicted
mean(preds  == myopia$MYOPIC)
#lasso model accuracy 
#[1] 0.8980583

#Evaluate logistic regression assumptions 
#Goodness of fit
pR2(model)
#
sum(residuals(model, type = "pearson")^2)
#[1] 574.3662
deviance(model)
#[1] 303.761
1 - pchisq(deviance(model), df.residual(model))
#[1] 1

#collinearity 
collin <- cor(subset(myopia, select=c(SPHEQ, PARENTS, SPORTHR, GENDER ,ACD , READHR)))
dev.off()
library(corrplot)
corrplot(collin , type="upper")
M <- collin
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(subset(myopia, select=c(SPHEQ, PARENTS, 
                                           SPORTHR, GENDER ,ACD , READHR)))
head(p.mat[, 1:6])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", 
                          "#77AADD", "#4477AA"))

#create correlation plot
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

#Fit GLM model
myopia$READING <- myopia$STUDYHR + myopia$READHR
mnull <- glm(MYOPIC ~1, data = data, family = "binomial")
summary(mnull)
mfull <-glm(MYOPIC~.-ID-STUDYYEAR-AL-DIOPTERHR, data = data,
            family = "binomial")
summary(mfull)
summary(step(mnull, scope=list(lower=mnull,upper=mfull), 
             direction='both' ))
model1 <- glm(formula = MYOPIC ~ SPHEQ + PARENTS + SPORTHR + GENDER 
              + STUDYHR + ACD + READHR, family = "binomial", data = data)

#evaluate GLM model 
With(model, null.deviance - deviance)
With(model, df.null - df.residual)
With (model, pchisq(null.deviance - deviance, df.null - df.residual, 
                    lower.tail = FALSE))

#analyze table of deviance
anova(model, test="Chisq")

#evaluate GLM model accuracy
#set threshold 0.5
fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$MYOPIC)
print(paste('Accuracy',1-misClasificError))
#[1] "Accuracy 0.879032258064516"

#plot ROC curve
library(ROCR)
p <- predict(model, newdata=test, type="response")
pr <- prediction(p, test$MYOPIC)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
#[1] 0.8537769

#split into different train and test set
# 80% of the sample size
smp_size <- floor(0.80 * nrow(data))
# set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]
##threshold
fitted.results <- predict(modelts,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.3,1,0)
misClasificError <- mean(fitted.results != test$MYOPIC)
print(paste('Accuracy',1-misClasificError))
[1] "Accuracy 0.887096774193548"
table(predicted = fitted.results , actual = test$MYOPIC)

modelts <- glm(formula = MYOPIC ~ SPHEQ + PARENTS + SPORTHR + GENDER 
               + STUDYHR + ACD + READHR, family = "binomial", data = train )

#LOGISTIC 
#Performance
table(predicted = fitted.results, actual = test$MYOPIC)
#actual predicted
mean(fitted.results  == test$MYOPIC)
#[1] 0.8790323

#residuals
plot(modelts$residuals)
plot(residuals(modelts, type="deviance") ) 

#quanify optimism
my.valid <- validate(model, method="boot", B=1000)
my.valid
my.calib <- calibrate(model, method="boot", B=1000)
par(bg="white", las=1)
plot(my.calib, las=1)
