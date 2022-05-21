# Pre-processing exercises

# 3.1 Creating dummy variables

Packages <- c("caret", "AppliedPredictiveModeling", "tidyverse")
lapply(Packages, library, character.only = TRUE)

dummiesmpg <- dummyVars(hwy~., data = mpg)
mpgdummies <- predict(dummiesmpg, newdata = mpg)
mpgdummies <- as.data.frame(mpgdummies)

mpgdummies

dim(mpgdummies) # 234 83

# 3.2 Zero and near zero variance predictors

nzv2 <- nearZeroVar(mpgdummies, saveMetrics = TRUE)
#54

filtered_mpg_dummies <- mpgdummies[,-nzv2]
dim(filtered_mpg_dummies) # 234 29

# 3.3 Identifying and Removing Highly Correlated Predictors

mpg_dummies_correlation <- cor(filtered_mpg_dummies)
summary(mpg_dummies_correlation)

high_corr_mpg <- sum(abs(mpg_dummies_correlation[upper.tri(mpg_dummies_correlation)])>0.75)
summary(high_corr_mpg[upper.tri(high_corr_mpg)])

descrCor3 <- cor(filtered_mpg_dummies)
highCorr3 <- sum(abs(descrCor3[upper.tri((descrCor3))]) >0.75)

summary(highCorr3)
summary(descrCor3[upper.tri(descrCor3)])

# We clearly have correlations about 0.75

highlyCorDescr3 <- findCorrelation(descrCor3, cutoff = 0.75)
filtered_mpg_dummies <- filtered_mpg_dummies[,-highlyCorDescr3]

descrCor4 <- cor(filtered_mpg_dummies)
summary(descrCor4[upper.tri((descrCor4))])

#### Linear Dependencies

comboInfo1 <- findLinearCombos(filtered_mpg_dummies)
comboInfo1

### PreProcess Centering and Scaling

inTrain <- sample(seq(along = filtered_mpg_dummies, length(filtered_mpg_dummies)/2))
inTrain

train <- filtered_mpg_dummies[inTrain,]
test <- filtered_mpg_dummies[-inTrain,]

preProcValues3 <- preProcess(train, method = c("center", "scale"))

trainTransformed1 <- predict(preProcValues3, train)
testTranformed1 <- predict(preProcValues3, test)

trainTransformed1
testTranformed1
