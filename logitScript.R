library(tidyverse)
library(data.world)
library(MASS)

### IMPORT DATA
project <- "https://data.world/devtl/fatalpoliceshootings/"
df <- data.world::query(data.world::qry_sql("SELECT * FROM fatalPoliceShootingsCleaned"), dataset = project)
df <- df %>% as.tibble(df)
### MAP FACTORS TO HIERARCHICAL VALUES

predictors <- df%>%dplyr::select("armed","age","gender","race","signs_of_mental_illness","flee")

# signs of mental illness: 1=T, 0=F
x <- factor(rep(predictors$signs_of_mental_illness))
levels(x) <- 0:1
predictors$signs_of_mental_illness = x

# fleeing: 3=car, 2=foot, 1=other, 0=not fleeing
y <- factor(rep(predictors$flee))
levels(y) = c(3,2,0,1)
predictors$flee <- y

# gender: 1=M, 0=F
w <- factor(rep(predictors$gender))
levels(w) <- 0:1
predictors$gender = w

# armed=1 or unarmed=0
z <- unlist(
  lapply(
    predictors$armed, function(s){ifelse(s == "unarmed", as.integer(0), as.integer(1))}
  )
)
predictors <- predictors %>% mutate(armedOrUnarmed = z)

### TRAIN/TEST 
df1 <- cbind(threat = df$threat_level,predictors)

# after filtering out a class level, we now use the remaining data to train and test the model
df2 <- filter(df1, threat != "undetermined")
train2 <- sample(1:nrow(df2), 0.9*nrow(df2))
testDf2 <- na.omit(df2[-train2,])
testDf2$threat <- factor(testDf2$threat) # removes "undetermined" level from testDf2

### LOG REGRESSION
glm.fit = glm(threat ~ age + gender + flee + signs_of_mental_illness + armedOrUnarmed, 
            data = df2, 
            family = binomial, 
            subset = train2)

summary(glm.fit)
# label classes based on probability threshold for 'other'
glm.pred <- rep("attack", length(glm.probs))
glm.pred[glm.probs > 0.45] <- "other"

testGroup = testDf2$threat
table(glm.pred, testGroup)
mean(glm.pred == testGroup)


### ROC curve
library(pROC)

glmPred <- predict(glm.fit, newdata = testDf2)

glmPred <- lapply(glmPred, function(x){ifelse(x>0, 2, 1)}) # 1=attack, 2=other

ROCPred <- as.vector(glmPred, mode = "numeric")

ROC <- roc(testGroup, as.numeric(ROCPred))

plot(ROC, main="ROC curve Theat Levels", xlab="Specificity (False Positives)", 
     ylab="Sensitivity (True Positives)")    

