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
train <- sample(1:nrow(df1), 0.9*nrow(df1))
testDf <- na.omit(df1[-train,])


### QDA
qda.fit <- qda(threat ~ age + gender + flee + signs_of_mental_illness + armedOrUnarmed, data = df1, subset = train)

qdaPred <- data.frame(predict(qda.fit,testDf))

qdaClass <- qdaPred$class
testGroup = testDf$threat
table(qdaClass, testGroup)
mean(qdaClass == testGroup)
