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
trainDf<- na.omit(df1[train,])

### LDA
lda.fit <- lda(threat ~ age + gender + flee + signs_of_mental_illness + armedOrUnarmed, data = df1, subset = train)

ldaPred <- data.frame(predict(lda.fit,testDf))

library(gridExtra)
pl1 <- ggplot(ldaPred) + geom_histogram(mapping = aes(x=x.LD1)) + facet_wrap(~ class)
pl2 <- ggplot(ldaPred) + geom_histogram(mapping = aes(x=x.LD2)) + facet_wrap(~ class)
grid.arrange(pl1,pl2, nrow =2)
pl3 <- ggplot(ldaPred) + geom_boxplot(mapping = aes(x=class, y=x.LD1))
pl4 <- ggplot(ldaPred) + geom_boxplot(mapping = aes(x=class, y=x.LD2))
grid.arrange(pl3,pl4, nrow =2)

ggplot(ldaPred, mapping = aes(x.LD1, x.LD2, color = class)) + geom_point(alpha = 0.5) + theme_minimal()

testGroup = testDf$threat
ldaClass=ldaPred$class
table(ldaClass, testGroup)
mean(ldaClass == testGroup)

## Thresholds
# want to better predict 'other'
threshPred <- ldaPred %>% dplyr::mutate(newClass = ifelse(posterior.other > .35, 2, ldaClass))
table(threshPred$newClass, testGroup)

#k-fold 
kLDA <- function(k, formula, data){
  groups <- cut(1:nrow(df1), k, labels=FALSE)[sample(1:nrow(df1))]
  pred <- lapply(1:k, function(i, formula, data){
    loo = which(groups == i)
    z = lda(formula,data[-loo,])
    predict(z,data[loo,])},
    formula,data)
  
  return(pred)
}

kLDA(5, threat ~ age + gender + flee, df1)