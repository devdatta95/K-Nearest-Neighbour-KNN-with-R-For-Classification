
data <- read.csv("C:/Users/LENOVO/Desktop/R programs/Logistic_Regressioin_Diabetes_dataset.csv")
head(data)
str(data)
names(data)

##############################################################
#                   DATA PRE-PROCESSING                      #
##############################################################

data$Pregnancies <- as.numeric(data$Pregnancies)
data$Glucose <- as.numeric(data$Glucose)
data$BloodPressure <- as.numeric(data$BloodPressure)
data$SkinThickness <-as.numeric(data$SkinThickness)
data$Insulin <- as.numeric(data$Insulin)
data$Age <- as.numeric(data$Age)
data$Outcome <- as.factor(data$Outcome)
str(data)

summary(data)
boxplot(data)
sapply(data, function(x) sum(is.na(x)))

############################ Pregnancies ####################
boxplot(data$Pregnancies)
summary(data$Pregnancies)
upper <- 6.0 + 1.5 * IQR(data$Pregnancies)
upper

data$Pregnancies[data$Pregnancies > upper] <- upper
boxplot(data$Pregnancies)

############################ Glucose ####################
boxplot(data$Glucose)
summary(data$Glucose)


lower <- 99 - 1.5 * IQR(data$Glucose)
lower
data$Glucose[data$Glucose < lower] <- lower
boxplot(data$Glucose)

############################ BloodPressure ####################
boxplot(data$BloodPressure)
summary(data$BloodPressure)
upper <- 80 + 1.5 * IQR(data$BloodPressure)
upper


data$BloodPressure[data$BloodPressure > upper] <- upper
boxplot(data$BloodPressure)

lower <- 62 - 1.5 * IQR(data$BloodPressure)
lower
data$BloodPressure[data$BloodPressure < lower] <- lower
boxplot(data$BloodPressure)
############################ SkinThickness ####################
boxplot(data$SkinThickness)
summary(data$SkinThickness)
upper <- 32 + 1.5 * IQR(data$SkinThickness)
upper


data$SkinThickness[data$SkinThickness > upper] <- upper
boxplot(data$SkinThickness)

############################ Insulin ####################
boxplot(data$Insulin)
summary(data$Insulin)
upper <- 127 + 1.5 * IQR(data$Insulin)
upper


data$Insulin[data$Insulin > upper] <- upper
boxplot(data$Insulin)


############################ BMI ####################
boxplot(data$BMI)
summary(data$BMI)
upper <- 36.60 + 1.5 * IQR(data$BMI)
upper


data$BMI[data$BMI > upper] <- upper
boxplot(data$BMI)

lower <- 27.30 - 1.5 * IQR(data$BMI)
lower
data$BMI[data$BMI < lower] <- lower
boxplot(data$BMI)

###################### DiabetesPedigreeFunction ####################

boxplot(data$DiabetesPedigreeFunction)
summary(data$DiabetesPedigreeFunction)
upper <- 0.6262 + 1.5 * IQR(data$DiabetesPedigreeFunction)
upper


data$DiabetesPedigreeFunction[data$DiabetesPedigreeFunction > upper] <- upper
boxplot(data$DiabetesPedigreeFunction)

############################ Age ####################

boxplot(data$Age)
summary(data$Age)
upper <- 41 + 1.5 * IQR(data$Age)
upper


data$Age[data$Age > upper] <- upper
boxplot(data$Age)

boxplot(data)



##############################################################
#                   DATA NORMALIZATIONN                      #
##############################################################

# method 1 when you take a sqrt of n for k

table(data$Outcome) 

normalize <- function(x) {
  return((x - min(x)) / (max(x)-min(x)))}
summary(data$Insulin)
hist(data$Insulin)
data_n <- as.data.frame(lapply(data[1:8], normalize))
summary(data_n$Insulin)
hist(data_n$Insulin)


##############################################################
#                   DATA PARTIION                            #
##############################################################

data_train <- data_n[1:540,]
data_test <- data_n[541:768,]
data_train_labels <- data[1:540, 9]
data_test_labels <- data[541:768,9]

##############################################################
#                  MANUAL METHOD FOR K VALUE                 #
##############################################################

library(class)

k=sqrt(nrow(data))
K

data_test$data_test_pred <- knn(train = data_train, test = data_test,cl= data_train_labels, k = 27)
library(e1071)
library(caret)
confusionMatrix(data_test$data_test_pred,data_test_labels)


##############################################################
#                    K FOLD CROSS VALLIDATION                #
##############################################################

# method 2 

library(caret)
Train <- createDataPartition(data$Outcome, p=0.7, list = FALSE)
training <- data[Train ,]
testing <- data[-Train, ]


library(e1071)
trControl <- trainControl(method = "repeatedcv", number = 10,repeats = 3)

knn_fit <- train(Outcome ~ ., 
                 data = training , 
                 method ="knn",
                 tuneLength= 20,
                 trainControl=trControl,
                 preProcess= c("center","scale"))

knn_fit

plot(knn_fit)

test_pred <- prediction(knn_fit, newdata= testing)
test_pred

confusionMatrix(test_pred, testing$Outcome)






