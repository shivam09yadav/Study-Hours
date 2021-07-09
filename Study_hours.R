############################ INFORMATION #######################################


#                           SHIVAM YADAV

#                  Prediction using Supervised ML in R
#       Predict the percentage of an student based on the no. of study hours.
#        What will be predicted score if a student studies for 9.25 hrs/ day?  


########################### Packages: ###########################################
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(ModelMetrics)

############################## Loading the data: ###################################
Hours <- read.csv("C://Users//shiva//OneDrive//Desktop//Sparks Foundation//Task 1/StudyHours.csv")
View(Hours)

########################## Exploratory Data Analysis: ##########################
str(Hours)

head(Hours)

summary(Hours)

# Minimum hour of studying is 1 hour.
# Maximum hour of studying is 9 hour.
# Minimum score is 17.
# Maximum score is 95.

hist(Hours$Hours)

# 3 students studied less than 2 hours.
# 4 students studied between 2-3 hours.
# 4 students studied between 3-4 hours.
# 2 students studied between 4-5 hours.
# 3 students studied between 5-6 hours.
# 2 students studied between 6-7 hours.
# 3 students studied between 7-8 hours.
# 3 students studied between 8-9 hours.
# 1 student studied between 9-10 hours.

hist(Hours$Scores)

# 2 students scored less than 20.
# 7 students scored between 20-30.
# 1 student scored between 30-40.
# 3 students scored between 40-50.
# 2 students scored between 50-60.
# 3 students scored between 60-70.
# 2 students scored between 70-80.
# 4 students scored between 80-90.
# 1 student scored more than 90.

# Checking the linearity:
plot(Hours ~ Scores, data = Hours)

# The relationship looks roughly linear, so we can proceed with the linear model.

# Visualize the Data:
# Scatter plot:
scatter.smooth(Hours$Hours, Hours$Score, main='Hours studied vs. Exam Score',xlab ="Hours",ylab ="Score")

# Correlation Analysis:
library("corrplot")
Hours <- Hours[, c(1:2)]
head(Hours)

sapply(Hours , class)

#correlation values of the attributes of our data
Hours.cor = cor(Hours)
corrplot(Hours.cor)

cor(Hours$Hours,Hours$Scores)

# There is a strong positive correlation between Hours and Scores(0.98).

###################### Splitting the data ######################################
set.seed(123) #setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(Hours) , 0.7*nrow(Hours))
training_set <- Hours[trainingRowIndex, ] #model training data
test_set <- Hours[-trainingRowIndex, ] #test data

#################### Building the model on trainingset: #########################
Lrmodel <- lm(Scores~Hours , data = training_set)
Lrmodel
summary(Lrmodel)
coefficients(Lrmodel)

#(Intercept)       Hours 
# 2.508894        10.044532

################### Visualize the results with a graph(Training_set): ########################
Hours_graph<-ggplot(Lrmodel, aes(x=Hours, y=Scores))+
  geom_point()

Hours_graph <- Hours_graph + geom_smooth(method="lm", col="blue")

Hours_graph <- Hours_graph +
  stat_regline_equation(label.x = 3, label.y = 7)

Hours_graph +
  theme_bw() +
  labs(title = "Hours V/S Scores",
       x = "Hours",
       y = "Scores")


################### Predicting the model on testset: ###########################
Lrpred <- predict(Lrmodel , test_set)
Lrpred

################## Prediction accuracy and error rates: ########################
actual_preds <- data.frame(cbind(actuals = test_set$Scores , predicts = Lrpred))
actual_preds

################ To compare Predicted value with actual value: #################
plot(test_set$Scores, type = "l" , lty = 1.8 , col = "yellow")
lines(Lrpred , type = "l" , col = "Blue")

############################ Validation ########################################
mae(test_set$Scores,Lrpred)

############### Predicted score if a student studies 9.25hr/day: ################
Student_score = 2.509 + 10*9.25  
Student_score


