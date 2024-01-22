"1 - Do men and women have the same grade average?
2 - Which major factors contribute to test outcomes?
3 - Is there a correlation between reading score and writing score?

Does parents formation depend on ethnicity
Does lunch depend on parents formation

Extra: What would be the best way to improve student scores on each test?
       How effective is the test preparation course? (if taken, what's the average score)"

"QUESTION 1"

setwd( "C:/Users/User/Desktop/Project")

performance <- read.csv("C:/Users/User/Desktop/Project/exams.csv", header = TRUE)
#performance[1:8,]

#Descriptive statistics: variables distribution, mean, standard deviation, correlation
install.packages("glmnet")
library(glmnet)
#For the males:
grade_males_m <- performance[performance$gender == "male", "math.score"]
grade_males_r <- performance[performance$gender == "male", "reading.score"]
grade_males_w <- performance[performance$gender == "male", "writing.score"]

length(grade_males_m)

var_grade_males_m <- var(grade_males_m)
var_grade_males_r <- var(grade_males_r)
var_grade_males_w <- var(grade_males_w)

var_grade_males <- (var_grade_males_w + var_grade_males_r + var_grade_males_m)/3

print(var_grade_males)

mean_grade_males_m <- mean(grade_males_m)
mean_grade_males_r <- mean(grade_males_r)
mean_grade_males_w <- mean(grade_males_w)

mean_grade_males <- (mean_grade_males_w + mean_grade_males_r + mean_grade_males_m)/3

print(mean_grade_males)

sd_grade_males_m <- sd(grade_males_m)
sd_grade_males_r <- sd(grade_males_r)
sd_grade_males_w <- sd(grade_males_w)

sd_grade_males <- (sd_grade_males_w + sd_grade_males_r + sd_grade_males_m)/3

print(sd_grade_males)


#For the females:
grade_females_m <- performance[performance$gender == "female", "math.score"]
grade_females_r <- performance[performance$gender == "female", "reading.score"]
grade_females_w <- performance[performance$gender == "female", "writing.score"]

#VARIANCE

var_grade_females_m <- var(grade_females_m)
var_grade_females_r <- var(grade_females_r)
var_grade_females_w <- var(grade_females_w)

var_grade_females <- (var_grade_females_w + var_grade_females_r + var_grade_females_m)/3

print(var_grade_females)

#MEAN

mean_grade_females_m <- mean(grade_females_m)
mean_grade_females_r <- mean(grade_females_r)
mean_grade_females_w <- mean(grade_females_w)

mean_grade_females <- (mean_grade_females_w + mean_grade_females_r + mean_grade_females_m)/3

print(mean_grade_females)

#STANDARD DEVIATION

sd_grade_females_m <- sd(grade_females_m)
sd_grade_females_r <- sd(grade_females_r)
sd_grade_females_w <- sd(grade_females_w)

sd_grade_females <- (sd_grade_females_w + sd_grade_females_r + sd_grade_females_m)/3

print(sd_grade_females)

#Number of male data
n_grades_male <- length(grade_males_m)
n_grades_female <- length(grade_females_m)

#CORRELATION BETWEEN THEM
grades_females <- (grade_females_r + grade_females_m + grade_females_w)/3
grades_males <- (grade_males_r + grade_males_m + grade_males_w)/3

print(grades_males[1])
print(grade_males_r[1])
print(grade_males_m[1]) 
print(grade_males_w[1])

cor_grades <- cor(grades_males,grades_females[1:n_grades_male])
test_cor_grades <- cor.test(grades_males,grades_females[1:n_grades_male])
print(test_cor_grades)

#CORRELATION = -0,01579
#approximately 0, so the variables are independent

performance[,male]<-as.factor(radon[,male])

male_performance <- performance[performance$gender == male,]
print(male_performance)
print(length(male_performance))
print(length(grades_males))

#ggplot(data = performance, aes(x = grades_males), group = male_performance,fill = male_performance) +
 # geom_histogram(alpha=0.5)+ theme_bw()

#HISTOGRAM OF THE GRADES FROM THE MALE SEX
ggplot(data = NULL) + #in this case, we are analyzing the data from the matrix grades_males and not the main performance
geom_histogram(aes(x = grades_males,y = ..density..),color = "black",fill="blue",alpha=0.5, bins = 40) + #binwidth=3
geom_density(aes(x=grades_males),alpha=1.25,color='pink',size=1)+  
theme_bw()+ #theme_linedraw()
labs(title= 'Histogram of male grades', x='Grades', y='density')+
#+scale_x_continuous(limits = c(0,1)) this is to limit but we don't need it here
geom_vline(xintercept = mean_grade_males, lty=4, color='green')

#HISTOGRAM OF THE GRADES FROM THE FEMALE SEX
ggplot(data = NULL) + 
geom_histogram(aes(x = grades_females,y = ..density..),color = "black",fill="pink",alpha=0.5, bins = 40) + #binwidth=3
geom_density(aes(x=grades_females),alpha=1.25,color='blue',size=1)+  
theme_bw()+
labs(title= 'Histogram of female grades', x='Grades', y='density')+
geom_vline(xintercept = mean_grade_females, lty=4, color='green')

#Q-Q PLOT: TESTING TO SEE IF DISTRIBUTIONS ARE NORMAL

#didn't use log because my distribution didn't have tail, not needed
#also it didn't improve
qqnorm(grades_males, main = "Normal qq-plot", ylab = "Empirical Quantiles", xlab = "Theoretical Quantiles")
qqline(grades_males, col="red", lty=4, lwd=2)

qqnorm(grades_females, main = "Normal qq-plot", ylab = "Empirical Quantiles", xlab = "Theoretical Quantiles")
qqline(grades_females, col="red", lty=4, lwd=2)

#distributions are normal, are independent and have an aproximatly close variance

#T-TEST

t_test <- t.test(grades_females,grades_males)
print(t_test)
t_test$statistic
t_test$p.value

#TESTING THE POWER OF THE TEST
#delta = true difference of means
#sd = standard deviation
#sig.level = significance level
#type and alternative = type of t test
power.t.test(n = length(grades_males), delta = abs(mean_grade_females-mean_grade_males), sd = (sd_grade_males+sd_grade_females)/2, 
             sig.level = 0.05, alternative = "two.sided",type = "two.sample")


#power.t.test(delta = abs(mean_grade_females-mean_grade_males), sd = (sd_grade_males+sd_grade_females)/2, sig.level = 0.05,power = 1,alternative = "two.sided",type ="two.sample")"

install.packages("MKinfer")
library(MKinfer)
perm.t.test(grades_males,grades_females)


wilcox.test(grades_males,grades_females)


#BOOTSTRAP

Nresample <- 5000 #number of samples it generates
means0 <- c()
means1 <- c()
for ( i in 1 : Nresample ) {
  x0_sample_i <- sample(grades_males, length(grades_males), replace=TRUE)
  x1_sample_i <- sample(grades_females, length(grades_females), replace=TRUE)
  means0 <- c(means0, mean(x0_sample_i))
  means1 <- c(means1, mean(x1_sample_i))
}

mean_means_male <- mean(means0)
mean_means_female <- mean(means1)
sd_means_male <- sd(means0)
sd_means_female <- sd(means1)
ci_means_male = 1.96*sd_means_male
ci_means_female = 1.96*sd_means_female


#PLOT OF BOOTSTRAP
ggplot(data = NULL) + 
  geom_histogram(aes(x = means0,y = ..density..),color = "black",fill="blue",alpha=0.5, bins = 70)+ 
  geom_density(aes(x=means0),alpha=1.25,color='orange',size=1)+ theme_bw()+
  labs(title= ' ', x='Grades', y='density')+
  geom_vline(xintercept = mean_means_male, lty=4, color='purple')+
  geom_histogram(aes(x = means1,y = ..density..),color = "black",fill="pink",alpha=0.5, bins = 70)+ 
  geom_density(aes(x=means1),alpha=1.25,color='orange',size=1)+ theme_bw()+
  geom_vline(xintercept = mean_means_female, lty=4, color='purple')

meanofmeans_BF_difference <- abs(mean_means_male-mean_means_female)
print(meanofmeans_BF_difference)
meanofmeans_BF_difference_ci_95 <- ci_means_female+ci_means_male
print(meanofmeans_BF_difference_ci_95)

#CONCLUSION: GRADES FEMALES > GRADES MALES

#WHICH FACTOR CONTRIBUTES THE MOST TO THE FINAL GRADE
#performance$gender
#performance$race.ethnicity
#performance$parental.level.of.education
#performance$lunch
#performance$test.preparation.course

nlevels(as.factor(bodyperf$race.ethnicity))
nlevels(as.factor(bodyperf$parental.level.of.education))
nlevels(as.factor(bodyperf$lunch))
nlevels(as.factor(bodyperf$performance$test.preparation.course))

#mean()
#var()
#std()
#summary()
#size()






race_males <- as.factor(performance[performance$gender == "male", "race.ethnicity"])
race_females <- performance[performance$gender == "female", "race.ethnicity"]

p_education_males <- as.factor((performance[performance$gender == "male", "parental.level.of.education"]))
p_education_females <- performance[performance$gender == "female", "parental.level.of.education"]

lunch_males <- as.factor((performance[performance$gender == "male", "lunch"]))
lunch_females <- performance[performance$gender == "female", "lunch"]

preparation_males <- as.factor((performance[performance$gender == "male", "test.preparation.course"]))
preparation_females <- performance[performance$gender == "female", "test.preparation.course"]

#LM REGRESSION 
out <- lm(grade_males~race_males+preparation_males+lunch_males+p_education_males,data=performance)
summary(out)

str(performance)

x <- model.matrix(grades_males ~ race_males + lunch_males + p_education_males + preparation_males -1, data=performance)
race_males <- model.matrix(~(race_males),data=performance)
p_education_males <- model.matrix(~(p_education_males),data=performance)
lunch_males <- model.matrix(~(lunch_males),data=performance)
preparation_males <- model.matrix(~(preparation_males),data=performance)

length(race_males)
length(lunch_males)
length(p_education_males)

race_females <- as.model.matrix(~(race_females),data=performance)
p_education_females <- model.matrix(~(p_education_females),data=performance)
lunch_females <- model.matrix(~(lunch_females),data=performance)
preparation_females <- model.matrix(~(preparation_females),data=performance)

#res0 <- model.matrix(~parental.level.of.education,data=performance)

data_males <- data.frame(race_males = race_males,p_education_males = p_education_males, lunch_males = lunch_males, preparation_males = preparation_males)
data_females <- data.frame(race_females=race_females,p_education_females=p_education_females,lunch_females=lunch_females,preparation_females=preparation_females)

elnet_mse(grades_males, data_males)
elnet_mse(grades_females, data_females)

#value = 0 for male grades vs. others
#value = 0 for female grades vs. others 

model <- elnet_coef(grades_males, data_males, 0.5)
elnet_coef(grades_females, data_females, 0.0)

model

summary(model)

str(performance)


#res <- model.matrix(~(race.ethnicity),data=performance)
#head(res[,-1])

#res0 <- model.matrix(~parental.level.of.education,data=performance)
#head(res[,-1])

#levels(performance)

#performance$

#performance[,"race.ethnicity"]<-as.factor(performance[,"race.ethnicity"])
#performance[,"parental.level.of.education"]<-as.factor(performance[,"parental.level.of.education"])
#race <- performance[,"race.ethnicity"]
#education <- performance[,"parental.level.of.education"]
#model2 <- lm(formula = education ~ race, data = performance)

#summary(model2)
#Anova(model2)




#function that determines mse of elastic net regression for 10 different
#values of alpha
elnet_mse <- function(Y, dataframe,p){
  train_rows <- sample(1:nrow(dataframe), 0.7*nrow(dataframe)) #p% of data in training set
  x.train <- as.matrix(dataframe[train_rows, ])
  x.test <- as.matrix(dataframe[-train_rows, ])
  y.train <- Y[train_rows]
  y.test <- Y[-train_rows]
  
  #let's test for several alpha values
  alphafits <- list()
  for (i in 0:10){
    fit <- paste0('alpha', i/10)
    alphafits[[fit]] <- cv.glmnet(x.train,y.train, type.measure="mse", alpha=i/10, family='gaussian')
  }
  
  results <-data.frame()
  for (i in 0:10){
    fit <- paste0('alpha', i/10)
    predicted <- predict(alphafits[[fit]], s=alphafits[[fit]]$lambda.1se, newx = x.test)
    mse <- mean((y.test-predicted)^2)
    temporary <- data.frame(alpha=i/10, mse=mse, fit=fit)
    results <- rbind(results, temporary)
  }
  return(results)
}

#function that determines coefficients of elastic net regression
elnet_coef <- function(Y, dataframe, alpha){
  train_rows <- sample(1:nrow(dataframe), .7*nrow(dataframe)) #70% of data in training set
  
  x.train <- as.matrix(dataframe[train_rows, ])
  x.test <- as.matrix(dataframe[-train_rows, ])
  y.train <- Y[train_rows]
  y.test <- Y[-train_rows]
  cv <- cv.glmnet(x.train,y.train, type.measure="mse", alpha=alpha, family='gaussian')
  
  elnet_fit <- glmnet(x.train,y.train, alpha=alpha, type.multinomial = "grouped",family='multinomial', lambda=cv$lambda.1se,standardize=FALSE )
  coefs <- coef(elnet_fit)
  return(coefs)
}


#start with Ridge regression (alpha=0)
ridge.fit <- cv.glmnet(x.train,y.train, type.measure="mse", alpha=0, family='gaussian')
ridge.predicted <- predict(ridge.fit, s=ridge.fit$lambda.1se, newx = x.test)
ridge.mse <- mean((y.test-ridge.predicted)^2)
coef(ridge.fit)

#Lasso regression
lasso.fit <- cv.glmnet(x.train,y.train, type.measure="mse", alpha=1, family='gaussian')
lasso.predicted <- predict(lasso.fit, s=lasso.fit$lambda.1se, newx = x.test)
lasso.mse <- mean((y.test-lasso.predicted)^2)

#Elastic net 
elnet.fit <- cv.glmnet(x.train,y.train, type.measure="mse", alpha=0.5, family='gaussian')
elnet.predicted <- predict(elnet.fit, s=elnet.fit$lambda.1se, newx = x.test)
elnet.mse <- mean((y.test-elnet.predicted)^2)