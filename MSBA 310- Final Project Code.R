# heart_disease = read.csv("/Users/dimadaouk/Desktop/MSBA310- Project/Dataset/heart_disease_health_indicators_BRFSS2015.csv")
heart_disease = read.csv("/Users/dimadaouk/Desktop/Fall 2021/MSBA 310- Applied Statistical Analysis/MSBA310- Project/Dataset/heart_disease_health_indicators_BRFSS2015.csv")

# Importing Libraries
library(carData)
library(car)
library(PresenceAbsence)
library(rpart)
library(rpart.plot)
library(broom)
library(tidyr)

# Exploring the data
dim(heart_disease)
names(heart_disease)
str(heart_disease)  

# Converting Categorical Variables into Factors
#heart_disease$HeartDiseaseorAttack = factor(heart_disease$HeartDiseaseorAttack, levels = c(0,1)) #labels = c("No Heart Disease/ Attack", "Heart Disease/Attack"))

heart_disease$HighBP = factor(heart_disease$HighBP, levels = c(0,1), labels = c("Normal Blood Pressure", "High Blood Pressure"))

heart_disease$HighChol = factor(heart_disease$HighChol, levels = c(0,1), labels = c("Normal Cholesterol Level", "High Cholesterol Level"))

heart_disease$CholCheck = factor(heart_disease$CholCheck, levels = c(0,1), labels = c("Not Checked Cholesterol in the Past 5 Years", "Checked Cholesterol in the Past 5 Years"))

heart_disease$Smoker = factor(heart_disease$Smoker, levels = c(0,1), labels = c("Not a Smoker", "Smoker"))

heart_disease$Stroke = factor(heart_disease$Stroke, levels = c(0,1), labels = c("Did Not Previously Have a Stroke", "Previously Had a Stroke"))

heart_disease$Diabetes = factor(heart_disease$Diabetes, levels = c(0,1,2), labels = c("Not Diabetic", "Pre-Diabetic", "Diabetic"))

heart_disease$PhysActivity = factor(heart_disease$PhysActivity, levels = c(0,1), labels = c("No Physical Activity", "Does Physical Activity"))

heart_disease$Fruits = factor(heart_disease$Fruits, levels = c(0,1), labels = c("Doesn't Consume Fruits Per Day", "Consumes At least 1 Fruit Per Day"))

heart_disease$Veggies = factor(heart_disease$Veggies, levels = c(0,1), labels = c("Doesn't Consume Vegetables Per Day", "Consumes At least 1 Vegetable Per Day"))

heart_disease$HvyAlcoholConsump = factor(heart_disease$HvyAlcoholConsump, levels = c(0,1), labels = c("Not a Heavy Drinker", "Heavy Drinker"))

heart_disease$AnyHealthcare = factor(heart_disease$AnyHealthcare, levels = c(0,1), labels = c("No Healthcare Coverage", "Has Healthcare Coverage"))

heart_disease$NoDocbcCost = factor(heart_disease$NoDocbcCost, levels = c(0,1), labels = c("No", "Yes"))

heart_disease$GenHlth = factor(heart_disease$GenHlth, levels = c(1,2,3,4,5), labels = c("Excellent", "Good", "Average", "Less than Average", "Poor"))

heart_disease$DiffWalk = factor(heart_disease$DiffWalk, levels = c(0,1), labels = c("No Difficulty Walking", "Difficulty Walking"))

heart_disease$Sex = factor(heart_disease$Sex, levels = c(0,1), labels = c("Male", "Female"))

heart_disease$Age = factor(heart_disease$Age, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13), labels = c("18-24", "25-30", "30-35", "35-40", "40-45", "45-50", "50-55", "55-60",
                                                                                                    "60-65", "65-70", "70-75", "75-80", "80+"))

heart_disease$Education = factor(heart_disease$Education, levels = c(1,2,3,4,5,6), labels = c("Never Attended School", "Up to Lower Elementary School", "Up to Upper Elementary School",
                                                                                              "Up to Middle School", "Up to High School", "College 4 Years or More"))

heart_disease$Income = factor(heart_disease$Income, levels = c(1,2,3,4,5,6,7,8), labels = c("< $10,000", "$10,000 - $20,000", "$20,000 - $30,000", "$30,000 - $40,000", "$40,000 - $50,000",
                                                                                            "$50,000 - $60,000", "$60,000 - $70,000", ">$75,000"))
str(heart_disease)

# Chi-Square
# Assessing the association between Heart Disease and High Blood Pressure
tbl = table(heart_disease$HeartDiseaseorAttack, heart_disease$HighBP)
tbl

prop_table = prop.table(tbl)
prop_table

chisq.test(tbl)
barplot(prop_table, beside = T, col = c("Blue", "Red"), ylim = c(0,0.6), las = 1, main = "Visual Association Between Heart Disease and Blood Pressure")
legend("topright", legend = rownames(prop_table), cex = 0.6, fill = c("Blue", "Red"))

# Assessing the association between Heart Disease and Age
tbl2 = table(heart_disease$HeartDiseaseorAttack, heart_disease$Age)
tbl2

prop_table2 = prop.table(tbl2)
prop_table2

chisq.test(tbl2)
barplot(prop_table2, beside = T, col = c("Blue", "Red"), ylim = c(0,0.14), las = 1, main = "Visual Association between Heart Disease and Age")
legend("topright", legend = rownames(prop_table2), cex = 0.6, fill = c("Blue", "Red"))

# Assessing the association between Heart Disease and Sex
tbl3 = table(heart_disease$HeartDiseaseorAttack, heart_disease$Sex)
tbl3

prop_table3 = prop.table(tbl3)
prop_table3

chisq.test(tbl3)
barplot(prop_table3, beside = T, col = c("Blue", "Red"), ylim = c(0,0.6), las = 1, main = "Visual Association between Heart Disease and Gender")
legend("topright", legend = rownames(prop_table3), cex = 0.6, fill = c("Blue", "Red"))

# Assessing the association between Heart Disease and the numerical variables: Graphically and By ANOVA test

boxplot(heart_disease$BMI~heart_disease$HeartDiseaseorAttack, ylab = "BMI", xlab = "Heart Disease/ Attack", col = "red", main = "Visual Association between Heart Disease and BMI")
ANOVA_test1 = aov(heart_disease$BMI~heart_disease$HeartDiseaseorAttack)
summary(ANOVA_test1)

boxplot(heart_disease$MentHlth~heart_disease$HeartDiseaseorAttack, col = "red", ylab = "Mental Health", xlab = "Heart Disease/ Attack", main = "Visual Association between Heart Disease and Mental Health")
ANOVA_test2 = aov(heart_disease$MentHlth~heart_disease$HeartDiseaseorAttack)
summary(ANOVA_test2)

prop.table(table(heart_disease$HeartDiseaseorAttack, heart_disease$MentHlth), 1)

boxplot(heart_disease$MentHlth~heart_disease$HeartDiseaseorAttack, col = "red", ylim = c(0,10), ylab = "Mental Health", xlab = "Heart Disease/ Attack", main = "Visual Association between Heart Disease and Mental Health")
ANOVA_test2 = aov(heart_disease$MentHlth~heart_disease$HeartDiseaseorAttack)
summary(ANOVA_test2)

boxplot(heart_disease$PhysHlth~heart_disease$HeartDiseaseorAttack, col = "red", ylab = "Physical Health", xlab = "Heart Disease/ Attack", main = "Visual Association between Heart Disease and Physical Health")
ANOVA_test3 = aov(heart_disease$PhysHlth~heart_disease$HeartDiseaseorAttack)
summary(ANOVA_test3)

# Splitting the data

split = sample(1:2, nrow(heart_disease), replace = TRUE, prob = c(0.70,0.30))
train = heart_disease[split == 1, ]
val = heart_disease[split == 2, ]

dim(train)
dim(val)

# Logistic Regression

# Model1
Model1 =  glm(HeartDiseaseorAttack~HighBP+ HighChol + CholCheck + BMI + Smoker + Stroke + Diabetes + PhysActivity + Fruits + 
                Veggies + HvyAlcoholConsump + AnyHealthcare + NoDocbcCost + GenHlth + MentHlth + PhysHlth + DiffWalk + Sex + 
                Age + Education + Income, data = train, family = "binomial")
summary(Model1)

# Odds Ratios for Model1
OR_vec1=exp(Model1$coefficients)
OR_vec1

# Confidence Interval for Odds Ratios for Model1
CI_OR1= exp(confint.default(Model1))
CI_OR1

# Predicting/ Classifying New Cases for Model1
# Gives the probability of each record in the validation data
predict1 = predict(Model1, newdata= val, type="response")
predict1

# Analyzing the Performance of Model 1 

# Data frame for actual and predicted for Model1
act_pred1=data.frame(ID=1:nrow(val),val$HeartDiseaseorAttack,predict1)
act_pred1

# Confusion matrix for Model1
conf_mat1=cmx(act_pred1)
conf_mat1

# Overall accuracy for Model1
total_acc1= pcc(conf_mat1) 
total_acc1

# Sensitivity for Model1
sens1=sensitivity(conf_mat1)
sens1

# Specificity for Model1
spec1=specificity(conf_mat1)
spec1

# Area under the ROC curve for Model1
auc(act_pred1) 

# Plot the ROC curve for Model1
auc.roc.plot(act_pred1, col = "red") 

# Model2: removing the insignificant predictors from Model1 and re-leveling
Model2 = glm(HeartDiseaseorAttack~HighBP+ HighChol + relevel(as.factor(CholCheck), ref = "Not Checked Cholesterol in the Past 5 Years") + 
               Smoker + Stroke + relevel(as.factor(Diabetes), ref = "Not Diabetic") + 
               HvyAlcoholConsump + NoDocbcCost + relevel(as.factor(GenHlth), ref = "Good") + DiffWalk + Sex + 
               relevel(as.factor(Age), ref = "60-65")  + relevel(as.factor(Income), ref = ">$75,000"), data = train, family = "binomial")
summary(Model2)

# Odds Ratios for Model2
OR_vec2=exp(Model2$coefficients)
OR_vec2

# Confidence Interval for Odds Ratios for Model2
CI_OR2= exp(confint.default(Model2))
CI_OR2

# Predicting/ Classifying new cases for Model2
# Gives the probability of each record in the validation data
predict2 = predict(Model2, newdata= val, type="response")
predict2

# Analyzing the performance of Model 2

# Data frame for actual and predicted for Model2
act_pred2=data.frame(ID=1:nrow(val),val$HeartDiseaseorAttack,predict2)
act_pred2

# Confusion matrix for Model2
conf_mat2=cmx(act_pred2)
conf_mat2

# Overall accuracy for Model2
total_acc2= pcc(conf_mat2) 
total_acc2

# Sensitivity for Model2
sens2=sensitivity(conf_mat2)
sens2

# Specificity for Model2
spec2=specificity(conf_mat2)
spec2

# Area under the ROC curve for Model2
auc(act_pred2) 

# Plot the ROC curve for Model2
auc.roc.plot(act_pred2, col = "red") 

# Model3: removing GenHlth
Model3 = glm(HeartDiseaseorAttack~HighBP+ HighChol + relevel(as.factor(CholCheck), ref = "Not Checked Cholesterol in the Past 5 Years") + 
               Smoker + Stroke + relevel(as.factor(Diabetes), ref = "Not Diabetic") + 
               HvyAlcoholConsump + NoDocbcCost + DiffWalk + Sex + relevel(as.factor(Age), ref = "60-65") +
               relevel(as.factor(Income), ref = ">$75,000"), data = train, family = "binomial")
summary(Model3)

# Odds Ratios for Model3
OR_vec3=exp(Model3$coefficients)
OR_vec3

# Confidence Interval for Odds Ratios for Model3
CI_OR3= exp(confint.default(Model3))
CI_OR3

# Predicting/ Classifying new cases for Model3
#Gives the probability of each record in the validation data
predict3 = predict(Model3, newdata= val, type="response")
predict3

# Analyzing the performance of Model 3

# Data frame for actual and predicted for Model3
act_pred3=data.frame(ID=1:nrow(val),val$HeartDiseaseorAttack,predict3)
act_pred3

# Confusion matrix for Model3
conf_mat3=cmx(act_pred3)
conf_mat3

# Overall accuracy for Model3
total_acc3= pcc(conf_mat3) 
total_acc3

# Sensitivity for Model3
sens3=sensitivity(conf_mat3)
sens3

# Specificity for Model3
spec3=specificity(conf_mat3)
spec3

# Area under the ROC curve for Model3
auc(act_pred3) 

# Plot the ROC curve for Model3
auc.roc.plot(act_pred3, col = "red")

# Model4: including only the *** significant predictors from Model 1 and re-leveled
Model4 = glm(HeartDiseaseorAttack~HighBP+ HighChol + relevel(as.factor(CholCheck), ref = "Not Checked Cholesterol in the Past 5 Years") + 
               Smoker + Stroke + HvyAlcoholConsump + NoDocbcCost + relevel(as.factor(GenHlth), ref = "Good") + DiffWalk + Sex + 
               relevel(as.factor(Age), ref = "60-65"), data = train, family = "binomial")

summary(Model4)

# Odds Ratios for Model4
OR_vec4=exp(Model4$coefficients)
OR_vec4

# Confidence Interval for Odds Ratios for Model4
CI_OR4= exp(confint.default(Model4))
CI_OR4

# Predicting/ Classifying new cases for Model4
# Gives the probability of each record in the validation data
predict4 = predict(Model4, newdata= val, type="response")
predict4

# Analyzing the performance of Model 4

# Data frame for actual and predicted for Model4
act_pred4=data.frame(ID=1:nrow(val),val$HeartDiseaseorAttack,predict4)
act_pred4

# Confusion matrix for Model4
conf_mat4=cmx(act_pred4)
conf_mat4

# Overall accuracy for Model4
total_acc4= pcc(conf_mat4) 
total_acc4

# Sensitivity for Model4
sens4=sensitivity(conf_mat4)
sens4

# Specificity for Model4
spec4=specificity(conf_mat4)
spec4

# Area under the ROC curve for Model4
auc(act_pred4) 

# Plot the ROC curve for Model4
auc.roc.plot(act_pred4, col = "red") 

# Plotting the 4 ROC Curves

ROC_b4os=data.frame(ID=1:nrow(val),val$HeartDiseaseorAttack,predict1,predict2,predict3,predict4)

auc.roc.plot(ROC_b4os,col=c(2,3,4,5),line.type=c(1,2,3,4),threshold = 1001, main="ROC Curves", legend.text=c("Model 1", "Model 2", "Model 3", "Model 4"))

# Comparing the Overall Accuracy, Sensitivity, Specificity for Models 1,2,3,4
model_number = rbind("Model1", "Model2", "Model3", "Model4")
performance_comparison = rbind(total_acc1, total_acc2, total_acc3, total_acc4)
sensitivity_comparison = rbind(sens1, sens2, sens3, sens4)
specificity_comparison = rbind(spec1, spec2, spec3, spec4)
AUC_comparison = rbind(auc(act_pred1), auc(act_pred2), auc(act_pred3), auc(act_pred4))
num_predictors = c(21, 13, 12, 11)
cbind(model_number, num_predictors, performance_comparison[1], sensitivity_comparison[1], specificity_comparison[1], AUC_comparison[1])

# Oversampling: 
set.seed(100)
# Splitting the original heart_disease dataset into 2 datasets (Class 0 and Class 1)
df_0 = subset(heart_disease, HeartDiseaseorAttack == 0)
df_1 = subset(heart_disease, HeartDiseaseorAttack == 1)

# n1 = number of 1s in the original data
n1 = nrow(df_1) 

# p = proportion of 0s from original data
p = nrow(df_0)/nrow(heart_disease)

# n0v = number of 0s needed in the validation
n0v = as.integer(p*0.5*n1/(1-p))
n0v

# Get 50% of 1s and 0s for training data (RANDOMLY selected)

# Creating the training dataset
class1_row_index = sample(1:nrow(df_1), as.integer(nrow(df_1)*0.5), replace = F)
class0_row_index = sample(1:nrow(df_0), as.integer(nrow(df_1)*0.5), replace = F)
train_oversampling = rbind(df_1[class1_row_index,], 
                           df_0[class0_row_index,])
View(train_oversampling)

# Creating the validation dataset: 
# Remaining 50% of Class 1 and n0v for validation where the amount of 0s must be 
# proportionate to the original data

# Remaining from class 0 
remaining_from_class0_df = df_0[-class0_row_index,]
class0_remaining_row_index = sample(1:nrow(remaining_from_class0_df), n0v, replace = F)

valid_oversampling = rbind(df_1[-class1_row_index,], 
                           df_0[class0_remaining_row_index,])
View(valid_oversampling)

# Validating that the proportions are the same
prop.table(table(heart_disease$HeartDiseaseorAttack))
prop.table(table(valid_oversampling$HeartDiseaseorAttack))

# Running a Logistic Regression Using Oversampled Data

# Model1 oversampled
Model1 =  glm(HeartDiseaseorAttack~HighBP+ HighChol + CholCheck + BMI + Smoker + Stroke + Diabetes + PhysActivity + Fruits + 
                Veggies + HvyAlcoholConsump + AnyHealthcare + NoDocbcCost + GenHlth + MentHlth + PhysHlth + DiffWalk + Sex + 
                Age + Education + Income, data = train_oversampling, family = "binomial")
summary(Model1)

# Odds Ratios for Model1 after oversampling
OR_vec1=exp(Model1$coefficients)
OR_vec1

# Confidence Interval for Odds Ratios for Model1 after oversampling
CI_OR1= exp(confint.default(Model1))
CI_OR1

# Predicting/ Classifying new cases for Model1 after oversampling
# Gives the probability of each record in the validation data
predict1 = predict(Model1, newdata= valid_oversampling, type="response")
predict1

# Analyzing the performance of Model 1 after oversampling

# Data frame for actual and predicted for Model1 after oversampling
act_pred1=data.frame(ID=1:nrow(valid_oversampling),valid_oversampling$HeartDiseaseorAttack,predict1)
act_pred1

# Confusion matrix for Model1 after oversampling
conf_mat1=cmx(act_pred1)
conf_mat1

# Overall accuracy for Model1 after oversampling
total_acc1= pcc(conf_mat1) 
total_acc1

# Sensitivity for Model1 after oversampling
sens1=sensitivity(conf_mat1)
sens1

# Specificity for Model1 after oversampling
spec1=specificity(conf_mat1)
spec1

# Area under the ROC curve for Model1 after oversampling
auc(act_pred1) 

# Plot the ROC curve for Model1 after oversampling
auc.roc.plot(act_pred1, col = "red") 

# Model2 oversampled: removing the insignificant predictors found from the oversampled Model1 and re-leveling

Model2 =  glm(HeartDiseaseorAttack~HighBP+ HighChol + relevel(as.factor(CholCheck), ref = "Not Checked Cholesterol in the Past 5 Years") + 
                Smoker + Stroke + relevel(as.factor(Diabetes), ref = "Not Diabetic") + NoDocbcCost + relevel(as.factor(GenHlth), ref = "Good") + MentHlth +
                PhysHlth + DiffWalk + Sex + relevel(as.factor(Age), ref = "60-65") + relevel(as.factor(Income), ref = ">$75,000"), data = train_oversampling, 
              family = "binomial")

summary(Model2)

# Odds Ratios for Model2 after oversampling
OR_vec2=exp(Model2$coefficients)
OR_vec2

# Confidence Interval for Odds Ratios for Model2 after oversampling
CI_OR2= exp(confint.default(Model2))
CI_OR2

# Predicting/ Classifying new cases for Model2 after oversampling
# Gives the probability of each record in the validation data
predict2 = predict(Model2, newdata= valid_oversampling, type="response")
predict2

# Analyzing the performance of Model 2 after oversampling

# Data frame for actual and predicted for Model2 after oversampling
act_pred2=data.frame(ID=1:nrow(valid_oversampling),valid_oversampling$HeartDiseaseorAttack,predict2)
act_pred2

# Confusion matrix for Model2 after oversampling
conf_mat2=cmx(act_pred2)
conf_mat2

# Overall accuracy for Model2 after oversampling
total_acc2= pcc(conf_mat2) 
total_acc2

# Sensitivity for Model2 after oversampling
sens2=sensitivity(conf_mat2)
sens2

# Specificity for Model2 after oversampling
spec2=specificity(conf_mat2)
spec2

# Area under the ROC curve for Model2 after oversampling
auc(act_pred2) 

# Plot the ROC curve for Model2 after oversampling
auc.roc.plot(act_pred2, col = "red") 

# Model3 oversampled: removing the subjective questions (MenHlth and GenHlth) from Model2

Model3 =  glm(HeartDiseaseorAttack~HighBP+ HighChol + relevel(as.factor(CholCheck), ref = "Not Checked Cholesterol in the Past 5 Years") + Smoker + Stroke + 
                relevel(as.factor(Diabetes), ref = "Not Diabetic") + HvyAlcoholConsump + NoDocbcCost + DiffWalk + Sex + 
                relevel(as.factor(Age), ref = "60-65") + relevel(as.factor(Income), ref = ">$75,000"), data = train_oversampling, family = "binomial")
summary(Model3)

# Odds Ratios for Model3 after oversampling
OR_vec3=exp(Model3$coefficients)
OR_vec3

# Confidence Interval for Odds Ratios for Model3 after oversampling
CI_OR3= exp(confint.default(Model3))
CI_OR3

# Predicting/ Classifying new cases for Model3 after oversampling
# This will give me the probability of each record in the validation data
predict3 = predict(Model3, newdata= valid_oversampling, type="response")
predict3

# Analyzing the performance of Model 3

# Data frame for actual and predicted for Model3 after oversampling
act_pred3=data.frame(ID=1:nrow(valid_oversampling),valid_oversampling$HeartDiseaseorAttack,predict3)
act_pred3

# Confusion matrix for Model3 after oversampling
conf_mat3=cmx(act_pred3)
conf_mat3

# Overall accuracy for Model3 after oversampling 
total_acc3= pcc(conf_mat3) 
total_acc3

# Sensitivity for Model3 after oversampling 
sens3=sensitivity(conf_mat3)
sens3

# Specificity for Model3 after oversampling
spec3=specificity(conf_mat3)
spec3

# Area under the ROC curve for Model3 after oversampling 
auc(act_pred3) 

# Plot the ROC curve for Model3 after oversampling 
auc.roc.plot(act_pred3, col = "red") 

# Model4 oversampled- keeping only the *** significant predictors from Model1

Model4 =  glm(HeartDiseaseorAttack~HighBP+ HighChol + relevel(as.factor(CholCheck), ref = "Not Checked Cholesterol in the Past 5 Years") + 
                Smoker + Stroke + NoDocbcCost + DiffWalk + Sex + relevel(as.factor(Age), ref = "60-65"), data = train_oversampling, 
              family = "binomial")

# Odds Ratios for Model4 after oversampling 
OR_vec4=exp(Model4$coefficients)
OR_vec4

# Confidence Interval for Odds Ratios for Model4 after oversampling 
CI_OR4= exp(confint.default(Model4))
CI_OR4

# Predicting/ Classifying new cases for Model4 after oversampling 
# Gives the probability of each record in the validation data
predict4 = predict(Model4, newdata= valid_oversampling, type="response")
predict4

# Analyzing the performance of Model 4 after oversampling

# Data frame for actual and predicted for Model4 after oversampling 
act_pred4=data.frame(ID=1:nrow(valid_oversampling),valid_oversampling$HeartDiseaseorAttack,predict4)
act_pred4

# Confusion matrix for Model4 after oversampling 
conf_mat4=cmx(act_pred4)
conf_mat4

# Overall accuracy for Model4 after oversampling 
total_acc4= pcc(conf_mat4) 
total_acc4

# Sensitivity for Model4 after oversampling 
sens4=sensitivity(conf_mat4)
sens4

# Specificity for Model4 after oversampling 
spec4=specificity(conf_mat4)
spec4

# Area under the ROC curve for Model4 after oversampling 
auc(act_pred4) 

# Plot the ROC curve for Model4 after oversampling 
auc.roc.plot(act_pred4, col = "red") 

# Plotting the 4 ROC Curves- oversampled

ROC_os=data.frame(ID=1:nrow(valid_oversampling),valid_oversampling$HeartDiseaseorAttack,predict1,predict2,predict3,predict4)
auc.roc.plot(ROC_os,col=c(2,3,4,5),line.type=c(1,2,3,4),threshold = 1001, main="ROC Curves- After Oversampling", legend.text=c("Model 1", "Model 2", "Model 3", "Model 4"))

# Comparing the Overall Accuracy, Sensitivity, Specificity for Models 1,2,3,4 After Oversampling
model_number = rbind("Model1", "Model2", "Model3", "Model4")
performance_comparison = rbind(total_acc1, total_acc2, total_acc3, total_acc4)
sensitivity_comparison = rbind(sens1, sens2, sens3, sens4)
specificity_comparison = rbind(spec1, spec2, spec3, spec4)
AUC_comparison = rbind(auc(act_pred1), auc(act_pred2), auc(act_pred3), auc(act_pred4))
num_predictors = c(21, 14, 12, 9)
cbind(model_number, num_predictors, performance_comparison[1], sensitivity_comparison[1], specificity_comparison[1], AUC_comparison[1])

# Decision Tree
# Building the Tree
tree <- rpart(as.factor(train_oversampling$HeartDiseaseorAttack) ~. -BMI -PhysActivity -Fruits -Veggies -HvyAlcoholConsump -AnyHealthcare -Education, data = train_oversampling, control = rpart.control(minsplit=1000, cp = 0.0001))

printcp(tree)

tree$cptable

bestcp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

tree.pruned=prune(tree, cp = bestcp)


#Visualizing the tree

rpart.plot(tree.pruned,type=4,extra=2,
           main="heart disease", cex= 0.35)


#Predictive Performance of the Tree

library(PresenceAbsence)

pred=predict(tree.pruned, newdata=valid_oversampling,type="prob")

pred=pred[,2]

act_pred=data.frame(ID=1:nrow(valid_oversampling),valid_oversampling$HeartDiseaseorAttack,pred)#

conf_mat=cmx(act_pred)

total_acc= pcc(conf_mat,st.dev = FALSE)

sens=sensitivity(conf_mat,st.dev = FALSE)# to obtain snesitivty 

spec=specificity(conf_mat,st.dev = FALSE)# to obtain specificity

accuracy_measures=c(total_acc,sens,spec)

names(accuracy_measures)=c("Overall accuracy", "Sensitivity", "Specificity")


#ROC Curve and AUC

arc=auc(act_pred) #to obtain the area under Roc  curve

auc.roc.plot(act_pred,col="red")

#####################################################

#---Get Coefficients, Odds ratios and their CI ---
get_coff_and_OR <-function(log.model){
  cof <- log.model$coefficients
  OR_vec <- exp(log.model$coefficients)
  CI_OR<- exp(confint.default(log.model))
  result <- data.frame(cbind(cof, OR_vec, CI_OR))
  names(result) <- c("Coefficent", "Odds Ratio", "Lower", "Upper")
  result$Note <- ifelse(result$Lower<=1 & result$Upper>=1,"<-DONT USE","")
  return(result)
}

results = get_coff_and_OR(Model2)
View(results)

tidy_Model3 = broom::tidy(Model2)
View(tidy_Model2)
