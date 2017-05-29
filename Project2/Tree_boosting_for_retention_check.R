library(tree)


library(ISLR)


df = read.csv('retention.csv')

hist(df)
summary(df)
View(df)
#Creating a binary categorical variable High based on 2nd day retention
#variable and adding it to the original data frame.
df$platform = as.factor(df$platform)
df$ip_ip = as.factor(df$ip_ip)
df$ip_country = as.factor(df$ip_country)
df$ip_region = as.factor(df$ip_region)
df$ip_city = as.factor(df$ip_city)
df$ip_timezone = as.factor(df$ip_timezone)
df$client_mobile_os = as.factor(df$client_mobile_os)
df$client_mobile_device = as.factor(df$client_mobile_device)
df$client_mobile_language = as.factor(df$client_mobile_language)
df$client_mobile_uid = as.factor(df$client_mobile_uid)
df$client_mobile_device_aid = as.factor(df$client_mobile_device_aid)
df$register_os = as.factor(df$register_os)
df$register_ip_region = as.factor(df$register_ip_region)
df$register_ip_city = as.factor(df$register_ip_city)

df = df[, !(colnames(df) %in% c("register_ip_city","","register_ip_region","ip_ip","client_mobile_device","client_mobile_language","client_mobile_uid","client_mobile_device_aid","ip_country","ip_region","ip_city",""))]

High = ifelse(df$Cohort_period < 2, "No", "Yes")
df = data.frame(df, High)
df$High
#Fit a tree to the data; note that we are excluding retention from the formula.
tree.retention = tree(High ~ . - Cohort_period, split = "deviance", data = df)
summary(tree.retention)


#Plotting the classification tree.
plot(tree.retention)
text(tree.retention, pretty = 0) #Yields category names instead of dummy variables.

#Detailed information for the splits of the classification tree.
tree.carseats


#Splitting the data into training and test sets by an 70% - 30% split.
set.seed(0)
train = sample(1:nrow(df), 7*nrow(df)/10) #Training indices.
df.test = df[-train, ] #Test dataset.
High.test = High[-train] #Test response.

#Ftting and visualizing a classification tree to the training data.
tree.retention = tree(High ~ . - Cohort_period, data = df, subset = train)
plot(tree.retention)
text(tree.retention, pretty = 0)
summary(tree.retention)
tree.retention

#Using the trained decision tree to classify the test data.
tree.pred = predict(tree.retention, df.test, type = "class")
tree.pred[1:100]
High.test[1:100]
#Assessing the accuracy of the overall tree by constructing a confusion matrix.
table(tree.pred, High.test)
(60 + 42)/120

#Performing cross-validation in order to decide how many splits to prune; using
#misclassification as the basis for pruning.
set.seed(0)
cv.retention = cv.tree(tree.retention, FUN = prune.misclass)

#Pruning the overall tree to have 4 terminal nodes; choose the best tree with
#4 terminal nodes based on cost complexity pruning.
par(mfrow = c(1, 1))
prune.retention = prune.misclass(tree.retention, best = 4)
plot(prune.retention)
text(prune.retention, pretty = 0)

#Assessing the accuracy of the pruned tree with 4 terminal nodes by constructing
#a confusion matrix.
tree.pred = predict(prune.retention, df.test, type = "class")
table(tree.pred, High.test)
(53 + 33)/120





##########################
#####Regression Trees#####
##########################

#Creating a training set on 70% of the data.
set.seed(0)
train = sample(1:nrow(df), 7*nrow(df)/10)

.
tree.retention1 = tree(Cohort_period ~ . - High, df, subset = train)
summary(tree.retention1)

#Visually inspecting the regression tree.
plot(tree.retention1)
text(tree.retention1, pretty = 0)


#Pruning the tree to have 4 terminal nodes.
prune.retention1 = prune.tree(tree.retention1, best = 10)
par(mfrow = c(1, 1))
plot(prune.retention1)
text(prune.retention1, pretty = 0)

#Calculating and assessing the MSE of the test data on the overall tree.
yhat = predict(tree.retention1, newdata = df[-train, ])
yhat
df.test = df[-train, "High"]
df.test
plot(yhat, df.test)
abline(0, 1)
mean((yhat - df.test)^2)

#########################################################################################
library(randomForest)

#Fitting an initial random forest to the training subset.
set.seed(0)
rf.retention = randomForest(High ~ . - Cohort_period, data = df, subset = train, importance = TRUE)
rf.retention


#Varying the number of variables used at each step of the random forest procedure.
set.seed(0)
oob.err = numeric(13)
for (mtry in 1:13) {
  fit = randomForest(High ~ . - Cohort_period, data = df[train, ], mtry = mtry)
  oob.err[mtry] = fit$mse[500]
  cat("We're performing iteration", mtry, "\n")
}
########################################################################################

library(gbm)

#Fitting 7,000 trees with a depth of 4.
set.seed(0)
boost.retention = gbm(High ~ . - Cohort_period, data = df,
                   distribution = "gaussian",
                   n.trees = 7000,
                   interaction.depth = 4)

#Inspecting the relative influence.
par(mfrow = c(1, 1))
summary(boost.retention)