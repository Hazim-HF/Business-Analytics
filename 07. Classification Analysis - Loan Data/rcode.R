Train50 = read.csv(file.choose())
Test50 = read.csv(file.choose())

library(dplyr)

Train50 = Train50 %>% mutate(Creditability = as.factor(Creditability)) %>%
  mutate(Account.Balance = as.factor(Account.Balance)) %>%
  mutate(Payment.Status.of.Previous.Credit = as.factor(Payment.Status.of.Previous.Credit)) %>%
  mutate(Purpose = as.factor(Purpose)) %>%
  mutate(Value.Savings.Stocks = as.factor(Value.Savings.Stocks)) %>%
  mutate(Length.of.current.employment = as.factor(Length.of.current.employment)) %>%
  mutate(Sex...Marital.Status = as.factor(Sex...Marital.Status)) %>%
  mutate(Most.valuable.available.asset = as.factor(Most.valuable.available.asset)) %>%
  mutate(Type.of.apartment = as.factor(Type.of.apartment)) %>%
  mutate(Concurrent.Credits = as.factor(Concurrent.Credits))

Test50 = Test50 %>% mutate(Creditability = as.factor(Creditability)) %>%
  mutate(Account.Balance = as.factor(Account.Balance)) %>%
  mutate(Payment.Status.of.Previous.Credit = as.factor(Payment.Status.of.Previous.Credit)) %>%
  mutate(Purpose = as.factor(Purpose)) %>%
  mutate(Value.Savings.Stocks = as.factor(Value.Savings.Stocks)) %>%
  mutate(Length.of.current.employment = as.factor(Length.of.current.employment)) %>%
  mutate(Sex...Marital.Status = as.factor(Sex...Marital.Status)) %>%
  mutate(Most.valuable.available.asset = as.factor(Most.valuable.available.asset)) %>%
  mutate(Type.of.apartment = as.factor(Type.of.apartment)) %>%
  mutate(Concurrent.Credits = as.factor(Concurrent.Credits))

LogisticModel50 <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + 
                         Value.Savings.Stocks + Length.of.current.employment + Sex...Marital.Status + 
                         Most.valuable.available.asset + Type.of.apartment + Concurrent.Credits + 
                         Duration.of.Credit..month.+ Credit.Amount + Age..years., 
                       family=binomial, 
                       data = Train50)

LogisticModel50final <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + 
                              Purpose + Length.of.current.employment + Sex...Marital.Status, 
                            family=binomial, 
                            data = Train50)

#fit50 <- fitted.values(LogisticModel50S1)
fit50 <- fitted.values(LogisticModel50final) #

Threshold50 <- rep(0,500)
for (i in 1:500)
  if(fit50[i] >= 0.5) Threshold50[i] = 1

library(gmodels)
CrossTable(Train50$Creditability, Threshold50, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=F, data=Train50)

acc_log_train = sum(diag(as.matrix(table(Train50$Creditability,Threshold50))))/500 

library(ROCR)


testdat = read.csv(file.choose())

prr = predict(LogisticModel50final, data=testdat, type='response')

prod_pred = prediction(fit50, Train50$Creditability)
prod_pred2 = prediction(prr, testdat$Creditability)

perf = performance(prod_pred, 'tpr', 'fpr')
perf2 = performance(prod_pred2, 'tpr', 'fpr')

par(mfrow=c(2,3))
plot(perf, main='Train: Logistic');abline(a=0, b=1, col='red')
plot(perf2, main='Test: Logistic');abline(a=0, b=1, col='red')

acc_log_test = sum(diag(as.matrix(table(testdat$Creditability,Threshold50))))/500

detach(package:performance)
search()

# Tree Based Method

library(tree)

tree_model <- tree(Creditability ~ Account.Balance+Duration.of.Credit..month.+Payment.Status.of.Previous.Credit+
                       Purpose+Credit.Amount+Value.Savings.Stocks+Length.of.current.employment+Instalment.per.cent+Sex...Marital.Status+
                       Guarantors+Duration.in.Current.address+Most.valuable.available.asset+Age..years.+Concurrent.Credits+
                       Type.of.apartment+No.of.Credits.at.this.Bank+Occupation+No.of.dependents+Telephone, data=Train50, method="class")

Test50 = read.csv(file.choose())

summary(tree_model)
plot(tree_model)
text(tree_model, pretty=0,cex=0.6)

par(mfrow=c(2,2))

# Train Data

Train50_pred <- predict(tree_model, Train50, type = 'class')
table(Train50_pred, Train50$Creditability)

acc_tree_train = sum(diag(as.matrix(table(Train50_pred, Train50$Creditability))))/500

Train50_pred2 <- predict(tree_model, Train50)

pred_train_tree=prediction(Train50_pred2[,2],Train50$Creditability)
plot(performance(pred_train_tree,'tpr', 'fpr'), main='Train: Tree');abline(a=0, b=1, col='red')

# Prune Train Data

Train50_prune8 <- prune.misclass(tree_model, best=8)
Test50_prune8_pred <- predict(Train50_prune8, Train50, type="class")
table(Test50_prune8_pred, Train50$Creditability)

acc_tree_train_prune = sum(diag(as.matrix(table(Test50_prune8_pred, Train50$Creditability))))/500

Test50_prune8_pred2 <- predict(Train50_prune8, Train50)

pred_train_prune_tree=prediction(Test50_prune8_pred2[,2],Train50$Creditability)
plot(performance(pred_train_prune_tree,'tpr', 'fpr'), main='Prune Train: Tree');abline(a=0, b=1, col='red')

# Test Data

Test50_pred <- predict(Train50_tree, Test50, type = 'class')
table(Test50_pred, Test50$Creditability)

acc_tree_test = sum(diag(as.matrix(table(Test50_pred, Test50$Creditability))))/500

Test50_pred2 <- predict(Train50_tree, Test50)

pred_test_tree=prediction(Test50_pred2[,2],Train50$Creditability)
plot(performance(pred_test_tree,'tpr', 'fpr'), main='Test: Tree');abline(a=0, b=1, col='red')

# Prune Test Data

Test50_prune8 <- prune.misclass(Train50_tree, best=8)
Test50_prune8_pred <- predict(Train50_prune8, Test50, type="class")
table(Test50_prune8_pred, Test50$Creditability)

acc_tree_test_prune = sum(diag(as.matrix(table(Test50_prune8_pred, Test50$Creditability))))/500

Test50_prune8_pred2 <- predict(Train50_prune8, Test50)

pred_test_prune_tree=prediction(Test50_prune8_pred2[,2],Train50$Creditability)
plot(performance(pred_test_prune_tree,'tpr', 'fpr'), main='Prune Test: Tree');abline(a=0, b=1, col='red')

# Plotting

par(mfrow=c(2,3))
plot(perf, main='Train: Logistic');abline(a=0, b=1, col='red')
plot(performance(pred_train_tree,'tpr', 'fpr'), main='Train: Tree');abline(a=0, b=1, col='red')
plot(performance(pred_train_prune_tree,'tpr', 'fpr'), main='Prune Train: Tree');abline(a=0, b=1, col='red')
plot(perf2, main='Test: Logistic');abline(a=0, b=1, col='red')
plot(performance(pred_test_tree,'tpr', 'fpr'), main='Test: Tree');abline(a=0, b=1, col='red')
plot(performance(pred_test_prune_tree,'tpr', 'fpr'), main='Prune Test: Tree');abline(a=0, b=1, col='red')

# Accuracy table

accuracy_table = data.frame(model = c('Logistic Regression', 'Tree-Based Method', 'Prune Tree-Based Method'),
                            train = c(acc_log_train, acc_tree_train, acc_tree_train_prune),
                            test = c(acc_log_test, acc_tree_test, acc_tree_test_prune))
