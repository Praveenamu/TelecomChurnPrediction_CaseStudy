# TelecomChurnPrediction_CaseStudy
Machine Learning Model
title: "Project"
author: "Praveena_Munnam_vxw648."
date: "2023-05-04"

```{r}
data <- read.csv("TelecomChurnPrediction.csv")
```


```{r}
str(data)
```


## Checking the Missing values 

```{r}
sum(is.na(data))
```
```{r}
colSums(is.na(data))
```
## Checking the proportion

```{r}
sum(is.na(data$TotalCharges))/nrow(data)
```
This subset is 0.16% of our data and is quite small. We will remove these cases in order to accomodate our further analyses.

# Data Cleaning 
## Removing na values
`
```{r}
data = data %>% na.omit()
```

```{r}
sum(is.na(data))
```
```{r}
data = dplyr::select(data,-customerID)
```

## Exploratary analysis

Let’s analyze the relation among the features to our target variable (Churn)
1. Churn against categorical features

```{r}
data %>% select_if(is.character) %>% names(.)
```
There are 15 categorical variables. I will break the analysis into three segments.



## Customer Profile

```{r}
cprofile_p1 <- data %>% 
  select(Churn, gender) %>% count(Churn, gender) %>% 
  ggplot(aes(gender, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("grey41", "sienna2"))


cprofile_p2 <- data %>% 
  select(Churn, Partner) %>% count(Churn, Partner) %>% 
  ggplot(aes(Partner, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("grey41", "sienna2"))

cprofile_p3 <- data %>% 
  select(Churn, Dependents) %>% count(Churn, Dependents) %>%
  ggplot(aes(Dependents, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("grey41", "sienna2"))

cprofile_p4 <- data %>% 
  mutate(SeniorCitizen = ifelse(SeniorCitizen == "1", "Yes", "No")) %>% 
  select(Churn, SeniorCitizen) %>% count(Churn, SeniorCitizen) %>% 
  ggplot(aes(SeniorCitizen, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("grey41", "sienna2"))

subplot1 <- ggarrange(cprofile_p1, cprofile_p2, cprofile_p3, cprofile_p4,  ncol = 2, nrow = 2, 
                      common.legend = TRUE, 
                      legend = "bottom")
subplot1

```



Customer who has Dependents has lower Churn Rate (17.44%) than those who do not have.

Customer who has Phone Service has higher Churn Rate (82.56%) than those who do not have.

SeniorCitizen Customer has lower Churn Rate (25.47%) than those who are not Senior Citizen.

The variance in gender does not seems to have a significant influence to customer churn rate. Both male and female have almost same Churn Rate.



## Service subscription

```{r}
services_p8 <- data %>% 
  select(Churn, InternetService) %>% count(Churn, InternetService) %>% 
  ggplot(aes(InternetService, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("grey41", "sienna2"))

services_p9 <- data %>% 
  select(Churn, PhoneService) %>% count(Churn, PhoneService) %>% 
  ggplot(aes(PhoneService, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("grey41", "sienna2"))

subplot4 <- ggarrange(services_p8, services_p9, 
                    ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
subplot4

```

Customer who subscribe to InternetSevice using Fiber Optic has higher churn rate last month compare to those who use DSL or even no using intenet service.

The variance in PhoneService have little influence to customer churn rate. Customer who used PhoneService has higher churn rate than those who not.

```{r}
services_p1 <- data %>% 
  select(Churn, MultipleLines) %>% count(Churn, MultipleLines) %>% 
  ggplot(aes(MultipleLines, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("grey41", "sienna2"))

services_p2 <- data %>% 
  select(Churn, OnlineSecurity) %>% count(Churn, OnlineSecurity) %>% 
  ggplot(aes(OnlineSecurity, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("grey41", "sienna2"))

services_p3 <- data %>% 
  select(Churn, OnlineBackup) %>% count(Churn, OnlineBackup) %>% 
  ggplot(aes(OnlineBackup, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("grey41", "sienna2"))

services_p4 <- data %>% 
  select(Churn, DeviceProtection) %>% count(Churn, DeviceProtection) %>% 
  ggplot(aes(DeviceProtection, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("grey41", "sienna2"))

services_p5 <- data %>% 
  select(Churn, TechSupport) %>% count(Churn, TechSupport) %>% 
  ggplot(aes(TechSupport, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("grey41", "sienna2"))

services_p6 <- data %>% 
  select(Churn, StreamingTV) %>% count(Churn, StreamingTV) %>% 
  ggplot(aes(StreamingTV, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("grey41", "sienna2"))

services_p7 <- data %>% 
  select(Churn, StreamingMovies) %>% count(Churn, StreamingMovies) %>% 
  ggplot(aes(StreamingMovies, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("grey41", "sienna2"))


subplot2 <- ggarrange(services_p1, services_p2, services_p3, services_p4,
                    services_p5, services_p6, services_p7, 
                    #labels = c("MultipleLines", "OnlineSecurity",  "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies"),
                    ncol = 2, nrow = 4, common.legend = TRUE, legend = "bottom")
subplot2
```



## Contact + Billing type

```{r}
subsinfo_p1 <- data %>% 
  select(Churn, Contract) %>% count(Churn, Contract) %>% 
  ggplot(aes(Contract, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("grey41", "sienna2"))

subsinfo_p2 <- data %>% 
  select(Churn, PaymentMethod) %>% count(Churn, PaymentMethod) %>% 
  ggplot(aes(PaymentMethod, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("grey41", "sienna2"))

subsinfo_p3 <- data %>% 
  select(Churn, PaperlessBilling) %>% count(Churn, PaperlessBilling) %>% 
  ggplot(aes(PaperlessBilling, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("grey41", "sienna2"))

subplot3 <- ggarrange(subsinfo_p1, subsinfo_p2, subsinfo_p3, 
                      ncol = 2, nrow = 2,
                      common.legend = TRUE, legend = "bottom")
subplot3

```
Customers who sign longer contract have lower churn rate.
Customers who choose paperless billing have higher churn rate.
Customers who pay with electronic check have higher churn rate than customers who pay with other methods.



## Churn against numerical variables

```{r}
data %>% select_if(is.numeric) %>% names(.)
```
```{r}
bp1 <- data %>% 
  select(tenure) %>% 
  ggplot(aes(tenure)) + geom_boxplot(fill = "sienna2", color = "grey41")

bp2 <- data %>% 
  select(MonthlyCharges) %>% 
  ggplot(aes(MonthlyCharges)) + geom_boxplot(fill = "sienna2", color = "grey41")

bp3 <- data %>% 
  select(TotalCharges) %>% 
  ggplot(aes(TotalCharges)) + geom_boxplot(fill = "sienna2", color = "grey41")

subplot4 <- ggarrange(bp1, bp2, bp3, 
                      ncol = 1, nrow = 3,
                      common.legend = TRUE, legend = "bottom")
subplot4

```


there is no outliers for each of numeric feature.

```{r, include=FALSE}
library(magrittr)
```

```{r}
library(dplyr)
library(ggplot2)
library(gridExtra)
library(gtable)
library(grid)

graph1 = data %>% 
  group_by(tenure, Churn) %>% 
  summarise(Number = n()) %>% 
  ggplot(aes(tenure, Number)) +
  geom_line(aes(col = Churn)) +
  labs(x = "Tenure (month)",
       y = "",
       title = "Churn Based on Tenure") +
  scale_x_continuous(breaks = seq(0, 80, 10))

graph2 = data %>% 
  group_by(MonthlyCharges, Churn) %>% 
  summarise(Number = n()) %>% 
  ggplot(aes(MonthlyCharges, Number)) +
  geom_line(aes(col = Churn)) +
  labs(x = "Monthly Charges",
       y = "",
       title = "Churn Based on Monthly Charges") +
  scale_x_continuous(breaks = seq(0, 120, 20))

graph3 = data %>% 
  group_by(TotalCharges, Churn) %>% 
  summarise(Number = n()) %>% 
  ggplot(aes(TotalCharges, Number)) +
  geom_line(aes(col = Churn)) +
  labs(x = "Total Charges",
       y = "",
       title = "Churn Based on Total Charges") +
  scale_x_continuous(breaks = seq(0, 9000, 1000))

legend = gtable_filter(ggplotGrob(graph2), "guide-box") 

#plot graphs
grid.arrange(
  arrangeGrob(graph1+ theme(legend.position="none"), graph2 + theme(legend.position="none"), graph3 + theme(legend.position="none"), nrow = 3,
             left = textGrob("Number of Customer", rot = 90, vjust = 1, 
                             gp = gpar(fontsize = 12))),
  legend, 
  widths=unit.c(unit(1, "npc") - legend$width, legend$width), 
  nrow=1)
```

From the graph we can see that the highest churn happen mostly after 1 month usage. It is noticeable that customers leave much less frequently after 25 months. The monthly charges chart shows that most of the loyal customers that stayed with the company had a lower monthly charge than most of the customers that churned.

## Correlation matrix
```{r}
library(RColorBrewer)
```


```{r}
library(corrplot)

num.cols = sapply(data, is.numeric)
cor.data = cor(data[, num.cols])
corrplot(cor.data, method = "color", col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(7), 
         type = "lower", addCoef.col = "black", tl.col = "black", tl.cex = 0.7)
```
The plot shows high correlations between Totalcharges & tenure and between TotalCharges & MonthlyCharges. 



## Hypothesis Testing
Hypothesis testing is a statistical method used to determine whether an observed effect or relationship in a sample is likely to occur in the population from which the sample was drawn or not. 

For example, if the variable is categorical and the research question involves comparing the proportions of two groups, a chi-square test might be appropriate. If the variable is continuous and normally distributed, a t-test or ANOVA might be appropriate.

# Is there a significant difference in the tenure of customers who churn and those who don't churn?
## two-sample t-test 

```{r}
# Subset the data into two groups based on churn status
churn <- subset(data, Churn == "Yes")
no_churn <- subset(data, Churn == "No")

# Perform a two-sample t-test to compare the means of tenure between the two groups
t.test(churn$tenure, no_churn$tenure)
```

The null hypothesis for this test is that there is no significant difference in the tenure of customers who churn and those who don't churn. The alternative hypothesis is that there is a significant difference in the tenure between the two groups.

The p-value of the test is less than 0.05, which suggests strong evidence against the null hypothesis. Therefore, we reject the null hypothesis and conclude that there is a significant difference in the tenure of customers who churn and those who don't churn.

The sample mean of the customers who churned is 17.97913, while the sample mean of the customers who did not churn is 37.65001. This suggests that customers who have a tenure of less than or equal to 17.98 are more likely to churn than those who have a higher tenure.


## Total Charges
# Is there a significant difference in the total charges of customers who churn and those who don't churn?
```{r}
t.test(churn$MonthlyCharges, no_churn$MonthlyCharges)
```
The null hypothesis is that there is no significant difference in the Monthly charges between the two groups. The alternative hypothesis is that there is a significant difference in the Monthly charges between the two groups.

The test result shows a  p-value less than 2.2e-16. Since the p-value is less than the significance level of 0.05, we reject the null hypothesis and conclude that there is a significant difference in the Monthly charges between customers who churn and those who don't churn.



# chi-square test of independence
Is there a significant difference in the proportion of customers with dependents between those who churn and those who don't churn?
## Dependent variable
```{r}
# Create a contingency table of the counts of customers with and without dependents for each group
table_churn <- table(churn$Dependents)
table_no_churn <- table(no_churn$Dependents)

# Combine the tables into a single contingency table
cont_table <- rbind(table_churn, table_no_churn)

# Perform the Chi-square test
chisq.test(cont_table)
```
the null hypothesis assumes that there is no significant difference in the proportion of customers with dependents between those who churn and those who don't churn. The alternative hypothesis assumes that there is a significant difference in the proportion of customers with dependents between these two groups.

In this case, the p-value is less than 2.2e-16, which is significantly smaller than the standard significance level of 0.05.

Therefore, we can reject the null hypothesis and conclude that there is a significant difference in the proportion of customers with dependents between those who churn and those who don't churn.


## Gender Variable

```{r}
table_churn <- table(churn$gender)
table_no_churn <- table(no_churn$gender)

# Combine the tables into a single contingency table
cont_table <- rbind(table_churn, table_no_churn)

# Perform the Chi-square test
chisq.test(cont_table)
```
The p-value being greater than the significance level of 0.05 suggests that we fail to reject the null hypothesis, which states that there is no significant difference in the proportion of customers with gender between those who churn and those who don't churn. Therefore, we can conclude that there is no significant difference in the proportion of customers with gender between those who churn and those who don't churn.



```{r}
p20 <- ggplot(data, aes(x = Churn)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) +
  scale_fill_manual(values = c("grey41", "sienna2"))
p20
```
## Cleaning the dataset
.

```{r, include=FALSE}
library(plyr)
```

# The SeniorCitizen variable is coded ‘0/1’ rather than yes/no. We can recode this to ease our interpretation of models.
```{r}
data$SeniorCitizen <- as.factor(mapvalues(data$SeniorCitizen,
                                          from=c("0","1"),
                                          to=c("No", "Yes")))
```


# ‘No phone service’ response to ‘No’ for the MultipleLines variable.
```{r}
data$MultipleLines <- as.factor(mapvalues(data$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))
```

# ‘No internet service’ to ‘No’ for these variables.

```{r}
data$OnlineSecurity <- ifelse(data$OnlineSecurity == "No internet service", "No", data$OnlineSecurity)
data$TechSupport <- ifelse(data$TechSupport  == "No internet service", "No", data$TechSupport)
data$StreamingTV = ifelse(data$StreamingTV  == "No internet service", "No", data$StreamingMovies)
data$StreamingMovies = ifelse(data$StreamingMovies == "No internet service", "No", data$StreamingMovies)
data$OnlineBackup = ifelse(data$OnlineBackup == "No internet service", "No", data$OnlineBackup)
data$DeviceProtection = ifelse(data$OnlineBackup == "No internet service", "No", data$DeviceProtection)
```

```{r}
data$DeviceProtection = ifelse(data$DeviceProtection == "No internet service", "No", data$DeviceProtection)
```

```{r}
View(data)
```

```{r}
library(forcats)
data$Churn <- as_factor(data$Churn)
levels(data$Churn)
```
# Feature engieneering
## Selecting the variables

```{r}
set.seed(1)
r1 = lm(as.numeric(Churn) ~ ., data = data)
summary(r1)
```

```{r}
r2 = step(r1, direction = "backward")
```
```{r}
summary(r2)
```
## Checking the Multicollinearity

```{r}
set.seed(1)
vif(r2)
```
```{r}
names(data)
```


## Removing insignificant variables
remove gender, partner, streaming Tv, streaming movies
    
```{r}
data = dplyr::select(data,-gender)
data = dplyr::select(data,-Partner)
data = dplyr::select(data, -StreamingTV)
data = dplyr::select(data, -StreamingMovies)
```
    
```{r}
names(data)
```

## Spliting the dataset

```{r}
set.seed(1)
split <- sample(nrow(data), nrow(data)*0.8)
datatrain <- data[split,]
datatest <- data[-split,]
```


```{r}
table(datatrain$Churn)
```

### DOWNSAMPLE

```{r}
traindown <- downSample(x = subset(datatrain, select = -Churn),
                        y = datatrain$Churn,
                        list = F,
                        yname = "Churn")

table(traindown$Churn)
```
```{r}
levels(traindown$Churn)
```
```{r}
names(traindown)
```
   
## Building models



```{r, include=FALSE}
library(rpart)
library(rattle)
```



## Decision tree
We will use the rpart library in order to use recursive partitioning methods for decision trees. This exploratory method will identify the most important variables related to churn in a hierarchical format.
```{r}
set.seed(1)
tree11 <- rpart(Churn ~., data = traindown)
fancyRpartPlot(tree11, palettes = c("Greys", "Oranges"), sub="Full classification tree")
```
From this decision tree, we can interpret the following:

The contract variable is the most important

```{r}
printcp(tree11)
```
The root node contains 1487 errors out of the 2974 values (50%)


```{r}
dt.preds <- predict(tree11, datatest, type = "class")
confusionmatrix.dt = caret::confusionMatrix(dt.preds, datatest$Churn, positive = "Yes")
confusionmatrix.dt
```

```{r}
library(ROCR)
tree11 <- rpart(Churn ~., data = traindown)
dt.preds <- predict(tree11, datatest, type = "prob")[, 2]
Pred_val4 <- prediction(dt.preds, datatest$Churn)
ROCperf4 <- performance(Pred_val4, "tpr", "fpr")
plot(ROCperf4, colorize = TRUE)
abline(0, 1, lty = 2)
auc_dt <- round(as.numeric(performance(Pred_val4, "auc")@y.values[[1]]), 2)
verbose <- TRUE
if (verbose) {
  text(0.8, 0.2, paste0("AUC = ", auc_dt), col = "black", cex = 1)
}
```

```{r}
plotcp(tree11)
```
The dashed line is set at the minimum xerror + xstd. Any value below the line would be considered statistically significant. A good choice for CP is often the largest value for which the error is within a standard deviation of the mimimum error. In this case, the smallest cp is at 0.01    

## Pruning the tree


```{r}
(opt <- which.min(tree11$cptable[,"xerror"]))
(cp <- tree11$cptable[opt, "CP"])
Ptree <- prune(tree11, cp = cp)
```
```{r}
Ptree
```

```{r}
fancyRpartPlot(Ptree, palettes = c("Greys", "Oranges"), sub="Pruned classification tree")
```



```{r}
pred.tree_p.train <- predict(Ptree,datatrain,type = "class")

confusionmatrix.dtp = caret::confusionMatrix(data = pred.tree_p.train,
                reference = datatrain$Churn,
                positive = "Yes")
confusionmatrix.dtp
```
```{r}
set.seed(1)
pred.tree_p <- predict(Ptree, datatest, type = "prob")[, 2]
Pred_val3 <- prediction(pred.tree_p, datatest$Churn) 
ROCperf3 <- performance(Pred_val3, "tpr", "fpr")
plot(ROCperf3, colorize = TRUE)
abline(0, 1, lty = 2)
auc_ptree <- round(as.numeric(performance(Pred_val3, "auc")@y.values[[1]]), 2)
text(0.8, 0.2, paste0("AUC = ", auc_ptree), col = "black", cex = 1)

```
## Logistic Regression

```{r}
logr1 = glm(Churn ~ ., data = traindown, family = binomial)
summary(logr1)
```


## Make predictions for logistic regressions

```{r}
predprob = predict.glm(logr1, newdata = datatest, type = "response")
predclass <- ifelse(predprob > 0.5,"Yes","No")
confusionmatrix.log = caret::confusionMatrix(factor(predclass, levels = c("Yes", "No")),
                factor(datatest$Churn, levels = c("Yes", "No")), positive = "Yes")
confusionmatrix.log
```

## ROC Curve

```{r}
library(ROCR)
```


```{r}
pred <- prediction(predict(logr1, newdata = datatest, type = "response"), datatest$Churn)
auc <- round(as.numeric(performance(pred, measure = "auc")@y.values[[1]]), 3)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE, main = "ROC Curve")
text(0.5, 0.5, paste("AUC:", auc))
```


## Random forest
K- Cross validation method

```{r}
library(caret)
ctrl <- trainControl(method = "cv", number=5, 
                     classProbs = TRUE, summaryFunction = twoClassSummary)

#Exploratory random forest model selection
Untunedram <- train(Churn ~., data = traindown,
                 method = "rf",
                 ntree = 75,
                 tuneLength = 5,
                 metric = "ROC",
                 trControl = ctrl)
Untunedram
```
The model found that the optimal value for ‘mtry’ is 2. From this model we can investigate the relative importance of the churn predictor variables.


```{r}
un.preds <- predict(Untunedram, datatest, type = "raw")
confusionmatrix.un = caret::confusionMatrix(un.preds, datatest$Churn, positive = "Yes")
confusionmatrix.un
```


```{r}
set.seed(1)
un.preds <- predict(Untunedram, datatest, type = "prob")[,2]
Pred_val5 <- prediction(un.preds, datatest$Churn) 
ROCperf5 <- performance(Pred_val5, "tpr", "fpr")
plot(ROCperf5, colorize = TRUE)
abline(0, 1, lty = 2)
auc_uram <- round(as.numeric(performance(Pred_val5, "auc")@y.values[[1]]), 2)
text(0.8, 0.2, paste0("AUC = ", auc_uram), col = "black", cex = 1)
```


# Tuned random forest model with hyperparameters

```{r}
# Set up the tuning grid for random forest
tunedGrid <- expand.grid(mtry = c(2, 4, 6))
```


```{r}
ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
```

```{r}
Tunedram <- train(Churn ~., data = traindown,
                  method = "rf",
                  ntree = 500,
                  tuneGrid = tunedGrid,
                  metric = "ROC",
                  trControl = ctrl)
Tunedram
```

```{r}
pred_tuned <- predict(Tunedram, datatest, type = "raw")
confusionmatrix_tuned <- confusionMatrix(pred_tuned, datatest$Churn, positive = "Yes")
confusionmatrix_tuned
```

```{r}
set.seed(1)
pred_tuned <- predict(Tunedram, datatest, type = "prob")[,2]
Pred_val6 <- prediction(pred_tuned, datatest$Churn) 
ROCperf6 <- performance(Pred_val6, "tpr", "fpr")
plot(ROCperf6, colorize = TRUE)
abline(0, 1, lty = 2)
auc_ram <- round(as.numeric(performance(Pred_val6, "auc")@y.values[[1]]), 2)
text(0.8, 0.2, paste0("AUC = ", auc_ram), col = "black", cex = 1)


```
```{r}
varImp(Tunedram)
```


```{r}
rf_object <- Tunedram$finalModel
var_imp <- randomForest::importance(rf_object, type = 2)
var_imp_df <- data.frame(Variables = row.names(var_imp),
                         Importance = round(var_imp[, "MeanDecreaseGini"], 2)) %>%
              arrange(desc(Importance))
ggplot(var_imp_df, aes(x = reorder(Variables, Importance), y = Importance)) + 
  geom_bar(stat = "identity", fill = "sienna2") +
  coord_flip() +
  labs(title = "Variable Importance Plot", y = "Importance", x = "Predictor Variable")
```

```{r}
pred1.accuracy <- confusionmatrix.dt$overall[1]
pred2.accuracy <- confusionmatrix.dtp$overall[1]
pred3.accuracy <- confusionmatrix.log$overall[1]
pred4.accuracy <- confusionmatrix.un$overall[1]
pred5.accuracy <- confusionmatrix_tuned$overall[1]

## Model Evaluation
results <- data.frame(
  Model = c("Decision Tree", "Pruning Decision Tree", "Logistic Regression", "Untuned Random Forest", "Tuned Random Forest"),
  Accuracy = sprintf("%.2f", c(pred1.accuracy, pred2.accuracy, 
                               pred3.accuracy, pred4.accuracy, pred5.accuracy)),
  Sensitivity = sprintf("%.2f", c(confusionmatrix.dt$byClass["Sensitivity"],
                                   confusionmatrix.dtp$byClass["Sensitivity"],
                                   confusionmatrix.log$byClass["Sensitivity"],
                                   confusionmatrix.un$byClass["Sensitivity"],
                                   confusionmatrix_tuned$byClass["Sensitivity"])),
  Specificity = sprintf("%.2f", c(confusionmatrix.dt$byClass["Specificity"],
                                   confusionmatrix.dtp$byClass["Specificity"],
                                   confusionmatrix.log$byClass["Specificity"],
                                   confusionmatrix.un$byClass["Specificity"],
                                   confusionmatrix_tuned$byClass["Specificity"])),
  AUC = sprintf("%.2f", c(as.numeric(performance(Pred_val4, "auc")@y.values[[1]]),
                           as.numeric(performance(Pred_val3, "auc")@y.values[[1]]),
                           as.numeric(performance(pred, measure = "auc")@y.values),
                           as.numeric(performance(Pred_val5, "auc")@y.values[[1]]),
                           as.numeric(performance(Pred_val6, "auc")@y.values[[1]])))
)

results

```

#Resamples of Tranining data

```{r}
ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
tree11 <- train(Churn ~., data = traindown, method = "rpart", trControl = ctrl)

```
```{r}
library(caret)

ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)

tree11 <- train(Churn ~., data = traindown, method = "rpart", trControl = ctrl)
Ptree <- train(Churn ~., data = traindown, method = "rpart2", trControl = ctrl)
logr1 <- train(Churn ~., data = traindown, method = "glm", trControl = ctrl)
Untunedram <- train(Churn ~., data = traindown, method = "rf", ntree = 75, tuneLength = 5, metric = "ROC", trControl = ctrl)
tunedGrid <- expand.grid(mtry = c(2, 4, 6))
Tunedram <- train(Churn ~., data = traindown, method = "rf", ntree = 500, tuneGrid = tunedGrid, metric = "ROC", trControl = ctrl)

res <- resamples(list(DecisionTree = tree11, 
                      PruneDecisionTree = Ptree, 
                      LogisticRegression = logr1, 
                      UntunedRandomForest = Untunedram,  
                      TunedRandomForest = Tunedram))

dotplot(res)
```

