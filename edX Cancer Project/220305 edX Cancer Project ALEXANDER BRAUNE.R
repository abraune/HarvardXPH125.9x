##############################################################################+
# R Setup // install needed components                          
##############################################################################+

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(corrplot) 
library(randomForest)
library(e1071) 

options(max.print=999999)



##############################################################################+
# Load data     
##############################################################################+

# download file
dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00383/risk_factors_cervical_cancer.csv", dl)
data <- read.csv(dl)



##############################################################################+
# Data exploration and imputation 
##############################################################################+

# check variable classes first

# define function returning column classes
colclasses <- function(df) {
  t(as.data.frame(lapply(data, function(x) paste(class(x), collapse = ','))))
}

# return data column classes
colclasses(data)

# preview of dataset to find reason for numeric columns being recognized as strings
t(head(data))%>%
  knitr::kable()

#replace strings with NA
data <- na_if(data, "?") 
data <- na_if(data, "")

#convert to numeric columns
data <- as.data.frame(apply(data, 2, as.numeric)) 


# replace NA with median values of the columns
for(i in 1:32) {                                   
  data[ , i][is.na(data[ , i])] <- median(data[ , i], na.rm = TRUE)
}

#check for na
colSums(is.na(data))

#check for empty columns
colSums(data, na.rm = T)

#drop empty columns
i <- (colSums(data, na.rm=T) !=0)
data <- data[, i]

# create indications counter columns
data$cancer_indications = data$Schiller+data$Hinselmann+data$Citology+data$Biopsy

# signal cancer if at least one indication is found (1 for yes, 0 for no) 
data$cancer[data$cancer_indications <1] <- 0
data$cancer[data$cancer_indications >0] <- 1


# extract variables subset without results
data_var<-data[c(1:30)]

# extract actual results subset
y_act <- as.factor(data$cancer)


##############################################################################+
# target variable distribution 
##############################################################################+

# display result column sums
barplot(table(y_act), xlab = "0 = No cancer indication, 1 = at least one cancer indication )", ylab = "cases")

# sum cases per indicator
colSums(data[c(31:34)])
barplot(colSums(data[c(31:34)]))

#show number of identifier distribution
filter(data,cancer>0) %>%
  group_by(cancer_indications) %>% 
  summarize(sum=sum(cancer)) %>%
  ggplot(aes(cancer_indications,sum)) +
  geom_bar(stat="identity")+
  labs(title = "Number of positive identifiers for positive cases")

##############################################################################+
# variable distributions
##############################################################################+     

#plot variable distribution by cancer status
data %>% select(-35) %>% select(-34) %>% select(-33) %>% select(-32) %>% select(-31) %>%
  gather("feature", "value", -(cancer)) %>%
  ggplot(aes(value, fill = as.factor(cancer))) +
  geom_density(alpha = 0.5) +
  xlab("Variable values") +
  ylab("Density") +
  theme(legend.position = "top",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        legend.title=element_blank()) +
  scale_fill_discrete(labels = c("no cancer", "cancer")) +
  facet_wrap(~ feature, scales = "free", ncol = 4)

#plot variable distribution by indication count
data %>% select(-36) %>% select(-34) %>% select(-33) %>% select(-32) %>% select(-31) %>%
  gather("feature", "value", -(cancer_indications)) %>%
  ggplot(aes(value, fill = as.factor(cancer_indications))) +
  geom_density(alpha = 0.5) +
  xlab("Variable values") +
  ylab("Density") +
  theme(legend.position = "top",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        legend.title=element_blank()) +
  scale_fill_discrete(labels = c("0 indications", "1", "2", "3", "4")) +
  facet_wrap(~ feature, scales = "free", ncol = 4)


# exclude near zero variance columns
data_var_nzv <- data_var[,-nearZeroVar(data_var)]

##############################################################################+
# Variable Correlation 
##############################################################################+


# find correlation and between variables
cor <- cor(data_var, method = "spearman")
corrplot(cor, order = 'hclust', addrect = 2)



# name variables with high correlation
high_cor <- findCorrelation(cor, cutoff=0.75)
(names(data[high_cor]))


##############################################################################+
# create train and validation data set  
##############################################################################+

#partition datasets, 70% for training, 30% for validation
set.seed(2, sample.kind="Rounding") 
test_index <- createDataPartition(y = data$Age, times = 1, p = 0.3, list = FALSE)

# split variable dataset
train_var <- data_var[-test_index,]
validate_var <- data_var[test_index,]

# split results dataset
train_y_act <- as.factor(data[-test_index,36])
validate_y_act <- as.factor(data[test_index,36])

# split near sero variance dataset
train_var_nzv <- data_var_nzv[-test_index,]
validate_var_nzv <- data_var_nzv[test_index,]


# check dimensions of datasets
cat("# rows: ",dim(train_var)[1])
cat("# fields: ",dim(train_var)[2])

# check dimensions of datasets
cat("# rows: ",dim(validate_var)[1])
cat("# fields: ",dim(validate_var)[2])


##############################################################################+
# Prediction Model 1 Training 
##############################################################################+


# create log regression model
set.seed(192)
model1 <- glm(train_y_act ~ . , data=train_var_nzv, family = "binomial", maxit = 25)

# predict responses with trained log function
y_fc1_glm = predict(model1, newdata = train_var_nzv, type = 'response')

# translate into binary results of cancer
y_fc1 <- factor(ifelse(y_fc1_glm > 0.5, 1, 0))

# model evaluation
confusionMatrix(train_y_act, y_fc1)$overall["Accuracy"]



##############################################################################+
# Prediction Model 2 Training
##############################################################################+


#RandomForest creation 
set.seed(392)
model2 = randomForest(x = train_var,
                           y = train_y_act,
                           ntree = 100)
# write results
y_fc2 = predict(model2, newdata = train_var, type = 'class')

# show importance of variables
importance <- varImp(model2)
varImpPlot(model2) 

# model evaluation
confusionMatrix(train_y_act, y_fc2)$overall["Accuracy"]




##############################################################################+
# Prediction Model 3 Training
##############################################################################+
#SVM creation
set.seed(592)
model3 = svm(formula = train_y_act ~ .,
                  data = train_var,
                  type = 'C-classification',
                  kernel = 'linear')
# write results
y_fc3 = predict(model3, newdata = train_var, type = 'class')

# model evaluation
confusionMatrix(train_y_act, y_fc3)$overall["Accuracy"]

##############################################################################+
# Prediction Model 1 Validation
##############################################################################+

# predict responses with log function
y_fc1_glm = predict(model1, newdata = validate_var_nzv, type = 'response')

# translate into binary results of cancer
y_fc1 <- factor(ifelse(y_fc1_glm > 0.5, 1, 0))

# model evaluation
confusionMatrix(validate_y_act, y_fc1)



##############################################################################+
# Prediction Model 2 Validation
##############################################################################+

# write results
y_fc2 = predict(model2, newdata = validate_var, type = 'class')

# show importance of variables
importance <- varImp(model2)
varImpPlot(model2) 

# model evaluation
confusionMatrix(validate_y_act, y_fc2)




##############################################################################+
# Prediction Model 3 Validation
##############################################################################+

# write results
y_fc3 = predict(model3, newdata = validate_var, type = 'class')

# model evaluation
confusionMatrix(validate_y_act, y_fc3)





