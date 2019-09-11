#import data
df_hr = read.csv('https://raw.githubusercontent.com/arikunco/machinelearning/master/dataset/HR_comma_sep.csv')


#mengetahui isi data
head(df_hr)

#mengetahui summary data
summary(df_hr)


#library yang di butuhkan
#install.packages('caTools')
library('caTools')
library(rpart)
library(randomForest)

# Menyiapkan sample data
df_hr$left <- as.factor(df_hr$left)

smp_size = floor(0.8 * nrow(df_hr))
smp_size

set.seed(123)

train_ind = sample(seq_len(nrow(df_hr)), size = smp_size)


train_data = df_hr[train_ind, ]
test_data = df_hr[-train_ind, ]

head(train_data)
head(test_data)

summary(train)
summary(test)


# Decision Tree 
tree <- rpart(left ~ ., 
              data = data.frame(train_data), method = "class")

predict(tree, data.frame(test_data),type="class" )
table(test_data[,'left'],predict(tree, data.frame(test_data),type="class"))



# Random forest 
# randomforest training 
randomFor <- randomForest(left ~ ., data = data.frame(train_data), ntree=100, importance = TRUE)
predict(randomFor, data.frame(test_data),type="class")
table(test_data[,'left'],predict(tree, data.frame(test_data), type="class"))


#confusion matrix Decision Tree 

TP_tree <- conf_decisionTree[1, 1]
FN_tree <- conf_decisionTree[1, 2]
FP_tree <- conf_decisionTree[2,1]
TN_tree <- conf_decisionTree[2,2]

#confusion matrix Random forest

TP_forest <- conf_randomFor[1, 1]
FN_forest <- conf_randomFor[1, 2]
FP_forest <- conf_randomFor[2,1]
TN_forest <- conf_randomFor[2,2]

#akurasi decision tree
akurasi_tree <- (TP_tree + TN_tree)/(TP_tree+FN_tree+FP_tree+TN_tree)
akurasi_tree
#0.9722222

#akurasi random forest
akurasi_forest <- (TP_forest+TN_forest)/(TP_forest+FN_forest+FP_forest+TN_forest)
akurasi_forest
#0.9933333


#presisi decision tree
presisi_tree <- TP_tree / (TP_tree+FP_tree)
presisi_tree
#0.9750363


#presisi random forest
presisi_forest <- TP_forest / (TP_forest+FP_forest)
presisi_forest
#0.9926858


#recall decision tree
recall_tree <- TP_tree / (TP_tree+FN_tree)
recall_tree
#0.9885227


#recall random forest
recall_forest <- TP_forest / (TP_forest+FN_forest)
recall_forest
#0.9985285
