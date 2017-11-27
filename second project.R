setwd("C:/Users/Krishna/Downloads")
getwd()
train_variants=read.csv("HealthCare_1.csv",header=TRUE)
library("tm")
library("tidyr")
library("tibble")
#Converting "DATA" variable to corpus
corpus_1 = VCorpus(VectorSource(train_variants$DATA))
#Converting "SUMMARY" variable to corpus
corpus_2=  VCorpus(VectorSource(train_variants$SUMMARY))
# Combining both corpora  to single corpus 
corpus_3= Corpus(VectorSource(mapply(function(x,y)paste(content(x),content(y)),corpus_2,corpus_1)))
library("rlang")
library("SnowballC")
library("stringr")
corpus_3 = tm_map(corpus_3,content_transformer(stringi::stri_trans_tolower))
corpus_3 = tm_map(corpus_3,removePunctuation,preserve_intra_word_dashes=F)
corpus_3 = tm_map(corpus_3, removeWords, stopwords("english"))
corpus_3 = tm_map(corpus_3, removeNumbers)

# I have created a  my own stopword list
corpus_3 = tm_map(corpus_3,removeWords,c("tab","she","the","mom","wrote","her","and","that","with","for","was","not","will","has","you","this","other","would","have","taken","back","can","like","they","then","this","but","sent","last","when","been","know","had","time","give","about","faxed","days","your","lisa","pls","get","him","are","provider","one","day","next","any","also","mary","thanks","used","seen","told","should","there","did","since","2015","may","march","april","januari","june","juli","februari","pleas","fs24","sscharaux00","new","home"))
corpus_3 = tm_map(corpus_3, PlainTextDocument,language="english")
corpus_3 = tm_map(corpus_3, stemDocument,language="english")
corpus_3 = tm_map(corpus_3, stripWhitespace)
rm(corpus)
# Converting the corpus into Sparse Matrix for the purpose of analysis
dtm = DocumentTermMatrix(corpus_3,control = list(weighting = weightTfIdf))
dtm
dtm = removeSparseTerms(dtm, sparse = 0.95) 
dtm
txt_data_1=as.matrix(dtm)
txt_data.new_1=as.data.frame(txt_data_1)
txt_data_1$xxxxxxxx=NULL
txt_data_1$bxxxxxxxx=NULL
txt_data_1$XXX=NULL
txt_data_1$bxxxxxxxxb=NULL
txt_data_1$xxxxxxxx=NULL
> 
train_variants= read.csv("HealthCare.csv",header=TRUE)
count_word = colSums(txt_data_1)
length(count_word)
freq_word=tibble(name = attributes(count_word)$names, count = count_word)



count_word = colSums(txt_data_1)
length(count_word)
freq_word=tibble(name = attributes(count_word)$names, count = count_word)
library("wordcloud")
wordcloud(names(count_word),count_word,min.freq = 100,scale = c(6,.1), colors = brewer.pal(6, 'Dark2'))

write(freq_word)
library("dplyr")
library("tidyr")
library("Matrix")
summary(train_variants)
summary(train_variants$categories)
summary(train_variants$sub_categories)
summary(train_variants$categories,train_variants$sub_categories)
str(train_variants)

train_data=subset(train_variants,select=c("categories","sub_categories","previous_appointment"))
#ONE HOT ENCODING
sparse_matrix= sparse.model.matrix(categories~.-1,data =train_data)
df1=as.data.frame(as.matrix(sparse_matrix)) 
sm1=sparse.model.matrix(sub_categories~.-1,data=train_data)
df2=as.data.frame(as.matrix(sm1))
df3=cbind(df2,df1)
colnames(df1)
str(df3)
df3$previous_appointmentYES=NULL
colnames(df3)=c("v1","v2","v3","v4","v5","v6","v7","v8","v9","v10","v11","v12","v13","v14","v15","v16","v17","v18","v19","v20","v21",'v22',"v23","v24","v25","v26")
train=cbind(df3,txt_data.new_1)
str(train$categories)
model_train=train[sample(nrow(train),43128,replace=F),]
test_data=train[!(1:nrow(train)) %in% as.numeric(row.names(model_train)),]
library("randomForest")
library("MLmetrics")
library("caret")
library("e1071")
Accuracy(y_pred=pred_df$pred,y_true=test_data$categories)
confusionMatrix(pred_df$pred,test_data$categories)

colnames(model_train[1:25])
colnames(model_train[1:26])
library("mlr")
library("rfsrc")
labels = colnames(model_train)[1:25]
model_train$v25=as.logical(model_train$v25)
model.task = makeMultilabelTask( data = model_train, target = labels)
model.task1=makeMultilabelTask(data = model_train, target = labels)
train_model$v25=as.logical(train_model$v25)
#using model
lrn.rfsrc = makeLearner("multilabel.randomForestSRC")
lrn.rFerns=makeLearner("multilabel.rFerns")
library("rFerns")
lrn.xgboost=makeLearner("multilabel.rFerns")
train_variants$categories=as.logical(train_variants$categories)
model_train$categories=as.logical(as.integer(model_train$categories))
model_train$sub_categories=as.logical(as.integer(model_train$sub_categories))
#Building rfsrc modelon training data
mod=train(lrn.rfsrc,model.task)
saveRDS(mod,"mymodel_rfsrc.rds")
#Building rferns model on train data
mod_rferns=train(lrn.rFerns,model.task,predict)
saveRDS(mod_rferns,"mymodel_rferns.rds")
#predicting test data on rfsrc model
pred_rfsrc_N=predict(mod,newdata = test_data[,26:193])
#predicting test data on rferns model
pred_rferns=predict(mod,newdata = test_data[,26:193])
#Accuracy of  rfsrccmodel
AUC(y_pred=pred_rfsrc_N$data$response.v25,y_true=test_data$v25)
#Accuracy of rferns model
MultiLogLoss(y_pred=pred_rferns$data,y_true=test_data[,1:25])
pred_rfsrc$data$response.v2
pred_rfsrc_new=as.numeric(pred_rfsrc$data)
library("randomForestSRC")
prediction_data_rfsrc=as.data.frame(pred_rfsrc_N$data)
prediction_data_rferns=as.data.frame(pred_rferns$data)
print(mod_rferns)
print(mod)
write.csv(prediction_data_rfsrc)
write.csv(prediction_data_rferns)
