setwd("C:/Users/Krishna/Downloads")
getwd()
train_variants=read.csv("HealthCare_1.csv",header=TRUE)
test_variants=read.csv("test_variants.csv")
library("tm")
library("tidyr")
library("tibble")
library("quanteda")

rm(text)
#Starting preprocessing:converting the text into corpus anddoing feature engineering with tf-idf method.
set.seed(1723)
corpus_1 = VCorpus(VectorSource(train_variants$DATA))
corpus_2=  VCorpus(VectorSource(train_variants$SUMMARY))
corpus=tm_reduce(corpus_2,corpus_1)
corpus_3=Corpus(VectorSource(mapply(function(x, y) paste(content(x), content(y)), corpus_2,corpus_1)
))
rm(corpus_3)
corpus=corpus_3
rm(corpus_3)
library("quanteda")
library("rlang")
library("SnowballC")
library("stringr")
corpus = tm_map(corpus,content_transformer(stringi::stri_trans_tolower))
corpus=gsub("{}","",corpus)
corpus=gsub("}"," ")
corpus = tm_map(corpus,removePunctuation,preserve_intra_word_dashes=F)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, removeNumbers)
Clean_String <- function(string){
  # Lowercase
  corpus_3=tolower(string)
  #' Remove everything that is not a number or letter (may want to keep more 
  #' stuff in your actual analyses). 
  corpus<- stringr::str_replace_all(corpus_3,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  corpus_3 <- stringr::str_replace_all(corpus_3,"[\\s]+", " ")
  # Split it
  corpus_3 <- stringr::str_split(corpus_3, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(corpus_3 == "")
  if(length(indexes) > 0){
    
    corpus_3 <- corpus_3[-indexes]
  } 
  return(temp)
}
corpus <- tm_map(corpus_3, function(x) iconv(x, "latin1", "ASCII", sub=""))
# I have created a  my own stopword list
corpus = tm_map(corpus,removeWords,c("tab","she","the","mom","wrote","her","and","that","with","for","was","not","will","has","you","this","other","would","have","taken","back","can","like","they","then","this","but","sent","last","when","been","know","had","time","give","about","faxed","days","your","lisa","pls","get","him","are","provider","one","day","next","any","also","mary","thanks","used","seen","told","should","there","did","since","2015","may","march","april","januari","june","juli","februari","pleas","fs24","sscharaux00","new","home","want","his","name","today","work","spoke","say","ask","have","send","done","xxxxpar","thank","what","now","doe","them","just","all","talk","xxxx","par"))
corpus = tm_map(corpus, PlainTextDocument,language="english")
corpus = tm_map(corpus, stemDocument,language="english")
corpus = tm_map(corpus, stripWhitespace)
                                     rm(corpus)
# Converting the corpus into Sparse Matrix for the purpose of analysis
dtm = DocumentTermMatrix(corpus,control = list(weighting = weightTfIdf))
dtm
dtm = removeSparseTerms(dtm, sparse = 0.95) 
dtm
txt_data=as.matrix(dtm)
txt_data.new=as.data.frame(txt_data)
train_variants= read.csv("HealthCare.csv",header=TRUE)
count_word = colSums(txt_data)
length(count_word)
freq_word=tibble(name = attributes(count_word)$names, count = count_word)
write(freq_word)
ord <- order(freq_word,decreasing=TRUE)
to(freq_word,10)
top_n(freq_word,-10)
library("wordcloud")
wordcloud(names(count_word), count_word, min.freq = 100,scale = c(6,.1), colors = brewer.pal(6, 'Dark2'))
 summarise(train_variants$categories)                                    
library("dplyr")
library("tidyr")
 library("Matrix")
 summary(train_variants)
 summary(train_variants$categories)
 summary(train_variants$sub_categories)
 summary(train_variants$categories,train_variants$sub_categories)
 str(train_variants)
 train_data=subset(train_variants,select=c("categories","sub_categories","previous_appointment"))
 train=cbind(df3,txt_data.new)
 str(train$categories)
 model_train=train[sample(nrow(train),45806,replace=F),]
 test_data=train[!(1:nrow(train)) %in% as.numeric(row.names(model_train)),]
 txt_data.new$`2015`=NULL
 sparse_matrix= sparse.model.matrix(categories~.-1,data =train_data)
 df1=as.data.frame(as.matrix(sparse_matrix)) 
 sm1=sparse.model.matrix(sub_categories~.-1,data=train_data)
 df2=as.data.frame(as.matrix(sm1))
 df3=cbind(df2,df1)
 str(df3)
 colnames(df3,do.NULL = TRUE)
 df3$previous_appointmentYES=NULL
 library("randomForest")
 rf=randomForest(categories,sub_categories~.,data=model_train,ntree=500)
 pred=predict(rf,test_data[,-1])
 pred_df=as.data.frame(pred)
 library("MLmetrics")
 library("caret")
 library("e1071")
 Accuracy(y_pred=pred_df$pred,y_true=test_data$categories)
 confusionMatrix(pred_df$pred,test_data$categories)
 rf.cv <- rf.crossValidation(rf, test_data[,-1], p=0.10, n=99, ntree=501) 
 train$`2015`=NULL
 rf_new=randomForestSRC::rfsrc((categories,sub_categories)~.,data=model_train,ntree=500)
 lrn.rfsrc = makeLearner(".xgboost")
 colnames(model_train[1:25])
 colnames(model_train[1:26])
 model_train.new=model_train[1:26,header=FALSE,27:176,header=TRUE]
 library("mlr")
 data = getTaskData(yeast.task)
 labels = colnames(model_train)[1:25]
 model.task = makeMultilabelTask( data = model_train, target = labels)
 model.task1=makeMultilabelTask(data = model_train, target = labels)
 lrn.rfsrc = makeLearner("multilabel.randomForestSRC")
 lrn.rFerns=makeLearner("multilabel.rFerns")
 library("rFerns")
 lrn.xgboost=makeLearner("multilabel.rFerns")
 train_variants$categories=as.logical(train_variants$categories)
 model_train$categories=as.logical(as.integer(model_train$categories))
 model_train$sub_categories=as.logical(as.integer(model_train$sub_categories))
 mod=train(lrn.rfsrc,model.task)
 mod_rferns=train(lrn.rFerns,model.task)
 pred_rfsrc_N=predict(mod,newdata = test_data[,26:176])
 pred_rferns=predict(mod_rferns,newdata=test_data[,26:176])
 test_data$categories=as.logical(as.integer(test_data$categories))
 test_data$sub_categories=as.logical(as.integer(test_data$sub_categories))
 Accuracy(y_pred=pred_rferns$data,y_true=test_data[,1:25])
 confusionMatrix()
 Accuracy(y_pred=pred_rfsrc$data,y_true=test_data[,1:25])
 pred_rfsrc$data$response.v2
 pred_rfsrc_new=as.numeric(pred_rfsrc$data)
 colnames(df3[1])="v1"
 assertthat.dataframe(model_train, names = "strict")
 df3$previous_appointmentYES=NULL
 write.csv(df3)
 library("randomForestSRC")
 rename(model_train, c("categoriesAPPOINTMENTS"="v1"))                                     
                       [2] "categoriesASK_A_DOCTOR"                                     
                       [3] "categoriesLAB"                                              
                       [4] "categoriesMISCELLANEOUS"                                    
                       [5] "categoriesPRESCRIPTION"                                     
                       [6] "sub_categoriesCANCELLATION"                                 
                       [7] "sub_categoriesCHANGE OF HOSPITAL"                           
                       [8] "sub_categoriesCHANGE OF PHARMACY"                           
                       [9] "sub_categoriesCHANGE OF PROVIDER"                           
                       [10] "sub_categoriesFOLLOW UP ON PREVIOUS REQUEST"                
                       [11] "sub_categoriesLAB RESULTS"                                  
                       [12] "sub_categoriesMEDICATION RELATED"                           
                       [13] "sub_categoriesNEW APPOINTMENT"                              
                       [14] "sub_categoriesOTHERS"                                       
                       [15] "sub_categoriesPRIOR AUTHORIZATION"                          
                       [16] "sub_categoriesPROVIDER"                                     
                       [17] "sub_categoriesQUERIES FROM INSURANCE FIRM"                  
                       [18] "sub_categoriesQUERIES FROM PHARMACY"                        
                       [19] "sub_categoriesQUERY ON CURRENT APPOINTMENT"                 
                       [20] "sub_categoriesREFILL"                                       
                       [21] "sub_categoriesRESCHEDULING"                                 
                       [22] "sub_categoriesRUNNING LATE TO APPOINTMENT"                  
                       [23] "sub_categoriesSHARING OF HEALTH RECORDS (FAX, E-MAIL, ETC.)"
                       [24] "sub_categoriesSHARING OF LAB RECORDS (FAX, E-MAIL, ETC.)"   
                       [25] "sub_categoriesSYMPTOMS"               ))

sum(is.na(df3))
colnames(df3)=c("v1","v2","v3","v4","v5","v6","v7","v8","v9","v10","v11","v12","v13","v14","v15","v16","v17","v18","v19","v20","v21",'v22',"v23","v24","v25","v26")
str(model_train$v1)
model_train$v25=as.logical(model_train$v25)
corpus_1 = VCorpus(VectorSource(train_variants$DATA))
corpus_1 = tm_map(corpus_1,removePunctuation,preserve_intra_word_dashes=F)
corpus_1 = tm_map(corpus_1,content_transformer(stringi::stri_trans_tolower))
corpus_1 = tm_map(corpus_1, removeWords, stopwords("english"))
corpus_1 = tm_map(corpus_1, removeNumbers)
corpus_1 = tm_map(corpus_1,removeWords,c("tab","she","the","mom","wrote","her","and","that","with","for","was","not","will","has","you","this","other","would","have","taken","back","can","like","they","then","this","but","sent","last","when","been","know","had","time","give","about","faxed","days","your","lisa","pls","get","him","are","provider","one","day","next","any","also","mary","thanks","used","seen","told","should","there","did","since","2015","may","march","april","januari","june","juli","februari","pleas","fs24","sscharaux00","new","home","want","his","name","today","work","spoke","say","ask","have","send","done","xxxxpar","thank","what","now","doe","them","just","all","talk","xxxx","par"))
corpus_1 = tm_map(corpus_1, PlainTextDocument,language="english")
corpus_1 = tm_map(corpus_1, stemDocument,language="english")
corpus_1 = tm_map(corpus_1, stripWhitespace)
rm(corpus)
# Converting the corpus into Sparse Matrix for the purpose of analysis
dtm = DocumentTermMatrix(corpus_1,control = list(weighting = weightTfIdf))
dtm
dtm = removeSparseTerms(dtm, sparse = 0.95) 
dtm
txt_data_1=as.matrix(dtm)
txt_data.new_1=as.data.frame(txt_data_1)
train_variants= read.csv("HealthCare.csv",header=TRUE)
count_word = colSums(txt_data_2)
length(count_word)
freq_word=tibble(name = attributes(count_word)$names, count = count_word)
corpus_2 = VCorpus(VectorSource(train_variants$SUMMARY))
corpus_2 = tm_map(corpus_2,removePunctuation,preserve_intra_word_dashes=F)
corpus_2 = tm_map(corpus_2,content_transformer(stringi::stri_trans_tolower))
corpus_2 = tm_map(corpus_2, removeWords, stopwords("english"))
corpus_2 = tm_map(corpus_2, removeNumbers)
corpus_2 = tm_map(corpus_2,removeWords,c("tab","she","the","mom","wrote","her","and","that","with","for","was","not","will","has","you","this","other","would","have","taken","back","can","like","they","then","this","but","sent","last","when","been","know","had","time","give","about","faxed","days","your","lisa","pls","get","him","are","provider","one","day","next","any","also","mary","thanks","used","seen","told","should","there","did","since","2015","may","march","april","januari","june","juli","februari","pleas","fs24","sscharaux00","new","home","want","his","name","today","work","spoke","say","ask","have","send","done","xxxxpar","thank","what","now","doe","them","just","all","talk","xxxx","par"))
corpus_2 = tm_map(corpus_2, PlainTextDocument,language="english")
corpus_2 = tm_map(corpus_2, stemDocument,language="english")
corpus_2 = tm_map(corpus_2, stripWhitespace)
rm(corpus)
# Converting the corpus into Sparse Matrix for the purpose of analysis
dtm = DocumentTermMatrix(corpus_2,control = list(weighting = weightTfIdf))
dtm
dtm = removeSparseTerms(dtm, sparse = 0.95) 
dtm
txt_data_2=as.matrix(dtm)
txt_data.new_2=as.data.frame(txt_data_2)
train_1=cbind(df3,txt_data.new_2,txt_data.new_1)
model_train_1=train_1[sample(nrow(train),45806,replace=F),]
test_data_1=train_1[!(1:nrow(train)) %in% as.numeric(row.names(model_train_1)),]
data = getTaskData(yeast.task)
labels = colnames(model_train_1)[1:25]
model.task_n = makeMultilabelTask( data = model_train_1, target = labels)
model.task1_n=makeMultilabelTask(data = model_train_1, target = labels)
lrn.rfsrc = makeLearner("multilabel.randomForestSRC")
lrn.rFerns=makeLearner("multilabel.rFerns")
print(mod_rferns)
print(mod)
control <- trainControl(method = "cv", number = 10)

