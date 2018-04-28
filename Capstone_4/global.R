library(rsconnect)
library(wordVectors)
library(magrittr)
library(dplyr)
library(tidytext)
library(lubridate)
library(tm)
library(tidyverse)
library(stringr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(readr)
library(tidyr)
library(igraph)
library(ggraph)
library(widyr)
library(topicmodels)
library(Matrix)
library(janeaustenr)
library(scales)
library(textstem)
library(DT)
library(shiny)
library(shinythemes)
library(tools)
library(shinycssloaders)

library(caret)
library(mlr)
library(mldr)
library(viridis)
library(gridExtra)
library(FSelector)
library(randomForestSRC)
library(caTools)
library(stratification)
library(splitstackshape)
library(mldr.datasets)



# Set a nice colour scheme
my_theme <- function(base_size = 10, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 10),
      panel.grid.major = element_line(color = "gray"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#f7fdff"),
      strip.background = element_rect(fill = "#001d60", color = "#00113a", size =0.5),
      strip.text = element_text(face = "bold", size = 10, color = "white"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey5", fill = NA, size = 0.5)
    )
}

theme_set(my_theme())


myfillcolors=c("#ff003f","#0094ff", "#ae00ff" , "#94ff00", "#ffc700","#fc1814")
mycolors=c("#db0229","#026bdb","#48039e","#0d7502","#c97c02","#c40c09")


#Load data
#getwd()
#setwd("/Users/Ben/Documents/Berkeley/Capstone/Data Files/Rshiny/Capstone_4")


patents <- read.csv("PatentWithTopicsFinal.csv", na.strings = "NA")
patents <- na.omit(patents)
patents <- sample_n(patents, 100)


#set as logical to make column into true or false values
patents$G05D <- as.logical(patents$G05D)
patents$B60W <- as.logical(patents$B60W)
patents$G01S <- as.logical(patents$G01S)
patents$G08G <- as.logical(patents$G08G)
patents$B62D <- as.logical(patents$B62D)
patents$G01C <- as.logical(patents$G01C)
patents$G06K <- as.logical(patents$G06K)
patents$B25J <- as.logical(patents$B25J)
patents$Y10S <- as.logical(patents$Y10S)
patents$G05B <- as.logical(patents$G05B)
patents$G06T <- as.logical(patents$G06T)
patents$Y10T <- as.logical(patents$Y10T)
patents$G06F <- as.logical(patents$G06F)
patents$B60K <- as.logical(patents$B60K)
patents$G06Q <- as.logical(patents$G06Q)

#set as transmute into order to extract true values from each patent classifcation
proccpccount <- transmute( patents,
                          countG05D = table(patents$G05D)["TRUE"],
                          countB60W = table(patents$B60W)["TRUE"],
                          countG01S = table(patents$G01S)["TRUE"],
                          countG08G = table(patents$G08G)["TRUE"],
                          countB62D = table(patents$B62D)["TRUE"],
                          countG01C = table(patents$G01C)["TRUE"],
                          countG06K = table(patents$G06K)["TRUE"],
                          countB25J = table(patents$B25J)["TRUE"],
                          countY10S = table(patents$Y10S)["TRUE"],
                          countG05B = table(patents$G05B)["TRUE"],
                          countG06T = table(patents$G06T)["TRUE"],
                          countY10T = table(patents$Y10T)["TRUE"],
                          countG06F = table(patents$G06F)["TRUE"],
                          countB60K = table(patents$B60K)["TRUE"],
                          countG06Q = table(patents$G06Q)["TRUE"]
)

#converting into dataframe of counts of each histogram
newcpccount<-head(proccpccount,1)
newcpccount<-as.data.frame(newcpccount)
newcpccount <- gather(newcpccount, key = "key", value = "value") 

#Carrying out further preprocessing for scores in Rshiny

#creating new dataframe for scores
newpatents <- patents[, -c(16:18)] #drop labels we aren't using
write.csv(newpatents, "Fifteen_Label.csv", row.names=FALSE) #make sure to delete the column with cell numbers for stratified sampling

newpatents <- read.csv("Fifteen_Label.csv")


taskG05D = makeClassifTask(id = "G05D", data=newpatents[,-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)],target = "G05D",positive="TRUE")
taskB60W = makeClassifTask(id = "B60W", data=newpatents[,-c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)],target = "B60W",positive="TRUE")
taskG01S = makeClassifTask(id = "G01S", data=newpatents[,-c(1,2,4,5,6,7,8,9,10,11,12,13,14,15)],target = "G01S",positive="TRUE")
taskG08G = makeClassifTask(id = "G08G", data=newpatents[,-c(1,2,3,5,6,7,8,9,10,11,12,13,14,15)],target = "G08G",positive="TRUE")
taskB62D = makeClassifTask(id = "B62D", data=newpatents[,-c(1,2,3,4,6,7,8,9,10,11,12,13,14,15)],target = "B62D",positive="TRUE")

taskG01C = makeClassifTask(id = "G01C", data=newpatents[,-c(1,2,3,4,5,7,8,9,10,11,12,13,14,15)],target = "G01C",positive="TRUE")
taskG06K = makeClassifTask(id = "G06K", data=newpatents[,-c(1,2,3,4,5,6,8,9,10,11,12,13,14,15)],target = "G06K",positive="TRUE")
taskB25J = makeClassifTask(id = "B25J", data=newpatents[,-c(1,2,3,4,5,6,7,9,10,11,12,13,14,15)],target = "B25J",positive="TRUE")
taskY10S = makeClassifTask(id = "Y10S", data=newpatents[,-c(1,2,3,4,5,6,7,8,10,11,12,13,14,15)],target = "Y10S",positive="TRUE")
taskG05B = makeClassifTask(id = "G05B", data=newpatents[,-c(1,2,3,4,5,6,7,8,9,11,12,13,14,15)],target = "G05B",positive="TRUE")


taskG06T = makeClassifTask(id = "G06T", data=newpatents[,-c(1,2,3,4,5,6,7,8,9,10,12,13,14,15)],target = "G06T",positive="TRUE")
taskY10T = makeClassifTask(id = "Y10T", data=newpatents[,-c(1,2,3,4,5,6,7,8,9,10,11,13,14,15)],target = "Y10T",positive="TRUE")
taskG06F = makeClassifTask(id = "G06F", data=newpatents[,-c(1,2,3,4,5,6,7,8,9,10,11,12,14,15)],target = "G06F",positive="TRUE")
taskB60K = makeClassifTask(id = "B60K", data=newpatents[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,15)],target = "B60K",positive="TRUE")
taskG06Q = makeClassifTask(id = "G06Q", data=newpatents[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)],target = "G06Q",positive="TRUE")

#extracting labels so that scoring can be done
score_labels <- head(newpatents,1)
#colnames(score_labels)[1:2]


# Set multilabel task
#labels=c("G05D","B60W","G01S","G08G","B62D")
#multi.task = makeMultilabelTask(id = "multi", data=patents,target=labels)
#
#
## Stratified sampling of the dataset
#folds.patents <- stratified.kfolds(datamldr)
#train <- folds.patents[[1]]$train
#test <- folds.patents[[1]]$test
#
#train.set=row.names(train$dataset)%>%as.integer()
#str(train.set)
#
#max(train.set)
#
# Problem transformation method
lrn.rfsrc = makeLearner("multilabel.randomForestSRC",predict.type = "prob")

#Core learner
lrn.core= makeLearner("classif.rpart", predict.type = "prob")

#5 Wrapped learners
lrn.binrel=makeMultilabelBinaryRelevanceWrapper(lrn.core)
lrn.chain=makeMultilabelClassifierChainsWrapper(lrn.core)
lrn.nest=makeMultilabelNestedStackingWrapper(lrn.core)
lrn.dbr= makeMultilabelDBRWrapper(lrn.core)
lrn.stack=makeMultilabelStackingWrapper(lrn.core)


#---------------


df <-read.csv("PatentWithTopicsFinal.csv")
#df$Abs <- as.character(df$Abs)

df$Abstract <- as.character(df$Abstract)

df$IssueDate <- as.factor(df$IssueDate)

# Converting text vector into a dataframe
text_column <- "Abstract"
tokenized_text <- data_frame(line = 1:length(df[,c(text_column)]), text = df[,c(text_column)]) %>%
  unnest_tokens(word, text)

#Words Count
word_counts <- tokenized_text %>% 
  count(word, sort = TRUE)  %>% 
  mutate(word = reorder(word, n))

#count the number of times a word appears in abstracts during a year
create_abstract_words <- function(df){
  abstract_words <- df %>% 
    unnest_tokens(word, Abstract) %>%
    count(IssueDate, word, sort=TRUE) %>% 
    ungroup()      
}
abstract_words <- create_abstract_words(df)

#count of the total number of words within a year
total_words <- abstract_words %>% 
  group_by(IssueDate) %>% 
  summarize(total = sum(n))

abstract_words <- left_join(abstract_words, total_words, by = "IssueDate")

#frequency of every word in a year
abstract_words_freq <- abstract_words %>%
  group_by(IssueDate) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

#create a dataframe with bigrams that appear in the text
create_abstract_bigrams <- function(df){
  abstract_bigrams <- df %>% 
    select(Abstract, IssueDate) %>% 
    unnest_tokens(bigram, Abstract, token = "ngrams", n = 2) %>% 
    separate(bigram, c("word1", "word2"), sep = " ")
  
  abstract_bigrams
}
abstract_bigrams <- create_abstract_bigrams(df)

#compute the tf-idf of bigrams
abstract_bigrams_tf_idf <- abstract_bigrams %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(IssueDate, bigram) %>%
  bind_tf_idf(bigram, IssueDate, n) %>%
  arrange(desc(tf_idf))

#count of bigrams
bigram_count <- abstract_bigrams %>%
  count(word1, word2, sort = TRUE)

#create a dataframe with trigrams that appear in the text
create_abstract_trigrams <- function(df){
  abstract_trigrams <- df %>% 
    select(Abstract, IssueDate) %>% 
    unnest_tokens(trigram, Abstract, token = "ngrams", n = 3) %>% 
    separate(trigram, c("word1", "word2", "word3"), sep = " ") 
}
abstract_trigrams <- create_abstract_trigrams(df)

#compute the tf-idf of trigrams
abstract_trigrams_tf_idf <- abstract_trigrams %>%
  unite(trigram, word1, word2, word3, sep = " ") %>%
  count(IssueDate, trigram) %>%
  bind_tf_idf(trigram, IssueDate, n) %>%
  arrange(desc(tf_idf))

#count of trigrams
trigram_count <- abstract_trigrams %>%
  count(word1, word2, word3, sort = TRUE)

select_patents_certain_year <- function(df, selected_year){
  #select patents where IssueDate belongs only to selected_year
  abstract_section_words <- df %>%
    filter(IssueDate %in% selected_year) %>%
    mutate(section = row_number()) %>%
    filter(section > 0) %>%
    unnest_tokens(word, Abstract) 
}
compute_words_correlation <- function(df, selected_year, min_count){
  #compute pairwise correlations between words in the patent
  word_cors <- select_patents_certain_year(df, selected_year) %>%
    group_by(word) %>%
    filter(n() >= min_count) %>%
    pairwise_cor(word, section, sort = TRUE)
  word_cors
}

#global parameters
min_year <- min(as.integer(as.character(df$IssueDate)), na.rm = T)
max_year <- max(as.integer(as.character(df$IssueDate)), na.rm = T)
unique_years <- sort(unique(na.omit(df$IssueDate)), decreasing = T)
seed <- 2016
num_topics <- 50