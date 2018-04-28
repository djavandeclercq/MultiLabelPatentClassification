#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(rsconnect)
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



# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  
  ##INPUTS##
  
  #CPC Label Input for Top Histogram Plot  
  cpcs_count_for_plot <- reactive({
    req(input$cpcs_count)
  })  
  
  # Create new df that contains words and their frequency ( where frequency > min_count)
  more_frequent_words <- reactive({ 
    req(input$min_freq) # ensure availablity of value before proceeding
    word_counts %>% 
      filter(n > input$min_freq)
  })
  
  total_words_subset <- reactive({
    req(input$years_selection)
    start_year <- as.character(input$years_selection[1])
    end_year <- as.character(input$years_selection[2])
    total_words %>% 
      subset(IssueDate %in% c(start_year:end_year))
  })
  
  abstract_words_subset <- reactive({
    req(input$years_selection)
    start_year <- as.character(input$years_selection[1])
    end_year <- as.character(input$years_selection[2])
    abstract_words %>% 
      subset(IssueDate %in% c(start_year:end_year))
  }) 
  
  abstract_bigrams_subset <- reactive({
    req(input$years_selection)
    start_year <- as.character(input$years_selection_bigrams[1])
    end_year <- as.character(input$years_selection_bigrams[2])
    num_words <- input$num_top_specific_bigrams
    
    abstract_bigrams_tf_idf %>%
      subset(IssueDate %in% c(start_year:end_year)) %>% 
      mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%
      group_by(IssueDate) %>%
      top_n(num_words) %>%
      ungroup %>%
      arrange(desc(tf_idf))
    
  })  
  
  abstract_trigrams_subset <- reactive({
    req(input$years_selection)
    start_year <- as.character(input$years_selection_trigrams[1])
    end_year <- as.character(input$years_selection_trigrams[2])
    num_words <- input$num_top_specific_trigrams
    
    abstract_trigrams_tf_idf %>%
      subset(IssueDate %in% c(start_year:end_year)) %>% 
      mutate(trigram = factor(trigram, levels = rev(unique(trigram)))) %>%
      group_by(IssueDate) %>%
      top_n(num_words) %>%
      ungroup %>%
      arrange(desc(tf_idf))
    
  })  
  
  bigrams_graph <- reactive({
    bigram_count %>%
      filter(n > input$min_freq_bigram) %>%
      graph_from_data_frame()
  })  
  
  trigrams_graph <- reactive({
    trigram_count %>%
      filter(n > input$min_freq_trigram) %>%
      graph_from_data_frame()
  })  
  
  # Create a subset of data filtering for selected title types
  word_correlations <- reactive({
    req(input$selected_years) # ensure availablity of value before proceeding
    req(input$min_count_for_correlation)
    
    compute_words_correlation(df, input$selected_years, input$min_count_for_correlation) 
  })  
  
  word_correlations_selected_year <- reactive({  
    req(input$num_most_correlated_words)
    req(input$word_of_interest)
    
    word_correlations() %>%
      filter(item1==input$word_of_interest) %>%
      group_by(item1) %>%
      top_n(input$num_most_correlated_words)
  })  
  
  word_correlations_for_graph <- reactive({  
    req(input$min_correlation)
    
    word_correlations() %>%
      filter(correlation >= input$min_correlation) 
  }) 
  
  
  
  
  #Collecting number of CPC labels to run scores
  num_labels_for_scores <- reactive({
    req(input$num_labels)
  })
  
  #Generating scores
  
  
  
  ##OUTPUTS##
  
  
  #generating CPC Histogram plots
  output$CPC_Plot <- renderPlot({
    newcpccount %>%
      mutate(key=reorder(key, value)) %>%
      top_n(input$cpcs_count)%>%
      ggplot(aes(key, value)) +
      geom_bar(stat="identity") + 
      coord_flip()

  })
  
  
  
  #generating scumble plot
  output$scumble_plot <- renderPlot({
    datamldr=mldr_from_dataframe(patents,labelIndices=c(1:input$cpcs_count))
    plot(type="LC",datamldr, color.function = colorspace::heat_hcl)
  })
  
  # Create word_freq_plot object the plotOutput function is expecting 
  output$word_freq_plot <- renderPlot({
    ggplot(data = more_frequent_words(), aes(word,n)) +
      geom_col(show.legend = FALSE) +
      ylab("Word Count") + 
      xlab(NULL) +
      ggtitle("Most Frequent Words") +
      theme(plot.title = element_text(hjust = 0.5)) +
      coord_flip()
  })
  
  output$word_freq_cloud <- renderPlot({
    word_counts %>%
      with(wordcloud(word, n, max.words = input$num_cloud_words, colors=brewer.pal(8, "Dark2")))
  })
  
  output$word_count_per_year <- renderPlot({
    ggplot(data=total_words_subset()) +
      geom_col(aes(reorder(IssueDate, -total), total, fill=IssueDate)) +
      ylab("Total number of words") +
      xlab("Issue Date") +
      ggtitle("Total number of words per year") +
      theme(plot.title = element_text(hjust = 0.5)) +
      coord_flip()
  })
  
  output$word_freq_per_year <- renderPlot({
    ggplot(data=abstract_words_subset(), 
           aes(n/total, fill = IssueDate)) +
      geom_histogram(show.legend = FALSE) +
      xlim(NA, 0.0009) +
      ggtitle("Distribution of words frequencies") +
      theme(plot.title = element_text(hjust = 0.5)) + 
      facet_wrap(~IssueDate, ncol = 2, scales = "free_y")
  })
  
  output$zipf_law_plot <- renderPlot({
    abstract_words_freq %>%
      ggplot(aes(rank, `term frequency`, color = IssueDate)) +
      geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
      geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) +
      ggtitle("Zip's Law Plot") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_log10() +
      scale_y_log10()
  })
  
  
  output$top_bigrams <- renderPlot({
    ggplot(data = abstract_bigrams_subset(), aes(reorder(bigram, tf_idf), tf_idf, fill = IssueDate)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(~IssueDate, ncol = 2, scales = "free") +
      coord_flip()
  })
  
  output$top_trigrams <- renderPlot({
    ggplot(data = abstract_trigrams_subset(), aes(reorder(trigram, tf_idf), tf_idf, fill = IssueDate)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(~IssueDate, ncol = 2, scales = "free") +
      coord_flip()
  }) 
  
  output$bigrams_network <- renderPlot({
    set.seed(seed)
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    ggraph(bigrams_graph(), layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                     arrow = a, end_cap = circle(.07, 'inches')) +
      geom_node_point(color = "gold", size = 5) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      theme_void()
  }) 
  
  output$trigrams_network <- renderPlot({
    set.seed(seed)
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    ggraph(trigrams_graph(), layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                     arrow = a, end_cap = circle(.07, 'inches')) +
      geom_node_point(color = "gold", size = 5) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      theme_void()
  })  
  
  output$correlated_words <- renderPlot({
    data <- word_correlations_selected_year()
    if (dim(data)[1] == 0) {
      return(NULL)
    }   
    data %>%
      ungroup() %>%
      mutate(item2 = reorder(item2, correlation)) %>%
      ggplot(aes(item2, correlation, fill=item2)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ item1, scales = "free_x") +
      coord_flip()
  })
  
  output$correlations_network <- renderPlot({
    data <-  word_correlations_for_graph()
    if (dim(data)[1] == 0) {
      return(NULL)
    }    
    data %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation),
                     edge_colour = 'darkred',
                     show.legend = TRUE) +
      geom_node_point(size = 5) +
      geom_node_text(aes(label = name), repel = TRUE,
                     point.padding = unit(0.2, "lines")) +
      theme_void()
  })
  
  
  #generating scores
  output$chroma_plot <- renderPlot({
    
    #creating dataframe with new label input
    datamldr1=mldr_from_dataframe(newpatents,labelIndices=c(1:input$num_labels))
    
    multi_task_labels <- colnames(score_labels)[1:input$num_labels]
    
    newpatents <- newpatents[,-c((input$num_labels+1):15)]
    
    multi.task = makeMultilabelTask(id = "multi", data=newpatents,target=multi_task_labels)
    
    folds.newpatents <- stratified.kfolds(datamldr1)
    train <- folds.newpatents[[1]]$train
    test <- folds.newpatents[[1]]$test
    train.set=row.names(train$dataset)%>%as.integer()
    str(train.set)
    max(train.set)
    
    
    #Training 6 models on train subset
    mod.rf=train(lrn.rfsrc,multi.task,subset=train.set)
    mod.binrel=mlr::train(lrn.binrel,multi.task,subset=train.set)
    mod.chain=mlr::train(lrn.chain,multi.task,subset=train.set)
    mod.nest=mlr::train(lrn.nest,multi.task,subset=train.set)
    mod.dbr=mlr::train(lrn.dbr,multi.task,subset=train.set)
    mod.stack=mlr::train(lrn.stack,multi.task,subset=train.set)
    
    #Prediction on test subset
    pred.rf=predict(mod.rf,newdata=test$dataset)
    pred.binrel=predict(mod.binrel,newdata=test$dataset)
    pred.chain=predict(mod.chain,newdata=test$dataset)
    pred.nest=predict(mod.nest,newdata=test$dataset)
    pred.dbr=predict(mod.dbr,newdata=test$dataset)
    pred.stack=predict(mod.stack,newdata=test$dataset)
    
    # Performance analysis
    
    
    measures=list(multilabel.acc,multilabel.f1,multilabel.hamloss,multilabel.subset01,multilabel.ppv,multilabel.tpr)
    
    p1=performance(pred.rf,measures)
    p2=performance(pred.binrel,measures)
    p3=performance(pred.chain,measures)
    p4=performance(pred.nest,measures)
    p5=performance(pred.dbr,measures)
    p6=performance(pred.stack,measures)
    
    performance=as.data.frame(rbind(p1,p2,p3,p4,p5,p6))
    
    performance$model=c("RandomForest","Binaryrelevance","Chains","Nested","DBR","Stacking")
    
    plong=gather(performance,metrics,value,multilabel.acc:multilabel.tpr, factor_key=TRUE)
    ggplot(plong)+geom_point(aes(x=model,y=value,color=metrics),size=5,alpha=0.7)+facet_grid(metrics~.)+coord_flip()+theme_bw()+scale_color_manual(values=mycolors)
    
  })
  
  output$matrix_plot <- renderPlot({
    
    #creating dataframe with new label input
    datamldr1=mldr_from_dataframe(newpatents,labelIndices=c(1:input$num_labels))
    
    multi_task_labels <- colnames(score_labels)[1:input$num_labels]
    
    newpatents <- newpatents[,-c((input$num_labels+1):15)]
    
    multi.task = makeMultilabelTask(id = "multi", data=newpatents,target=multi_task_labels)
    
    folds.newpatents <- stratified.kfolds(datamldr1)
    train <- folds.newpatents[[1]]$train
    test <- folds.newpatents[[1]]$test
    train.set=row.names(train$dataset)%>%as.integer()
    str(train.set)
    max(train.set)
    
    
    #Training 6 models on train subset
    mod.rf=train(lrn.rfsrc,multi.task,subset=train.set)
    mod.binrel=mlr::train(lrn.binrel,multi.task,subset=train.set)
    mod.chain=mlr::train(lrn.chain,multi.task,subset=train.set)
    mod.nest=mlr::train(lrn.nest,multi.task,subset=train.set)
    mod.dbr=mlr::train(lrn.dbr,multi.task,subset=train.set)
    mod.stack=mlr::train(lrn.stack,multi.task,subset=train.set)
    
    #Prediction on test subset
    pred.rf=predict(mod.rf,newdata=test$dataset)
    pred.binrel=predict(mod.binrel,newdata=test$dataset)
    pred.chain=predict(mod.chain,newdata=test$dataset)
    pred.nest=predict(mod.nest,newdata=test$dataset)
    pred.dbr=predict(mod.dbr,newdata=test$dataset)
    pred.stack=predict(mod.stack,newdata=test$dataset)
    
    # Performance analysis
    
    
    measures=list(multilabel.acc,multilabel.f1,multilabel.hamloss,multilabel.subset01,multilabel.ppv,multilabel.tpr)
    
    p1=performance(pred.rf,measures)
    p2=performance(pred.binrel,measures)
    p3=performance(pred.chain,measures)
    p4=performance(pred.nest,measures)
    p5=performance(pred.dbr,measures)
    p6=performance(pred.stack,measures)
    
    performance=as.data.frame(rbind(p1,p2,p3,p4,p5,p6))
    
    performance$model=c("RandomForest","Binaryrelevance","Chains","Nested","DBR","Stacking")
    
    plong=gather(performance,metrics,value,multilabel.acc:multilabel.tpr, factor_key=TRUE)
    
    ggplot(plong)+geom_tile(aes(x=model,y=metrics,fill=value),color="black")+geom_text(aes(x=model,y=metrics,label=round(value,3)),color="black")+scale_fill_distiller(palette = "Spectral")
    
  })
  

  
})
