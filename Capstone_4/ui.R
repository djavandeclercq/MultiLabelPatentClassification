#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

#Load data
#getwd()
#setwd("/Users/Ben/Documents/Berkeley/Capstone/Data Files/Rshiny/Capstone_4")


# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("united"),
  
  navbarPage(title="Machine Learning",
             
             tabPanel("Plots",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          sliderInput("cpcs_count",
                                      "Top CPCs:",
                                      min = 5,
                                      max = 15,
                                      step = 5,
                                      value = 5)
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          width = 9, 
                          
                          tabsetPanel(
                            
                            tabPanel("Top CPCs" ,
                                      plotOutput(outputId="CPC_Plot", height="600px") %>% withSpinner()
                            ),
                            
                            
                            tabPanel("Scumble", 
                                     plotOutput(outputId="scumble_plot", height = "600px") %>% withSpinner()
                                     )
                          )
                          
                        )
                        
                      ) 
                      
             ),
             tabPanel("Unigrams",
                      sidebarLayout(
                        # Inputs
                        sidebarPanel(
                          width = 3,
                          # Header
                          h4("Word Count"),
                          
                          # Select number of words in the cloud
                          numericInput(inputId = "num_cloud_words", 
                                       label = "Number of words in the cloud:", 
                                       min = 20, max = 200, 
                                       value = 100,
                                       step = 10),
                          
                          hr(),
                          # Select minimum frequency of the word
                          numericInput(inputId = "min_freq", 
                                       label = "Minimum Frequency:", 
                                       min = max(word_counts$n)%/%10, max = max(word_counts$n), 
                                       value = max(word_counts$n)%/%10,
                                       step = 50),
                          
                          hr(),
                          
                          #  Select years
                          sliderInput("years_selection", 
                                      label = h4("Select years to observe"), 
                                      min = max(min_year,1990), 
                                      max = max_year, 
                                      value = c(2010, 2016),
                                      round = TRUE,
                                      step = 1,
                                      sep = ""
                          ),
                          
                          # Built with Shiny by RStudio
                          br(),
                          h5("Built with",
                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                             "by",
                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                             ".")
                          
                        ),
                        # Output:
                        mainPanel(
                          width = 9,
                          tabsetPanel(id = "tabspanel", type = "tabs",
                                      tabPanel(title = "Word Count",
                                               br(),
                                               plotOutput(outputId = "word_freq_cloud") %>% withSpinner() ,
                                               br(),
                                               plotOutput(outputId = "word_freq_plot") %>% withSpinner() ),
                                      tabPanel(title = "Number of words over years", 
                                               br(),
                                               plotOutput(outputId = "word_count_per_year") %>% withSpinner() ),
                                      tabPanel(title = "Zipf's Law",
                                               br(),
                                               plotOutput(outputId = "word_freq_per_year") %>% withSpinner() ,
                                               br(),
                                               plotOutput(outputId = "zipf_law_plot", height = "700px") %>% withSpinner() )
                                      
                          )
                        )
                      )   
             ),
             tabPanel("Bigrams",
                      sidebarLayout(
                        # Inputs
                        sidebarPanel(
                          width = 3,
                          #  Select years
                          sliderInput("years_selection_bigrams", 
                                      label = h4("Select years to observe"), 
                                      min = max(min_year,1990), 
                                      max = max_year, 
                                      value = c(2010, 2016),
                                      round = TRUE,
                                      step = 1,
                                      sep = ""
                          ),
                          
                          hr(),
                          # Select number of most specific bigrams to plot
                          numericInput(inputId = "num_top_specific_bigrams", 
                                       label = "Number of top specific bigrams:", 
                                       min = 1, max = 50, 
                                       value = 10,
                                       step = 2),
                          
                          hr(),
                          # Select minimum frequency of the bigram
                          numericInput(inputId = "min_freq_bigram", 
                                       label = "Minimum Bigram Frequency:", 
                                       min = 50, max = max(bigram_count$n), 
                                       value = 50,
                                       step = 25),  
                          
                          # Built with Shiny by RStudio
                          br(),
                          h5("Built with",
                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                             "by",
                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                             ".")
                          
                        ),
                        # Output:
                        mainPanel(
                          width = 9,
                          tabsetPanel(id = "tabspanel", type = "tabs",
                                      tabPanel(title = "Most Specific Bigrams ", 
                                               br(),
                                               plotOutput(outputId = "top_bigrams",
                                                          height = "800px") %>% withSpinner() )
                                      
                                      
                          )
                        )
                      )   
             ),
             tabPanel("Trigrams",
                      sidebarLayout(
                        # Inputs
                        sidebarPanel(
                          width = 3,
                          #  Select years
                          sliderInput("years_selection_trigrams", 
                                      label = h4("Select years to observe"), 
                                      min = max(min_year,1990), 
                                      max = max_year, 
                                      value = c(2010, 2016),
                                      round = TRUE,
                                      step = 1,
                                      sep = ""
                          ),
                          
                          hr(),
                          # Select number of most specific trigrams to plot
                          numericInput(inputId = "num_top_specific_trigrams", 
                                       label = "Number of top specific trigrams:", 
                                       min = 1, max = 50, 
                                       value = 10,
                                       step = 2),
                          
                          hr(),
                          # Select minimum frequency of the trigram
                          numericInput(inputId = "min_freq_trigram", 
                                       label = "Minimum Trigram Frequency:", 
                                       min = 25, max = max(trigram_count$n), 
                                       value = 25,
                                       step = 25), 
                          # Built with Shiny by RStudio
                          br(),
                          h5("Built with",
                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                             "by",
                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                             ".")
                          
                        ),
                        # Output:
                        mainPanel(
                          width = 9,
                          tabsetPanel(id = "tabspanel", type = "tabs",
                                      tabPanel(title = "Most Specific Trigrams ", 
                                               br(),
                                               plotOutput(outputId = "top_trigrams",
                                                          height = "800px") %>% withSpinner() )
                                      
                                      
                          )
                        )
                      )   
             ),
             
             
             tabPanel("Scores",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          sliderInput("num_labels",
                                      "Number of CPCs to be evaluated:",
                                      min = 2,
                                      max = 15,
                                      step = 1,
                                      value = 5)
                        ),
                        
                        mainPanel(
                          
                          width = 9, 
                          
                          tabsetPanel(
                            
                            tabPanel("Chromatography" ,
                                      plotOutput(outputId="chroma_plot", height="600px") %>% withSpinner()
                            ),
                            
                            
                            tabPanel("Matrix", 
                                      plotOutput(outputId="matrix_plot", height = "600px") %>% withSpinner()
                            )
                          )
                        )
                        
  )
)
)

)
)
                          
                          
                          

                          


             #         mainPanel(
             #           width = 9,
             #           tabsetPanel(id = "tabspanel", type = "tabs",
             #                       tabPanel(title = "Countries comparison", 
             #                                br(),
             #                                plotOutput(outputId = "patents_per_country",
             #                                           height = "800px") %>% withSpinner() ),
             #                       tabPanel(title = "Countries evolution", 
             #                                br(),
             #                                plotOutput(outputId = "patents_per_country_overtime") %>% withSpinner() )
             #                       
             #                       
             #           )
             #         )
                      
             #,
             
#             tabPanel("Scumble",
#                      sidebarLayout(
#                        # Inputs
#                        sidebarPanel(
#                          width = 3,
#                          # Header
#                          h4("Label Count"),
#                          
#                          # Select number of CPC labels 
#                          numericInput(inputId = "cpc_labels", 
#                                       label = "Number of CPC labels:", 
#                                       min = 5, max = 15, 
#                                       value = 100,
#                                       step = 5),
#                          
#                          
#                          
#                          # Built with Shiny by RStudio
#                          br(),
#                          h5("Built with",
#                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
#                             "by",
#                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
#                             ".")
#                          
#                        ),
#                        # Output:
#                        mainPanel(
#                          width = 9,
#                          tabsetPanel(id = "tabspanel", type = "tabs",
#                                      tabPanel(title = "Scrumble plot",
#                                               br(),
#                                               plotOutput(outputId = "scrumble_plot") %>% withSpinner() 
#                                               
#                                               
#                                      )
#                          )
#                        )   
#                      )
#             
#             )
#             
             
  