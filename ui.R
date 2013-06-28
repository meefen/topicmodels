library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Topic Modeling and Visualization of Knowledge Forum Discussion"),
  
  sidebarPanel(
    wellPanel(
      h4("Brief Explanation"),
      HTML("This work is inspired by Norma Ming's presentation at CSCL 2013. 
The main goal is to develop useful visualizations to help interpreting complex 
knowledge spaces produced in knowledge building. This tool applies topic 
modeling using latent Dirichlet allocation (LDA, rather than pLSA in Ming's 
work) on a set of Knowledge Forum notes, and extracts <i>k</i> topics in the 
discussion. Based on LDA results, each note could be represented as a vector in
a <i>k</i>-dimensional space. To visualize notes in a 2-D space, locally linear 
embedding (LLE) is then conducted to reduce note representation from k- to 
2-dimension. Results are visulized using a (static) Google Motion Chart that 
allows further exploration of data. Words under each topic and the most possible
topic of each note are also presented to help make sense of the results.")),
    
    wellPanel(
      sliderInput("num_words", "# of words to display for each topic:", 
                  min=3, max=10, value=3)
    ),
    
    HTML(paste(textOutput("hits"), 
               "by <a href ='http://bodongchen.com/' 
               target='_blank'>Bodong Chen</a>"))
  ),
  
  mainPanel(
    tabsetPanel(
      # Visualization
      tabPanel("Visualization", 
               tableOutput("gvPlot"),
               h4("Summary of Topics:"),
               tableOutput("gvSummary")
               #                br(),
               #                plotOutput("main_plot", height="450px")
      ),
      
      # Details
      tabPanel("Topics and Notes",
               h4("Notes and their estimated topics:"),
               tableOutput("gvDetails"),
               h4("Topics and their (stemmed) words:"),
               tableOutput("gvTopics"))
      
#       # Motion chart
#       tabPanel("Interactive Plot",
#                tableOutput("gvPlot"))
    )
    
  )
))
