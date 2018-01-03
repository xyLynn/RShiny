library(tm)
library(wordcloud2)
library(plyr)
library(shiny)
library(shinythemes)
library(ggplot2)

ui <- tagList(
  shinythemes::themeSelector(),
  navbarPage("XLin_NLP", 
             tabPanel("Normalization",
                      fluidPage(
                        fluidRow(
                          column(5, fileInput("file", "Upload the file"),
                                 helpText("Default max file size is 500MB")),
                          column(5, selectInput("dl_1", "Select data to download:", c("Raw Data", 
                                                                                      "Normalized Data")),
                                 downloadButton(outputId = "dl1", label = "Download"))
                        ),
                        tags$hr(),
                        fluidRow(
                          uiOutput("tabs")
                        )
                      )),
             tabPanel("Word Frequency",
                      fluidPage(
                        titlePanel("Frequency and Word Cloud"),
                        sidebarLayout(
                          sidebarPanel(
                            
                            numericInput("wordno", "Select the words amount: ", value = 200, min = 100, step = 20),
                            numericInput("fontsize", "Select the word size:", value = 1.5, min = 0.5, step = 0.1),
                            selectInput("shape", "Select the shape of the cloud: ", 
                                        c("Circle" = "circle", 
                                          "Diamond" = "diamond", 
                                          "Triangle" = "triangle", 
                                          "Star" = "star")),
                            radioButtons("bcolor", "Select background color:",
                                         c("White" = "white",
                                           "Black" = "black",
                                           "Teal" = "teal",
                                           "Red" = "red",
                                           "Green" = "green",
                                           "Gray" = "gray")),
                            checkboxGroupInput("wcolor", "Select word color:",
                                               c("Red" = "red",
                                                 "Yellow" = "yellow",
                                                 "Green" = "green",
                                                 "Teal" = "teal",
                                                 "Black" = "Black",
                                                 "White" = "White",
                                                 "Random Light" = "random-light")),
                            hr(),
                            selectInput("dl_2", "Select data to download:",
                                        c("Word Frequency_Raw", "Word Frequency_Normalized")),
                            downloadButton(outputId = "dl2", label = "Download")
                          ),
                          mainPanel(
                            uiOutput("freq_tabs")
                          )))),
             tabPanel("Review Frequency",
                      fluidPage(
                        fluidRow(
                          column(4, fileInput("afile", "Upload the file")),
                          column(4, numericInput("top_review_freq", "Select the top review products:", value = 6, min = 1, step = 1)),
                          column(4, selectInput("dl_3", "Select data to download:",
                                                c("Sentiment Score_Individual", "Sentiment Score_Product", "Top Reviewed Products")),
                                 downloadButton(outputId = "dl3", label = "Download"))
                        ),
                        tags$hr(),
                        fluidRow(
                          uiOutput("product_tabs")
                        )
                      )),
             tabPanel("Scatter Plots",
                      fluidPage(
                        fluidRow(
                          tableOutput("corr_table")),
                        fluidRow(
                          plotOutput("plot", height = "1500px")
                        )
                      )),
             tabPanel("Observation",
                      fluidPage(
                        titlePanel("Conclusion"),
                        mainPanel(h4("In normalization, I remove all punctuation, convert uppercase to lowercase, stemming, and remove stop words. Since removing stop words will remove negation words, like 'not', 'wont', so I use the file after 3 normalization steps to calculate the sentiment score."),
                                  h4("After calculating the correlation for top products, these numbers are not so big. So that may not be a good way to predict the review stars."),
                                  h4("Then I use tf-itf as weight to recalculate the score, and then calculate the correlation again. The correlations even become smaller. (The code is in another R file) "),
                                  h4("So far, I think playing vanilla may be better.")
                        )))
             
  ))

options(shiny.maxRequestSize = 500*1024^2)

server <- function(input, output){
  data <- reactive({
    nfile <- input$file
    if (is.null(nfile)){return()}
    read.csv(file = nfile$datapath, header = T, stringsAsFactors = F)
  })
  
  # First Tab
  after_step_2 <- reactive({
    test <- data()
    review1 <- gsub("(\n|<br />)"," ",test$Text)
    review1 <- stripWhitespace(review1)
    review1 <- removePunctuation(review1)
    review2 <- sapply(review1, tolower)
    review2 <- stemDocument(review2, language = "english")
    review2
  })
  
  after_step_3 <- reactive({
    review2 <- after_step_2()
    stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
    stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
    review3 <- stringr::str_replace_all(review2, stopwords_regex, '')
    review3
  })
  
  norm_data <- reactive({
    test <- data()
    review3 <- after_step_3()
    normalized_review <- cbind(test[, c(1:3, 10)], review3)
    colnames(normalized_review)[4:5] <- c("Review", "Normalized Review")
    normalized_review
  })
  
  output$raw_table <- DT::renderDataTable(DT::datatable({
    if (is.null(data())){return()}
    test <- data()
    test
  }))
  
  output$normalized_table <- DT::renderDataTable(DT::datatable({
    if (is.null(data())){return()}
    normalized_review <- norm_data()
    normalized_review
  }))
  
  output$dl1 <- downloadHandler(
    filename = function(){
      paste(input$dl_1, "csv", sep = ".")
    },
    content = function(file){
      if (input$dl_1 == "Raw Data"){
        write.csv(data(), file, row.names = FALSE)
      } else {
        write.csv(norm_data(), file, row.names = FALSE)
      }
    }
  )
  
  output$tabs <- renderUI({
    if (is.null(data())){return()}
    else
      tabsetPanel(tabPanel("Raw Data", fluidRow(DT::dataTableOutput("raw_table"))),
                  tabPanel("Normalized Data", fluidRow(DT::dataTableOutput("normalized_table")))
      )
  })
  
  # Second Tab
  raw_freq <- reactive({
    review2 <- after_step_2()
    review4 <- paste0(review2, collapse = " ")
    rdf <- strsplit(review4, " ")
    rdf2 <- unlist(rdf)
    rfreq <- table(rdf2)
    rfreq1 <- sort(rfreq, decreasing = T)
    rfreq_table <- as.data.frame(rfreq1) 
    rfreq_table <- rfreq_table[!(rfreq_table$rdf2 == ""),]
    colnames(rfreq_table)[1:2] <- c("Word", "Frequency")
    rfreq_table
  })
  
  norm_freq <- reactive({
    review3 <- after_step_3()
    review5 <- paste0(review3, collapse = " ")
    ndf <- strsplit(review5, " ")
    ndf2 <- unlist(ndf)
    nfreq <- table(ndf2)
    nfreq1 <- sort(nfreq, decreasing = T)
    nfreq_table <- as.data.frame(nfreq1) 
    nfreq_table <- nfreq_table[!(nfreq_table$ndf2 == ""),]
    colnames(nfreq_table)[1:2] <- c("Word", "Frequency")
    nfreq_table
  })
  
  output$rfreq_table <- DT::renderDataTable(DT::datatable({
    raw_freq_table <- raw_freq()
    row.names(raw_freq_table) <- NULL
    raw_freq_table
  }))
  
  output$wordcloud_raw <- renderWordcloud2({
    raw_freq_table <- head(raw_freq(), input$wordno)
    wordcloud2(raw_freq_table, shape = input$shape, size = input$fontsize, color = input$wcolor, minRotation = -pi/2, maxRotation = -pi/2,
               backgroundColor = input$bcolor)
  })
  
  output$nfreq_table <- DT::renderDataTable(DT::datatable({
    freq_table <- norm_freq()
    colnames(freq_table)[1:2] <- c("Word", "Frequency")
    row.names(freq_table) <- NULL
    freq_table
  }))
  
  output$wordcloud_norm <- renderWordcloud2({
    freq_table <- head(norm_freq(), input$wordno)
    wordcloud2(freq_table, shape = input$shape, size = input$fontsize, color = input$wcolor, minRotation = -pi/2, maxRotation = -pi/2,
               backgroundColor = input$bcolor)
  })
  
  output$dl2 <- downloadHandler(
    filename = function(){
      paste(input$dl_2, "csv", sep = ".")
    },
    content = function(file){
      if (input$dl_2 == "Word Frequency_Raw"){
        write.csv(raw_freq(), file, row.names = FALSE)
      } else {
        write.csv(norm_freq(), file, row.names = FALSE)
      }
    }
  )
  
  output$freq_tabs <- renderUI({
    if (is.null(data())){return()}
    else
      tabsetPanel(tabPanel("Frequency Table_Raw", fluidRow(DT::dataTableOutput("rfreq_table"))),
                  tabPanel("Word Cloud_Raw", wordcloud2Output("wordcloud_raw", width = "120%", height = "700px")),
                  tabPanel("Frequency Table_Norm", fluidRow(DT::dataTableOutput("nfreq_table"))),
                  tabPanel("Word Cloud_Norm", wordcloud2Output("wordcloud_norm", width = "120%", height = "700px"))
      )})
  
  # Third Tab
  afdata <- reactive({
    anfile <- input$afile
    if (is.null(anfile)){return()}
    read.table(file = anfile$datapath, stringsAsFactors = F)
  })
  
  score_table <- reactive({
    afinn <- afdata()
    review3 <- after_step_3()
    test <- data()
    sw <- c("", "")
    for (i in 1:length(review3)){
      y <- unlist(strsplit(review3[i], " "))
      y <- y[y != ""]
      x <- afinn$V1
      z <- y[y %in% x == TRUE]
      z <- paste0(z, collapse = ", ")
      k <- afinn$V2[x %in% y == TRUE]
      k <- sum(k)
      s <- cbind(k, z)
      sw <- rbind(sw, s)
    }
    score_word <- cbind(test[, c(1:3, 7)], sw[2:length(sw[,1]), 1:2])
    colnames(score_word)[5:6] <- c("Sentiment Score", "Words for Score") 
    score_word$`Sentiment Score` <- as.numeric(as.character(score_word$`Sentiment Score`))
    score_word
  })
  
  average_score_table <- reactive({
    score_word <- score_table()
    test_score_word <- score_word[c("ProductId", "Sentiment Score")]
    sum_product <- ddply(test_score_word, .(ProductId), summarize, 
                         Number_of_Reviews=length(ProductId), 
                         Average_Sentiment_Score=mean(`Sentiment Score`))
    sum_product
  })
  
  top_product_table <- reactive({
    sum_product <- average_score_table()
    temp <- sum_product[order(sum_product$Number_of_Reviews, decreasing = T), ]
    top_reviews <- unique(temp$Number_of_Reviews)[1:input$top_review_freq]
    top_table <- temp[as.character(temp$Number_of_Reviews) %in% as.character(top_reviews) == TRUE, ]
    top_table
  })
  
  output$ss_table <- DT::renderDataTable(DT::datatable({
    if (is.null(afdata())){return()}
    score_word <- score_table()
    row.names(score_word) <- NULL
    score_word
  }))
  
  output$sum_table <- DT::renderDataTable(DT::datatable({
    if (is.null(afdata())){return()}
    sum_product <- average_score_table()
    row.names(sum_product) <- NULL
    sum_product
  }))
  
  output$t_table <- DT::renderDataTable(DT::datatable({
    if (is.null(afdata())){return()}
    top_table <- top_product_table()
    top_table
  }))
  
  output$dl3 <- downloadHandler(
    filename = function(){
      paste(input$dl_3, "csv", sep = ".")
    },
    content = function(file){
      if (input$dl_3 == "Sentiment Score_Individual"){
        write.csv(score_table(), file, row.names = FALSE)
      } else if(input$dl_3 == "Sentiment Score_Product"){
        write.csv(average_score_table(), file, row.names = FALSE)
      } else {
        write.csv(top_product_table(), file, row.names = FALSE)
      }
    }
  )
  
  output$product_tabs <- renderUI({
    if (is.null(data())){return()}
    else
      tabsetPanel(tabPanel("Sentiment Score_Individual", fluidRow(DT::dataTableOutput("ss_table"))),
                  tabPanel("Sentiment Score_Product", fluidRow(DT::dataTableOutput("sum_table"))),
                  tabPanel("Top Reviewed Products", fluidRow(DT::dataTableOutput("t_table")))
                  
      )})
  
  # Forth Tab
  output$plot <- renderPlot({
    if (is.null(afdata())){return()}
    score_word <- score_table() 
    top_table <- top_product_table()
    top_product <- score_word[score_word$ProductId %in% top_table$ProductId == TRUE,]
    ggplot(top_product)+geom_point(aes(`Sentiment Score`, Score))+facet_grid(ProductId ~ .)
  })
  
  output$corr_table <- renderTable({
    if (is.null(afdata())){return()}
    score_word <- score_table() 
    top_table <- top_product_table()
    top_product <- score_word[score_word$ProductId %in% top_table$ProductId == TRUE,]
    result <- by(top_product[,4:5], top_product$ProductId, function(top_product){cor(top_product$`Sentiment Score`, top_product$Score)})
    corr_df <- as.data.frame(as.matrix(result))
    corr_df <- t(corr_df)
    corr_df
  }, digits=4)
  
}

shinyApp(ui = ui, server = server)