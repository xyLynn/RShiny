library(shiny)
library(reshape2)
library(plyr)
library(igraph)
library(networkD3)
library(DT)

ui <- (navbarPage("XiaoyiLin_SNA",
                  tabPanel("Data", 
                           fluidPage(
                             titlePanel("Upload Data"),
                             sidebarLayout(
                               sidebarPanel(
                                 fileInput("file", "Upload the file"),
                                 helpText("Default max file size is 10MB"),
                                 tags$hr(),
                                 # h5("Select the read.table parameters below:"),
                                 radioButtons(inputId = "header", label = "1. Whether the file contains the names of variables as its first line:",
                                              choices = c(True = 1, 
                                                          False = 0)),
                                 # checkboxInput(inputId = "header", label = "Header", value = FALSE),
                                 radioButtons(inputId = "sep", label = "2. Select the field separator character:", 
                                              choices = c(Comma = ",", 
                                                          Semicolon = ";",
                                                          Tab = "\t",
                                                          Space = " "),
                                              selected = ",")
                               ),
                               mainPanel(
                                 uiOutput("tabs")
                               )
                             )
                           )),
                  tabPanel("Graph",
                           fluidPage(
                             titlePanel("Make a Graph"),
                             sidebarLayout(
                               sidebarPanel(
                                 selectInput("group", "Select group:", c("Centrality_degree" = "centra_group", 
                                                                         "Betweenness"  = "between_group", 
                                                                         "Outdegree" = "out_group", 
                                                                         "Indegree" = "in_group")),
                                 selectInput("num_name", "Show degree or name:", c("Name" = "name",
                                                                                   "Centrality_degree",
                                                                                   "Betweenness",
                                                                                   "Outdegree",
                                                                                   "Indegree")),
                                 numericInput("nodesize", "Change the node size: ", value = 5, min = 1),
                                 numericInput("font", "Change the font size: ", value = 16, min = 1),
                                 radioButtons(inputId = "show_name", label = "Whether show the node name:",
                                              choices = c("Yes" = 1, 
                                                          "No" = 0))
                               ),
                               mainPanel(
                                 uiOutput("graph_tabs")
                               )
                             )
                           )),
                  tabPanel("Observation",
                           fluidPage(
                             titlePanel("Conclusion"),
                             mainPanel(h4("1. Usually, the person who has high centrality degree will also have high betweenness degree. For example, Kay has both the highest centrality degree and betweenness degree."),
                                       h4("2. Drag one of the node in the graph, if the other nodes move a lot or are very easy to follow the dragging node, then this node(person) has high influence in the organization. In the other words, the person is important in the organization. 
                                          At the same time, he/she also has high betweenness degree and centrality degree.")
                                       )))
                  
                             ))

options(shiny.maxRequestSize = 10*1024^2)

server <- function(input, output){
  # upload file
  data <- reactive({
    nfile <- input$file
    if (is.null(nfile)){return()}
    read.table(file = nfile$datapath, sep = input$sep, header = as.numeric(input$header))
  })
  # manipulate data to tall table
  data_long <- reactive({
    temp <- data()
    colnames(temp)[1] <- "calling"
    melt(temp, id.vars = "calling", measure.vars = colnames(data())[2:length(colnames(data()))], 
         variable.name = "called", value.name = "times")
  })
  
  
  # display data sample
  ## display raw data
  output$table <- DT::renderDataTable(DT::datatable({
    if (is.null(data())){return()}
    newdata1 <- data()
    colnames(newdata1)[1] <- "name"
    newdata1
  }))
  
  ## display tall table data 
  output$ltable <- DT::renderDataTable(DT::datatable({
    if (is.null(data())){return()}
    newdata2 <- data_long()
    newdata2
  }))
  
  
  # calculate total calls, persons who received the most calls and who palced most calls (plyr package)
  ## manapulate data by group(calling & called)
  callingt <- reactive({
    ddply(data_long(), ~calling, summarise, callingsum=sum(times))
  })
  calledt <- reactive({
    ddply(data_long(), ~called, summarise, calledsum=sum(times))
  })
  
  ## calculate total calls
  output$totalcalls <- renderText({
    paste("Total calls placed: ", sum(callingt()$callingsum))
  })
  
  ## show the person who placed the most calls
  output$person1 <- renderText({
    p1 <- callingt()[which(callingt()$callingsum == max(callingt()$callingsum)), ]
    paste(paste(p1$calling, "placed the most calls, and he/she placed", p1$callingsum, "calls."))
  })
  
  ## show the person who received the most calls
  output$person2 <- renderText({
    p2 <- calledt()[which(calledt()$calledsum == max(calledt()$calledsum)), ]
    paste(paste(p2$called, "received the most calls, and he/she received", p2$calledsum, "calls."))
  })
  
  output$tabs <- renderUI({
    if (is.null(data())){return()}
    else
      tabsetPanel(tabPanel("Raw Data", fluidRow(DT::dataTableOutput("table"))),
                  tabPanel("Tall Table", fluidRow(DT::dataTableOutput("ltable"))),
                  tabPanel("Summary", textOutput("totalcalls"), textOutput("person1"), textOutput("person2")))
  })
  
  
  
  # delete the row where times = 0 in tall table
  link_data <- reactive({
    if (is.null(data())){return()}
    data_long()[!(data_long()$times == 0),]
  })
  
  # calculate four degrees
  measure_table <- reactive({
    if (is.null(data())){return()}
    grdata <- graph.data.frame(link_data())
    
    cendr <- degree(grdata, mode = "all", normalized = F)
    bet <- centr_betw(grdata, directed = F)$res
    outdr <- degree(grdata, mode = "out", normalized = F)
    indr <- degree(grdata, mode = "in", normalized = F)
    mt <- data.frame(Centrality_degree = cendr, 
                     Betweenness = bet, 
                     Outdegree = outdr, 
                     Indegree = indr)
    mt <- cbind(rownames(mt), mt)
    rownames(mt) <- NULL
    colnames(mt)[1] <- "Name"
    
    mt
  })
  
  # create groups depend on the degree measure & create Nodes data for forceNetwork
  graph_nodes_data <- reactive({
    if (is.null(data())){return()}
    
    node_measure_table <- measure_table()
    size <- rep(input$nodesize, length(node_measure_table$Name))
    nodes_data <- data.frame(cbind(as.character(node_measure_table$Name), size))
    colnames(nodes_data) <- c("name", "size")
    nodes_data <- merge(nodes_data, node_measure_table, by.x = "name", by.y = "Name")
    
    for (i in 1:length(nodes_data$Centrality_degree)){
      if (nodes_data$Centrality_degree[i] %in% (sort(nodes_data$Centrality_degree, TRUE)[1:3])){
        nodes_data$centra_group[i] = "Top 3"
      } else {
        nodes_data$centra_group[i] = "Remain"
      }
    }
    
    for (i in 1:length(nodes_data$Betweenness)){
      if (nodes_data$Betweenness[i] %in% (sort(nodes_data$Betweenness, TRUE)[1:3])){
        nodes_data$between_group[i] = "Top 3"
      } else {
        nodes_data$between_group[i] = "Remain"
      }
    }
    
    for (i in 1:length(nodes_data$Outdegree)){
      if (nodes_data$Outdegree[i] %in% (sort(nodes_data$Outdegree, TRUE)[1:3])){
        nodes_data$out_group[i] = "Top 3"
      } else {
        nodes_data$out_group[i] = "Remain"
      }
    }
    
    for (i in 1:length(nodes_data$Indegree)){
      if (nodes_data$Indegree[i] %in% (sort(nodes_data$Indegree, TRUE)[1:3])){
        nodes_data$in_group[i] = "Top 3"
      } else {
        nodes_data$in_group[i] = "Remain"
      }
    }
    
    nodes_data
  })
  
  # create Links data for forceNetwork
  graph_link_data <- reactive({
    if (is.null(data())){return()}
    
    temp_link_data <- link_data()
    temp_link_data$calling.index = match(temp_link_data$calling, graph_nodes_data()$name)-1
    temp_link_data$called.index = match(temp_link_data$called, graph_nodes_data()$name)-1
    temp_link_data
  })
  
  # display the table contains four measures
  output$display_measure_table <- DT::renderDataTable(DT::datatable({
    temp_measure_table <- measure_table()
    if (is.null(data())){return()}
    temp_measure_table
  }))
  
  # display graph
  ## display graph whithout arrow
  output$network_graph_1 <- renderForceNetwork({
    if (is.null(data())){return()}
    
    ColourScale <- 'd3.scaleOrdinal().range(["#2ca02c", "#d62728"])'
    
    forceNetwork(Links = graph_link_data(), Nodes = graph_nodes_data(),
                 Source = "calling.index", Target = "called.index",
                 Value = "times", NodeID = input$num_name, Nodesize = "size",
                 radiusCalculation = JS("Math.sqrt(d.nodesize)+10"), 
                 opacityNoHover = input$show_name, linkDistance = JS("function(d){return (d.value + 10) * 10}"),
                 fontSize = input$font, legend = TRUE,
                 Group = input$group, opacity = 0.8, zoom = TRUE, 
                 colourScale = JS(ColourScale))
  })
  
  ## display outbound calls graph 
  output$network_graph_2 <- renderForceNetwork({
    if (is.null(data())){return()}
    
    ColourScale <- 'd3.scaleOrdinal().range(["#2ca02c", "#d62728"])'
    
    forceNetwork(Links = graph_link_data(), Nodes = graph_nodes_data(),
                 Source = "calling.index", Target = "called.index",
                 Value = "times", NodeID = input$num_name, Nodesize = "size",
                 radiusCalculation = JS("Math.sqrt(d.nodesize)+10"), 
                 opacityNoHover = input$show_name, linkDistance = JS("function(d){return (d.value + 10) * 10}"),
                 fontSize = input$font, legend = TRUE, arrows = TRUE,
                 Group = input$group, opacity = 0.8, zoom = TRUE, 
                 colourScale = JS(ColourScale))
  })
  
  ## display inbound calls graph
  output$network_graph_3 <- renderForceNetwork({
    if (is.null(data())){return()}
    
    ColourScale <- 'd3.scaleOrdinal().range(["#2ca02c", "#d62728"])'
    
    forceNetwork(Links = graph_link_data(), Nodes = graph_nodes_data(),
                 Source = "called.index", Target = "calling.index",
                 Value = "times", NodeID = input$num_name, Nodesize = "size",
                 radiusCalculation = JS("Math.sqrt(d.nodesize)+10"), 
                 opacityNoHover = input$show_name, linkDistance = JS("function(d){return (d.value + 10) * 10}"),
                 fontSize = input$font, legend = TRUE, arrows = TRUE,
                 Group = input$group, opacity = 0.8, zoom = TRUE, 
                 colourScale = JS(ColourScale))
  })
  
  # provide download option for measure table
  output$download <- downloadHandler(
    filename = function(){
      paste(input$group, "csv", sep = ".")
    },
    content = function(file){
      write.csv(measure_table(), file, row.names = FALSE)
    }
  )
  
  output$graph_tabs <- renderUI({
    if (is.null(data())){return()}
    else
      tabsetPanel(tabPanel("Computation", fluidRow(DT::dataTableOutput("display_measure_table")),
                           downloadButton(outputId = "download", label = "Download the table")),
                  tabPanel("Connection", forceNetworkOutput("network_graph_1", height="600px")),
                  tabPanel("Outbound", forceNetworkOutput("network_graph_2", height="600px")),
                  tabPanel("Inbound", forceNetworkOutput("network_graph_3", height="600px")))
  })
}

shinyApp(ui = ui, server = server)