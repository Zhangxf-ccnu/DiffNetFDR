# server.R
library(DiffNetFDR)
library(ggplot2)

shinyServer(
  
  function(input, output, session) {
    options(shiny.maxRequestSize = 30*1024^2)
    
    observeEvent(input$run, {
      updateNavbarPage(session, "master",
                       selected = "Visualize Results"
      )
    })
    
    platforms = eventReactive(input$run, {
      return(c(paste("1", input$exprName1, sep=ifelse(input$exprName1 == "", yes = "", no = "-"))
      ))
    })
    
   
    fit = eventReactive(input$run, {
      
      # Read in expression dataset
      dataset = read.table(file = input$expr$datapath, sep = "", row.names = 1)

      # Read in group information
      group = read.table(file = input$grouping$datapath, sep = "", row.names = 1)
      
      # Remove rows with zero variance
      dataset = dataset[apply(dataset, 1, var) > 0,]
      
      X = dataset
      group = as.numeric(unlist(group))
      fit = try(DiffNet.FDR(X=X, group = group, alpha = input$alpha, test.type = input$test.type, parallel = input$parallel, nCpus = input$numCore))
      
      
      return(fit)
      
    })
    
    
    output$network = renderVisNetwork({
      network.for.plot = fit()$Diff.net.connected
      V(network.for.plot)$color = input$nodecolor
      V(network.for.plot)$size = (input$maxnodesize-input$minnodesize)*(degree(network.for.plot)-min(degree(network.for.plot)))/(max(degree(network.for.plot))-min(degree(network.for.plot))) + input$minnodesize
      V(network.for.plot)$label.cex = input$labelcex
      
      E(network.for.plot)$color = input$edgecolor
      E(network.for.plot)$width = input$edgewidth
      
      visIgraph(network.for.plot, layout = input$layout)%>% 
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
      #   plotNetwork(fit(), threshold = input$thresh, thresh.type = input$threshType, layout = input$layout)
    })
    
    output$table = renderTable({
      Degree_sort = sort(degree(fit()$Diff.net.connected), decreasing=TRUE)
      hubtable = data.frame(Node = names(Degree_sort), Degree = as.integer(Degree_sort))[1:input$hubs,]
    }, digits = 3)
    

    
    
    output$summary1 = renderText({
      N = dim(fit()$W)[1]
      paste("Total edges possible:", N*(N-1)/2)
    })
    
    output$summary2 = renderText({
      paste("Number of differential edges:", gsize(fit()$Diff.net))
    })
    
    
    output$download = downloadHandler(
      
      filename = function(){ paste(input$dlname, ".", input$fileformat, sep="") },
      content = function(file) {
        
        id = which((lower.tri(fit()$Diff.edge)*fit()$Diff.edge)!=0,arr.ind = T)
        out = data.frame(Node1=rownames(fit()$Diff.edge)[id[,1]],Node2=rownames(fit()$Diff.edge)[id[,2]])
        
        if (input$fileformat == "txt") write.table(out, file, sep = "\t",
                                                   row.names = FALSE, quote = FALSE)
        else write.csv(out, file, row.names = FALSE, quote = FALSE)
      }
    )
    
  }
)
