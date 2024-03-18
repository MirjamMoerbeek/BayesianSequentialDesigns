library(shiny)
library(ggplot2)

function(input, output) {
  
  results=readRDS("results.rds")
  resultssummary=readRDS("resultssummary.rds")
  resultssummary=resultssummary[,2:11]
###############################################################################################################################################
### histogram for n2
###############################################################################################################################################
  output$hist.n2 <- renderPlot({
  HypSet=as.integer(input$HypSet)
  if(HypSet==1)
  ES=as.integer(input$ES1)
  else
    ES=as.integer(input$ES2)
  BFtarget=as.integer(input$BFtarget)
  if(HypSet==1)
    fraction=as.integer(input$fraction1)
  else
    fraction=as.integer(input$fraction2)
  n2.min=as.integer(input$n2.min)
  n2.max=as.integer(input$n2.max)

  batch=10000*HypSet+1000*ES+100*n2.min+10*BFtarget+1*fraction
    
  selection=results[results[,1]==batch,]
  n2=selection[,n2.max+6]
  BF=selection[,n2.max+1]
  BF=1/BF
  log10BF=log10(BF)
  
  BFtarget=c(3,5,10,20)[BFtarget]
  n2.max=c(30,50,100,200,50000)[n2.max]

  if(ES == 1)
  {
    Decision=rep(NA,5000)
    Decision[BF>1/BFtarget]=2
    Decision[BF>BFtarget]=1
    Decision[BF<1/BFtarget]=3
  }

  if(ES == 2)
  {
    Decision=rep(NA,5000)
    Decision[BF<BFtarget]=2
    Decision[BF<1/BFtarget]=1
    Decision[BF>BFtarget]=3
  }
  
  Decision=as.character(Decision)
  Decision[Decision=="2"]="None"
  Decision[Decision=="3"]="Correct"
  Decision[Decision=="1"]="Incorrect"
  Decision=factor(Decision,levels=c("Incorrect", "None", "Correct"))
  
  dat=data.frame(n2,Decision)
  
  g <- ggplot(dat, aes(n2)) + scale_fill_brewer(palette = "Spectral")
  g + geom_histogram(aes(fill=Decision), 
                     bins = 20, 
                     col="black", 
                     size=.1) + theme_bw() + xlab("Number of clusters per group") + ylab("Number of generated data sets") + 
    scale_fill_manual(values=c("Incorrect"="red", "None"="orange", "Correct"="green"),drop=FALSE) + theme(legend.position = c(0.5, 0.85))+
    theme(text = element_text(size = 20))  
     })

###############################################################################################################################################
### histogram for BF
###############################################################################################################################################
  output$hist.BF <- renderPlot({
    HypSet=as.integer(input$HypSet)
    if(HypSet==1)
      ES=as.integer(input$ES1)
    else
      ES=as.integer(input$ES2)
    BFtarget=as.integer(input$BFtarget)
    if(HypSet==1)
      fraction=as.integer(input$fraction1)
    else
      fraction=as.integer(input$fraction2)
    n2.min=as.integer(input$n2.min)
    n2.max=as.integer(input$n2.max)
    
    batch=10000*HypSet+1000*ES+100*n2.min+10*BFtarget+1*fraction
    
    selection=results[results[,1]==batch,]
    n2=selection[,n2.max+6]
    BF=selection[,n2.max+1]
    BF=1/BF
    log10BF=log10(BF)

    BFtarget=c(3,5,10,20)[BFtarget]
    n2.max=c(30,50,100,200,50000)[n2.max]
    
    if(ES == 1)
    {
      Decision=rep(NA,5000)
      Decision[BF>1/BFtarget]=2
      Decision[BF>BFtarget]=1
      Decision[BF<1/BFtarget]=3
    }
    
    if(ES == 2)
    {
      Decision=rep(NA,5000)
      Decision[BF<BFtarget]=2
      Decision[BF<1/BFtarget]=1
      Decision[BF>BFtarget]=3
    }
    
    Decision=as.character(Decision)
    Decision[Decision=="2"]="None"
    Decision[Decision=="3"]="Correct"
    Decision[Decision=="1"]="Incorrect"
    Decision=factor(Decision,levels=c("Incorrect", "None", "Correct"))
    
    dat=data.frame(n2,Decision)
    
    g <- ggplot(dat, aes(log10BF)) + scale_fill_brewer(palette = "Spectral")
    g + geom_histogram(aes(fill=Decision), 
                       bins = 100, 
                       col="black", 
                       size=.1) + theme_bw()  + ylab("Number of generated data sets") + 
      scale_fill_manual(values=c("Incorrect"="red", "None"="orange", "Correct"="green"),breaks=c("Incorrect", "None", "Correct"),labels=c("Incorrect", "None", "Correct"),drop=FALSE) +
      scale_x_continuous(name="Bayes Factor", limits=c(-1.5,7.5),breaks= c(-4,-3,-2,-1,0,1,2,3,4,5,6,7), labels=c("10^-4","10^-3","10^-2","10^-1","1","10","10^2","10^3","10^4","10^5","10^6","10^7")) + 
      theme(legend.position = c(0.85, 0.85)) +   
      geom_vline(xintercept =c(log10(1/BFtarget), log10(BFtarget)),color="black", lwd=0.75, lty=2) +
      theme(text = element_text(size = 20))  
  })

###############################################################################################################################################
### text output: mean n2
###############################################################################################################################################
  output$mean.n2 <- renderText({ 
    HypSet=as.integer(input$HypSet)
    if(HypSet==1)
      ES=as.integer(input$ES1)
    else
      ES=as.integer(input$ES2)
    BFtarget=as.integer(input$BFtarget)
    if(HypSet==1)
      fraction=as.integer(input$fraction1)
    else
      fraction=as.integer(input$fraction2)
    n2.min=as.integer(input$n2.min)
    n2.max=as.integer(input$n2.max)
    
    batch=10000*HypSet+1000*ES+100*n2.min+10*BFtarget+1*fraction
    
    selection=results[results[,1]==batch,]
    n2=selection[,n2.max+6]
    
    mean.n2=round(mean(n2))
    
    paste("Mean number of clusters per group: ", mean.n2)
  })

###############################################################################################################################################
### text output: median n2
###############################################################################################################################################
  output$median.n2 <- renderText({ 
    HypSet=as.integer(input$HypSet)
    if(HypSet==1)
      ES=as.integer(input$ES1)
    else
      ES=as.integer(input$ES2)
    BFtarget=as.integer(input$BFtarget)
    if(HypSet==1)
      fraction=as.integer(input$fraction1)
    else
      fraction=as.integer(input$fraction2)
    n2.min=as.integer(input$n2.min)
    n2.max=as.integer(input$n2.max)
    
    batch=10000*HypSet+1000*ES+100*n2.min+10*BFtarget+1*fraction
    
    selection=results[results[,1]==batch,]
    n2=selection[,n2.max+6]
    
    median.n2=median(n2)
    
    paste("Median number of clusters per group: ", median.n2)
  })
  
###############################################################################################################################################
### text output: max n2
###############################################################################################################################################
  output$max.n2 <- renderText({ 
    HypSet=as.integer(input$HypSet)
    if(HypSet==1)
      ES=as.integer(input$ES1)
    else
      ES=as.integer(input$ES2)
    BFtarget=as.integer(input$BFtarget)
    if(HypSet==1)
      fraction=as.integer(input$fraction1)
    else
      fraction=as.integer(input$fraction2)
    n2.min=as.integer(input$n2.min)
    n2.max=as.integer(input$n2.max)
    
    batch=10000*HypSet+1000*ES+100*n2.min+10*BFtarget+1*fraction
    
    selection=results[results[,1]==batch,]
    n2=selection[,n2.max+6]
    
    max.n2=max(n2)
    
    paste("Maximum number of clusters per group: ", max.n2)
  })

###############################################################################################################################################
### text output: percentage correct decision
###############################################################################################################################################
  
  output$percnonerror <- renderText({ 
    HypSet=as.integer(input$HypSet)
    if(HypSet==1)
      ES=as.integer(input$ES1)
    else
      ES=as.integer(input$ES2)
    BFtarget=as.integer(input$BFtarget)
    if(HypSet==1)
      fraction=as.integer(input$fraction1)
    else
      fraction=as.integer(input$fraction2)
    n2.min=as.integer(input$n2.min)
    n2.max=as.integer(input$n2.max)
    
    batch=10000*HypSet+1000*ES+100*n2.min+10*BFtarget+1*fraction
    
    selection=results[results[,1]==batch,]
    BF=selection[,n2.max+1]
    BF=1/BF
    BFtarget=c(3,5,10,20)[BFtarget]
    
    
    if(ES==1)
      percnonerror=100*length(BF[BF<1/BFtarget])/5000
    else
      percnonerror=100*length(BF[BF>BFtarget])/5000
    percnonerror=round(percnonerror,2)
    paste("Percentage data sets for which correct hypothesis is favoured: ", percnonerror)
  })
  
###############################################################################################################################################
### text output: percentage incorrect decision
###############################################################################################################################################
  
  output$percerror <- renderText({ 
    HypSet=as.integer(input$HypSet)
    if(HypSet==1)
      ES=as.integer(input$ES1)
    else
      ES=as.integer(input$ES2)
    BFtarget=as.integer(input$BFtarget)
    if(HypSet==1)
      fraction=as.integer(input$fraction1)
    else
      fraction=as.integer(input$fraction2)
    n2.min=as.integer(input$n2.min)
    n2.max=as.integer(input$n2.max)
    
    batch=10000*HypSet+1000*ES+100*n2.min+10*BFtarget+1*fraction
    
    selection=results[results[,1]==batch,]
    BF=selection[,n2.max+1]
    BF=1/BF
    BFtarget=c(3,5,10,20)[BFtarget]
    
    
    if(ES==1)
      percerror=100*length(BF[BF>BFtarget])/5000
    else
      percerror=100*length(BF[BF<1/BFtarget])/5000
    percerror=round(percerror,2)
    paste("Percentage data sets for which incorrect hypothesis is favoured: ", percerror)
  })
  
###############################################################################################################################################
### text output: percentage no decision
###############################################################################################################################################
  
  output$percinconclusive <- renderText({ 
    HypSet=as.integer(input$HypSet)
    if(HypSet==1)
      ES=as.integer(input$ES1)
    else
      ES=as.integer(input$ES2)
    BFtarget=as.integer(input$BFtarget)
    if(HypSet==1)
      fraction=as.integer(input$fraction1)
    else
      fraction=as.integer(input$fraction2)
    n2.min=as.integer(input$n2.min)
    n2.max=as.integer(input$n2.max)
    
    batch=10000*HypSet+1000*ES+100*n2.min+10*BFtarget+1*fraction
    
    selection=results[results[,1]==batch,]
    BF=selection[,n2.max+1]
    BF=1/BF
    BFtarget=c(3,5,10,20)[BFtarget]
    
    percinconclusive=100*(1-length(BF[BF>BFtarget])/5000-length(BF[BF<1/BFtarget])/5000)
    percinconclusive=round(percinconclusive,2)
   
    paste("Percentage data sets for which neither hypothesis is favoured: ", percinconclusive)
  })
  
  
###############################################################################################################################################
### table with summary of results
###############################################################################################################################################
  output$ResultsTable <- DT::renderDataTable({
    resultssummary=as.data.frame(resultssummary)
    colnames(resultssummary)=c("Hypothesis","Effect size", "BFtarget", "Fraction","Minimum number of clusters per group","Maximum number of clusters per group", "Mean number of clusters per group", "Correct decision (%)", "Incorrect decision (%)", "Indecisions (%)")  # column names in results matrix 
    
   
    datatable(resultssummary,rownames=FALSE, options = list(pageLength = 24))
  })
  

}