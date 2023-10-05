##### mean number of clusters per groiup, and percentages correct, incorrect and indecsions 
paste("hypothesis set: ",results$hypset)
paste("delta: ",results$delta)
paste("n2.min:",results$n2.min)
paste("BFtarget:",results$BFtarget)
paste("fraction:",results$fraction)

n2=results$n2.30.out                              # user selects specifies maximum number of clusters per group
BF=results$BF.30.out                              # user selects specifies maximum number of clusters per group
if (results$delta>0) BF=1/results$BF.200.out      # take inverse of BF so that BF>1 favors the hypothesis that generated the data
BFtarget=results$BFtarget
n2.max=max(n2)

round(mean(n2))

percnonerror=100*length(BF[BF>BFtarget])/5000
percnonerror=round(percnonerror,2)
paste("Percentage data sets for which correct hypothesis is favoured: ", percnonerror)

percerror=100*length(BF[BF<1/BFtarget])/5000
percerror=round(percerror,2)
paste("Percentage data sets for which incorrect hypothesis is favoured: ", percerror)

percinconclusive=100*(1-length(BF[BF>BFtarget])/5000-length(BF[BF<1/BFtarget])/5000)
percinconclusive=round(percinconclusive,2)
paste("Percentage data sets for which neither hypothesis is favoured: ", percinconclusive)

##### results: histograms for number of clusters per group and for Bayes factor
n2=results$n2.30.out
BF=1/results$BF.30.out      # take inverse of BF. As such BF>BFtarget implies the correct hypothesis is favored
BFtarget=results$BFtarget

if(results$delta == 0)
  {
  Decision=rep(NA,5000)
  Decision[BF>1/BFtarget]=2
  Decision[BF>BFtarget]=1
  Decision[BF<1/BFtarget]=3
}

if(results$delta > 0)
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


log10BF=log10(BF)
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
