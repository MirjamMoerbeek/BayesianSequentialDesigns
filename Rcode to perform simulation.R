library(lme4)	                                                                   # to fit a mixed model
library(bain)	                                                                   # to calculate the Bayes factor

f.SeqBayesCRT=function(hypset,delta,n2.min,BFtarget,fraction)                    
  {
 
  nr.trials=5000                                                                 # number of replications
  n1 = 4                                                                         # common cluster size
  var.e=0.9                                                                      # variance individual level
  var.u=0.1                                                                      # variance cluster level

  ### create vectors to store output for a maximum of 50,000, 200, 100 and 50 clusters per arm
  BF.Inf.out=BF.200.out=BF.100.out=BF.50.out=BF.30.out=rep(NA,nr.trials)         # vectors with BF values
  n2.Inf.out=n2.200.out=n2.100.out=n2.50.out=n2.30.out=rep(NA,nr.trials)         # vectors with number of clusters per arm

  set.seed(12345)                                                                # seed for random number generation

  start=date()
  
  for(ii in 1:nr.trials)
  {
    cat("simulation run",ii,"\n")

    ####### generate population with 50,000 clusters in both conditions
    ID.c.pop=rep(seq(1:50000),each=n1)                                           # cluster identifier for control
    ID.i.pop=50000+rep(seq(1:50000),each=n1)                                     # cluster identifier for intervention

    treat.c.pop=rep(0,length=length(ID.c.pop))                                   # treatment condition = 0 in control
    treat.i.pop=rep(1,length=length(ID.i.pop))                                   # treatment condition = 1 in intervention

    var.u.c.pop=rnorm(n=50000,mean=0,sd=sqrt(var.u))    
    var.u.c.pop=rep(var.u.c.pop,each=n1)                                         # random effects at cluster level in control

    var.u.i.pop=rnorm(n=50000,mean=0,sd=sqrt(var.u))
    var.u.i.pop=rep(var.u.i.pop,each=n1)                                         # random effects at cluster level in intervention

    var.e.c.pop=rnorm(n=length(ID.c.pop),mean=0,sd=sqrt(var.e))                  # random effects at subject level in control
    var.e.i.pop=rnorm(n=length(ID.i.pop),mean=0,sd=sqrt(var.e))                  # random effects at subject level in intervention

    resp.c.pop=var.u.c.pop+var.e.c.pop                                           # response vector for control
    resp.i.pop=delta+var.u.i.pop+var.e.i.pop                                     # response vector for intervention
    
    ### make certain to enter the while loop by using BF=1 (even though this may not be the true value)
    BF=1
    n2=n2.min
    
    ### increase sample size if needed
    while(BF<BFtarget & BF>1/BFtarget & n2<50000 )
      {

      ######## increase n2 by 1 and sample n2 clusters per treatment condition from population
                                                     
      ID.c=ID.c.pop[ID.c.pop<=n2]
      ID.i=ID.i.pop[ID.i.pop<=(50000+n2)]
      ID=c(ID.c,ID.i)                                                            # vector with cluster identifiers

      treat.c=treat.c.pop[ID.c.pop<=n2]
      treat.i=treat.i.pop[ID.i.pop<=(50000+n2)]
      treat=c(treat.c,treat.i)                                                   # vector with treatment condition

      resp.c=resp.c.pop[ID.c.pop<=n2]
      resp.i=resp.i.pop[ID.i.pop<=(50000+n2)]
      resp=c(resp.c,resp.i)                                                      # vector with responses

      data.CRT=cbind(resp,ID,treat)
      data.CRT=as.data.frame(data.CRT)                                           # data frame

      results.lmer=lmer(resp ~ as.factor(treat)-1 + (1|ID), data = data.CRT, REML=FALSE)  # fit model to data
      # summary(results.lmer)                                                       # print results

      ### calculate design effect for both treatment conditions
      var.e.est=as.data.frame(VarCorr(results.lmer))[2,4]                           # variance of residual
      var.u.est=as.data.frame(VarCorr(results.lmer))[1,4]                            # variance of random intercept
      ICC=var.u.est/(var.u.est+var.e.est)                                                    # ICC
      DE=1+(n1-1)*ICC                                                                # design effect
      
      ### get required input for bain
      estimates=fixef(results.lmer)                                              # means in both treatment conditions
      names(estimates)=c("Cont","Int")                                           # give names to these means
      ngroup=table(data.CRT$treat)/DE                                            # create a vector containing the effective sample sizes in both treatment conditions  

      cov.c=as.matrix(vcov(results.lmer)[1,1])                                   # covariance matrix for control
      cov.i=as.matrix(vcov(results.lmer)[2,2])                                   # covariance matrix for intervention
      covariances=list(cov.c,cov.i)                                              # collect these covariance matrices in a list

      # test hypotheses with the function bain from the package bain
      # Note that there are multiple groups characterized by one mean, therefore group_parameters=1. 
      # Note that are no joint parameters, therefore, joint_parameters=0.

      if(hypset == 0)                                                            # for Bayesian null hypothesis testing
      {
      results.bain <- bain(estimates,hypotheses <-"Cont=Int",n=ngroup,Sigma=covariances,group_parameters=1,joint_parameters = 0, fraction = fraction)
      BF=results.bain$fit[1,11]                                                  # BF of H0 (delta=0) versus complement (delta!=0)
      }
      
      if(hypset == 1)                                                            # for Bayesian informative hypothesis testing
      {
      results.bain=bain(estimates,hypotheses <-"Cont>Int; Cont<Int",n=ngroup,Sigma=covariances,group_parameters=1,joint_parameters = 0, fraction = fraction)
      BF=results.bain$BFmatrix[1,2]                                              # get Bayes Factor from Bayes Factor matrix
      }
      
      if(n2<=30)
      {
        BF.30.out[ii]=BF
        n2.30.out[ii]=n2
      }
      
      if(n2<=50)
        {
          BF.50.out[ii]=BF
          n2.50.out[ii]=n2
        }
      
      if(n2<=100)
        {
          BF.100.out[ii]=BF
          n2.100.out[ii]=n2
        }
      
      if(n2<=200)
        {
          BF.200.out[ii]=BF
          n2.200.out[ii]=n2
        }
      
      if(n2<=50000)
        {
          BF.Inf.out[ii]=BF
          n2.Inf.out[ii]=n2
      }
      
      n2=n2+1                                                                    # increase number of clusters per condition by 1
    }
  }
  stop=date()
  output=list(nr.trials=nr.trials,
              hypset=hypset,
              delta=delta,
              n2.min=n2.min,
              BFtarget=BFtarget,
              fraction=fraction,
              batch=batch,
              BF.Inf.out=BF.Inf.out,
              BF.200.out=BF.200.out,
              BF.100.out=BF.100.out,
              BF.50.out=BF.50.out,
              BF.30.out=BF.30.out,
              n2.Inf.out=n2.Inf.out,
              n2.200.out=n2.200.out,
              n2.100.out=n2.100.out,
              n2.50.out=n2.50.out,
              n2.30.out=n2.30.out,
              start=start,
              stop=stop)
  return(output)
}


### example call to function
results=f.SeqBayesCRT(hypset=0,delta=0.35,n2.min=15,BFtarget=3,fraction=1)       # hypset=0 for null hypothesis testing and hypset=1 for informative hypothesis testing
