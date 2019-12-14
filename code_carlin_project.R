###################################################################################
# Task: Create a visual for Goodman Bacon diff in diff weights
# Author: Patrick Carlin (patcarli@iu.edu)
# Date: 12 December 2019
# Input data: Diff in diff variables 
# Output data: Diff in diff output as well as
#   graphs with Goodman Bacon weights on x axis and Coefficients on y axis
###################################################################################
#Set working directory to where the files are
setwd("C:/Users/Pat C/Box/_My Documents/Active/Class/Statistical Computing/Project")

#Define the necessary functions- the main function is called bacon
bacon = function(outcome,post,treat,post_treat,place,time, cluster, st_errors='stata',output_path = getwd()){
  if(!require(estimatr)){
    install.packages("estimatr")
    library(estimatr)
  }
  if(!require(nlme)){
    install.packages("nlme")
    library(nlme)
  }
  if(!require(plyr)){
    install.packages("plyr")
    library(plyr)
  }
  
  lmr = lm_robust(outcome~post_treat, fixed_effects = ~place + time, clusters = cluster, se_type = st_errors)
  pro = proportions(place,outcome,time)
  weight = weights_bacon(pro[1],pro[2],pro[3],pro[4],pro[5],pro[6],pro[7],pro[8:1007])
  diff = dd(outcome,place,time,post_treat)
  
  pdf(paste(output_path,"/goodman_bacon.pdf",sep=""))
  plot(weight,diff,main = "Goodman-Bacon Weights",xlab="Weights",ylab="Coefficients", col=c("Blue","Blue","Red","Red"), pch=c(19,19,4,4)) 
  abline(h=0,lty=2,lwd=2)
  dev.off()
  
  bacon_weights = data.frame(weight,diff,bacon_labels)
  write.csv(bacon_weights,paste(output_path,"/goodman_bacon.csv",sep=""))
  
  return(summary(lmr))
}

