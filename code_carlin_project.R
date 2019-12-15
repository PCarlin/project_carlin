###################################################################################
# Task: Create a visual for Goodman Bacon diff in diff weights
# Author: Patrick Carlin (patcarli@iu.edu)
# Date: 12 December 2019
# Input data: Diff in diff variables 
# Output data: Diff in diff output as well as
#   graphs with Goodman Bacon weights on x axis and Coefficients on y axis
###################################################################################
#Set working directory to where the files are
#setwd("C:/Users/Pat C/Box/_My Documents/Active/Class/Statistical Computing/Project")

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
  pro = proportions(place,outcome,time,post)
  weight = weights_bacon(pro[1],pro[2],pro[3],pro[4],pro[5],pro[6],pro[7],pro[8:1007])
  diff = dd(outcome,place,time,post_treat)
  
  pdf(paste(output_path,"/goodman_bacon.pdf",sep=""))
  plot(weight,diff,main = "Goodman-Bacon Weights",xlab="Weights",ylab="Coefficients", col=c("Blue","Blue","Red","Red"), pch=c(19,19,4,4)) 
  abline(h=0,lty=2,lwd=2)
  dev.off()
  
  bacon_weights = data.frame(weight,diff)
  write.csv(bacon_weights,paste(output_path,"/goodman_bacon.csv",sep=""))
  
  return(summary(lmr))
}

treatment_timing = function(place_input, place, time, post){
  return(min(subset(time, place==place_input & post==1)))
}

adjusted_treatment = function(place_input,time_input, place, time,post_treat){
  grand_mean_treatment = mean(post_treat)
  place_mean = mean(subset(post_treat,place == place_input))
  time_mean = mean(subset(post_treat,time == time_input))
  adjusted_treatment = subset((post_treat-grand_mean_treatment) - (place_mean-grand_mean_treatment)
                              - (time_mean-grand_mean_treatment),place==place_input & time==time_input)
  return(adjusted_treatment)
}

proportions = function(place,outcome,time,post){
  treatment_timing = sapply(place,treatment_timing,place,time,post)
  treatment_timing[treatment_timing == "Inf"] = 3000
  index_treatment = unique(treatment_timing)
  
  early_adopters = subset(place,treatment_timing==index_treatment[1])
  n_early = length(early_adopters)
  early_adopters = unique(subset(place,treatment_timing==index_treatment[1]))
  late_adopters = subset(place,treatment_timing==index_treatment[2])
  n_late = length(late_adopters)                       
  late_adopters = unique(subset(place,treatment_timing==index_treatment[2]))
  never_adopters = subset(place,treatment_timing==index_treatment[3])
  n_never = length(never_adopters)
  never_adopters = unique(subset(place,treatment_timing==index_treatment[3]))
  n_total = length(outcome)
  n_early = n_early/n_total
  n_late = n_late/n_total
  n_never = n_never/n_total
  
  time_total = length(unique(time))
  place_total = length(unique(place))
  df = data.frame(place,time)
  
  adj_treat = rep(NA,length(df[[1]]))
  for (i in 1:length(df[[1]])){
    adj_treat[i] = adjusted_treatment(df[i,1],df[i,2], place, time, post_treat) 
  }
  
  time_share_early = (max(time)-index_treatment[1]+1)/((max(time)-min(time))+1)
  time_share_late = (max(time)-index_treatment[2]+1)/((max(time)-min(time))+1)
  
  return(c(n_never,n_early,n_late,time_share_early,time_share_late,time_total,place_total,adj_treat))
}

weights_bacon = function(n_never,n_early,n_late,time_share_early,time_share_late,time_total,place_total,adj_treat){
  weight_sku = (n_early*n_never*time_share_early*(1-time_share_early))/
    ((1/(time_total*place_total))*sum(adj_treat^2))
  weight_slu = (n_late*n_never*time_share_late*(1-time_share_late))/
    ((1/(time_total*place_total))*sum(adj_treat^2))
  weight_mukl = (1-time_share_early)/(1-(time_share_early-time_share_late))
  weight_skl = (n_early*n_late*(time_share_early-time_share_late)*(1-(time_share_early-time_share_late)))/
    ((1/(time_total*place_total))*sum(adj_treat^2))
  weight_late_cntl = weight_skl*weight_mukl
  weight_early_cntl = -(weight_skl*weight_mukl)
  return(c(weight_sku,weight_slu,weight_late_cntl,weight_early_cntl))
}

dd = function(outcome,place,time,post_treat){
  treatment_timing = sapply(place,treatment_timing,place,time,post)
  treatment_timing[treatment_timing == "Inf"] = 3000
  index_treatment = unique(treatment_timing)
  
  never_adopters_pre_early = mean(subset(outcome,year < index_treatment[1] & treatment_timing == index_treatment[3]))
  never_adopters_post_early = mean(subset(outcome,year >= index_treatment[1] & treatment_timing == index_treatment[3]))
  never_adopters_pre_late = mean(subset(outcome,year < index_treatment[2] & treatment_timing == index_treatment[3]))
  never_adopters_post_late = mean(subset(outcome,year >= index_treatment[2] & treatment_timing == index_treatment[3]))
  
  early_adopters_pre_early = mean(subset(outcome,year < index_treatment[1] & treatment_timing == index_treatment[1]))
  early_adopters_post_early = mean(subset(outcome,year >= index_treatment[1] & treatment_timing == index_treatment[1]))
  late_adopters_pre_late = mean(subset(outcome,year < index_treatment[2] & treatment_timing == index_treatment[2]))
  late_adopters_post_late= mean(subset(outcome,year >= index_treatment[2] & treatment_timing == index_treatment[2]))
  
  
  early_cntl_early_pre = mean(subset(outcome,year >= index_treatment[1] & year < index_treatment[2] & treatment_timing == index_treatment[1]))
  early_cntl_early_post = mean(subset(outcome,year >= index_treatment[2] & treatment_timing == index_treatment[1]))
  early_cntl_late_pre = mean(subset(outcome,year >= index_treatment[1] & year < index_treatment[2] & treatment_timing == index_treatment[2]))
  early_cntl_late_post = mean(subset(outcome,year >= index_treatment[2] & treatment_timing == index_treatment[2]))
  
  late_cntl_early_pre = mean(subset(outcome,year < index_treatment[1] & treatment_timing == index_treatment[1]))
  late_cntl_early_post = mean(subset(outcome,year >= index_treatment[1] & year < index_treatment[2] & treatment_timing == index_treatment[1]))
  late_cntl_late_pre = mean(subset(outcome,year < index_treatment[1] & treatment_timing == index_treatment[2]))
  late_cntl_late_post = mean(subset(outcome,year >= index_treatment[1] & year < index_treatment[2] & treatment_timing == index_treatment[2]))
  
  beta_early_never = (early_adopters_post_early - early_adopters_pre_early) - (never_adopters_post_early - never_adopters_pre_early)
  beta_late_never = (late_adopters_post_late - late_adopters_pre_late) - (never_adopters_post_late - never_adopters_pre_late)
  beta_early_cntl = (early_cntl_late_post - early_cntl_late_pre) - (early_cntl_early_post - early_cntl_early_pre)
  beta_late_cntl = (late_cntl_early_post - late_cntl_early_pre) - (late_cntl_late_post - late_cntl_late_pre)
  
  return(c(beta_early_never,beta_late_never,beta_early_cntl,beta_late_cntl))
}

load("fake_data_project.RData")
bacon(outcome,post,treat,post_treat,state,year,state)