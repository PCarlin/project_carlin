###################################################################################
# Task: Testing the function bacon
# Author: Patrick Carlin (patcarli@iu.edu)
# Date: 12 December 2019
# Input data: code_carlin_project.R 
# Output data: testthat output
###################################################################################

setwd("C:/Users/Pat C/Box/_My Documents/Active/Class/Statistical Computing/Project")

source('code_carlin_project.R')

if(!require(testthat)){
  install.packages("testthat")
  library(testthat)
}

test_that("Simple Functions Return Correct Values with Provided Data", {
  expect_equal(treatment_timing("AL",c("AL","AL","AL","AL","AL"),c(1901,1902,1903,1904,1905),c(0,0,1,1,1)),1903)
  expect_equal(adjusted_treatment(c("AL","AL","AL","AL","AL"),c(1901,1902,1903,1904,1905), "AL", 1903,c(0,0,1,1,1)),0)
  expect_equal(weights_bacon(.25,.25,.5,.33,.33,15,5,c(0.5,0.3,0.3,0.1,0.1,0.2,0.6,0.7,0.2,0.8,0.6,0.01,0.7,0.3,0.02)),
                                                      c(0.350078111805438,0.700156223610876,0,0))
})

test_that("Functions Return Correct Type of Data", {
  expect_equal(class(treatment_timing("AL",c("AL","AL","AL","AL","AL"),c(1901,1902,1903,1904,1905),c(0,0,1,1,1))),"numeric")
  expect_equal(typeof(weights_bacon(.25,.25,.5,.33,.33,15,5,c(0.52055311,0.28368973,0.27984078,0.09557787,0.05240121,0.22474658,
                                                             0.56071443,0.66681220,0.23417615,0.81725573,0.62716355,0.01050599,
                                                             0.73215510,0.28667766,0.02117444))),"double")
  
})