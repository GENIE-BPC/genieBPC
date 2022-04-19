library(testthat)
library(genieBPC)

if(genieBPC:::check_synapse_login() == FALSE){
  test_check("genieBPC")
}

