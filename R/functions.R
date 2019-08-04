
"
@title: functions for effect size calculation
@author: Alan Ho MMStat

(C) Alan Ho
"

# LIBRARIES ------------------------------------------------------------------------------
library(tidyverse)


# FUNCTIONS ------------------------------------------------------------------------------
compute_r = function(mean1, mean2, s1, s2, n, p){
  
  mean_diff = mean2 - mean1
  # print(mean_diff)
  df = n - 1
  p_onetail = p/2
  
  t = abs(qt(p_onetail, df, lower.tail = TRUE))
  s_diff = sqrt(n)/t * abs(mean_diff)
  r = (s1^2 + s2^2 - s_diff^2)/(2*s1*s2)
  
  return(r)
}

s_pooled = function(s1, s2, n1, n2){
  
  v_pooled = ((n1-1)*s1^2 + (n2-1)*s2^2)/( n1 + n2 - 2)
  
  sqrt(v_pooled)
  
}

s_unpooled = function(s1, s2, n1, n2){
  
  v_unpooled = s1^2/n1 + s2^2/n2
  
  sqrt(v_unpooled)
  
}

s_diff = function(s1, s2, r){
  
  v_diff = s1^2 + s2^ - 2 * r * s1 * s2
  
  sqrt(v_diff)
  
}

cohen_d.independent = function(mean1, mean2, s1, s2, n1, n2, standard.f = 's_pooled'){
  
  diff = mean1 - mean2 
  s = get(standard.f)(s1 = s1, s2 = s2, n1 = n1, n2 = n2)
  
  diff / s
  
  
}

# source: Borenstein et al.
cohen_d.matched = function(mean_diff, s_diff){
  
  mean_diff/s_diff
  
}

independent_d.variance = function(d, n1, n2){
  
  (n1+n2)/(n1*n2) + d^2/(2*(n1+n2))
  
  
}

match_d.variance = function(d, r, n){
  
  # r is the correlation between timepoint 1 and 2
  (1/n + d^2/(2*n))*2*(1-r)
  
}

