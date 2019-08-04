# utf-8 encoding
"
@title:         Meta-analysis
@author:        Alan Ho
@date:          17-11-2018
#description:   Generates the forest plot + all associated statistics
"
# SOURCES ------------------------------------------------------------
source('es_gen.R')



# LIBRARIES ----------------------------------------------------------
library(meta)
library("metafor")
library(metaviz) 
library(magrittr)

args(metacont)




# FOREST PLOT  -------------------------------------------------------
plot_forest = function(pathtofile){
  
  data = read.csv(pathtofile)
  meta_results = metagen(
    TE=g, 
    seTE=sqrt(vg), 
    studlab = paste(author, year),
    byvar = type,
    data = data,
    sm = "Hedge's g",
    title = 'Meta-Analysis',
    method.tau = 'REML')
  
  
  dev.off()
  
  meta::forest(meta_results, 
               slab = paste(data$author, sep = ", "),
               ilab = data$year,
               leftcols = c('author','year'),
               ilab.xpos = -5,
               xlab = 'Symptom Reduction (relative to control/baseline)',
               comb.random = T,
               comb.fixed = F,
               subgroup = T,
               print.I2 = T,
               print.subgroup.labels=T,
               bylab = data$type)
  
  meta::funnel(meta_results)
}
