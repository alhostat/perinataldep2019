# utf-8 encoding
"
@title:         Meta-analysis
@author:        Alan Ho
@date:          17-11-2018
#description:   Generates the forest plot + all associated statistics
"
# LIBRARIES ----------------------------------------------------------
library(meta)
library(metafor)
library(metaviz) 
library(rlist)

# FUNCTIONS --------------------------------------------------------------------
" For computing the trim-fill and regtest for funnel plot asymmetry"
diagnostics = function(df){
  res <- rma(d, vd, data = df)
  regtest.mod = regtest(res, model = "lm")
  rtf <- trimfill(res)
  
  list(
    'rma' = res, 
    'regtest' = regtest.mod,
    'regtest_results' = tibble(
      z = regtest.mod$zval, 
      p = regtest.mod$pval
    ),
    'trim' = rtf,
    'trim_results' = tibble(
      estimate = rtf$beta[1,1],
      z = rtf$zval,
      p = rtf$pval
    )
  )
  
}

"Generates forest plot with subgroup analyses included"
generate_forestplot = function(results, df){
  
  meta_results = metagen(
    TE=d, 
    seTE=sqrt(vd), 
    studlab = paste(author, year),
    byvar = type,
    data = df,
    sm = "Cohen's d",
    title = 'Meta-Analysis',
    method.tau = 'REML')
  
  meta::forest(meta_results, 
               slab = paste(df$author, sep = ", "),
               ilab = df$year,
               leftcols = c('author','year'),
               ilab.xpos = -5,
               xlab = 'Symptom Reduction (relative to control/baseline)',
               comb.random = T,
               comb.fixed = F,
               subgroup = T,
               print.I2 = T,
               print.subgroup.labels=T,
               bylab = df$type)
  
}


# FREQUENTIST RESULTS ----------------------------------------------------------

meta_results = metagen(
  TE=d, 
  seTE=sqrt(vd), 
  studlab = paste(author, year),
  byvar = type,
  data = perinatal_data,
  sm = "Cohen's d",
  title = 'Meta-Analysis',
  method.tau = 'REML')



trim_fill.results = list(
  'perinatal_data',
  'depression',
  'anxiety',
  'worry'
) %>% 
  map(function(df){
    
    labels = df
    
    if(df == 'perinatal_data') labels = 'Overall'
    
    res = diagnostics(get(df))$trim_results %>% 
            mutate(Subgroup = labels) %>% 
            select(Subgroup, everything())
    
    
  }) %>% 
  reduce(bind_rows)


"Subgroup Analysis Results"
subgroup_analysis = list.append(
  summary(meta_results )$within.random[1:6], 
  summary(meta_results )$I2.w[[1]]
) %>% 
  enframe() %>% 
  unnest() %>% 
  mutate(
    name = ifelse(name == "", 'I2', name),
    groups = rep(meta_results$bylevs, 7)
  ) %>% 
  spread(name, value) %>% 
  select(groups, TE, lower, upper, z, p, I2)


