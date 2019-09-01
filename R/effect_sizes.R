"
@title: effect size calculation
@author: Alan Ho MMStat

(C) Alan Ho
"
# SOURCES --------------------------------------------------------------------------------
source('R/functions.R')

# LIBRARIES ------------------------------------------------------------------------------
library(readxl)
library(tidyverse)
library(stringr)

# Austin ----------------------------------------------------------
austin = readxl::read_excel('extraction.xlsx', sheet = 'Austin') %>% 
  dplyr::mutate(
    treat_y = treat_percent/100 * treat_total,
    treat_no = (1-treat_percent/100) * treat_total,
    control_y = control_percent/100 * control_total,
    control_no = (1-control_percent/100) * control_total,
    
    OR = (treat_y/treat_no)/(control_y/control_no),
    logOR = log(OR),
    v.logOR = 1/treat_y + 1/treat_no + 1/control_y + 1/control_no,
    
    # converting to d using Hasselblad and Hedges method 
    d = logOR * (sqrt(3)/pi),
    v = v.logOR * 3/(pi^2)
  )

# Bevan et al. ----------------------------------------------------------
bevan = readxl::read_excel('extraction.xlsx', sheet = 'Bevan') %>% 
  dplyr::mutate(
    
    r = compute_r(mean_t0, mean_t1, sd_t0, sd_t1, n, p),
    d = -t/sqrt(n),
    v = match_d.variance(d, r, n)
  )

# Beddoe ----------------------------------------------------------
readxl::read_excel('extraction.xlsx', sheet = 'Beddoe') %>% 
  dplyr::mutate(
    
    MSe = MS / fratio
    
  )

# Bowen et al. ----------------------------------------------------------
bowen = readxl::read_excel('extraction.xlsx', sheet = 'Bowen') %>% 
  dplyr::mutate(
    
    extract = map(Value, function(string){
      
      temp = str_split(string, ' ')[[1]][c(-2,-5)]
      names(temp) <- c('treat_mean', 'treat_sd', 'control_mean', 'control_sd', 't', 'p')
      
      temp['t'] <- str_replace(temp['t'], ',', '')
      
      as.data.frame(temp) %>% 
        rownames_to_column() %>% 
        dplyr::mutate(temp = as.numeric(as.character(temp))) %>%
        spread(rowname, temp) %>% 
        mutate(n1 = 19, n2 = 18)
    }),
    
    statistics = map(extract, function(df){
      
      d = cohen_d.independent(mean1 = df$treat_mean,
                              mean2 = df$control_mean,
                              s1 = df$treat_sd,
                              s2 = df$control_sd,
                              n1 = df$n1,
                              n2 = df$n2,
                              standard.f = 's_pooled')
      
      v = independent_d.variance(d = d,
                                 n1 = df$n1,
                                 n2 = df$n2)
      
      tibble(d = d, v = v, n_treat = df$n1, n_control = df$n2)
      
    })
    
    
  ) %>% 
  unnest(statistics)

# Chabrol ---------------------------------------------------------------
chabrol = readxl::read_excel('extraction.xlsx', sheet = 'Chabrol') %>% 
  dplyr::mutate(
    
    mean_diff = treat_mean - control_mean,
    s = s_pooled(treat_sd, control_sd, treat_n, control_n),
    d = mean_diff/s,
    v = independent_d.variance(d, treat_n, control_n)
    
  ) 

# Cho et al. ------------------------------------------------------
cho = readxl::read_excel('extraction.xlsx', sheet = 'Cho') %>% 
  dplyr::mutate(
    eta2 = SSgroup/SStotal,
    f = sqrt( eta2 / ( 1 - eta2 ) ),
    d = -(f * 2),
    v = independent_d.variance(d, treat_n, control_n)
  )


# Di blasio -------------------------------------------------------------
diblasio = readxl::read_excel('extraction.xlsx', sheet = 'Di Blasio') %>% 
  dplyr::mutate(
    
    mean_diff = treat_mean - control_mean,
    s = s_pooled(treat_sd, control_sd, treat_n, control_n),
    d = mean_diff/s,
    v = independent_d.variance(d, treat_n, control_n)
    
  )

# Dimidjian ---------------------------------------------------------------- 
"Significant decrease in depressive symptoms - hence the d should be negative"
dimidjian = readxl::read_excel('extraction.xlsx', sheet = 'Dimidjian') %>% 
  dplyr::mutate(
    d = -d,
    v = match_d.variance(d, r, n)
    
  )


# Dunn -------------------------------------------
dunn = readxl::read_excel('extraction.xlsx', sheet = 'Dunn') %>% 
  filter(adju == 'y') %>% 
  mutate(
    OR = (treat_y/treat_no)/(control_y/control_no),
    logOR = log(OR),
    v.logOR = 1/treat_y + 1/treat_no + 1/control_y + 1/control_no,
    
    # converting to d using Hasselblad and Hedges method 
    d = logOR * (sqrt(3)/pi),
    v = v.logOR * 3/(pi^2)
  ) 




# Green -----------------------------------------------------------------------
green = readxl::read_excel('extraction.xlsx', sheet = 'Green') %>% 
  dplyr::mutate(
    
    # best guess of the correlation between time points
    r = compute_r(mean_t0, mean_t1, sd_t0, sd_t1, n, p),
    
    mean_diff = mean_t1 - mean_t0, 
    
    s_diff = sqrt(sd_t0^2 + sd_t1^2 - 2 * r * sd_t0 * sd_t1),
    
    d = mean_diff / s_diff,
    
    v = match_d.variance(d, r, n)
    
  ) 


# Goodman ----------------------------------------------------------------
"MAAS and SCS go up"
goodman = readxl::read_excel('extraction.xlsx', sheet = 'Goodman') %>% 
  dplyr::mutate(
    f = sqrt( eta2 / ( 1 - eta2 ) ),
    r = compute_r(mean_t0, mean_t1, sd_t0, sd_t1, n, p),
    d = (f * 2),
    d = ifelse(Measure %in% c('MAAS', 'SCS'), d, -d),
    v = match_d.variance(d, r, n)
  )




# Guardian et al. -------------------------------------------------
guardino = readxl::read_excel('extraction.xlsx', sheet = 'Guardino') %>% 
  filter(Time == 't1') %>% 
  dplyr::mutate( 
    
    mean_diff = treat_mean - control_mean,
    s = s_pooled(treat_sd, control_sd, treat_n, control_n),
    d = mean_diff/s,
    v = independent_d.variance(d, treat_n, control_n)
    
  ) 


# Krusche et al. --------------------------------------------------------

krusche = readxl::read_excel('extraction.xlsx', sheet = 'Krusche') %>% 
  dplyr::mutate(
    f = sqrt( eta2 / ( 1 - eta2 ) ),
    d = f * 2,
    v = independent_d.variance(d, n1, n2)
  )

krusche_gad7 = tibble(
  diff_treat = -3.88,
  diff_control = -2.23,
  f_treat = 18.42,
  f_control = 14.27,
  t_treat = sqrt(f_treat),
  t_control = sqrt(f_control),
  n_treat = 22,
  n_control = 50,
  s_treat = (diff_treat*n_treat)/(-1*t_treat),
  s_control = (diff_control*n_control)/(-1*t_control),
  
  # 
  s_pooled = s_unpooled(s_treat, s_control, n_treat, s_control),
  d = (diff_treat - diff_control)/s_pooled,
  v = independent_d.variance(d, n_treat, n_control)
)


# Luberto ---------------------------------------------------------------------
luberto = readxl::read_excel('extraction.xlsx', sheet = 'Luberto') %>% 
  dplyr::mutate(
    
    # best guess of the correlation between time points
    r = compute_r(mean_t1, mean_t2, sd_t1, sd_t2, n, p2),
    
    mean_diff = mean_t1 - mean_t0, 
    
    s_diff = sqrt(sd_t0^2 + sd_t1^2 - 2 * r * sd_t0 * sd_t1),
    
    d = mean_diff / s_diff,
    
    v = match_d.variance(d, r, n)
    
  ) 

# Milgrom ---------------------------------------------------------

# Pisson et al. ------------------------------------------------------------
pisson = readxl::read_excel('extraction.xlsx', sheet = 'Pisson')%>% 
  group_by(Author, Measure, Time, r) %>% 
  nest() %>% 
  spread(Time, data) %>% 
  dplyr::mutate(
    
    mean_diff = pmap(
      
      list(get('1'), get('2')),
      
      .f = function(df1, df2){
        
        tibble(
          treat_diff = df1$treat_mean - df2$treat_mean,
          control_diff = df1$control_mean - df2$control_mean     
        )
        
      }),
    
    s_diff = pmap(
      
      list(get('1'), get('2'), r),
      
      .f = function(df1, df2, r){
        tibble(
          treat.change_diff = s_diff(s1 = df1$treat_sd, s2 = df2$treat_sd, r = r),
          control.change_diff = s_diff(s1 = df1$control_sd, s2 = df2$control_sd, r = r)
        )
        
      }),
    
    
    d = pmap(
      
      list(get('1'), get('2'), mean_diff, s_diff), 
      
      .f = function(t1, t2, mean_diff, s_diff){
        
        cohen_d.independent(
          mean1 = mean_diff$treat_diff,
          mean2 = mean_diff$control_diff,
          s1 = s_diff$treat.change_diff,
          s2 = s_diff$control.change_diff,
          n1 = t1$treat_n,   # because they have the same sample sizes across time
          n2 = t1$control_n,
          standard.f = 's_pooled'
        )  
        
      }),
    
    
    v = pmap(
      
      list(d, get('1'), get('2')),
      
      .f = function(d, t1, t2){
        
        n1 = t1$treat_n
        n2 = t1$control_n
        
        independent_d.variance(d = d, n1 = n1, n2 = n2)
        
      })
    
    
  ) %>% 
  unnest(d, v)


# Woolhouse -------------------------------------------------------

woolhouse = readxl::read_excel('extraction.xlsx', sheet = 'Woolhouse') %>% 
  dplyr::mutate(
    treat = map(treat_mean_CI, extract_mean.ci),
    control = map(control_mean_CI, extract_mean.ci),
    treat_s = pmap(
      
      list(treat, treat_n, df),
      
      .f = compute_s.within
      
    ),
    
    control_s = pmap(
      
      list(control, control_n, df),
      
      .f = compute_s.within
      
    ),
    # 
    # mean_diff = pmap(
    #   
    #   list(treat, control),
    #   
    #   .f = function(t, c) t$mean - c$mean
    #   
    # ),
    
    d = pmap(
      
      list(treat, control, treat_s, control_s, treat_n, control_n),
      
      .f = function(t, c, ts, cs, nt, nc) {
        
        cohen_d.independent(t$mean, c$mean, ts, cs, nt, nc)
        
      }),
    
    v = pmap(
      list(d,treat_n, control_n),
      
      .f = independent_d.variance
    )
    
  ) %>% 
  unnest(d, v)


# Vieten et al. ---------------------------------------------------------
vieten = readxl::read_excel('extraction.xlsx', sheet = 'Vieten') %>% 
  dplyr::mutate(
    v = pmap(
      
      list(d, n_treat, n_control),
      
      .f = independent_d.variance
      
    )
    
  ) %>% 
  unnest(v) %>% 
  dplyr::select(Author, Measure, d, v, n_treat, n_control)


# Zhang ------------------------------------------------------------------
zhang_df = readxl::read_excel('extraction.xlsx', sheet = 'Zhang')

"If there are any notable baseline differences"
zhang_stats = zhang_df %>% 
  select(Author, Measure, Group, mean_t0, sd_t0, n_t0) %>% 
  group_by(Measure, Group) %>% 
  nest() %>% 
  spread(Group, data) %>% 
  dplyr::mutate(
    
    stats = pmap(
      
      list(TxAU, MM),
      
      .f = function(c, t){
        
        m1 = t$mean_t0
        m2 = c$mean_t0
        s1 = t$sd_t0
        s2 = c$sd_t0
        n1 = t$n_t0
        n2 = c$n_t0
        
        t = (m1 - m2)/sqrt(s1^2/n1 + s2^2/n2)
        df = n1 + n2 - 2
        p = pt(t, df)
        
        tibble(
          t = t,
          df = df,
          p = p
        )
        
      })
    
  ) %>% 
  unnest(stats)

"effect size for zhang"

zhang = (zhang_df %>% 
           gather(Statistic, Value,  n_t0 :sd_t2) %>% 
           separate(Statistic, c('Stat', 'Time')) %>% 
           spread(Stat, Value) %>% 
           group_by(Group) %>% 
           nest() %>% 
           mutate(
             
             data = pmap(
               
               list(Group, data), 
               
               .f = function(grp, df){
                 
                 newlabel = paste(grp, c('mean', 'n', 'sd'), sep = '_')
                 names(newlabel) = c('mean', 'n', 'sd')
                 df %>% rename_at(vars(c('mean', 'n', 'sd')), ~newlabel)
                 
               })
             
           ))$data %>% 
  reduce(bind_cols) %>% 
  select(-Author1, -Measure1, -Time1) %>% 
  dplyr::mutate( 
    
    mean_diff = MM_mean - TxAU_mean,
    s = s_pooled(MM_sd, TxAU_sd, MM_n, TxAU_n),
    d = mean_diff/s,
    v = independent_d.variance(d, MM_n, TxAU_n)
    
  ) 

