## Stats 506, F20
## Final Project
## 
## This R script is for Final Project
## Author:  Hongfan Chen, chenhf@umich.edu
## Updated: November, 25, 2020
# 79: -------------------------------------------------------------------------

# libraries: 
library(tidyverse)
# directories: ----------------------------------------------------------------
path = './data'

# data cleaning: --------------------------------------------------------------
## 2012 CBECS data
### OPEN24 NWKER PUBID SQFTC PBA
cbecs_file = sprintf('%s/2012_public_use_data_aug2016.csv', path)
cbecs_min = sprintf('%s/cbecs_min.csv', path)
if ( !file.exists(cbecs_min) ) {
  cbecs = read_delim(cbecs_file, delim = ',' ) %>%
    select(id = PUBID, w = FINALWT,  `square footage`= SQFTC,
           `24-Hour` = OPEN24, number = NWKER, activity = PBA)
  write_delim(cbecs, path = cbecs_min, delim = ',')
} else {
  cbecs = read_delim(cbecs_min, delim = ',')
}

## codebook
cb_file = sprintf('%s/2012microdata_codebook.xlsx', path)
codebook = readxl::read_xlsx(cb_file) %>% 
  as.data.frame()

## variables of interest
variables = c(`square footage` = 'SQFTC',
              `24-Hour` = 'OPEN24', activity = 'PBA')
codes = codebook %>% 
  filter(`Variable\r\nname` %in% variables)

## cleaning the codebook and select the needed factor
decode = str_replace_all(codes$`Values/Format codes`,
                         pattern = "\\r\\n", replacement = "") %>%
  str_replace_all(pattern = "' = '", replacement = "placeholder") %>%
  str_replace_all(pattern = "''", replacement = "placeholder") %>%
  str_replace_all(pattern = "'", replacement = "") %>%
  str_split(pattern = "placeholder")

## create a vector consisting of levels and labels
var_factor = c()
for (i in 1:length(decode)) {
  level_idx = seq(1, length(decode[[i]]), by = 2)
  label_idx = seq(2, length(decode[[i]]), by = 2)
  var_factor[[i]] = list(decode[[i]][level_idx], decode[[i]][label_idx])
}

## apply the levels and labels to the factor
cbecs = cbecs %>% 
  mutate(activity = factor(activity,
                           levels = var_factor[[1]][[1]],
                           labels = var_factor[[1]][[2]]),
         `square footage` = factor(`square footage`,
                                   levels = var_factor[[2]][[1]],
                                   labels = var_factor[[2]][[2]]),
         `24-Hour` = factor(`24-Hour`,
                            levels = var_factor[[3]][[1]],
                            labels = var_factor[[3]][[2]]),
         id = as.double(id)
         ) %>%
  drop_na()

# point estimates of TV means by division and region: -------------------------
avg_wf = cbecs %>%
  group_by(`square footage`, activity, `24-Hour`) %>%
  summarize(avg_wf = sum(w * number)/sum(w))

# for CI's, make rep_weights long format: -------------------------------------
cbecs_full = read_delim(cbecs_file, delim = ',')
long_weights = 
  cbecs_full %>% 
  select( id = PUBID, FINALWT1:FINALWT197) %>%
  pivot_longer(
    cols = starts_with('FINALWT'),
    names_to = 'rep',
    names_prefix = 'FINALWT',
    values_to = 'rw'
  ) %>%
  mutate(rep = as.integer(rep),
         id = as.double(id)
          )

# compute confidence intervals, using replicate weights: ----------------------

## replicat means
## should be very cautious here because sum(rw) can equal to 0 due to jackknife
## method of estimating standard error
avg_wf_rep = cbecs %>%
  select(-w) %>%
  left_join(long_weights, by = 'id') %>%
  group_by(`square footage`, activity, `24-Hour`, rep) %>%
  summarize(avg_wf_rep = ifelse(sum(rw) != 0,
                                sum(rw * number)/sum(rw),
                                0),
            .groups = 'drop')

## variance of replicate means around the point estimate
var_wf = avg_wf_rep %>%
  left_join(avg_wf, by = c('square footage', 'activity', '24-Hour')) %>%
  group_by(`square footage`, activity, `24-Hour`) %>%
  summarize(v = sum({avg_wf_rep - avg_wf}^2), 
            .groups = 'drop')
avg_wf = avg_wf %>%
  left_join(var_wf, by = c('square footage', 'activity', '24-Hour'))

## construct mean's CI
m = qnorm(.975)
avg_wf = avg_wf %>%
  mutate(
    se = sqrt(v),
    lwr = avg_wf - m * se,
    upr = avg_wf + m * se,
    CI = sprintf('%.3f (%.3f-%.3f)', avg_wf, lwr, upr)
    )

# construct the first part of the table and help the next step to
# filter rows with complete status
avg_wf_table = avg_wf %>%
  select(`square footage`, activity, `24-Hour`, CI) %>%
  pivot_wider(id_cols = c('square footage', 'activity'),
              names_from = `24-Hour`,
              values_from = CI) %>%
  drop_na()

# filter rows with variable `24-Hour` has both value yes and no.
complete_buildings = avg_wf_table %>% 
  pivot_longer(
    cols = c('Yes', 'No'),
    names_to = "fake"
  ) %>%
  select(-fake, -value)

# merge with the avg_wf, find the complete observations.
# really wierd here, if I use left_join the observations will be duplicated???
# complete_buildings %>%
#   left_join(avg_wf, by = c('square footage', 'activity'))
# but semi_join can be a replacement.
# construct CI for difference in average work force
complete_data = avg_wf %>%
  semi_join(complete_buildings, by = c('square footage', 'activity')) %>%
  select(`square footage`, activity, `24-Hour`, avg_wf, v) %>%
  pivot_wider(id_cols = c('square footage', 'activity'),
              names_from = `24-Hour`,
              values_from = c('avg_wf', 'v')
              ) %>% 
  mutate(diff_avg = avg_wf_Yes - avg_wf_No,
         v = v_Yes + v_No,
         se = sqrt(v),
         lwr = pmax(diff_avg - m * se),
         upr = pmin(diff_avg + m * se),
         diffCI = sprintf('%.3f (%.3f-%.3f)', diff_avg, lwr, upr)
         )

# merge with the previous table to form a complete table
tab = complete_data %>%
  select(`square footage`, activity, diffCI) %>%
  left_join(avg_wf_table, by = c('square footage', 'activity')) %>%
  rename(Difference = diffCI,
         `OPEN 24` = Yes,
         `Not OPEN 24` = No,
         `Square Footage Category` = `square footage`,
         `Principal Building Activity` = activity)

# create a function for plotting
visual = function(data){
  data %>% 
    ggplot( aes(x = diff_avg, y = activity, color = activity)
            ) +
    geom_point(
      position = position_dodge2(width = 0.5)
      ) +
    geom_errorbar(
      aes(xmin = lwr, xmax = upr),
      position = position_dodge(width = 0.5),
      alpha = 0.75
      ) +
    geom_vline(xintercept = 0, lty = 'dashed') +
    facet_wrap(~`square footage`) +
    theme_bw() +
    xlab('difference in average work forces') 
}
data_1 = complete_data %>%
  filter(`square footage` %in% c("1,001 to 5,000 square feet",
                             "5,001 to 10,000 square feet",
                             "10,001 to 25,000 square feet",
                             "25,001 to 50,000 square feet"))
data_2 = complete_data %>%
  filter(`square footage` %in% c("50,001 to 100,000 square feet",
                                 "100,001 to 200,000 square feet",
                                 "200,001 to 500,000 square feet",
                                 "500,001 to 1 million square feet",
                                 "Over 1 million square feet"))
datalist = list(data_1, data_2)
graphname = c("./graph/1.png", "./graph/2.png")
for (i in 1:2) {
visual(datalist[[i]])
ggsave(graphname[i],
       width = 12,
       height = 6)
}
