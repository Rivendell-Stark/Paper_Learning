
library(data.table)
library(ggplot2)
library(fixest)
library(dplyr)
library(lubridate)
#library(collapse)
library(MatchIt)


tables_dir <- paste0(results_dir, '/tables/') 
figures_dir <- paste0(results_dir, '/figures/') 

paper_theme <-  theme_classic() + theme(axis.text = element_text(size = 16, color = "black"),
                                        axis.title = element_text(size = 20),
                                        legend.text = element_text(size = 20),
                                        legend.title = element_text(size = 20))

merger_announcement_t <- as.Date("2017-03-01")   #Month when the merger was announced -- DO NOT CHANGE. 
dv_shut_t <- as.Date("2017-06-01")               #Month when DogVacay was shut down -- DO NOT CHANGE.
base_t <- as.Date("2017-02-01")                  #Coefficients for this base month are 0.
first_t <- as.Date("2016-01-01")                 #Coefficients for this base month are 0.

data_buyers_both <- fread(file = paste0(data_dir, 'matched_buyer_attrition.csv'))
data_buyers_both[, last_txn_2016_clean := as.Date(last_txn_2016, origin = '1970-01-01')]
data_buyers_both[is.na(as.character(last_txn_2016_clean)), last_txn_2016_clean := as.Date('2020-01-01')]
data_buyers_both[, ym_last_txn_2016_clean := paste0(substr(last_txn_2016_clean, 1, 7), '-01')]

data_buyers_both <- data_buyers_both[!is.na(avg_price_2016)] 
m.out1 <- matchit(has_stay_post_rover ~ num_txn_2016 + num_convos_2016 + ym_last_txn_2016_clean + has_repeat_pre + avg_price_2016 + rover_share_cut_flipped, data = data_buyers_both, method = "cem")
m.data1 <- match.data(m.out1, drop.unmatched = T) 

m.data1 <- data.table(m.data1)
# Conditional on transactions:
m.data1_txn <- m.data1[num_txn_2016 > 0]
# m.data1_txn <- data_buyers_both[num_txn_2016 > 0]
reg_matched <- feols(num_txn_post_shutdown ~ is_dv + num_txn_2016 + avg_price_2016 | ym_last_txn_2016_clean + rover_share_cut_flipped, data = m.data1_txn, weights = m.data1_txn$weights, se = 'white')
reg_matched_repeat <- feols(num_txn_post_shutdown ~ is_dv*has_repeat_pre + num_txn_2016 + avg_price_2016 | ym_last_txn_2016_clean + rover_share_cut_flipped, data = m.data1_txn, weights = m.data1_txn$weights, se = 'white')
reg_matched2_migrate <- feols(num_txn_post_shutdown ~ is_dv*has_repeat_pre*for_dv_did_seller_migrate + is_dv + num_txn_2016 + avg_price_2016 |ym_last_txn_2016_clean + rover_share_cut_flipped, data = m.data1_txn, weights = m.data1_txn$weights, se = 'white')
reg_matched2_past_txn <- feols(num_txn_post_shutdown ~ is_dv*num_txn_2016 + is_dv + num_txn_2016 + avg_price_2016 |ym_last_txn_2016_clean + rover_share_cut_flipped, data = m.data1_txn, weights = m.data1_txn$weights, se = 'white')
reg_matched2_past_sink <- feols(num_txn_post_shutdown ~ is_dv*has_repeat_pre*for_dv_did_seller_migrate + is_dv*num_txn_2016 + is_dv + num_txn_2016 + avg_price_2016 |ym_last_txn_2016_clean + rover_share_cut_flipped, data = m.data1_txn, weights = m.data1_txn$weights, se = 'white')
myDict = c("is_dv" = "DogVacay User", "has_repeat_pre" = "Has Repeat Stay", 'for_dv_did_seller_migrate' = "DogVacay Seller Migrated", 'num_txn_2016' = '# 2016 Stays', 'ym_last_txn_2016_clean' = "Month of Last Stay", "rover_share_cut_flipped" = "Platform Share", "num_txn_post_shutdown" = "# Transactions Post DogVacay Shutdown", 'avg_price_2016' = "Avg. Nightly Price (2016)")

# Diff in txn rates:
library(broom)
coef_baseline <- tidy(reg_matched) %>% as.data.table %>% filter(term == 'is_dv') %>% select(estimate) 
coef_baseline/m.data1_txn[, mean(num_txn_post_shutdown)]

mean_of_y <- round(m.data1_txn[, mean(num_txn_post_shutdown)], 2)
# num_txn_2016
# extraline
this_extraline <- list("{title:\\midrule}Mean of Y"= c(rep(as.character(mean_of_y), 3), paste0(as.character(mean_of_y))))

# output to log 
print(etable(reg_matched, reg_matched_repeat, reg_matched2_migrate, reg_matched2_past_sink, dict = myDict, extraline = this_extraline))

# save table 
etable(reg_matched, reg_matched_repeat, reg_matched2_migrate, reg_matched2_past_sink, dict = myDict, style.tex = style.tex("aer", fixef.suffix = " FE"), fitstat = ~ r2 + n, tex = TRUE, file = paste0(tables_dir, 'new_attrition.tex'), replace = T, extraline = this_extraline)

print(paste0('Table 1 saved to: ', tables_dir, 'new_attrition.tex'))

table_df <- etable(reg_matched, reg_matched_repeat, reg_matched2_migrate, reg_matched2_past_sink, dict = myDict, style.tex = style.tex("aer", fixef.suffix = " FE"), fitstat = ~ r2 + n, tex = FALSE, extraline = this_extraline)





