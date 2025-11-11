####################################################################################################################################################
############################################################ 平台层面 DID 回归分析 #################################################################
####################################################################################################################################################


# 市场-市场组-分组 的唯一对应
marketgrptomarket <- unique(this_sample[, list(market, market_grp, rover_share_cut)])
# 读取按买家平台聚合的数据

# 遍历每个平台
for (platform in platforms) {
  print(platform)
  nameappend = paste0('_', platform)
  # 数据预处理
  this_sample <- getplatformdt(platform, stays_convos_needs_agg, stays_convos_needs_agg_platform)
  # 添加market_grp 与 rover_share_cut 变量，标识分组信息
  this_sample <- merge(this_sample, marketgrptomarket, by = 'market')
  # 格式化时间为日期变量，为每个年月分配唯一时间标识，并按照时间排序
  this_sample[, year_month_clean := as.Date(as.yearmon(year_month), frac = 0)]
  setorder(this_sample, 'year_month')
  this_sample[, time_id := .GRP, by = list(year_month_clean)]
  # 标识处理组与对照组
  this_sample[, is_treatment := as.numeric(rover_share_cut != top_group & year_month_clean >= (merger_announce_ym))]
  # 按市场组-时间去重
  this_sample <- unique(this_sample, by = c("market_grp",'time_id'))
  # 定义结果变量
  outcomes = c('log_buyers',  'log_stays', 'success_rate') 
  outcome_names= c('Buyers\n(log)',  'Transactions (log)', 'Request Match Rate')
  # 缺失值处理
  this_sample[is.na(log_buyers), log_buyers := 0]
  this_sample[is.na(log_sitters), log_sitters := 0]
  this_sample[is.na(log_needs), log_needs := 0]
  this_sample[is.na(log_convos), log_convos := 0]
  this_sample[is.na(log_stays), log_stays := 0]
  this_sample[, convosperneed := num_convos / unique_needs]
  # 排除结果变量中的Inf值
  for (outcome in outcomes) {
    this_sample[!(is.finite(get(outcome))), (outcome) := NA]
  }
  # 针对每个平台，根据matching_list生成配对数据集
  paired_data_list <- createPairedDataSet(matching_list, this_sample, outcomes)
  # 并行计算回归结果
  clusterExport(cl,varlist = c('dyadRobust', 'createCoefficientPlotDyad', 'createCoefficientPlot', 'paper_theme', 'paired_data_list'), envir=environment())
  res <- foreach(i = 1:length(outcomes)) %dopar% runRegression_outcome(outcomes[i], se_adjust = seadjust)
  # 回归结果格式化，并保存结果(.RData文件)
  res_formatted <- reformat_res(res)
  plot_list_matched <- res_formatted[[1]]
  outputfile = paste0(plot_dir, '/plot_list_matched', nameappend, '.RData')
  outcome_list <- outcomes
  save(plot_list_matched, outcome_list, outcome_names, file = outputfile)
  print("Done.")
}