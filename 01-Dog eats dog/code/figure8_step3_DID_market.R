####################################################################################################################################################
############################################################ 市场层面 DID 回归分析 #################################################################
####################################################################################################################################################

print("Market")

# 根据matching_list生成配对数据集
paired_data_list <- createPairedDataSet(matching_list, this_sample, outcomes)

# 根据配对数据集生成回归系数表格和系数图列表
res <- foreach(i = 1:length(outcomes)) %dopar% runRegression_outcome(outcomes[i], se_adjust = seadjust)

# 回归结果格式化，并保存结果(.RData文件)
res_formatted <- reformat_res(res)
plot_list_matched <- res_formatted[[1]]
outputfile = paste0(plot_dir, '/plot_list_matched_all.RData')
outcome_list <- outcomes
save(plot_list_matched, outcome_list, outcome_names, file = outputfile)

print("Done.")

