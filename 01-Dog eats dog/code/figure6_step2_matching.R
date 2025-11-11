####################################################################################################################################################
############################################################ 协变量倾向得分匹配 ####################################################################
####################################################################################################################################################

# 基于倾向得分进行匹配: 生成: matching_list
matching_list <- list()
for(i in 1:(length(unique_groups) - 1)){ 
  print(paste0('Starting ', unique_groups[i]))
  this_plot_list <- list()       # 临时存储当前分组的绘图结果
  this_estimates_list <- list()  # 临时存储当前分组的估计结果
  this_matching_list <- list()   # 临时存储当前分组的匹配结果
  this_group <- unique_groups[i] # 当前待匹配的分组
  this_data <- this_sample[(rover_share_cut == top_group | rover_share_cut == this_group), # 只保留对照组和当前待匹配分组
                           list(time_id, market_grp, is_treatment, unique_sitters, log_stays)] # 只保留相关协变量和关键变量
  this_panel <- PanelData(as.data.frame(this_data),
                          time.id = "time_id",
                          unit.id = "market_grp",
                          treatment = "is_treatment",
                          outcome = 'log_stays'
  )
  this_matching <- PanelMatch(panel.data = this_panel,
                              lag = 1,
                              refinement.method = refinement,
                              qoi = "att",
                              size.match = 1,
                              match.missing = T,
                              covs.formula = matching_formula,
                              lead = leads_matching
  )
  matching_list[[i]] <- this_matching
}

