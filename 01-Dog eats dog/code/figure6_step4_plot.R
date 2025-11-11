##################################################################################################################################
#################################################### FIGURE 6: 模型1回归结果 #####################################################
##################################################################################################################################

bins <- as.character(sort(unique(this_sample$rover_share_cut)))
bins <- c(bins[5], bins[1:4])

# 加载绘图数据
# Get matched results -- Dogvacay
load(paste0(figures_dir_matching,'plot_list_matched_DogVacay.RData'))
plot_dv <- plot_list_matched
# Get matched results -- Rover
load(paste0(figures_dir_matching,'plot_list_matched_Rover.RData'))
plot_rover <- plot_list_matched

# 定义结果变量与图表标签
outcome_list_users <- outcomes 
outcome_names_users <- c("Number of Buyers (log)","Transactions (log)", "Request Match Rate")

# 移除冗余变量
rm(plot_list_matched,outcome_list,outcome_names)

# 筛选要展示的结果变量
vars_to_keep <- match(c("log_stays", "success_rate"),outcome_list_users)

# 生成并保存Rover平台的图表（Figure 6a）
gg1 <- create_plot(plot_rover,vars_to_keep,outcome_names_users); gg1 
ggsave(paste0(figures_dir_plot, "figure_6a.png"), gg1, width = 18, height = 14*2/5)

# 生成并保存DogVacay平台的图表（Figure 6b）
gg2 <- create_plot(plot_dv,vars_to_keep,outcome_names_users); gg2 
ggsave(paste0(figures_dir_plot, "figure_6b.png"), gg2, width = 18, height = 14*2/5)

print(paste0('Figure 6a written to: ', figures_dir_plot, "figure_6a.png"))
print(paste0('Figure 6b written to: ', figures_dir_plot, "figure_6b.png"))