###################################################################################################################################
################################################### FIGURE 9: 商户和平台的收入变化 ################################################
###################################################################################################################################

vars_to_keep <- match(c("log_gtv", "log_commission"),outcome_list_matching)
gg7 <- create_plot(plot_matching,vars_to_keep,outcome_names_matching); gg7 
ggsave(paste0(figures_dir_plot, "figure_9.png"), gg7, width = 18, height = 14*2/5)

print(paste0('Figure 9 written to: ', figures_dir_plot, "figure_9.png"))
