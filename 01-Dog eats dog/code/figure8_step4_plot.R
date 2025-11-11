###########################################################################################################################################################
############################################################### FIGURE 8: 模型3回归结果 ###################################################################
###########################################################################################################################################################

bins <- as.character(sort(unique(this_sample$rover_share_cut)))
bins <- c(bins[5], bins[1:4])

load(paste0(figures_dir_matching,'plot_list_matched_all.RData'))
plot_matching <- plot_list_matched
outcome_list_matching <- outcome_list
outcome_names_matching <- outcome_names

vars_to_keep <- match(c("log_stays", "success_rate"),outcome_list_matching)
gg5 <- create_plot(plot_matching,vars_to_keep,outcome_names_matching); gg5
ggsave(paste0(figures_dir_plot, "figure_8a.png"), gg5, width = 18, height = 14*2/5)
print(paste0('Figure 8a written to: ', figures_dir_plot, "figure_8a.png"))


vars_to_keep <- match(c("log_num_stays_newb", "success_rate_newb"),outcome_list_matching)
gg6 <- create_plot(plot_matching,vars_to_keep,outcome_names_matching); gg6
ggsave(paste0(figures_dir_plot, "figure_8b.png"), gg6, width = 18, height = 14*2/5)

print(paste0('Figure 8b written to: ', figures_dir_plot, "figure_8b.png"))

