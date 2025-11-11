#####################################################################################
############### Figure 2: 平台α占据不同市场份额时，合并导致的效用变化 ###############
#####################################################################################

max_k <- 1
min_k <- 0
solve_all_eqb_l2 <- Vectorize(function(x) solve_all_eqb(Z, x))
vec_cs <- c(0, seq(min_k, max_k, .02))
results_as_c_varies <- solve_all_eqb_l2(vec_cs)

results_as_c_varies <- as.data.table(t(results_as_c_varies))
results_as_c_varies <- results_as_c_varies[, lapply(.SD, as.numeric)]
results_as_c_varies[, k := vec_cs]
results_as_c_varies[, share := n1/(n1 + n2)]
results_as_c_varies <- results_as_c_varies[share >= 0 & share <= 1]

results_as_c_varies[, tc_change := (merger_solution$tcstar - (tc1 + tc2))]
results_as_c_varies[, nfx_change := merger_solution$nfxstar - (nfx1 + nfx2)]
results_as_c_varies[, u_change := (merger_solution$ustar - (u1 + u2))]
results_as_c_varies[, u_change_nooo := (nfx_change - tc_change)]

maxu <- results_as_c_varies[which.max(u_change)]

points <- results_as_c_varies[k==.5]
points[, nfxmintc1 := (nfx1 - tc1)]
annotate_size <- 5

plot_merger_gains <- ggplot(results_as_c_varies,  aes(x = share, y = nfx_change)) + geom_line(linetype = "dashed") +
  geom_line(aes(x = share, y = u_change_nooo), linetype = "solid") +
  geom_line(aes(x = share, y = - tc_change), linetype = "dotted") + 
  geom_line(aes(x = share, y = u_change), linetype = '3313') +
  xlab(expression(paste("Pre-Merger Platform ", alpha, " Share"))) + ylab("Merger Effects on Combined Platform Utility") + geom_hline(yintercept = 0) + paper_theme +
  annotate(geom="text", x=.50, y = points$nfx_change +.01, label="Platform Gains from Network Effects", size = annotate_size) +
  annotate(geom="text", x=.50, y = -points$tc_change + .03, label="Platform Losses from Differentiation", size = annotate_size) +
  annotate(geom="text", x=.50, y = points$u_change_nooo +.01, label="Platform Gains from Merger", size = annotate_size) +
  annotate(geom="text", x=.50, y = points$u_change -.015, label='Market Surplus from Merger', size = annotate_size) 

plot_merger_gains

ggsave(plot_merger_gains, file = 'figure2_plot_merger_gains.png', path = paste0(results_dir,"/figures/"), width = 10, height = 8)