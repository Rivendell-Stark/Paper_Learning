setwd("D:/学期课程/07_数据分析与经济决策/03_课程作业/课堂论文/")
rm(list = setdiff(ls(), c("data_dir","results_dir")))

# 数据/图片/表格路径设置
results_dir <- paste0(getwd(), "/results/")
data_dir <- paste0(getwd(), "/data/")


library(data.table)
library(ggplot2)
library(stargazer)
library(zoo)
library(survival)
library(statar)
library(stringr)
library(ggthemes)
library(directlabels)
library(geosphere)
library(lfe)
library(gridExtra)

paper_theme <-  theme_classic() + theme(axis.text = element_text(size = 16, color = "black"),
                                        axis.title = element_text(size = 20),
                                        legend.text = element_text(size = 20),
                                        legend.title = element_text(size = 20))


##########################################################################################################################################
################################################# FIGURE 5：收购前Rover市场份额分组示意图 ################################################
##########################################################################################################################################

dt <- fread(paste0(data_dir, 'stays_convos_needs_agg_bygrid_zip_seller.csv'))
this_sample <- unique(dt[, list(market, mkt_share_gtv, rover_share_cut)], by = 'market')
bin_cutoffs <- sort(as.numeric(substr(unique(this_sample$rover_share_cut),6,8)))


plot_dist_shares <- ggplot(this_sample, aes(x = mkt_share_gtv)) + geom_histogram(binwidth=.025) + theme_classic() + xlab("Rover Share") + ylab("Num. Markets") + paper_theme
plot_dist_shares <- plot_dist_shares + geom_vline(xintercept=bin_cutoffs,color="red")
plot_dist_shares


ggsave(plot_dist_shares, file = 'figure5_shares_sample_dist.png', path = paste0(results_dir, "/figures/"), width = 10, height = 7)