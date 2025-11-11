### 这张图展示了2016~2017年间两个平台的佣金费率变化
### 模拟数据生成时设定为15%，所以画出来也是根水平线。


rm(list=ls())
setwd("D:/学期课程/07_数据分析与经济决策/03_课程作业/课堂论文/")
figure_dir <- paste0(getwd(), "/results/figures/")
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

##################################### Prepare needs_convos_stays to aggregate at market level #########################################

dt <- fread(paste0(data.dir, 'needs_convos_stays_clean.csv'))
dt <- dt[successful_stay == 1]

#######################################################################################################################################
##################################################### Figure 4: commission fees #######################################################
#######################################################################################################################################

# uses individual level data 
dt[, start_ym := as.yearmon(stay_start)]
dt <- unique(dt[start_ym < as.yearmon("2018-01-01")])
dt[,`:=`(feepct_pvt = mean(rover_keeps/buyer_pays)),.(imported_stay, start_ym)]

dt_plot <- dt
dt_plot[start_ym>as.yearmon(as.Date("2017-03-29")) & imported_stay == 1, `:=` (buyerp_per_night_pvt=NA, sellerp_per_night_pvt=NA, feepct_pvt=NA)]


agg_rover_feepct <- ggplot(dt_plot[start_ym > as.yearmon("2012-02-01")], aes(start_ym, feepct_pvt,group = platform, color=platform)) + geom_line() + 
  geom_dl(aes(label = platform), method = list("last.bumpup", color = 'black', cex = 1.6, dl.trans(x = x + 0.2))) + guides(colour = FALSE) +
  xlab("Month") + ylab("Average Commission Fee (%)") + scale_x_continuous(limits=c(min(dt$start_ym),max(dt$start_ym)+1.5)) + scale_y_continuous(limits=c(0,NA)) +
  theme_classic() + theme(axis.text = element_text(size = 16, color = "black"), axis.title = element_text(size = 20),legend.position = c(0.2,0.8)) +
  geom_vline(xintercept=as.yearmon(as.Date("2017-03-29"))) + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())

ggsave(agg_rover_feepct, file = "figure4_agg_rover_feepct.png", path = figure_dir, width = 10, height =  7)


rm(list = setdiff(ls(), "agg_rover_feepct"))
agg_rover_feepct
