# 定义结果变量
rover_outcomes <- c("log_stays_rover", "success_rate_rover", "log_sitters_rover", "log_buyers_rover",  'log_gtv_rover', 'log_commission_rover', 'gtv_rover', 'commission_rover') 
rover_names <- c("Rover Stays (log)", "Rover Match Rate", "Sitters (log)", "Buyers (log)",'Rover GTV (log)', 'Rover Commission (log)',  'Rover GTV', 'Rover Commission')

market_outcomes <- c("log_stays",  "success_rate", "success_rate_newb", 'log_num_stays_newb', 'log_gtv', 'log_commission') 
market_names <- c("Transactions (log)", "Request Match Rate", "Request Match Rate", "Transactions (log)", "GTV (log)", "Commission (log)")

outcomes <- c(rover_outcomes, market_outcomes)
all_names <- c(rover_names, market_names)
outcome_names <- all_names

# 参数配置
seadjust = 0 # 标准误调整标识
tables_dir <- paste0(results_dir, 'tables/')                        # 表格路径设置
plot_dir <- paste0(results_dir, 'figures/did_zip_seller/matching/') # 图片路径设置
figures_dir_matching <- paste0(results_dir, 'figures/did_zip_seller/matching/')
figures_dir_plot <- paste0(results_dir, 'figures/did_zip_seller/')

merger_discussion_ym <- '2016-12-01' # 合并讨论开始时间：2016年12月1日
merger_announce_ym <- '2017-03-01'   # 合并宣布时间：2017年3月1日
merger_transfer_ym <- '2017-04-01'   # 合并开始时间：2017年4月1日
dv_off_ym <- '2017-06-01'            # 合并结束时间：2017年6月1日

size_mult <- 1.5           # 匹配时的约束条件：确保处理组和对照组的倾向得分差异不超过 1.5 倍标准差
matching_formula <- as.formula(~ I(lag(unique_sitters, 1:12))) # 倾向得分匹配的协变量公式：基于过去12期的商家数量进行匹配
refinement = 'CBPS.match'  # 匹配方法：CBPS
leads_matching <- c(0:9)   # 匹配后的事件研究时间范围：处理后0~9期（并购完成后9个观测期）

if (exists("cl")) stopCluster(cl) # 并行计算配置, 停止旧集群
cl <- makeCluster(num_cores)      # 创建并行计算集群（启动2个核心）
registerDoParallel(cl)            # 注册并行计算环境（让后续函数支持并行）
clusterEvalQ(cl, library("lfe"))  # 在每个并行核心上加载必要的包（确保核心能调用所需函数）
clusterEvalQ(cl, library("forcats"))
clusterEvalQ(cl, library("ggplot2"))
clusterEvalQ(cl, library("data.table")) 
clusterEvalQ(cl, library("dyadRobust"))

min_stays <- 50                          # 市场最低订单量阈值：仅保留订单量≥50的市场，排除过小/不活跃市场
market_var = c('zip_seller')             # 市场标识变量：卖家邮编（zip_seller）
platforms <- c('DogVacay', 'Rover')      # 平台分类
bin_groups <- c(0, .2, 0.4, 0.6, 0.8, 1) # 样本按Rover市场份额分组设置

