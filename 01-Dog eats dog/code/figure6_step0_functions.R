# 所有的自定义函数

library(data.table)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(statar)
library(zoo)
library(stargazer)
library(haven)
library(ggrepel)
library(grid)
library(gridExtra)
library(directlabels)
library(patchwork)
library(cowplot)
library(forcats)
library(broom)
library(Hmisc)
library(stringr)
library(cowplot)
library(plyr)
library(alpaca)
library(ggplotify)
library(devtools)
library(foreach)
library(PanelMatch)
library(purrr)
library(varhandle)
library(ggpubr)
library(xtable)
library(formattable)
library(parallel)
library(doParallel)
library(dyadRobust) 
theme_set(theme_pubr())

### 标准函数，Various Standard Functions

# 默认主题
paper_theme <- theme_classic() + theme(axis.text = element_text(size = 16, color = "black"), axis.title = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20))
# 生成颜色
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
# 提取字符串右侧字符
substrRight <- function(x, n) substr(x, nchar(x)-n+1, nchar(x))
# 字符串按宽度自动换行
wrap_text <- function(string,w) {
  string_wrapped <- strwrap(string, width = w, simplify = FALSE)
  string_new <- sapply(string_wrapped, paste, collapse = "\n")
  return(string_new)
}
# 提取小数部分，Reverse truncation of numbers
revtrunc <- function(x) x-trunc(x)
# 有缺失值处理的均值
meanNA <- function(x){mean(x, na.rm = TRUE)}
# 创建文件夹的函数
createfolders <- function(filedir) {
  if (!file.exists(filedir)){
    dir.create(filedir, recursive = T)
    print(paste0('File created: ', filedir))
  }
}
# 聚类稳健标准误函数，Cluster robust variance estimation function
robust.se.nodfc <- function(model, cluster){
  require(sandwich)
  require(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- 1
  #   dfc <- (M/(M-1)) * ((N-1)/(N-K))
  uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  rcse.se <- coeftest(model, rcse.cov)
  return(list(rcse.cov, rcse.se))
}


### 核心函数
# 数据清洗与预处理
getplatformdt <- function(platform, stays_convos_needs_agg, stays_convos_needs_agg_platform) {
  # platform：平台
  # stays_convos_needs_agg: 「市场 - 月度」级聚合数据
  # stays_convos_needs_agg_platform: 「平台 - 市场 - 月度」级聚合数据
  
  ### 处理平台级数据
  # 1. 市场变量转为字符型（避免因子型导致的分组错误）
  stays_convos_needs_agg_platform[, market := as.character(market)]
  
  # 2. 对数变换（针对计数类变量，缓解异方差，适合回归分析）
  ## 注：+1是为了避免变量值为0时log(0)报错
  stays_convos_needs_agg_platform[, log_buyers := log(unique_buyers + 1)]    # 买家数（对数）
  stays_convos_needs_agg_platform[, log_sitters := log(unique_sitters + 1)]  # 服务提供者数（对数）
  stays_convos_needs_agg_platform[, log_needs := log(unique_needs + 1)]      # 需求数（对数）
  stays_convos_needs_agg_platform[, log_convos := log(num_convos + 1)]       # 对话数（对数）
  stays_convos_needs_agg_platform[, log_stays_all := log(num_stays_all + 1)] # 总入住/订单数（对数）
  stays_convos_needs_agg_platform[, log_stays := log(num_stays + 1)]         # 目标入住/订单数（对数）
  
  # 3. 变量重命名
  stays_convos_needs_agg_platform[, price := buyerp_per_night] # 每晚买家支付价格
  
  # 4. 比率类指标（反映“效率”或“密度”）
  stays_convos_needs_agg_platform[, matchesperbuyer := num_stays / unique_buyers]  # 人均订单数（订单/买家）
  stays_convos_needs_agg_platform[, needsperbuyer := unique_needs / unique_buyers] # 人均需求数（需求/买家）
  stays_convos_needs_agg_platform[, convosperbuyer := num_convos / unique_buyers]  # 人均对话数（对话/买家）
  
  # 5. 财务类指标（总交易额、佣金）
  ## GTV
  stays_convos_needs_agg_platform[,gtv := buyerp * num_stays] # GTV（总交易额）= 单价×订单数
  stays_convos_needs_agg_platform[is.na(gtv),gtv := 0]        # 缺失值填充为0（无订单时GTV=0）
  stays_convos_needs_agg_platform[, log_gtv := log(gtv + 1)]  # GTV对数变换
  
  ## 服务提供者总收入
  stays_convos_needs_agg_platform[, seller_receives := sellerp * num_stays]    # 服务提供者总收入=卖家单价×订单数
  stays_convos_needs_agg_platform[is.na(seller_receives),seller_receives := 0] # 缺失值填充为0
  
  ## 平台佣金收入
  stays_convos_needs_agg_platform[, commission := gtv - seller_receives]   # 平台佣金=GTV-卖家收入
  stays_convos_needs_agg_platform[, log_commission := log(commission + 1)] # 佣金对数变换
  
  ### 处理市场级数据
  # 1. 市场变量转为字符型，去重（确保「市场-月度」唯一）
  stays_convos_needs_agg[, market := as.character(market)]
  stays_convos_needs_agg <- unique(stays_convos_needs_agg)
  
  # 2. 市场整体订单数对数变换（用于后续与平台订单数对比）
  stays_convos_needs_agg[, log_stays_both := log(num_stays + 1)]
  
  # 3. 时间变量标准化（排序+统一格式，为面板数据做准备）
  setorder(stays_convos_needs_agg, market, year_month) # 按年份-月份排序
  stays_convos_needs_agg[, year_month_clean := as.Date(as.yearmon(year_month), frac = 0)] # 将year_month（如"2015-01"）转为日期格式（每月1号），方便时间计算
  stays_convos_needs_agg[, year := year(year_month_clean)]                                # 提取年份（2015/2016/2017）
  
  # 4. 分组标识变量（用于固定效应回归或分组分析）
  stays_convos_needs_agg[, grid_month := .GRP, by = list(market, month(year_month_clean))]      # 市场×月份 分组ID
  stays_convos_needs_agg[, cbsa_month := .GRP, by = list(cbsa_seller, month(year_month_clean))] # 卖家邮编区域×月份 分组ID
  
  # 5. 并购相关变量
  stays_convos_needs_agg[, merger_count := interval(ymd(merger_discussion_ym), ymd(year_month_clean))/months(1)]               # 当前月份与合并月份的间隔
  stays_convos_needs_agg[, merger_count_factor := factor(merger_count*as.numeric(merger_count >= 0) - 10*(merger_count<= -1))] # 并购间隔因子化（并购后为原值，并购前统一为-10）
  
  # 6. 市场规模变量（按平台分组计算2016年总订单数，用于筛选活跃市场）
  stays_convos_needs_agg[, tot_stays_2016 := sum(num_stays*as.numeric(year == 2016)), .(market)]
  
  # 7. 按Rover市场份额分组，并排序
  stays_convos_needs_agg[, rover_share_cut := cut2(mkt_share_gtv, bin_groups)] 
  stays_convos_needs_agg[, rover_share_cut := fct_reorder(rover_share_cut, mkt_share_gtv)] # 按市场份额排序
  
  # 8. 数据筛选（核心：限定分析时间范围+活跃市场）
  # 时间范围：2015-01 至 2017-12（排除2014年及2018年数据）
  # 活跃市场：2016年订单数 ≥ min_stays
  stays_convos_needs_agg = stays_convos_needs_agg[year_month > as.yearmon('2014-12-01') & year_month < as.yearmon("2018-01-01") & num_stays_2016 >= min_stays]
  
  
  # convert region to dummy
  # 9. 分类变量转哑变量：region 和 division
  region <- to.dummy(stays_convos_needs_agg$census_region_name, "region")
  division <- to.dummy(stays_convos_needs_agg$census_division_name, "division")
  stays_convos_needs_agg = cbind(stays_convos_needs_agg, region)
  stays_convos_needs_agg = cbind(stays_convos_needs_agg, division)
  
  ### 提取数据
  # 1. 从平台级数据中筛选：指定平台+目标时间范围，并选择核心字段
  platform_agg = stays_convos_needs_agg_platform[
    year_month > as.yearmon('2014-12-01') & year_month < as.yearmon("2018-01-01") & 
      user_platform == platform,
    list(market, year_month, cbsa_seller, log_buyers, log_sitters, log_needs, log_convos, log_stays,
         success_rate, needsperbuyer, convosperbuyer, matchesperbuyer, star_5, return, repeated_stay,
         median_lead_time, median_lead_time_stay, num_convos, num_stays, unique_needs, buyerp_per_night,
         current_repeat_share)]
  
  return(platform_agg)
}
# 绘制系数图：适用于dyadRobust回归结果
createCoefficientPlotDyad <- function(res, this_outcome) {
  # res：回归结果
  # this_outcome: 结果变量名称（如 “订单量对数”“GTV 增长率”）
  
  # 1. 数据预处理
  # 从回归结果中提取系数、标准误，并重命名变量（去掉变量名中的"time"前缀）
  dt <- data.table(Estimate = res$bhat, se = res$sehat, var = gsub('time', '', names(res$bhat)))
  dt <- dt[!grepl('Intercept', var)] # 移除截距项（Intercept）
  dt <- rbind(dt, data.table(Estimate = 0, se = 0, var= 't-1')) # 添加基准期（t-1）,系数和标准误设为0
  
  # 2. 计算95%置信区间
  # 公式：系数 ± 1.96×标准误（1.96是正态分布95%分位数）
  dt[, ciupper := Estimate + 1.96 * se] # 置信区间上限
  dt[, cilower := Estimate - 1.96 * se] # 置信区间下限
  
  # 3. 按时间排序
  dt[, time_numeric := as.numeric(substr(var, 2, nchar(var)))]   # 从时间变量名中提取数值（用于排序）
  dt[, var := fct_reorder(var, time_numeric)]                    # 按数值大小重排序因子变量var：时间从前往后
  
  # 4. 返回系数图
  this_plot <- ggplot(dt, aes(x = var, y = Estimate)) +            
    geom_point(size = 1) +                                         # 绘制系数点（大小为1）
    geom_linerange(aes(x = var, ymin = cilower, ymax = ciupper)) + # 绘制置信区间（垂直误差线）
    paper_theme +                                                  # 应用默认主题
    geom_hline(yintercept = 0, linetype = 'dotted') +              # 水平参考线（Y=0，判断显著性）
    ylab(this_outcome) +                                           # Y轴标签：结果变量名称（输入参数this_outcome）
    xlab("") +                                                     # X轴无标签：时间点已通过var展示
    geom_vline(aes(xintercept = as.factor('t+0')), alpha = 0.2) +  # 垂直虚线：事件发生点（合并开始）
    geom_vline(aes(xintercept = as.factor('t+4')), alpha = 0.2) +  # 垂直虚线：关键时间点（合并完成）
    theme_classic(base_size = 16)                                  # 经典主题，字体大小16
  return(this_plot)
}
# 绘制系数图：适用于felm回归结果
createCoefficientPlot <- function(r, this_outcome) {
  # r: felm回归结果
  # this_outcome: 结果变量名称
  
  # 数据预处理
  dt <- data.table(coef(summary(r)))   # 从 felm 回归结果中提取系数、标准误等信息，转为 data.table
  dt[, var :=  names(coefficients(r))] # 添加变量名列（对应每个系数的时间点，("time t-2",“time t+0”)
  dt[, var := gsub('time', '', var)]   # 清理变量名：去掉前缀"time " ("time t-2"→"t-2","time t+0"→"t+0")
  dt <- dt[var != '(Intercept)']       # 移除截距项
  setnames(dt, 'Cluster s.e.', 'se')   # 重命名标准误列为"se"
  # 设置基准期为t-1
  dt <- rbind(dt, data.table(Estimate = 0, se = 0, var = 't-1'), fill = T)
  # 计算95%置信区间
  dt[, ciupper := Estimate + 1.96 * se]
  dt[ , cilower := Estimate - 1.96 * se]
  # 按时间排序
  dt[, time_numeric := as.numeric(substr(var, 2, nchar(var)))]
  dt[, var := fct_reorder(var, time_numeric)]
  # 返回系数图
  this_plot <- ggplot(dt, aes(x = var, y = Estimate)) +
    geom_point(size = 1) +                                         # 绘制系数散点
    geom_linerange(aes(x = var, ymin = cilower, ymax = ciupper)) + # 垂直误差线：95%置信区间
    paper_theme +                                                  # 应用自定义主题
    geom_hline(yintercept = 0, linetype = 'dotted') +              # 水平参考线（Y=0）
    ylab(this_outcome) +                                           # Y轴标签：结果变量名称
    xlab("") +                                                     # X轴无标签
    geom_vline(aes(xintercept = 't+0'), alpha = 0.2) +             # 垂直虚线：t+0, 合并开始
    geom_vline(aes(xintercept = 't+4'), alpha = 0.2) +             # 垂直虚线：t+4, 合并结束
    theme_classic(base_size = 16)
  return(this_plot)
}
# 绘制系数图：特征变量分组描述图
make_demographics_plot <- function(var, axis_name, x_axis, dt_plot = comparedems) {
  # var: 要展示的特征变量名
  # axis_name: Y 轴标签
  # x_axis: 控制 X 轴显示
  # dt_plot: 绘图数据集（默认是 comparedems）
  
  gp <- ggplot(dt_plot[variable == var], aes(x = rover_share_cut)) +
    geom_point(aes(y = avg)) +                                                     # 散点：每个Rover份额分组的特征均值
    geom_errorbar(aes(ymin = avg - sd * 1.96, ymax = avg + sd * 1.96), width=.9) + # 误差线：95%置信区间
    paper_theme +                                                                  # 默认主题
    xlab("Rover Share") +                                                          # X轴固定为“Rover份额”（分组依据）
    ylab(axis_name) +                                                              # Y轴为特征变量名称
    geom_hline(yintercept = 0, alpha = 0.4)                                        # 水平参考线
  
  if (x_axis == 0) {
    gp <- gp + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  return(gp)
}
# 绘制系数图：分组对比Rover与DogVacay的结果变量
make_dv_rov_plot <- function(mean_var, axis_name, x_axis, dt_plot = comparedems) {
  # mean_var: 结果变量名称, 无后缀
  # axis_name: 图表 Y 轴标签
  # x_axis: 控制 X 轴显示
  # dt_plot: 绘图数据集（默认 comparedems）
  
  # 提取并合并两个平台的目标数据
  dt <- rbind(dt_plot[ variable == paste0(mean_var, 'dv'), list(rover_share_cut, platform, avg, sd)],
              dt_plot[variable == paste0(mean_var, 'rov'), list(rover_share_cut, platform, avg, sd)])
  
  # 绘制并返回对比图
  gp <- ggplot(dt, aes(x = rover_share_cut)) +
    geom_point(aes(y = avg, color = platform)) + # 散点图：按平台分颜色（color=platform），每个点是“份额分组+平台”的均值
    geom_errorbar(aes(ymin = avg - sd * 1.96, ymax = avg + sd * 1.96, color=platform), width=.9) + # 误差线：按平台分颜色，95%置信区间
    paper_theme + xlab("Rover Share") + ylab(axis_name)  + 
    labs(color = "Platform") +
    theme(legend.position = "none")  +
    geom_hline(yintercept = 0, alpha = 0.4)
  if (x_axis == 0) {
    gp <- gp + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  
  return(gp)
}
# 匹配数据：基于匹配结果（matching_list）构建配对数据集，并输出结果变量（y_zt - y_z't）
createPairedDataSet <- function(matching_list, data, outcomes) {
  # matching_list: 匹配结果列表
  # data: 原始面板数据（市场×时间维度）
  # outcomes: 待分析的结果变量名称
  
  # 分组遍历
  data_list <- list()
  for (j in 1:(length(unique_groups) - 1)) {
    bin = unique_groups[j]
    results = matching_list[[j]]
    
    # 1. 提取处理组市场ID，去除后缀
    markets = gsub("\\..*","",names(results$att))
    
    # 2. 生成matchedsets：构建处理组-对照组配对关系（含匹配权重）
    matchedsets = data.table()
    for (i in 1:length(markets)) {
      # 提取当前处理组市场的对照组权重（weights>0表示有效匹配的对照组）
      weights <- attr(results$att[[i]], 'weights') 
      # 构建配对数据：处理组市场ID、对照组市场ID、匹配权重
      set = data.table(market_grp = as.numeric(markets[i]),            # market_grp：处理组市场ID
                       control = as.numeric(names(which(weights> 0))), # control：权重>0的对照组市场ID
                       weight = weights[which(weights > 0)])           # weight：对照组权重
      matchedsets = rbind(matchedsets, set)                            # 合并所有处理组的配对关系
    }
    # 确保1个处理组只有1个对照组：有多个对照组时随机选取一个
    matchedsets[, eps := rnorm(nrow(matchedsets))]          # 生成随机数（用于多个对照组时随机抽样）
    matchedsets[, n := .N, .(market_grp)]                   # 统计每个处理组对应的对照组数量（n）
    matchedsets <- matchedsets[order(market_grp, eps)]      # 按处理组+随机数排序（保证抽样随机性）
    matchedsets[, ind := 1:.N, .(market_grp)]               # 按处理组分组，为每个对照组生成序号ind
    matchedsets <- matchedsets[ind == 1]                    # 每个处理组仅保留1个对照组（取排序后第1个，ind==1）
    matchedsets <- matchedsets[, list(market_grp, control)] # 简化数据：仅保留处理组ID和对照组ID
    
    # 3. 生成sel_data：筛选原始数据并标准化为处理组列名
    # 定义需要保留的核心列（处理组数据用）
    sel_cols <- c('market_grp', 'time_id', 'is_treatment', 'cbsa_seller', 'rover_share_cut', outcomes)
    # 处理组列名，并添加“_treat”后缀
    treated_colnames <- c('market_grp', 'time_id', 'is_treatment', 'cbsa_seller_treat', 'rover_share_cut_treat', paste0(outcomes, '_treat'))
    # 对照组列名，并添加“_control”后缀
    control_colnames <-  c('market_grp', 'time_id', 'is_treatment','cbsa_seller_control', 'rover_share_cut_control', paste0(outcomes, '_control'))
    # 筛选原始数据的核心列，并将结果变量列名重命名为带有“_treat”后缀的列名
    sel_data <- data[, .SD, .SDcols=sel_cols]
    setnames(sel_data, treated_colnames)
    
    # 4. 合并处理组与对照组数据，构建配对面板
    # 按处理组ID匹配，合并控制组数据
    dt_paired <- merge(matchedsets, sel_data, by = 'market_grp')  
    # 按“对照组ID+时间ID”匹配，合并对照组数据
    setnames(sel_data, control_colnames)                          # 重命名'sel_data'列名为对照组列名
    setnames(sel_data, 'market_grp', 'control_market_grp')        # 重命名‘market_grp'列为‘control_market_grp’
    dt_paired <- merge(dt_paired, sel_data, by.x = c('control', 'time_id'), by.y = c('control_market_grp', 'time_id')) 
    
    # 5. 计算结果变量差异（核心因果分析变量）
    for (this_outcome in outcomes) {
      y_treat <- paste0(this_outcome, '_treat')
      y_control <- paste0(this_outcome, '_control')
      y_diff <- paste0(this_outcome, '_diff')
      dt_paired[, (y_diff) := get(y_treat) - get(y_control)] # 结果变量差异 = 处理组结果变量 - 控制组结果变量
    }
    
    # 6. 清理变量 + 生成相对时间
    # 删除重复的’is_treatment‘列
    dt_paired[, is_treatment := is_treatment.x]
    dt_paired[, is_treatment.x := NULL]
    dt_paired[, is_treatment.y := NULL]
    # 设置基准时间：时间最早的处理组，其他组的相对时间转为因子（t-2,t-1,t+0,t+1,...）
    dt_paired[, time_id_relative := time_id - min(data[is_treatment == 1, time_id], na.rm = T)]    
    dt_paired[, time := factor(ifelse(time_id_relative >= 0,paste0('t+', time_id_relative),  paste0('t', time_id_relative)))]
    
    # 7. 存储处理完的数据
    data_list[[j]] <- dt_paired
  }
  
  # 返回所有组处理好的结果
  return(data_list)
}
# paired_data_list, 
# 基于匹配后的数据，进行回归分析，并输出回归结果和事件研究图列表
runRegression_outcome <- function(this_outcome, leads = c(-6:9), se_adjust = 1) {
  # this_outcome：结果变量名称
  # leads：事件研究的时间范围
  # se_adjust：用 dyadRobust() 计算配对稳健标准误
  
  # 初始化存储列表
  this_plot_list <- list()
  this_estimates_list <- list()
  
  # 遍历每个配对数据集的分组
  for (i in 1:length(paired_data_list)) {
    
    if (grepl('search', this_outcome)) {
      leads <- c(-2:9)
    }
    
    # 数据预处理
    this_data <- paired_data_list[[i]]                   # 提取当前分组的配对数据
    this_data <- this_data[time_id_relative %in% leads]  # 筛选时间范围（仅保留 leads 中的相对时间）
    this_data$time = relevel(this_data$time, "t-1")      # 设置基准期为 t-1
    this_data[, y := get(paste0(this_outcome, '_diff'))] # 设置结果变量为‘处理组-对照组’的差值
    
    # 回归分析，设置为配对稳健标准误(dyadRobust) | 模型1，3
    if (se_adjust == 1) {
      # OLS, y ~ time(时间哑变量因子)
      r <- lm(y ~ time, data = this_data)
      # 计算配对稳健标准误
      this_data[, dyad := as.factor(paste(market_grp, control, sep = '-'))] # 设置配对ID (dyad)
      out <- dyadRobust(fit = r,                        # OLS回归结果       # 计算配对稳健标准误(dyadRobust)
                        dat = as.data.frame(this_data), # 转换为data.frame
                        dyadid = "dyad",                # 配对ID列
                        egoid = 'market_grp',           # 处理组ID
                        alterid = "control")            # 控制组ID
      # 生成事件研究系数图
      this_plot <- createCoefficientPlotDyad(out, this_outcome)
      # 存储回归估计结果（含配对稳健标准误）
      this_estimates_list[[i]] <- out
    }
    # 回归分析，设置为聚类标准误（felm 固定效应回归） | 模型2
    else {
      # felm 回归：y ~ time（时间因子）| 0（无个体固定效应）| 0（无工具变量）| 聚类变量为处理组卖家所在的cbsa区域
      r <- felm(y ~ time | 0 | 0 | cbsa_seller_treat, data = this_data)
      # 生成事件研究系数图
      this_plot <- createCoefficientPlot(r, this_outcome)
      # 存储回归估计结果（含聚类标准误）
      this_estimates_list[[i]] <- r
    }
    # 存储当前分组的回归系数图
    this_plot_list[[i]] <- this_plot
  }
  # 返回最终结果：图列表 + 回归结果列表 + 结果变量名
  return(list(this_plot_list, this_estimates_list, this_outcome)) 
}
# outcomes
# 回归结果格式化
reformat_res <- function(res) {
  # res: runRegression_outcome()返回结果列表
  # res[[i]][[1]]: i结果变量的分组图列表
  # res[[i]][[2]]: i结果变量的分组回归结果列表
  
  # 初始化定义：整理后的结果
  plot_list_matched <- list()
  estimates_list_matched <- list()
  
  # 遍历结果列表，重新格式化，使结果变量名为“Key”，返回整理后的结果
  for (i in 1:length(res)) {
    outcome <- res[[i]][[3]]
    plot_list_matched[[outcomes[i]]] <- res[[i]][[1]]
    estimates_list_matched[[outcomes[i]]] <- res[[i]][[2]]
  }
  return(list(plot_list_matched, estimates_list_matched))
}
# 输出格式化的回归结果统计表格
output_stats <- function(y_name,dt,dig) {
  # y_name: 结果变量名称
  # dt: 回归数据集
  # dig: 系数的小数保留位数
  
  # 预处理
  bins <- unique(dt$share_fe) # 提取唯一分组
  dt_temp_all = data.table()  # 初始化回归结果列表
  # 遍历每个分组
  for (bin in bins) {
    # 回归：y_name ~ treated, data
    reg <- felm(get(y_name) ~ treated, dt[share_fe == bin])
    # 构建临时结果表：是否处理组、回归系数、p值、分组标识
    treated = c(0, 1)
    dt_temp <- as.data.table(cbind(treated,reg$coeff,reg$coeff,reg$pval, bin))
    setnames(dt_temp,c("treated",y_name,"coeff","pval", 'share_fe')) # 重命名列名
    # 结果格式化 
    dt_temp[,pval := as.numeric(pval)]                     # 将p值转为数值型（cbind后默认是字符型，需转换才能做显著性判断）
    dt_temp[,coeff:=comma(coeff,digits = dig)]             # 格式化系数：用 comma() 保留 dig 位小数（如 dig=3 则 2.4234→"2.423"）
    dt_temp[pval>.1,eval(y_name) := paste0(coeff)]         # 按p值添加显著性标记（学术规范：*p<0.1, **p<0.05, ***p<0.01）
    dt_temp[pval<=.1,eval(y_name) := paste0(coeff,"*")]
    dt_temp[pval<=.05,eval(y_name) := paste0(coeff,"**")]
    dt_temp[pval<=.01,eval(y_name) := paste0(coeff,"***")]
    dt_temp[treated == 0, eval(y_name) := paste0(coeff)]   # 对照组不添加显著性标记（仅展示原始系数）
    dt_temp[,`:=`(coeff = NULL, pval = NULL)]              # 删除中间列
    dt_temp_all = rbind(dt_temp_all, dt_temp)              # 将当前分组的结果合并到总表中
  }
  # 返回格式化的结果
  return(dt_temp_all)
}
# 画图函数：多绘图结果可视化。Create plot
create_plot <- function(plot_list, vars_to_keep, all_names, xscale = "", title_length = 0) {
  # plot_list   : 格式化的绘图列表
  # var_to_keep : 需要展示的结果变量
  # all_names   : 图表标注标签
  # xscale      : x 轴（时间）的格式类型
  # title_length: 结果变量标签的换行长度
  
  plot_t <- transpose(plot_list[vars_to_keep]) # 转置图列表
  y_labs <- c(list(textGrob("")))              # 清除y轴标签
  for(j in 1:length(vars_to_keep)){            # 对每个结果变量，都生成一组图
    # 统一y轴范围
    outcome_y_min <- min(unlist(lapply(plot_t, function(x) layer_scales(x[[j]])$y$range$range))) 
    outcome_y_max <- max(unlist(lapply(plot_t, function(x) layer_scales(x[[j]])$y$range$range)))
    # 统一x轴标签
    for(i in 1:(length(bins)-1)){              
      if(xscale == "") {
        plot_t[[i]][[j]] <- plot_t[[i]][[j]] + ylim(outcome_y_min, outcome_y_max)  + scale_x_discrete(breaks = c("t-6", "t+0","t+4","t+9"), labels = c("Sept16", "Mar17", "Jul17", "Dec17")) 
      } else {
        if (xscale == "yearmon") {
          plot_t[[i]][[j]] <- plot_t[[i]][[j]] + ylim(outcome_y_min, outcome_y_max)  + scale_x_yearmon(breaks = as.yearmon(c("2016-09-01", "2017-03-01", "2017-07-01", "2017-12-01")), labels = c("Sep16", "Mar17", "Jul17", "Dec17"), limits = as.yearmon(c("2016-09-01", "2017-12-01")))
        }
        if (xscale == "numeric") {
          plot_t[[i]][[j]] <- plot_t[[i]][[j]] + ylim(outcome_y_min, outcome_y_max)  + scale_x_continuous(breaks = c(1,8,12,16), labels = c("Sep16", "Mar17", "Jul17", "Dec17"), limits = c(1,16.2))
        }
      }
    }
    # y轴竖排结果变量标签
    if (title_length == 0) {
      wrapped_label <- wrap_text(all_names[vars_to_keep[j]],15)  
    } else {
      wrapped_label <- wrap_text(all_names[vars_to_keep[j]],20)
    }
    y_labs <- c(y_labs,list(textGrob(wrapped_label, gp = gpar(fontsize = 20), rot = 90)))
  }
  # 组合 y 轴标签列
  plot_labels <- wrap_plots(y_labs, ncol = 1, heights = c(.1,rep(1,length(vars_to_keep))))
  # 组合各个子图为大图
  a <- wrap_plots(c(list(textGrob(bins[2], gp = gpar(fontsize = 20))), plot_t[[1]]), nrow = length(vars_to_keep)+1, ncol = 1, heights = c(.1, rep(1,length(vars_to_keep)+1))) 
  b <- wrap_plots(c(list(textGrob(bins[3], gp = gpar(fontsize = 20))), plot_t[[2]]), nrow = length(vars_to_keep)+1, ncol = 1, heights = c(.1, rep(1,length(vars_to_keep)+1))) 
  c <- wrap_plots(c(list(textGrob(bins[4], gp = gpar(fontsize = 20))), plot_t[[3]]), nrow = length(vars_to_keep)+1, ncol = 1, heights = c(.1, rep(1,length(vars_to_keep)+1))) 
  d <- wrap_plots(c(list(textGrob(bins[5], gp = gpar(fontsize = 20))), plot_t[[4]]), nrow = length(vars_to_keep)+1, ncol = 1, heights = c(.1, rep(1,length(vars_to_keep)+1))) 
  big_plot <- wrap_plots(plot_labels, a, b, c, d, ncol = length(bins), widths = c(.2,rep(1,length(bins)-1))) & theme(axis.title.y=element_blank(), axis.text = element_text(color="black"))
  return(big_plot)
}
