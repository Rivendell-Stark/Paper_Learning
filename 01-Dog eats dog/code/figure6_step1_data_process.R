#############################################################################################################################################################
########################################################## 数据预处理：“订单-对话-需求”聚合数据 #############################################################
#############################################################################################################################################################

# stays_convos_needs_agg <- fread(paste0(data_dir, 'stays_convos_needs_agg_bygrid_zip_seller.csv'))

createfolders(plot_dir) # 只是创建一个文件夹，避免数据保存时报错

stays_convos_needs_agg[, market := as.character(market)] # 将market列转为字符型
stays_convos_needs_agg <- unique(stays_convos_needs_agg) # 去除重复值

# 时间处理：按时间排序并生成时间因子
setorder(stays_convos_needs_agg, market, year_month)                                                                         # 按“市场+时间”排序：保证时间序列的连续性，为后续滞后项、滚动统计做准备
stays_convos_needs_agg[, year_month_clean := as.Date(as.yearmon(year_month), frac = 0)]                                      # 将year_month（如“2016-12”）转为标准日期格式（每月1日）
stays_convos_needs_agg[, year := year(year_month_clean)]                                                                     # 提取年份列year: 用于2016年订单量汇总、年度固定效应控制
stays_convos_needs_agg[, merger_count := interval(ymd(merger_discussion_ym), ymd(year_month_clean))/months(1)]               # 计算“当前时间与并购讨论开始时间的月数差”：merger_count
stays_convos_needs_agg[, merger_count_factor := factor(merger_count*as.numeric(merger_count >= 0) - 10*(merger_count<= -1))] # 将间隔月数转换为因子变量：时间虚拟变量
# 分组处理
stays_convos_needs_agg[, grid_month := .GRP, by = list(market, month(year_month_clean))]       # 生成按“市场(邮编)+月份”分组的标识: grid_month
stays_convos_needs_agg[, cbsa_month := .GRP, by = list(cbsa_seller, month(year_month_clean))]  # 生成按“cbsa+月份”分组的标识: cbsa_month
stays_convos_needs_agg[, tot_stays_2016 := sum(num_stays*as.numeric(year == 2016)), .(market)] # 按市场(邮编)分组，汇总2016年的总交易量(num_stays): tot_stays_2016
# 地区因子生成：地区固定效应
region <- to.dummy(stays_convos_needs_agg$census_region_name, "region")       # 将普查区域名（census_region_name）转为虚拟变量，合并到原数据中
division <- to.dummy(stays_convos_needs_agg$census_division_name, "division") # 将普查分区名（census_division_name）转为虚拟变量，合并到原数据中
stays_convos_needs_agg = cbind(stays_convos_needs_agg, region)
stays_convos_needs_agg = cbind(stays_convos_needs_agg, division)


##### 筛选数据：限定时间范围为 ‘2014-12-01’ ~ ‘2018-01-01’
this_sample <- stays_convos_needs_agg[year_month > as.yearmon('2014-12-01') & year_month < as.yearmon("2018-01-01")]

# 计算分组断点，排序
this_sample[, rover_share_cut := cut2(mkt_share_gtv, bin_groups)]
this_sample[, rover_share_cut := fct_reorder(rover_share_cut, mkt_share_gtv)]
# 按时间排序数据集，分别按照年月和市场分组，生成唯一标识
setorder(this_sample, 'year_month')
this_sample[, time_id := .GRP, by = list(year_month_clean)]
this_sample[, market_grp := .GRP, by = list(market)]
# 对数化变量
this_sample[, log_stays_rover := log(num_stays_rover + 1)]
this_sample[, log_stays_both := log(num_stays + 1)]
this_sample[is.na(num_new_users), num_new_users := 0]
this_sample[, log_num_new_users := log(num_new_users + 1)]
this_sample[, num_oldb_stays := num_stays - num_newb_stays] 
this_sample[, log_stays_both_newb := log(num_newb_stays + 1)]
this_sample[, log_stays_both_oldb := log(num_oldb_stays + 1)]
this_sample[, log_stays := log_stays_both]
this_sample[is.na(num_new_users_convo_month), num_new_users_convo_month := 0]
this_sample[, log_num_new_users_convo_month := log(num_new_users_convo_month+1)]
this_sample[, price := buyerp_per_night]
this_sample[, success_rate_rover := num_stays_rover / unique_needs_rover]
this_sample[unique_needs_rover == 0, success_rate_rover := NA]
this_sample[num_searches_rover == 0, searchtoconvo_rate_rover := NA]
this_sample[num_searches_rover == 0, searchtostay_rate_rover := NA]
this_sample[, log_sitters_stay_rover := log(unique_sitters_rover_stay + 1)]
this_sample[, log_buyers_stay_rover := log(unique_buyers_rover_stay + 1)]
this_sample[, log_sitters_rover := log(unique_sitters_rover + 1)]
this_sample[, log_buyers_rover := log(unique_buyers_rover + 1)]
this_sample[, log_buyers := log(unique_buyers + 1)]
this_sample[, log_sitters := log(unique_sitters + 1)]
this_sample[, log_sitters_stay := log(unique_sitters_stay + 1)]
this_sample[, log_buyers_stay := log(unique_buyers_stay + 1)]
this_sample[, convosperneed := num_convos / unique_needs]
this_sample[, log_num_stays_newb := log(num_stays_newb + 1)]
this_sample[, log_unique_buyers_newb := log(unique_buyers_newb + 1)]
this_sample[, log_unique_buyers_stay_newb := log(unique_buyers_stay_newb + 1)]
this_sample[, log_unique_sitters_newb := log(unique_sitters_newb + 1)]
this_sample[, log_unique_sitters_stay_newb := log(unique_sitters_stay_newb + 1)]
this_sample[, convosperneed_rover := num_convos_rover / unique_needs_rover]
this_sample[, convosperneed_newb := num_newb_convos/unique_newb_needs]
this_sample[is.na(log_num_stays_newb), log_num_stays_newb := 0]
this_sample[is.na(log_unique_buyers_newb), log_unique_buyers_newb := 0]
this_sample[is.na(log_unique_buyers_stay_newb), log_unique_buyers_stay_newb := 0]
this_sample[is.na(log_unique_sitters_newb), log_unique_sitters_newb := 0]
this_sample[is.na(log_unique_sitters_stay_newb), log_unique_sitters_stay_newb := 0]
this_sample[, b2sratio := (unique_buyers + 1) / (unique_sitters + 1)]
this_sample[, b2sratio_rover := (unique_buyers_rover + 1) / (unique_sitters_rover + 1)]
# 指标计算
# 总交易价值（GTV）= 买家支付单价 * 住宿数（缺失值设为0，再取对数）
this_sample[,gtv := buyerp * num_stays]
this_sample[is.na(gtv), gtv := 0]
this_sample[, log_gtv := log(gtv + 1)]
# 平台佣金 = GTV - 卖家收入（再取对数）
this_sample[, seller_receives := sellerp * num_stays]
this_sample[is.na(seller_receives),seller_receives := 0]
this_sample[, commission := gtv - seller_receives]
this_sample[, log_commission := log(commission + 1)]
# Rover平台的GTV、佣金及对数形式 
this_sample[,gtv_rover := buyerp_rover * num_stays_rover]
this_sample[is.na(gtv_rover), gtv_rover := 0]
this_sample[, log_gtv_rover := log(gtv_rover + 1)]
this_sample[, seller_receives_rover := sellerp_rover * num_stays_rover]
this_sample[is.na(seller_receives_rover),seller_receives_rover := 0]
this_sample[, commission_rover := gtv_rover - seller_receives_rover]
this_sample[, log_commission_rover := log(commission_rover + 1)]

# 按Rover份额分组
unique_groups <- unique(this_sample[, rover_share_cut])
unique_groups <- sort(unique_groups)
# 设置对照组
top_group <- unique_groups[length(unique_groups)]
# 设置处理组标识
this_sample[, is_treatment := as.numeric(rover_share_cut != top_group & year_month_clean >= (merger_announce_ym))]
# 按“市场-时间”去重，确保每个市场每个时间点仅一条记录
this_sample <- unique(this_sample, by = c("market_grp",'time_id'))
