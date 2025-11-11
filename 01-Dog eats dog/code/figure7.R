library(data.table) 
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(statar)
library(lfe)
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
#install_github("insongkim/PanelMatch", dependencies=TRUE)
library(PanelMatch)
library(purrr)
library(varhandle)
library(ggpubr)
library(xtable)
library(formattable)
library(parallel)
library(doParallel)
library(fixest)
#library(dyadRobust)
theme_set(theme_pubr())

#############################################################################################################################################################
####################################################################### Functions ###########################################################################
#############################################################################################################################################################

### Various Standard Functions
paper_theme <-  theme_classic() + theme(axis.text = element_text(size = 16, color = "black"),
                                        axis.title = element_text(size = 20),
                                        legend.text = element_text(size = 20),
                                        legend.title = element_text(size = 20))

### Reproduce automatic ggplot color palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
### Reverse substring
substrRight <- function(x, n) substr(x, nchar(x)-n+1, nchar(x))
wrap_text <- function(string,w) {
  string_wrapped <- strwrap(string, width = w, simplify = FALSE)
  string_new <- sapply(string_wrapped, paste, collapse = "\n")
  return(string_new)
}
# Cluster robust variance estimation function
robust.se.nodfc <- function(model, cluster){
  require(sandwich)
  require(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- 1
  uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  rcse.se <- coeftest(model, rcse.cov)
  return(list(rcse.cov, rcse.se))
}
createfolders <- function(filedir) {
  if (!file.exists(filedir)){
    dir.create(filedir)
    print(paste0('File created: ', filedir))
  }
}
meanNA <- function(x){mean(x, na.rm = TRUE)}
### Reverse truncation of numbers
revtrunc <- function(x) x-trunc(x)


### 核心函数
getplatformdt <- function(platform, stays_convos_needs_agg_platform) {
  # create necessary variables
  stays_convos_needs_agg_platform[, market := as.character(market)]
  stays_convos_needs_agg_platform[, log_buyers := log(unique_buyers + 1)]
  stays_convos_needs_agg_platform[, log_sitters := log(unique_sitters + 1)]
  stays_convos_needs_agg_platform[, log_needs := log(unique_needs + 1)]
  stays_convos_needs_agg_platform[, log_convos := log(num_convos + 1)]
  stays_convos_needs_agg_platform[, log_stays_all := log(num_stays_all + 1)]
  stays_convos_needs_agg_platform[, log_stays := log(num_stays + 1)]
  stays_convos_needs_agg_platform[, price := buyerp_per_night]
  stays_convos_needs_agg_platform[, matchesperbuyer := num_stays / unique_buyers]
  stays_convos_needs_agg_platform[, needsperbuyer := unique_needs / unique_buyers]
  stays_convos_needs_agg_platform[, convosperbuyer := num_convos / unique_buyers]
  stays_convos_needs_agg_platform[, totalusers := unique_buyers+ unique_sitters]
  
  platform_agg = stays_convos_needs_agg_platform[year_month > as.yearmon('2014-12-01') & year_month < as.yearmon("2018-01-01") & user_platform == platform,
                                                 list(market, year_month, cbsa_seller, totalusers, unique_sitters, unique_buyers, log_buyers, log_sitters, log_needs, log_convos, log_stays, success_rate, needsperbuyer, convosperbuyer, matchesperbuyer,
                                                      star_5, return, repeated_stay, median_lead_time, median_lead_time_stay, num_convos, num_stays, unique_needs, buyerp_per_night, current_repeat_share)]
  
  return(platform_agg)
  # log stays both is total stays in the market, while log stays is the num stays for that platform
}
createCoefficientPlotDyad <- function(res, this_outcome) {
  dt <- data.table(Estimate = res$bhat, se = res$sehat, var = gsub('time', '', names(res$bhat)))
  dt <- dt[!grepl('Intercept', var)]
  dt <- rbind(dt, data.table(Estimate = 0, se = 0, var= 't-1'))
  dt[, ciupper := Estimate + 1.96 * se]
  dt[ , cilower := Estimate - 1.96 * se]
  dt[, time_numeric := as.numeric(substr(var, 2, nchar(var)))]
  dt[, var := fct_reorder(var, time_numeric)]
  this_plot <- ggplot(dt, aes(x = var, y = Estimate)) + geom_point(size = 1) + 
    geom_linerange(aes(x = var, ymin = cilower, ymax = ciupper)) + paper_theme + 
    geom_hline(yintercept = 0, linetype = 'dotted') + ylab(this_outcome) + xlab("") + 
    geom_vline(aes(xintercept = as.factor('t+0')), alpha = 0.2) + 
    geom_vline(aes(xintercept = as.factor('t+4')), alpha = 0.2) + theme_classic(base_size = 16)
  return(this_plot)
}
createCoefficientPlotDyadDiff <- function(res, this_outcome) {
  dt <- data.table(Estimate = res$bhat, se = res$sehat, var = gsub('time', '', names(res$bhat)))
  dt <- dt[!grepl('Intercept', var)]
  dt <- rbind(dt, data.table(Estimate = 0, se = 0, var= 't-1'))
  dt[, ciupper := Estimate + 1.96 * se]
  dt[ , cilower := Estimate - 1.96 * se]
  dt[, time_numeric := as.numeric(substr(var, 2, nchar(var)))]
  dt[, var := fct_reorder(var, time_numeric)]
  this_plot <- ggplot(dt, aes(x = var, y = Estimate)) + geom_point(size = 1) + 
    geom_linerange(aes(x = var, ymin = cilower, ymax = ciupper)) + paper_theme + 
    geom_hline(yintercept = 0, linetype = 'dotted') + ylab(this_outcome) + xlab("") + 
    geom_vline(aes(xintercept = as.factor('t+0')), alpha = 0.2) + 
    geom_vline(aes(xintercept = as.factor('t+4')), alpha = 0.2) + theme_classic(base_size = 16)
  return(this_plot)
}
createPairedDataSet <- function(matchedsets, data, outcomes) {
  data_list <- list()
  for (j in 1:(length(unique_groups) - 1)) {
    bin = unique_groups[j]
    sel_cols <- c('market_grp', 'time_id', 'is_treatment', 'cbsa_seller', 'rover_share_cut', outcomes)
    treated_colnames <- c('market_grp', 'time_id', 'is_treatment', 'cbsa_seller_treat', 'rover_share_cut_treat', paste0(outcomes, '_treat'))
    control_colnames <-  c('market_grp', 'time_id', 'is_treatment','cbsa_seller_control', 'rover_share_cut_control', paste0(outcomes, '_control'))
    sel_data <- data[, .SD, .SDcols=sel_cols]
    setnames(sel_data, treated_colnames)
    
    dt_paired <- merge(matchedsets, sel_data, by = 'market_grp')
    setnames(sel_data, control_colnames)
    setnames(sel_data, 'market_grp', 'control_market_grp')
    dt_paired <- merge(dt_paired, sel_data, by.x = c('control', 'time_id'), by.y = c('control_market_grp', 'time_id'))
    for (this_outcome in outcomes) {
      y_treat <- paste0(this_outcome, '_treat')
      y_control <- paste0(this_outcome, '_control')
      y_diff <- paste0(this_outcome, '_diff')
      dt_paired[, (y_diff) := get(y_treat) - get(y_control)]
    }
    dt_paired[, is_treatment := is_treatment.x]
    dt_paired[, is_treatment.x := NULL]
    dt_paired[, is_treatment.y := NULL]
    
    dt_paired[, time_id_relative := time_id - min(dt_paired[is_treatment == 1, time_id], na.rm = T)]    
    dt_paired[, time := factor(ifelse(time_id_relative >= 0,paste0('t+', time_id_relative),  paste0('t', time_id_relative)))]
    
    data_list[[j]] <- dt_paired
  }
  return(data_list)
}
runRegression_outcome <- function(this_outcome, matcheddatalist, leads = c(-6:9), se.adjust =T) {
  this_plot_list <- list()
  this_estimates_list <- list()
  for (i in 1:length(matcheddatalist)) {
    if (grepl('search', this_outcome)) {
      leads <- c(-2:9)
    }
    this_data <- matcheddatalist[[i]]
    this_data <- this_data[time_id_relative %in% leads]
    this_data$time = relevel(this_data$time, "t-1")
    this_data[, y := get(paste0(this_outcome, '_diff'))]
    if (se.adjust == T) {
      r <- lm(y ~ time, data = this_data)
      this_data[, dyad := as.factor(paste(market_grp, control, sep = '-'))]
      out <- dyadRobust(fit = r, 
                        dat = as.data.frame(this_data), 
                        dyadid = "dyad", 
                        egoid = 'market_grp',
                        alterid = "control")
      this_plot <- createCoefficientPlotDyad(out, this_outcome)
      this_estimates_list[[i]] <- out
    }
    else {
      r <- felm(y ~ time | 0 | 0 | cbsa_seller_treat, data = this_data)
      this_plot <- createCoefficientPlot(r, this_outcome)
      this_estimates_list[[i]] <- r
    }
    this_plot_list[[i]] <- this_plot
  }
  return(list(this_plot_list, this_estimates_list, this_outcome)) 
}
runRegression_outcomeDiff <- function(this_outcome, matcheddatalist, leads = c(-6:9), se.adjust =T) {
  this_plot_list <- list()
  this_estimates_list <- list()
  for (i in 1:length(matcheddatalist)) {
    if (grepl('search', this_outcome)) {
      leads <- c(-2:9)
    }
    this_data <- matcheddatalist[[i]]
    this_data <- this_data[time_id_relative %in% leads]
    this_data$time = relevel(this_data$time, "t-1")
    this_data[, y := get(paste0(this_outcome, '_diff'))]
    if (se.adjust == T) {
      r <- lm(y ~ time*istestpair, data = this_data)
      this_data[, dyad := as.factor(paste(market_grp, control, sep = '-'))]
      out <- dyadRobust(fit = r, 
                        dat = as.data.frame(this_data), 
                        dyadid = "dyad", 
                        egoid = 'market_grp',
                        alterid = "control")
      print(summary(out))
      this_plot <- createCoefficientPlotDyadDiff(out, this_outcome)
      this_estimates_list[[i]] <- out
    }
    else {
      r <- felm(y ~ time*istestpair | 0 | 0 | cbsa_seller_treat, data = this_data)
      print(summary(r))
      this_plot <- createCoefficientPlotDiff(r, this_outcome)
      this_estimates_list[[i]] <- r
    }
    this_plot_list[[i]] <- this_plot
  }
  return(list(this_plot_list, this_estimates_list, this_outcome)) 
}
createCoefficientPlot <- function(r, this_outcome) {
  dt <- data.table(coef(summary(r)))
  dt[, var :=  names(coefficients(r))]
  dt[, var := gsub('time', '', var)]
  dt <- dt[var != '(Intercept)']
  setnames(dt, 'Cluster s.e.', 'se')
  dt <- rbind(dt, data.table(Estimate = 0, se = 0, var = 't-1'), fill = T)
  dt[, ciupper := Estimate + 1.96 * se]
  dt[ , cilower := Estimate - 1.96 * se]
  dt[, time_numeric := as.numeric(substr(var, 2, nchar(var)))]
  dt[, var := fct_reorder(var, time_numeric)]
  this_plot <- ggplot(dt, aes(x = var, y = Estimate)) + geom_point(size = 1) + 
    geom_linerange(aes(x = var, ymin = cilower, ymax = ciupper)) + paper_theme + 
    geom_hline(yintercept = 0, linetype = 'dotted') + ylab(this_outcome) + xlab("") + 
    geom_vline(aes(xintercept = 't+0'), alpha = 0.2) + 
    geom_vline(aes(xintercept = 't+4'), alpha = 0.2) + theme_classic(base_size = 16)
  return(this_plot)
}
createCoefficientPlotDiff <- function(r, this_outcome) {
  dt <- data.table(coef(summary(r)))
  dt[, var :=  names(coefficients(r))]
  dt <- dt[grepl(':', var)]
  dt[, var := gsub('time', '', var)]
  dt[, var := gsub(':istestpair', '', var)]
  dt <- dt[var != '(Intercept)']
  setnames(dt, 'Cluster s.e.', 'se')
  dt <- rbind(dt, data.table(Estimate = 0, se = 0, var = 't-1'), fill = T)
  dt[, ciupper := Estimate + 1.96 * se]
  dt[ , cilower := Estimate - 1.96 * se]
  dt[, time_numeric := as.numeric(substr(var, 2, nchar(var)))]
  dt[, var := fct_reorder(var, time_numeric)]
  this_plot <- ggplot(dt, aes(x = var, y = Estimate)) + geom_point(size = 1) + 
    geom_linerange(aes(x = var, ymin = cilower, ymax = ciupper)) + paper_theme + 
    geom_hline(yintercept = 0, linetype = 'dotted') + ylab(this_outcome) + xlab("") + 
    geom_vline(aes(xintercept = 't+0'), alpha = 0.2) + 
    geom_vline(aes(xintercept = 't+4'), alpha = 0.2) + theme_classic(base_size = 16)
  return(this_plot)
}
reformat_res <- function(res) {
  plot_list_matched <- list()
  estimates_list_matched <- list()
  for (i in 1:length(res)) {
    outcome <- res[[i]][[3]]
    plot_list_matched[[outcomes[i]]] <- res[[i]][[1]]
    estimates_list_matched[[outcomes[i]]] <- res[[i]][[2]]
  }
  return(list(plot_list_matched, estimates_list_matched))
}

# puts selected DV and Rover market share markets into the same data set 
create_sample <- function(controlplat, controlshare, treatshare) {
  this_sample_dv <- getplatformdt('DogVacay', stays_convos_needs_agg_platform)
  this_sample_dv <- merge(this_sample_dv, marketgrptomarket, by = 'market')
  this_sample_dv[, year_month_clean := as.Date(as.yearmon(year_month), frac = 0)]
  this_sample_dv[, is_treatment := as.numeric(rover_share_cut != top_group & year_month_clean >= (merger_announce_ym))]
  setorder(this_sample_dv, 'year_month')
  this_sample_dv[, time_id := .GRP, by = list(year_month_clean)]
  this_sample_dv <- unique(this_sample_dv, by = c("market_grp",'time_id'))
  this_sample_dv[, platform := '2 - DV']
  
  # Rover controls 
  this_sample_rover <- getplatformdt('Rover', stays_convos_needs_agg_platform)
  this_sample_rover <- merge(this_sample_rover, marketgrptomarket, by = 'market')
  this_sample_rover[, year_month_clean := as.Date(as.yearmon(year_month), frac = 0)]
  this_sample_rover[, is_treatment := as.numeric(rover_share_cut != top_group & year_month_clean >= (merger_announce_ym))]
  setorder(this_sample_rover, 'year_month')
  this_sample_rover[, time_id := .GRP, by = list(year_month_clean)]
  this_sample_rover <- unique(this_sample_rover, by = c("market_grp",'time_id'))
  this_sample_rover[, platform := '1-Rover']
  
  this_sample <- rbind(this_sample_dv, this_sample_rover) 
  
  # replace DV control with Rover control 
  this_sample <- rbind(this_sample_dv[rover_share_cut == treatshare], this_sample_rover[rover_share_cut == controlshare])
  
  this_sample[is.na(log_buyers), log_buyers := 0]
  this_sample[is.na(log_sitters), log_sitters := 0]
  this_sample[is.na(log_needs), log_needs := 0]
  this_sample[is.na(log_convos), log_convos := 0]
  this_sample[is.na(log_stays), log_stays := 0]
  this_sample[, convosperneed := num_convos / unique_needs]
  
  for (outcome in outcomes) {
    this_sample[!(is.finite(get(outcome))), (outcome) := NA]
  }
  this_sample[, is_treatment := as.numeric(rover_share_cut != controlshare & year_month_clean >= (merger_announce_ym))]
  
  return(this_sample)
}

# assigns matches for a rover control group and dv treated group using market level data
# outputs a list: the raw matched object, and a data table that matches each treated market to its control unit (for readability)
getMatches <- function(dt, controlgroup, treatedgroup) {
  dt[, is_treatment := as.numeric(rover_share_cut != controlgroup & year_month_clean >= (merger_announce_ym))]
  
  dt <- unique(dt, by = c("market_grp",'time_id'))
  
  # get matches for all zip codes
  ### MATCHING REGRESSIONS
  matching_list <- list()
  this_plot_list <- list()
  this_estimates_list <- list()
  this_matching_list <- list()
  this_group <- treatedgroup
  this_data <- dt[(rover_share_cut == controlgroup | rover_share_cut == this_group)]
  this_matching <- PanelMatch(lag = 1, time.id = "time_id",
                              unit.id = "market_grp",
                              treatment = "is_treatment",
                              refinement.method = refinement,
                              data = as.data.frame(this_data),
                              match.missing = T,
                              covs.formula = matching_formula,
                              size.match = 1, qoi = "att",
                              outcome.var = 'log_stays',
                              lead = leads)
  matching_list[[1]] <- this_matching
  
  results = this_matching
  
  markets = gsub("\\..*","",names(results$att))
  
  matchedsets = data.table()
  for (i in 1:length(markets)) {
    weights <- attr(results$att[[i]], 'weights') 
    set = data.table(market_grp = as.numeric(markets[i]), control = as.numeric(names(which(weights> 0))), weight = weights[which(weights > 0)])
    matchedsets = rbind(matchedsets, set)
  }
  # randomly select a control market from set if matched size > 1
  matchedsets[, eps := rnorm(nrow(matchedsets))]
  matchedsets[, n := .N, .(market_grp)]
  matchedsets <- matchedsets[order(market_grp, eps)]
  matchedsets[, ind := 1:.N, .(market_grp)]
  matchedsets <- matchedsets[ind == 1]
  matchedsets <- matchedsets[, list(market_grp, control)]
  return(matchedsets)
}

# assigns matches for a rover control group and dv treated group using market level data
# AND creates data set where each observation is at the pair, time level 
MatchandCreateData <- function(dvshare, rovershare, marketlevelmatch = mktlevelmatch){
  dv_treated_group <- dvshare
  rover_control_group <- rovershare
  unique_groups <- c(dv_treated_group, rover_control_group)
  this_sample <- create_sample('Rover', rover_control_group, dv_treated_group)
  print(unique(this_sample[, list(platform, rover_share_cut)]))
  if (marketlevelmatch == T) {
    this_match <- getMatches(dt = this_sample_marketlevel, 
                             controlgroup = rover_control_group, 
                             treatedgroup = dv_treated_group)
  }
  else {
    this_match <- getMatches(dt = this_sample, 
                             controlgroup = rover_control_group, 
                             treatedgroup = dv_treated_group)
  }
  paired_data_list <- createPairedDataSet(this_match, this_sample, outcomes)
  return(list(paired_data_list, this_match))
}

# formats and saves the results 
saveResults <- function(res, nameappend) {
  res_formatted <- reformat_res(res)
  
  plot_list_matched <- res_formatted[[1]]
  estimates_list_matched <- res_formatted[[2]]
  
  outputfile = paste0(plot_dir, '/plot_list_matched_', nameappend, '.RData')
  outcome_list <- outcomes
  save(plot_list_matched, outcome_list, outcome_names, file = outputfile)
  
  estimatesoutputfile = paste0(estimates_dir, '/estimates_list_', nameappend, '.RData')
  save(estimates_list_matched, outcome_list, outcome_names, file = estimatesoutputfile)
  print(paste0('Plots saved in ', outputfile))
}

create_plot <- function(outcomes_v, outcomes_names) {
  for(j in 1:length(outcomes_v)) {
    this_y <- outcomes_v[j]
    this_yname <- outcomes_names[j]
    i <- 1
    plot_list <- list()
    for(this_share in bins){
      this_ds <- stays_convos_needs_agg_platform[rover_share_cut_flipped %in% c(this_share) & 
                                                   floor(year_month) > 2015]
      this_ds[, is_dv := as.numeric(user_platform == 'DogVacay')]
      this_ds[,this_yvar := get(this_y)]
      r <- feols(this_yvar ~ is_dv*i(year_month_clean, ref = '2017-02-01') | zip_seller + user_platform + year_month_clean, data = this_ds, cluster = 'cbsa_seller')
      tidy_r <- tidy(r)
      setDT(tidy_r)
      tidy_r[, year_mon := tstrsplit(term, '::', fixed = T, keep = 2L)]
      tidy_r <- rbind(tidy_r, data.table(year_mon = '2017-02-01', estimate = 0), fill = T)
      this_plot <- ggplot(tidy_r, aes(x = ymd(year_mon), y = estimate)) + geom_vline(xintercept= ymd(c("2017-03-01", "2017-07-01")), color="grey") + geom_point() + geom_linerange(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error)) + geom_hline(yintercept = 0, linetype = 'dashed') + 
        scale_x_date(breaks = ymd(c("2016-09-01", "2017-03-01", "2017-07-01", "2017-12-01")), labels = c("Sep16", "Mar17", "Jul17", "Dec17"), limits = ymd(c("2016-09-01", "2017-12-01"))) + 
        xlab('')  
      plot_list[[i]] <- this_plot
      i <- i + 1
    }
    all_plot_list[[j]] <- plot_list
    j <- j +1 
  }
  
  all_plot_list <- transpose(all_plot_list)
  y_labs <- c(list(textGrob("")))
  
  for(j in 1:length(outcomes_v)) { #
    outcome_y_min <- min(unlist(lapply(all_plot_list, function(x) layer_scales(x[[j]])$y$range$range)))
    outcome_y_max <- max(unlist(lapply(all_plot_list, function(x) layer_scales(x[[j]])$y$range$range)))
    for(i in 1:5){
      all_plot_list[[i]][[j]] <- all_plot_list[[i]][[j]] + ylim(outcome_y_min, outcome_y_max) 
    }
    y_labs <- c(y_labs,list(textGrob(wrap_text(outcomes_names[j],18), gp = gpar(fontsize = 20), rot = 90)))
  }
  
  bins <- as.list(bins)
  # Create labels
  plot_labels <- wrap_plots(y_labs, nrow = length(outcomes_v) + 1, ncol = 1, heights = c(.1, rep(1, length(outcomes_v) + 1)))
  
  # Combine plots
  a <- wrap_plots(c(list(textGrob(bins[[1]], gp = gpar(fontsize = 20))), all_plot_list[[1]]), nrow = length(outcomes_v) + 1, ncol = 1, heights = c(.1, rep(1, length(outcomes_v) + 1)))
  b <- wrap_plots(c(list(textGrob(bins[[2]], gp = gpar(fontsize = 20))), all_plot_list[[2]]), nrow = length(outcomes_v) + 1, ncol = 1, heights = c(.1, rep(1, length(outcomes_v) + 1)))
  c <- wrap_plots(c(list(textGrob(bins[[3]], gp = gpar(fontsize = 20))), all_plot_list[[3]]), nrow = length(outcomes_v) + 1, ncol = 1, heights = c(.1, rep(1, length(outcomes_v) + 1)))
  d <- wrap_plots(c(list(textGrob(bins[[4]], gp = gpar(fontsize = 20))), all_plot_list[[4]]), nrow = length(outcomes_v) + 1, ncol = 1, heights = c(.1, rep(1, length(outcomes_v) + 1)))
  e <- wrap_plots(c(list(textGrob(bins[[5]], gp = gpar(fontsize = 20))), all_plot_list[[5]]), nrow = length(outcomes_v) + 1, ncol = 1, heights = c(.1, rep(1, length(outcomes_v) + 1)))
  big_plot <- wrap_plots(plot_labels, a, b, c, d, e, ncol = 6, widths = c(.2, rep(1, 5))) & theme(axis.title.y=element_blank(), axis.text = element_text(color="black"))
  return(big_plot)
}

#############################################################################################################################################################
##################################################################### Preparations ##########################################################################
#############################################################################################################################################################

figures_dir_plot <- paste0(results_dir, 'figures/did_zip_seller/')

merger_discussion_ym <- '2016-12-01'
merger_announce_ym <- '2017-03-01'
merger_transfer_ym <- '2017-04-01'
dv_off_ym <- '2017-06-01'
bin_groups <- c(0, .2, 0.4, 0.6, 0.8, 1)

min_stays <- 50

matching_formula <- as.formula(~ I(lag(totalusers, 1:12))) 
refinement = 'CBPS.match'
leads <- c(-6:9)
leads_matching <- c(0:9)
lags <- 4 # this value doesn't matter since all units have the same treatment assignment


tables_dir <- paste0(results_dir, 'tables/')
fig_dir_main <- paste0(results_dir, 'figures/')

matchingfolder_name <- 'matching'

market_vars = c('zip_seller')
platforms <- c('DogVacay', 'Rover')
market_var = 'zip_seller'

runparallel <- F

# all markets and by platform
plot_dir <- paste0(fig_dir_main, 'did_', market_var, '/', matchingfolder_name)
createfolders(paste0(fig_dir_main, 'did_', market_var, '/', matchingfolder_name))


#############################################################################################################################################################
##################################################################### Data to Plot ##########################################################################
#############################################################################################################################################################

stays_convos_needs_agg <- fread(paste0(data_dir, 'stays_convos_needs_agg_bygrid_zip_seller.csv'))

######### CREATE MARKET LEVEL DATA SET TO USE IN MATCHING ###########################
# stays_convos_needs_agg <- fread(paste0(data_dir, 'stays_convos_needs_agg_bygrid_',market_var,'.csv'))
stays_convos_needs_agg[, market := as.character(market)]
#x_walk <- unique(stays_convos_needs_agg[,.(zip_seller,market)])
#stays_convos_needs_agg[,zip_seller := NULL]
stays_convos_needs_agg <- unique(stays_convos_needs_agg)

setorder(stays_convos_needs_agg, market, year_month)
stays_convos_needs_agg[, year_month_clean := as.Date(as.yearmon(year_month), frac = 0)]
stays_convos_needs_agg[, year := year(year_month_clean)]
stays_convos_needs_agg[, grid_month := .GRP, by = list(market, month(year_month_clean))]
stays_convos_needs_agg[, cbsa_month := .GRP, by = list(cbsa_seller, month(year_month_clean))]
stays_convos_needs_agg[, merger_count := interval(ymd(merger_discussion_ym), ymd(year_month_clean))/months(1)]
stays_convos_needs_agg[, merger_count_factor := factor(merger_count*as.numeric(merger_count >= 0) - 10*(merger_count<= -1))]
stays_convos_needs_agg[, year := year(ymd(year_month_clean))]
stays_convos_needs_agg[, tot_stays_2016 := sum(num_stays*as.numeric(year == 2016)), .(market)]

# convert region to dummy
region <- to.dummy(stays_convos_needs_agg$census_region_name, "region")
division <- to.dummy(stays_convos_needs_agg$census_division_name, "division")
stays_convos_needs_agg = cbind(stays_convos_needs_agg, region)
stays_convos_needs_agg = cbind(stays_convos_needs_agg, division)

this_sample <- stays_convos_needs_agg[year_month > as.yearmon('2014-12-01') & year_month < as.yearmon("2018-01-01")]

bin_groups <- c(0, .2, 0.4, 0.6, 0.8, 1)
n_bins <- length(bin_groups) - 2
this_sample[, rover_share_cut := cut2(mkt_share_gtv, bin_groups)]
this_sample[, rover_share_cut := fct_reorder(rover_share_cut, mkt_share_gtv)]
setorder(this_sample, 'year_month')
this_sample[, time_id := .GRP, by = list(year_month_clean)]
this_sample[, market_grp := .GRP, by = list(market)]

this_sample[, log_stays := log(num_stays + 1)]

unique_groups <- unique(this_sample[, rover_share_cut])
unique_groups <- sort(unique_groups)
top_group <- unique_groups[length(unique_groups)]

this_sample[, is_treatment := as.numeric(rover_share_cut != top_group & year_month_clean >= (merger_announce_ym))]
this_sample <- unique(this_sample, by = c("market_grp",'time_id'))

# market level results to use in matching 
this_sample_marketlevel <- this_sample
this_sample <- NULL
marketgrptomarket <- unique(this_sample_marketlevel[, list(market, market_grp, rover_share_cut)], by = 'market')


################# PLATFORM LEVEL COMPARISONS ##################################
stays_convos_needs_agg_platform <- fread(paste0(data_dir, 'needs_convos_stays_agg_', market_var, '_buyersbyplat_1year.csv'))
dt_extra <- fread(paste0(data_dir, 'needs_convos_stays_agg_', market_var, '_buyersbyplat_1year.csv'))
names(dt_extra)[!names(dt_extra) %in% names(stays_convos_needs_agg_platform)]

stays_convos_needs_agg_platform <- fread(paste0(data_dir, 'needs_convos_stays_agg_', market_var, '_buyersbyplat_1year.csv'))
marketgrptomarket[, market := as.numeric(market)]
stays_convos_needs_agg_platform = merge(stays_convos_needs_agg_platform, marketgrptomarket, 
                                        by = 'market')
stays_convos_needs_agg_platform <- stays_convos_needs_agg_platform[user_platform != 'Multihomer']
stays_convos_needs_agg_platform[, year_month_clean := as.Date(as.yearmon(year_month), frac = 0)]

stays_convos_needs_agg_platform[, rover_share_cut := as.character(rover_share_cut)]
stays_convos_needs_agg_platform[, rover_share_cut_flipped := 
                                  case_when(user_platform == 'Rover' ~ rover_share_cut, 
                                            rover_share_cut == '[0.0,0.2)' ~ '[0.8,1.0]',
                                            rover_share_cut == '[0.8,1.0]' ~ '[0.0,0.2)', 
                                            rover_share_cut == '[0.2,0.4)' ~ '[0.6,0.8)', 
                                            rover_share_cut == '[0.6,0.8)' ~ '[0.2,0.4)',
                                            TRUE ~ rover_share_cut)]
stays_convos_needs_agg_platform[, rover_share_cut_flipped := as.factor(rover_share_cut_flipped)]
unique(stays_convos_needs_agg_platform[, list(user_platform, rover_share_cut, rover_share_cut_flipped)])

stays_convos_needs_agg_platform[, user_platform := as.factor(user_platform)]
stays_convos_needs_agg_platform[, user_platform := relevel(user_platform, ref='Rover')]
stays_convos_needs_agg_platform[, rover_share_cut_flipped := relevel(rover_share_cut_flipped, ref='[0.8,1.0]')]

stays_convos_needs_agg[, year_month_clean := as.Date(as.yearmon(year_month), frac = 0)]

#### Multiple Outcomes: 

bins <- c('[0.0,0.2)', '[0.2,0.4)',  '[0.4,0.6)', '[0.6,0.8)', '[0.8,1.0]')
all_plot_list <- list()

stays_convos_needs_agg_platform[, log_stays := log(num_stays + 1)]
stays_convos_needs_agg_platform[, log_buyers := log(unique_buyers + 1)]
stays_convos_needs_agg_platform[, log_sellers := log(unique_sitters + 1)]


############################################ PLOT ##############################################################

big_plot <- create_plot(c("log_stays","success_rate") , c("Transactions (log)", "Request Match Rate")); big_plot
ggsave(paste0(figures_dir_plot, "dv_v_rv.png"), big_plot, width = 18, height = 14*2/5)

print(paste0('Figure 7 written to: ', figures_dir_plot, "dv_v_rv.png"))


