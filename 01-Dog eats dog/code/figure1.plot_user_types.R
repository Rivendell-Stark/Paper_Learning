library(optimx)
library(nleqslv)
library(ggplot2)
library(data.table)
library(latex2exp)
library(colorBlindness)


Z = 1 # 用户选择不使用平台的效用分布范围，即 ε_i ~ U(-Z, Z)
alpha = .9 # 网络效应的指数部分，作者假设v(·)的形式为指数形式，即`v(n) = n ^ alpha`
k <- .5 # 外生给定分割点：k1 = 0.5


paper_theme <-  theme_classic() + theme(axis.text = element_text(size = 12, color = "black"), 
                                        axis.title = element_text(size = 20),
                                        legend.text = element_text(size = 20),
                                        legend.title = element_text(size = 20))

# 定义网络效用函数为 v(s) = s ^ alpha
util_func <- function(s)  s^alpha

# 使用数值方法求解合并前双边平台的均衡
# x = (n1, n2), 从而x[1], x[2]为均衡时两个平台的使用人数
# y[1], y[2]是方程残差，目标是找到n1, n2，使y[1], y[2]为0

# 合并前，求解双边平台均衡
solve_all_eqb <- function(Z, k){
  solve_all <- function(x){
    
    y <- numeric(2)
    # 右边的两个式子是均衡条件的代数形式
    # ∫_{0}^{k}∂d∫_{-Z}^{v(n1)-d}(1/2)∂e - n1 = 0 
    y[1] <- ((1)/(2*Z))*(k*(util_func(x[1]) + Z) - k^2/2) - x[1]
    y[2] <- (1/(2*Z))*((1 - k)*(util_func(x[2]) + Z) - (1 - k)^2/2)- x[2]
    y
  }
  
  # 设定x的初始值
  xstart <- matrix(runif(480, min=0, max=1), ncol=2)
  
  # 数值方法求解
  ans <- searchZeros(xstart, solve_all, method="Broyden",global="dbldog")
  if(is.null(ans)){
    print("NO Solution.")
  }
  
  # 寻找最优解：α平台份额最大的解
  ans_x <- as.data.table(ans$x)
  ans_x[, share := V1/(V1 + V2)]
  zero_params <- ans_x[order(-share)][1]
  zero_params
  
  # n1, n2 为均衡时平台的人数
  n1 <- zero_params[1, V1]
  n2 <- zero_params[1, V2]
  
  # Other quantities:
  # val_over_oo1 / val_over_oo2    <- 选择使用平台的用户的外部效用（e）之和
  # u1 / u2                        <- 选择使用平台的用户的净效用（v(n)-d-e）之和
  # tc1 / tc2                      <- 选择使用平台的用户的偏好成本（d）之和
  # nfx1 / nfx2                    <- 选择使用平台的用户的网络效应（v(n)）之和
  
  val_over_oo1 <-  .5 *(1)/(2*Z) * integrate(function(x) (util_func(n1) - x)^2 - Z^2, 0, k)$value 
  val_over_oo2 <-  .5 *(1)/(2*Z) * integrate(function(x) (util_func(n2) - (1 - x))^2 - Z^2, k, 1)$value 
  
  u1 <- (1)/(2*Z) * integrate(function(x) (util_func(n1) - x)*(util_func(n1) - x + Z), 0, k)$value - val_over_oo1
  u2 <- (1)/(2*Z) * integrate(function(x) (util_func(n2) - (1 - x))*(util_func(n2) - (1 - x) + Z), k, 1)$value - val_over_oo2
  
  tc1 <- (1)/(2*Z) * integrate(function(x) (util_func(n1) - x + Z)*x, 0, k)$value 
  tc2 <- (1)/(2*Z) * integrate(function(x) (util_func(n2) - (1 - x) + Z)*(1 - x), k, 1)$value 
  
  nfx1 <- (1)/(2*Z) * integrate(function(x) (util_func(n1) - x + Z)*util_func(n1), 0, k)$value 
  nfx2 <- (1)/(2*Z) * integrate(function(x) (util_func(n2) - (1 - x) + Z)*util_func(n2), k, 1)$value 
  
  data.table(k = k,
             n1 = n1,
             n2 = n2,
             u1 = u1,
             u2 = u2,
             tc1 = tc1,
             tc2 = tc2,
             nfx1 = nfx1,
             nfx2 = nfx2,
             val_over_oo1 = val_over_oo1,
             val_over_oo2 = val_over_oo2)
}
baseline_solution <- solve_all_eqb(Z, k)

# 合并后，单平台均衡求解
solve_all_eqb_1p <- function(Z){
  solve_all_1p <- function(x){
    y <- numeric(1)
    y[1] <- (1/(2*Z))*(util_func(x[1]) - 1/2 + Z) - x[1]
    y
  }
  
  # 设定初始值
  xstart <- matrix(runif(80, min=0, max=1), ncol=1)
  
  # 数值方法求解，并取最大的解
  ans <- searchZeros(xstart, solve_all_1p, method="Broyden",global="dbldog")
  ans_x <- as.vector(ans$x)
  zero_params <- sort(ans_x, decreasing = TRUE)
  zero_params
  
  # 均衡人数
  nstar <- zero_params[1]
  
  # Other quantities:
  # val_over_oo    <- 选择使用平台的用户的外部效用（e）之和
  # u_star         <- 选择使用平台的用户的净效用（v(n)-d-e）之和
  # tc_star        <- 选择使用平台的用户的偏好成本（d）之和
  # nfx_star       <- 选择使用平台的用户的网络效应（v(n)）之和
  val_over_oo <- .5 * 1/(2*Z) * integrate(function(x) (util_func(nstar) - x)^2 - Z^2, 0, 1)$value
  ustar <- 1/(2*Z) * integrate(function(x) (util_func(nstar) - x + Z)*(util_func(nstar) - x), 0, 1)$value - val_over_oo
  tcstar <- 1/(2*Z) * integrate(function(x) (util_func(nstar) - x + Z)*x, 0, 1)$value 
  nfxstar <- 1/(2*Z) * integrate(function(x) (util_func(nstar) - x + Z)*util_func(nstar), 0, 1)$value 
  
  data.table(nstar = nstar,
             ustar = ustar,
             tcstar = tcstar,
             nfxstar = nfxstar,
             val_over_oo = val_over_oo)
}
merger_solution <- solve_all_eqb_1p(Z)

# 无差异曲线：合并前与合并后
indifference_curve <- function(x) ifelse(x <= baseline_solution$k, util_func(baseline_solution$n1) - x, util_func(baseline_solution$n2) - (1 - x))
indifference_curve_merger <- function(x) util_func(merger_solution$nstar) - x

# When do curves cross? 合并前后两条无差异曲线的交点
xstart <- matrix(runif(80, min=0, max=1), ncol=1)
ans <- searchZeros(xstart, function(x) indifference_curve(x) - indifference_curve_merger(x), method="Broyden",global="dbldog")
cross_of_curves <- as.numeric(ans$x)


##############################################################################################################################
################################################# Figure 1: 用户分类的示意图 #################################################
##############################################################################################################################

blank_dt <- data.table(x = seq(0, 1, 1/1000))
blank_dt[, r_indifference_curve := indifference_curve(seq(0, 1, 1/1000))]
blank_dt[, r_indifference_curve_merger := indifference_curve_merger(seq(0, 1, 1/1000))]

plot_user_types <- ggplot(blank_dt,  aes(x = x)) + 
  # From OO to Platform 1
  geom_ribbon(data = subset(blank_dt, x <= cross_of_curves), aes(ymin = r_indifference_curve, ymax = r_indifference_curve_merger), fill="#8c510a", alpha=0.5) +
  # From Platform 2 to OO
  geom_ribbon(data = subset(blank_dt, x > cross_of_curves), aes(ymin = r_indifference_curve, ymax = r_indifference_curve_merger), fill="#d8b365", alpha=0.5) + 
  # Always Platform 1
  geom_ribbon(data = subset(blank_dt, x <= baseline_solution$k), aes(ymin = -Z, ymax = r_indifference_curve), fill="#f6e8c3", alpha=0.5) + 
  # Platform 2 to Platform 1
  geom_ribbon(data = subset(blank_dt, x >= baseline_solution$k), aes(ymin = -Z, ymax = pmin(r_indifference_curve, r_indifference_curve_merger)), fill="#01665e", alpha=0.5) + 
  ylim(-Z, Z) + geom_function(fun = indifference_curve, colour = "#b2182b", size = 2)   + 
  geom_function(fun = indifference_curve_merger, size = 2) + theme_classic() +
  ylab(TeX(r'(Value of Outside Option ($\epsilon$))')) + xlab("Horizontal Preference (d)") + theme(text = element_text(size = 24)) +
  annotate(geom="text", x = .25, y = -.5, label="A", color="black", size = 14) +
  annotate(geom="text", x = .75, y = -.5, label="B", color="black", size = 14) +
  annotate(geom="text", x = .25, y = .2,  label = "C", color="black", size = 14) +
  annotate(geom="text", x = .85, y = -.1,  label = "D", color="black", size = 14) +
  annotate(geom="text", x = .54, y = -.92,  label = as.character(TeX("$k_{1}$")), color="black", size = 8, parse=TRUE) + 
  annotate(geom="text", x = .0, y = .2,  label = as.character(TeX("$k_{2}$")), color="black", size = 8, parse=TRUE) + 
  annotate(geom="text", x = .95, y = -.25,  label = as.character(TeX("$k_{5}$")), color="black", size = 8, parse=TRUE) +
  annotate(geom="text", x = .0, y = .65,  label = as.character(TeX("$k_{4}$")), color="black", size = 8, parse=TRUE) + 
  annotate(geom="text", x = .95, y = .4,  label = as.character(TeX("$k_{3}$")), color="black", size = 8, parse=TRUE)

ggsave(plot_user_types, file = 'figure_1_plot_user_types.png', path = paste0(results_dir,"/figures/"), width = 10, height = 8)  

