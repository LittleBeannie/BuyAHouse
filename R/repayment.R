#' 小小函数，可以直接忽略，已经检验过算法没问题哈哈哈
#' 计算每月的贷款
#'
#' @param A 贷款本金
#' @param β 月利息
#' @param m 期数（月）
#'
#' @return
#' @export
#'
#' @examples
repayment <- function(A, β, m){                # 贷款本金、月利息、期数（月）
  j = (1:m)                                    # 定义向量，用于中间过程计算
  X = A*β*(1+β)^(m)/((1+β)^(m)-1)              # 每期应还(本+息),见推导公式①
  I = c()
  I[j] = A*β*(1+β)^(j-1) - X*((1+β)^(j-1)-1)   # 每期应还利息,见推导公式②
  c = c()                                      # 定义一个空向量用于计算等额偿还本金，否则会报错
  c = X - I                                    # 每期应还本金
  a = c()
  a[j] = A*(1+β)^j - ((1+β)^j-1)/(β) * X       # 每期末本金余额,见推导公式①           
  
  ## 打印输出总利息，总本金
  # print(paste0('总利息：',round(sum(I),2)))
  # print(paste0('总本金：',round(sum(c),2)))
  
  ## 将等额本息还款数据输出至本地
  col1 = round(c,2)                             # 每期应还本金
  col2 = round(I,2)                             # 每期应还利息
  col3 = round(a,2)                             # 每期末本金余额
  col4 = round(X,2)                             # 每期应还
  
  #loan = data.frame(期数 = c(1:m), 
  #                  每期应还本金 = col1, 
  #                  每期应还利息 = col2, 
  #                  每期末本金余额 = col3, 
  #                  每期应还 = col4)
  return(unique(col4))
}
