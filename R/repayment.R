#' Calculate the monthly mortgage
#' 
#' @param A loan principal
#' @param beta monthly loan rate
#' @param m number of periods in months
#'
#' @return
#' @export
#'
#' @examples
repayment <- function(A, beta, m){             
  j = (1:m)                                    
  X = A*beta*(1+beta)^(m)/((1+beta)^(m)-1)     
  I = c()
  I[j] = A*beta*(1+beta)^(j-1) - X*((1+beta)^(j-1)-1)   
  c = c()   
  c = X - I       
  a = c()
  a[j] = A*(1+beta)^j - ((1+beta)^j-1)/(beta) * X            
  
  col1 = round(c,2)                             
  col2 = round(I,2)                             
  col3 = round(a,2)                            
  col4 = round(X,2)                          
  

  return(unique(col4))
}
