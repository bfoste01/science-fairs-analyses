# Get lower triangle of the correlation matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }

# Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }

# Funtion to reorder the correlation matrix
reorder_cormat <- function(cormat){
# Use correlation between variables as distance
dd <- as.dist((1-cormat)/2)
hc <- hclust(dd)
cormat <-cormat[hc$order, hc$order]
}


# Function to tidy lavaan output
tidy.lavaan <- function(x, 
                        conf.int = TRUE,
                        conf.level = 0.95,
                        standardized=FALSE,
                        ...){
    tidyframe <- parameterEstimates(x, 
                       ci=conf.int, 
                       level=conf.level,
                       standardized=standardized) %>% 
        as_data_frame() %>% 
        tibble::rownames_to_column() %>% 
        mutate(term=paste(lhs, op, rhs)) %>% 
        rename(estimate=est, 
               std.error = se,
               p.value=pvalue,
               statistic = z,
               conf.low=ci.lower,
               conf.hi=ci.upper) %>% 
        select(term, op, everything(), -rowname, -lhs, -rhs)
    return(tidyframe)
}
# Helper functions that allow string arguments for  dplyr's data modification functions like arrange, select etc. 
# Author: Sebastian Kranz

# Examples are below

#' Modified version of dplyr's filter that uses string arguments
#' @export
s_filter = function(.data, ...) {
  eval.string.dplyr(.data,"filter", ...)
}

#' Modified version of dplyr's select that uses string arguments
#' @export
s_select = function(.data, ...) {
  eval.string.dplyr(.data,"select", ...)
}

#' Modified version of dplyr's arrange that uses string arguments
#' @export
s_arrange = function(.data, ...) {
  eval.string.dplyr(.data,"arrange", ...)
}

#' Modified version of dplyr's arrange that uses string arguments
#' @export
s_mutate = function(.data, ...) {
  eval.string.dplyr(.data,"mutate", ...)
}

#' Modified version of dplyr's summarise that uses string arguments
#' @export
s_summarise = function(.data, ...) {
  eval.string.dplyr(.data,"summarise", ...)
}

#' Modified version of dplyr's group_by that uses string arguments
#' @export
s_group_by = function(.data, ...) {
  eval.string.dplyr(.data,"group_by", ...)
}

#' Internal function used by s_filter, s_select etc.
eval.string.dplyr = function(.data, .fun.name, ...) {
  args = list(...)
  args = unlist(args)
  code = paste0(.fun.name,"(.data,", paste0(args, collapse=","), ")")
  df = eval(parse(text=code,srcfile=NULL))
  df  
}
