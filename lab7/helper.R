aggregate_nodes <- function(data, group, 
                            col_names = list(x1 = "origRegion", x2 = "destRegion", weight = "flow")
                            ) {
  require("dplyr") 
  #require("rlang")
  
  label <- deparse(substitute(group)) %>% 
    str_replace_all("_", " ") %>% 
    str_replace("and", "&")
  x1 <- rlang::parse_expr(col_names$x1)
  x2 <- rlang::parse_expr(col_names$x2)
  weight <- rlang::parse_expr(col_names$weight)
  
  data <- data %>%
    mutate(!!quo_name(x1) := if_else(!!x1 %in% group, label, !!x1),
           !!quo_name(x2) := if_else(!!x2 %in% group, label, !!x2)) %>%
    group_by(!!x1, !!x2) %>%
    summarize(!!quo_name(weight) := sum(!!weight)) %>%
    ungroup()
  
  return(data)
}

