report_p <- function(p){
  out <- ifelse(p < 0.0001, "< 0.0001", 
                ifelse(p < 0.001, "< 0.001",
                       ifelse(p < 0.01, "< 0.01",
                              ifelse(p < 0.05, "< 0.05",
                                     paste0("= ", round(p, 2))))))
  return(out)
}

get_model_p <- function(model_summary){
  f <- model_summary$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
