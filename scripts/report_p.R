#those are functions that help extracting and reporting p-values

report_p <- function(p){
  #function takes a p value and returns the appropriate string you typically find in the literature.
  #This is helpful when work is in progress, models change and p-values cannot be "hardcoded"
  out <- ifelse(p < 0.0001, "< 0.0001", 
                ifelse(p < 0.001, "< 0.001",
                       ifelse(p < 0.01, "< 0.01",
                              ifelse(p < 0.05, "< 0.05",
                                     paste0("= ", round(p, 2))))))
  return(out)
}

get_model_p <- function(model_summary){
  #function takes the SUMMARY of the model (output of summary(model)) and returns the p-value of the model fit
  f <- model_summary$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=FALSE) #actually computing p by hand by looking at the F distribution
  attributes(p) <- NULL
  return(p)
}
