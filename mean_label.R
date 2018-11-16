#function to create a mean label for ggplot stat_summary plots
mean_label <- function(x,digits=2){
  return(data.frame(y = mean(x), label = round(mean(x),digits)))
}
