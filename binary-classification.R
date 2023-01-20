
ggplotROC <- function(fitval, y){
  roc <- rocit(score = fitval, class = y)
  meas <- measureit(roc,measure = c("ACC", "SENS", "SPEC"))                   
  return(ggplot() + geom_line(aes(x = 1 - meas$SPEC, y = measure$SENS)) + labs(title = 'ROC',x = '1 - Specifity', y = 'Sensitivity'))
}

AUC <- function(fitval,y){
  roc_mod <- rocit(score = fitval , class = y)
  return(roc_mod[['AUC']])
}
