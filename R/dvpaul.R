dvpaul<-function(formula, idvar,timevar, dataset){
  XVEC<-labels(terms(formula))
  idvar <- deparse(substitute(idvar))
  timevar <- deparse(substitute(timevar))
  yvar <- all.vars(formula)[1]
  dataz<-dataset %>% dplyr::select(all_of(c(idvar,timevar,yvar,XVEC)))
  colnames(dataz)<-c("idv","timev",yvar,XVEC)
  dataz<-na.omit(dataz)
  polyd<-polydata(idv,timev,dataz)
  fnew<-update(formula, . ~ . +t+t_cubed+t_squared)
  poly_model<-lm(fnew,polyd)
  print(summary(poly_model)$coef)
  return(poly_model)
}

