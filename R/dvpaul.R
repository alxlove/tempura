dvpaul<-function(formula, dataset,idvar,timevar){
  `%>%` <- magrittr::`%>%`
  XVEC<-labels(terms(formula))
  idvar <- deparse(substitute(idvar))
  timevar <- deparse(substitute(timevar))
  yvar <- all.vars(formula)[1]
  dataz<-dataset %>% dplyr::select(all_of(c(idvar,timevar,yvar,XVEC)))
  colnames(dataz)<-c("idv","timev",yvar,XVEC)
  dataz<-na.omit(dataz)
  polyd<-polydata(dataz,idv,timev)
  fnew<-update(formula, . ~ . +t+t_cubed+t_squared)
  poly_model<-lm(fnew,polyd)
  return(poly_model)
}

