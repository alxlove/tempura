polydata<-function(datasett,idvv,timevv){
  idvv <- deparse(substitute(idvv))
  timevv <- deparse(substitute(timevv))
  datasett$idt<-datasett[[idvv]]
  datasett$timet<-datasett[[timevv]]
  datasett<-datasett[order(datasett$idt,datasett$timet),]
  datasett<-datasett%>% group_by(idt) %>% mutate(t0=min(timet)) %>% ungroup()
  datasett$t<-datasett$timet-datasett$t0
  datasett$t_squared<-datasett$t^2
  datasett$t_cubed<-datasett$t^3
  datasett$t0<-NULL
  datasett$idt<-NULL
  datasett$timet<-NULL
  return(datasett)
}