polydata<-function(idvar,timevar,dataset){
  idvar <- deparse(substitute(idvar))
  timevar <- deparse(substitute(timevar))
  dataset$idt<-dataset[[idvar]]
  dataset$timet<-dataset[[timevar]]
  dataset<-dataset[order(dataset$idt,dataset$timet),]
  datasett<-dataset %>%
    dplyr::group_by(idt) %>%
    dplyr::mutate(t0=min(timet)) %>%
    dplyr::ungroup()
  datasett$t<-datasett$timet-datasett$t0
  datasett$t_squared<-datasett$t^2
  datasett$t_cubed<-datasett$t^3
  datasett$t0<-NULL
  datasett$idt<-NULL
  datasett$timet<-NULL
  return(datasett)
}
