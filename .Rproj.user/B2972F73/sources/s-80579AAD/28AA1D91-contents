dvgreg<-function(formula, dataset,idvar,timevar){
  
  
  dvgreg_lm <- function(formula, data, rhoval) {
    # Create an lm object using the formula and data
    model <- lm(formula, data = data)
    # Assign a new class to the lm object
    class(model) <- c("dvgreg_lm", class(model))
    # Store the rhoval value as an attribute of the object
    attr(model, "rhoval") <- rhoval
    # Return the modified lm object
    return(model)
  }
  
  summary.dvgreg_lm <- function(object, ...) {
    # Get the 'rho' value from the 'dvgreg_lm' object
    rhoval <- attr(object, "rhoval")
    # Print the 'rho' value below the formula
    cat("***************************************\n","Rho= ", rhoval, "\n***************************************\n\n")
    
    # Call the original 'summary.lm' function to generate the remaining summary output
    stats:::summary.lm(object, ...)
  }
  
  
  XVEC<-labels(terms(formula))
  idvar <- deparse(substitute(idvar))
  timevar <- deparse(substitute(timevar))
  yvar <- all.vars(formula)[1]
  alv<-c(idvar,timevar,yvar,XVEC)
  dataz<-dataset %>% dplyr::select(all_of(alv))
  XVEC<-paste0(XVEC,"_varorg")
  colnames(dataz)<-c("idv","timev","y",XVEC)
  dataz<-dataz[order(dataz$idv,dataz$timev),]

  dataz$temp<-ifelse(!is.na(dataz$y),1,0)
  dataz <- dataz %>%
    group_by(idv) %>%
    mutate(seqv = cumsum(temp)) %>% ungroup()
  dataz$temp<-NULL
  dataz$seqv<-ifelse(is.na(dataz$y),NA,dataz$seqv)
  dataz$seqv2<-dataz$seqv
  dataz$invtime=dataz$timev*-1
  
  dataz <- dataz %>%
    group_by(idv) %>%
    arrange(idv, timev) %>%
    fill(seqv2, .direction = "down") %>% 
    ungroup()
  
  dataz <- dataz %>%
    group_by(idv,seqv2) %>%
    arrange(idv, seqv2,invtime) %>%
    mutate(s = row_number() ) %>% 
    ungroup()
  
  dataz$s<-ifelse(!is.na(dataz$y),0,dataz$s)
  dataz$seqv2<-ifelse(dataz$s==0,dataz$seqv-1,dataz$seqv2)
  
  dataz <- dataz %>%
    group_by(idv, seqv2) %>%
    mutate(t_new = ifelse(s != 0, NA, sum(!is.na(seqv2))),.group="drop") %>%
    ungroup()
  dataz<-dataz %>% dplyr::select(-.group)
  
  dataz$t_new<-ifelse(dataz$seqv2==0,NA,dataz$t_new)
  
  miss_d<-dataz %>% dplyr::select(all_of(XVEC))
  colnames(miss_d)<-c(paste0("missobs_", XVEC))
  miss_d<-ifelse(is.na(miss_d),1,NA)
  dataz<-as.data.frame(cbind(dataz,miss_d))
  rm(miss_d)  
  
  smfnk<-function(vc){
      vc<-ifelse(is.na(vc),0,vc)
      oc<-sum(vc)
      return(oc)
      }

dataz <-dataz %>%
  group_by(idv, seqv2) %>%
  mutate(
    across(
      .cols=starts_with("missobs_"),
      .fns = smfnk,
      .names = "grdel_{.col}")) %>%
  ungroup()


#removed this, go back if it is, missing:
  #dataz$constant<-1
  dataz$ylag<-dataz$y
  dataz <- dataz %>%
    group_by(idv) %>%
    arrange(idv, timev) %>%
    fill(ylag, .direction = "down") %>% 
    ungroup()
  
  dataz$ylag<-ifelse(dataz$seqv>1,lag(dataz$ylag,1),NA)
  
  rhoseq<-seq(.1,.9,.1)
  ys<-as.data.frame(matrix(nrow=nrow(dataz),ncol=length(rhoseq)))
  dvgregw<-as.data.frame(matrix(nrow=nrow(dataz),ncol=length(rhoseq)))
  xs<-as.data.frame(matrix(nrow=nrow(dataz),ncol=(length(rhoseq)*length(XVEC))))
  colnames(ys)<-paste("ys_",rhoseq,sep="")
  colnames(dvgregw)<-paste("rho_multi_",rhoseq,sep="")
  colnames(xs)<-paste(rep(XVEC,length(rhoseq)),"_rohconum",sort(rep(rhoseq,length(XVEC))),sep="")
  
  
  #Loop to create X and Y data
  for(a in rhoseq){
    j<-a*10
    #create new y
    ys[,j]<-dataz$y-((1-a)^dataz$t_new)*dataz$ylag
    dvgregw[,j]<-(1-a)^dataz$s

    #create new xs       
    for (q in seq_along(XVEC)) {
      xs[[paste(XVEC[q],"_rohconum",a,sep = "")]]<-(1-a)^dataz$s*dataz[[XVEC[q]]]
      xs[[paste(XVEC[q],"_rohconum",a,sep = "")]]<-xs[[paste(XVEC[q],"_rohconum",a,sep = "")]]*a
      
    }
  }

  dataz<-as.data.frame(cbind(dataz,ys,xs,dvgregw))
  rm(xs,ys,dvgregw,a,j,q)


  #Modifying X
  dataz <- dataz %>% 
    group_by(idv,seqv2) %>%
    arrange(idv, seqv2,seqv) %>%
    mutate(across(colnames(dataz)[grepl("_rohconum0.",colnames(dataz))],~sum(.x, na.rm=T))) %>% ungroup()
  
  dataz<-dataz %>%
    mutate(across(colnames(dataz)[grepl("_rohconum0.",colnames(dataz))], 
                  ~ ifelse(is.na(dataz$y)|dataz$seqv==1, NA, .x)))
  
 for(a in rhoseq){
    for (q in seq_along(XVEC)) {
      dataz[[paste0(XVEC[q],"_rohconum",a)]]<-ifelse(dataz[[paste0("grdel_missobs_",XVEC[q])]]!=0,
                                                     NA,dataz[[paste0(XVEC[q],"_rohconum",a)]])
    }
  }
  rm(a,q)
  
  #############################################
  #W
  ##########################################
  dataz <- dataz %>% #############change dvgrew to reflect current one
    group_by(idv,seqv2) %>%
    arrange(idv, seqv2,seqv) %>%
    mutate(across(colnames(dataz)[grepl("rho_multi_",colnames(dataz))],
                  ~sum(.x, na.rm=T))) %>%
    ungroup()
  
  dataz[,grep("rho_multi_",colnames(dataz))]<-1/dataz[,grep("rho_multi_",colnames(dataz))]  
  dataz<-dataz %>%
    mutate(across(colnames(dataz)[grepl("rho_multi_",colnames(dataz))], 
                  ~ ifelse(is.na(dataz$y)|dataz$seqv==1, NA, .x)))
  
  
  
  maxr2<--99
  #modifyin vars
  for(a in rhoseq){
    dataz[[paste0("ys_",a)]]<-dataz[[paste0("rho_multi_",a)]]*dataz[[paste0("ys_",a)]]
    for (q in seq_along(XVEC)) {
      dataz[[paste0(XVEC[q],"_rohconum",a)]]<-dataz[[paste0("rho_multi_",a)]]*dataz[[paste0(XVEC[q],"_rohconum",a)]]
    }
    
    ###################################      
    ############Regression 
    ###################################

    
      subdata<-dataz %>% dplyr::select(idv,timev,paste0("ys_",a),paste0(XVEC,"_rohconum",a))
      colnames(subdata)<-alv
      m<-dvgreg_lm(formula,subdata,rhoval=a)
    if(summary(m)$r.squared>maxr2){
      maxr2<-summary(m)$r.squared
      model<-m
    }
    rm(m)
  }
  
  summary(model)
    return(model)
}


