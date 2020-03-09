#' Simulate Biometrically informed Multivariate Data
#' @description Generate paired multivariate data, given ACE parameters.
#' @importFrom stats rnorm sd
#' @param r_all Levels of relatedness; default is MZ and DZ twins c(1,.5).
#' @param npg_all Sample size per group; default is 500.
#' @param npergroup_all Vector of sample sizes by group; default repeats \code{npg_all} for all groups
#' @param variables Number of variables to generate; default is 2. Currently, limited to max of two variables.
#' @param mu_all Mean for each generated variable; default is 0.
#' @param mu_list List of means by variable; default repeats \code{mu_all} for all variables
#' @param r_vector Alternative, give vector of r cofficients for entire sample.
#' @param ace_all Vector of variance components for each generated variable; default is c(1,1,1).
#' @param ace_list Matrix of ACE variance components by variable, where each row is its own variable; default is to repeat \code{ace_all} for each variable.
#' @param ... Optional pass on additional inputs.
#' @param reliability_all Optional Additional error. Indicates correlation between True Y and Measured Y. Default is 1
#' @param prop_var_explained_all Optional Additional error. Indicates proportion of variance explained in True Y from Measured Y. Default is \code{reliability_all}^2
#' @param reliability_list Vector of Reliabilities for each generated variable; default is to repeat \code{reliability_all} for each variable
#' @param prop_var_explained_list Vector of R^2 for each generated variable; default is to repeat \code{prop_var_explained_all} for each variable
#'@param cov_a Shared variance for additive genetics (a); default is 1
#'@param cov_c Shared variance for shared-environment (c); default is 1
#'@param cov_e shared variance for non-shared-environment (e); default is 1
#'@param model Model type. Default is correlated factors model "Correlated"; alterative specification as a "Cholesky" model, where variable 1 accounts for variance in variable 2, is currently disabled.

#' @return Returns \code{data.frame} with the following:
#' \item{Ai_1}{genetic component for variable i for kin1}
#' \item{Ai_2}{genetic component for variable i for kin2}
#' \item{Ci_1}{shared-environmental component for variable i for kin1}
#' \item{Ci_2}{shared-environmental component for variable i for kin2}
#' \item{Ei_1}{non-shared-environmental component for variable i for kin1}
#' \item{Ei_2}{non-shared-environmental component for variable i for kin2}
#' \item{yi_1}{generated variable i for kin1}
#' \item{yi_2}{generated variable i for kin2}
#' \item{r}{level of relatedness for the kin pair}
#' \item{id}{id}


kinsim_multi <- function(
  r_all=c(1,.5),
  npg_all=500,
  npergroup_all=rep(npg_all,length(r_all)),
  mu_all=0,
  variables=2,
  mu_list=rep(mu_all,variables),
  reliability_list=NULL,
  r_vector=NULL, # alternative specification, give vector of rs
  ace_all=c(1,1,1), # variance default
  ace_list=matrix(rep(ace_all,variables),byrow=TRUE,nrow=variables),
  cov_a=0, #default shared variance for genetics
  cov_c=0, #default shared variance for c
  cov_e=0, #default shared variance for e
  ...){
  mu=NULL
  sA <- ace_list[,1]^0.5; sC <- ace_list[,2]^0.5; sE <- ace_list[,3]^0.5
  S2 <- diag(4)*-1+1
  
  datalist <- list()
  if(variables==1){
    data_v<-kinsim1(r=r_all,
                    npergroup=npergroup_all,	#
                    mu=mu_list[1],			#intercept
                    ace= ace_list[[1]],r_vector=r_vector
    )
    data_v$A1_u<-data_v$A1
    data_v$A2_u<-data_v$A2
    data_v$C1_u<-data_v$C1
    data_v$C2_u<-data_v$C2
    data_v$E1_u<-data_v$E1
    data_v$E2_u<-data_v$E2
    data_v$y1_u<-data_v$y1
    data_v$y2_u<-data_v$y2
    
    merged.data.frame =data_v
    names(merged.data.frame)[c(1,10)]<-c("id","r")
  }
  if(variables>2){
    stop("You have tried to generate data beyond the current limitations of this program. Maximum variables 2.")
  }
    if(is.null(r_vector)){
      id=1:sum(npergroup_all)
      for(i in 1:length(r_all)){
        n = npergroup_all[i]
        
        # Genetic Covariance
        sigma_a<-diag(4)+S2*r_all[i]
        sigma_a[1,3]<-cov_a;
        sigma_a[3,1]<-cov_a;sigma_a[2,4]<-cov_a;sigma_a[4,2]<-cov_a
        sigma_a[1,4]<-cov_a*r_all[i];sigma_a[4,1]<-cov_a*r_all[i];sigma_a[3,2]<-cov_a*r_all[i];sigma_a[2,3]<-cov_a*r_all[i]
        A.r <- rmvn(n,sigma=sigma_a)
        
        A.r[,1:2]<- A.r[,1:2]*sA[1]; A.r[,3:4]<- A.r[,3:4]*sA[2]
        
        # Shared C Covariance
        sigma_c<-diag(4)+S2*1
        sigma_c[1,3]<-cov_c;sigma_c[3,1]<-cov_c;sigma_c[2,4]<-cov_c;sigma_c[4,2]<-cov_c
        sigma_c[1,4]<-cov_c*1;sigma_c[4,1]<-cov_c*1;sigma_c[3,2]<-cov_c*1;sigma_c[2,3]<-cov_c*1
        C.r <- rmvn(n,sigma=sigma_c)
        C.r[,1:2]<- C.r[,1:2]*sC[1]; C.r[,3:4]<- C.r[,3:4]*sC[2]
        
        # Shared E Covariance
        sigma_e<-diag(4)+S2*0
        sigma_e[1,3]<-cov_e;sigma_e[3,1]<-cov_e;sigma_e[2,4]<-cov_e;sigma_e[4,2]<-cov_e
        E.r <- rmvn(n,sigma=sigma_e)
        E.r[,1:2]<- E.r[,1:2]*sE[1]; E.r[,3:4]<- E.r[,3:4]*sE[2]
        
        y.r <-  A.r + C.r + E.r
        
        
        y.r[,1:2]<-y.r[,1:2]+mu_list[1]
        y.r[,3:4]<-y.r[,3:4]+mu_list[2]
        r_ <- rep(r_all[i],n)
        
        data.r<-data.frame(A.r,C.r,E.r,y.r,r_)
        names(data.r)<-c("A1_1","A1_2","A2_1","A2_2","C1_1","C1_2","C2_1","C2_2","E1_1","E1_2","E2_1","E2_2","y1_1","y1_2","y2_1","y2_2","r")
        
        datalist[[i]] <- data.r
        names(datalist)[i]<-paste0("datar",r_all[i])
        print(r_all[i])
      }
      merged.data.frame = Reduce(function(...) merge(..., all=T), datalist)
      merged.data.frame$id<-id
    }else{
      id=1:length(r_vector)
      data_vector=data.frame(id,r_vector,matrix(rep(as.numeric(NA),length(id)*4),nrow=length(id),ncol=4))
      names(data_vector)<-c("id","r","A1_1","A1_2","A2_1","A2_2")
      unique_r= matrix(unique(r_vector))
      for(i in 1:length(unique_r)){
        n=length(r_vector[r_vector==unique_r[i]])
        
        # Genetic Covariance
        sigma_a<-diag(4)+S2*unique_r[i]
        sigma_a[1,3]<-cov_a;
        sigma_a[3,1]<-cov_a;sigma_a[2,4]<-cov_a;sigma_a[4,2]<-cov_a
        sigma_a[1,4]<-cov_a*unique_r[i];sigma_a[4,1]<-cov_a*unique_r[i];sigma_a[3,2]<-cov_a*unique_r[i];sigma_a[2,3]<-cov_a*unique_r[i]
        A.r <- rmvn(n,sigma=sigma_a)
        data_vector$A1_1[data_vector$r_vector==unique_r[i]] <- A.r[,1]*sA[1]
        data_vector$A1_2[data_vector$r_vector==unique_r[i]] <- A.r[,2]*sA[1]
        data_vector$A2_1[data_vector$r_vector==unique_r[i]] <- A.r[,3]*sA[2]
        data_vector$A2_2[data_vector$r_vector==unique_r[i]] <- A.r[,4]*sA[2]
        A.r[,1:2]<- A.r[,1:2]; A.r[,3:4]<- A.r[,3:4]*sA[2]
      }
      n=length(r_vector)
      A.r<-matrix(c(data_vector$A1_1,data_vector$A1_2,data_vector$A2_1,data_vector$A2_2),ncol=4,nrow=n)
      # Shared C Covariance
      sigma_c<-diag(4)+S2*1
      sigma_c[1,3]<-cov_c;sigma_c[3,1]<-cov_c;sigma_c[2,4]<-cov_c;sigma_c[4,2]<-cov_c
      sigma_c[1,4]<-cov_c*1;sigma_c[4,1]<-cov_c*1;sigma_c[3,2]<-cov_c*1;sigma_c[2,3]<-cov_c*1
      C.r <- rmvn(n,sigma=sigma_c)
      C.r[,1:2]<- C.r[,1:2]*sC[1]; C.r[,3:4]<- C.r[,3:4]*sC[2]
      
      # Shared E Covariance
      sigma_e<-diag(4)+S2*0
      sigma_e[1,3]<-cov_e;sigma_e[3,1]<-cov_e;sigma_e[2,4]<-cov_e;sigma_e[4,2]<-cov_e
      E.r <- rmvn(n,sigma=sigma_e)
      E.r[,1:2]<- E.r[,1:2]*sE[1]; E.r[,3:4]<- E.r[,3:4]*sE[2]
      
      
      y.r <- A.r
      y.r[,1:2]<-A.r[,1:2]*ace_list[1,1] + C.r[,1:2]*ace_list[1,2] + E.r[,1:2]*ace_list[1,3]
      y.r[,3:4]<-A.r[,3:4]*ace_list[2,1] + C.r[,3:4]*ace_list[2,2] + E.r[,3:4]*ace_list[2,3]
      y.r[,1:2]<-y.r[,1:2]+mu_list[1]
      y.r[,3:4]<-y.r[,3:4]+mu_list[2]
      y.r <- mu + A.r + C.r + E.r
      data.r<-data.frame(A.r,C.r,E.r,y.r,r_vector,id)
      names(data.r)<-c("A1_1","A1_2","A2_1","A2_2","C1_1","C1_2","C2_1","C2_2","E1_1","E1_2","E2_1","E2_2","y1_1","y1_2","y2_1","y2_2","r","id")
      
    
    datalist[[i]] <- data.r
    names(datalist)[i]<-paste0("datar",r_all[i])
    merged.data.frame = data.r
    }
  return(merged.data.frame)
}