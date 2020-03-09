#' Simulate Biometrically informed Univariate Data
#' @description Generate paired univariate data, given ACE parameters.
#' @param r Levels of relatedness; default is MZ and DZ twins c(1,.5)
#' @param npg Sample size per group; default is 100.
#' @param npergroup List of sample sizes by group; default repeats \code{npg} for all groups.
#' @param mu Mean for generated variable; default is 0.
#' @param ace Vector of variance components, ordered by c(a, c, e); default is c(1,1,1).
#' @param r_vector Alternative, give vector of relatedness cofficients for entire sample.
#' @param ... Optional pass on additional inputs.

#' @return Returns \code{data.frame} with the following:
#' \item{id}{id}
#' \item{A1}{genetic component for kin1}
#' \item{A2}{genetic component for kin2}
#' \item{C1}{shared-environmental component for kin1}
#' \item{C2}{shared-environmental component for kin2}
#' \item{E1}{non-shared-environmental component for kin1}
#' \item{E2}{non-shared-environmental component for kin2}
#' \item{y1}{generated variable for kin1 with mean of \code{mu}}
#' \item{y2}{generated variable for kin2 with mean of \code{mu}}
#' \item{r}{level of relatedness for the kin pair}


kinsim1 <- function(
  r=c(1,.5),
  npg=100,
  npergroup=rep(npg,length(r)),
  mu=0,
  ace=c(1,1,1),
  r_vector=NULL,
  ...){

  sA <- ace[1]^0.5;
  sC <- ace[2]^0.5;
  sE <- ace[3]^0.5

  S2 <- matrix(c(0,1,
                 1,0),2)
  datalist <- list()

  if(is.null(r_vector)){
    id=1:sum(npergroup)
    for(i in 1:length(r)){
      n = npergroup[i]

      A.r <- sA*rmvn(n,sigma=diag(2)+S2*r[i])
      C.r <- stats::rnorm(n,sd=sC);	C.r <- cbind(C.r,C.r)
      E.r <- cbind(stats::rnorm(n,sd=sE),stats::rnorm(n,sd=sE))

      y.r <- mu + A.r + C.r + E.r


      r_ <- rep(r[i],n)

      data.r<-data.frame(A.r,C.r,E.r,y.r,r_)
      names(data.r)<-c("A1","A2","C1","C2","E1","E2","y1","y2","r")
      datalist[[i]] <- data.r
      names(datalist)[i]<-paste0("datar",r[i])
    }
    merged.data.frame = Reduce(function(...) merge(..., all=T), datalist)
    merged.data.frame$id<-id
  }else{
    id=1:length(r_vector)
    data_vector=data.frame(id,r_vector)
    data_vector$A.r1<-as.numeric(NA)
    data_vector$A.r2<-as.numeric(NA)
    unique_r= matrix(unique(r_vector))
    for(i in 1:length(unique_r)){
      n=length(r_vector[r_vector==unique_r[i]])
      A.rz <- sA*rmvn(n,sigma=diag(2)+S2*unique_r[i])
      data_vector$A.r1[data_vector$r_vector==unique_r[i]] <- A.rz[,1]
      data_vector$A.r2[data_vector$r_vector==unique_r[i]] <- A.rz[,2]
    }
    n=length(r_vector)
    A.r<-matrix(c(data_vector$A.r1,data_vector$A.r2),ncol=2)
    C.r <- stats::rnorm(n,sd=sC);	C.r <- cbind(C.r,C.r)
    E.r <- cbind(stats::rnorm(n,sd=sE),stats::rnorm(n,sd=sE))

      y.r <- mu + A.r + C.r + E.r

    data.r<-data.frame(id,A.r,C.r,E.r,y.r,r_vector)
    names(data.r)<-c("id","A1","A2","C1","C2","E1","E2","y1","y2","r")
    datalist[[i]] <- data.r
    names(datalist)[i]<-paste0("datar",r[i])

    merged.data.frame = data.r
  }

  return(merged.data.frame)
}
