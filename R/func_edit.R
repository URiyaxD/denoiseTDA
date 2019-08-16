#------STORING FUNCTIONS ---------
## CUSTOM FUNCTIONS

#' @export
GridGen <- function(Xlim, Ylim, by){
  # Generates Grid 
  Xseq=seq(Xlim[1], Xlim[2], by=by)
  Yseq=seq(Ylim[1], Ylim[2], by=by)
  Grid=expand.grid(Xseq,Yseq)
  return(list(Grid=Grid, Xseq=Xseq, Yseq=Yseq))
}

#' @export
CircleGen<-function(init=c(0,0), R=1, N=100){
  #Generates points on Circle
  # N - Number of Datapoints 
  # R - Radius
  # init - center position c(x,y)
  
  fr<- 360/N
  x<-c()
  y<-c()
  ang=0
  while(ang<360 ){
    x<-c(x, R*cos(ang)+init[1])
    y<-c(y, R*sin(ang)+init[2])
    ang<-ang+fr
  }
  return(matrix(c(x,y), nrow = length(x), ncol = 2))
}


#' @export
NoisyGen<-function(Data, var=0.1, mean=0){
  # ADDS NOISE----
  # Mean - mean
  # var - variability
  # Feed in matrix output a noisy matrix 
  set<-c()
  for(i in 1:length(Data[1,])){
    noise<-rnorm(length(Data[,1]), mean, var)
    set<-c(set, Data[,i]+noise)
  }
  NoisyC<- matrix(set, nrow= length(Data[,1]), ncol= length(Data[1,]))
  return(NoisyC)
}

#' @export
ListToData<-function(List){
  #list to dataframe
  Data<- data.frame(row.names = 1:length(List[[1]]))
  j<-1
  for(i in names(List)){
    Data[, ncol(Data) + 1] <-List[[j]]
    names(Data)[ncol(Data)] <- paste0(i)
    j<-j+1
  }
  
  return(Data)
}

#' @export
MatToData<-function(Mat){
  # Matrix to dataframe
  if(length(Mat[1,])==2){
    Data<- data.frame(x=Mat[,1], y=Mat[,2])
  }
  if(length(Mat[1,])==1){
    Data<- data.frame(x=Mat[,1])
  }
  if(length(Mat[1,])==3){
    Data<- data.frame(x=Mat[,1], y=Mat[,2], z=Mat[,3])
  }
  return(Data)
}


#' @export
MatSample<- function(Mat, N=100){
  #select sample of the matrix
  ncol<-length(Mat[,1])
  nrow<-length(Mat[1,])
  sam<-sample(1:nrow, N)
  selected<-matrix(Mat[sam[1],], ncol=ncol )
  for( i in 2:N){
    selected<-rbind(selected, matrix(Mat[sam[i], ], ncol=ncol))
  }
  return(selected)
}

