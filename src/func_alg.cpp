#include <Rcpp.h>
#include <cmath> 
#include <stdexcept>

using namespace Rcpp;


/*Below is a code for compiling the Kloke's Algorithm 
 * More about is available at 
 * METHODS AND APPLICATIONS OF TOPOLOGICAL DATA ANALYSIS by Jennifer Novak Kloke: 
 *  https://stacks.stanford.edu/file/druid:yg805jw1021/mythesis-augmented.pdf
 * 
 * and meanshift algorithm 
 * Distance between points is a Euclidean distance 
 */



NumericMatrix gradient( NumericMatrix setX, NumericMatrix setP, double sigma){
  int lenRowP=setP.nrow();
  int lenRowX=setX.nrow();
  int lenCol=setX.ncol(); 
  NumericMatrix grad(lenRowX, lenCol);
  for(int x=0; x<lenRowX; x++){
    NumericVector sum(lenCol);
    for(int p=0; p< lenRowP;p++ ){
      // ------DISTANCE CALCULATOR-------------
      double dist=0;
      for(int j=0; j<lenCol; j++){
        double X=setX(x,j);
        double P=setP(p, j);
       
        dist=dist+(X-P)*(X-P); 
       
      }
      // ------END OF DISTANCE CALCULATOR------
      for(int i=0; i<lenCol; i++){
        sum[i]=sum[i]+(-(setX(x,i)-setP(p,i))/(sigma*sigma))*exp(-dist/(2*sigma*sigma));
        
      }
    }
    grad(x, _)=sum;
  }
  return grad;
}


NumericMatrix KlokeShift(NumericMatrix sample ,NumericMatrix set, double sigma, double omega, double step){
  int ncol=sample.ncol();
  int nrowP=set.nrow();
  int nrowX=sample.nrow();
  NumericMatrix V1=gradient(sample, set, sigma);
  
  NumericMatrix sampleC=clone(sample);
  NumericMatrix V2=gradient( sampleC, sample, sigma);
 
  NumericMatrix shift(nrowX, ncol);
  int SetSize=ncol*nrowX;
  for(int i=0; i<SetSize; i++){
    shift[i]=V1[i]/nrowP-(V2[i]*omega)/nrowX;
    
  }
  
  return shift;
}

double MaxMagnitude(NumericMatrix X){
  int ncol=X.ncol();
  int nrow=X.nrow();
  double max=0;
  for(int i=0; i<nrow; i++){
    double sum=0;
    for (int j=0; j<ncol; j++){
      sum=sum+X(i,j)*X(i,j);
    }
    double mag=sqrt(sum);
    if(mag>max){
      max=mag;
    }
  }
  return max;
}

// [[Rcpp::export]]
NumericMatrix KlokeAlg(NumericMatrix sample,NumericMatrix set, double sigma, double omega, int iter, double step){
  /* Kloke's algorithm 
   * set- the whole set 
   * sample- sample of the set 
   * sigma - one of the parameters (tough one - just sigma)
   * omega - one of the parameters (reduces shift)
   * iter - number of iterations
   * step - percentage od maximum displacement 
   */
  
  //----------COMPUTING MAX DISTANCE SHIFT-----------
  NumericMatrix FindMax=KlokeShift(sample, set, sigma, omega, step);
  double M=MaxMagnitude(FindMax);
  double c=step*M;
 
  //----------FINISH of determining c and M ---------
  // Cloning sample to Sn
  NumericMatrix Sn= clone(sample);
 
  int lenSet=set.nrow();
  int lenSample=sample.nrow();
  {
    using namespace std;
    if(lenSet<lenSample){
      throw invalid_argument("Yooo, dude probably you misplaced sample set with the original set, try to switch them.");
    }
  }
  int lenCol=sample.ncol();
  int size=lenCol*lenSample;
  //----------COMPUTING SHIFT AND SHIFTING POINTS ---
  for(int i=0; i<iter; i++){
    NumericMatrix shifting=KlokeShift(Sn, set, sigma, omega, step);
    
    for(int j=0; j<size; j++){
      Sn[j]=Sn[j]+c*shifting[j]/M;
    }
  }
  //---------FINISH computing shift------------------
  return Sn;
  
}

// [[Rcpp::export]]
NumericMatrix MeanShiftAlg(NumericMatrix set, double sigma, int iter, double step){
  /*Mean Shift Algorithm, set is a inital data 
    sigma is one of the parameters, 
    iter - is number of iterations
    step - is a percentage of maximum shift, which then shifts be that 
   */
  
  // Clonning set to Dn which will be shifted 
  NumericMatrix Dn=clone(set);
  
  int nrow=set.nrow();
  int ncol=set.ncol();
  //-------COMPUTING MAX DISTANCE SHIFT --------------
  NumericMatrix FindMax=gradient(set, set, sigma)/nrow;
  
  double M=MaxMagnitude(FindMax);
  double c=step*M;
  //-------FINISH of computing max distance shift-----
  //-------COMPUTING SHIFT AND SHIFTING SET-----------
  int size=nrow*ncol;
  for(int i=0; i< iter; i++){
    // Computing gradient
    NumericMatrix shift=gradient(set, set, sigma);
    // Shifting Points
    for(int j=0; j<size; j++){
      Dn[j]=Dn[j]+c*shift[j]/(nrow*M);
    }
  }
  //-------FINISH computing shift---------------------
  return Dn;
}

// [[Rcpp::export]]
NumericMatrix KDE(NumericMatrix set, double sigma){
  /* Kernal Density Estimator
   * set - set itself
   * sigma - sigma
   */
  
  int nrow=set.nrow();
  int ncol=set.ncol();
  NumericMatrix Out(nrow, ncol+1);
  for(int x=0; x<nrow; x++){
    double sum=0;
    for(int p=0; p<nrow; p++){
      double dist=0;
      for(int c=0; c<ncol; c++){
        dist=dist+(set(x,c)-set(p,c))*(set(x,c)-set(p,c));
      }
      sum=sum+exp(-dist/(2*sigma*sigma))/nrow;
    }
    for(int i=0; i<ncol; i++){
      Out(x,i)=set(x,i);
    }
    Out(x, ncol)=sum;
  }
  return Out;
}

