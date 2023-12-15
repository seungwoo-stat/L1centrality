#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(.dhatestimate)]]
NumericVector dhatestimate(NumericVector groupn, NumericVector groupmean) {
  int i, j, k, jup, jdown, tmpi, *cumn;
  double tmpd, *cumsum, weightmean;

  cumsum = R_Calloc((groupn.length() + 1), double);
  cumn = R_Calloc((groupn.length() + 1), int);
  cumsum[0] = 0.0;
  cumn[0] = 0;
  tmpd = 0.0;
  tmpi = 0;

  for (i = 0; i < groupn.length(); i++) {
    tmpd += groupn[i]*groupmean[i];
    tmpi += groupn[i];
    cumsum[i + 1] = tmpd;
    cumn[i + 1] = tmpi;
  }

  for(i=0;i<groupn.length();i++){
    if(i==0){
      j=i+1;
      while(groupmean[i]>groupmean[j]){
        weightmean = (cumsum[j+1]-cumsum[i]) / (cumn[j+1]-cumn[i]);
        for(k=i;k<=j;k++){
          groupmean[k] = weightmean;
        }
        j++;
      }
    }else if(i==groupn.length()-1){
      j=i-1;
      while(groupmean[i] < groupmean[j]){
        weightmean = (cumsum[i+1]-cumsum[j])/(cumn[i+1]-cumn[j]);
        for(k=j;k<=i;k++){
          groupmean[k] = weightmean;
        }
        j--;
      }
    }else{
      jup=i+1;
      while(groupmean[i] > groupmean[jup]){
        weightmean = (cumsum[jup+1]-cumsum[i])/(cumn[jup+1]-cumn[i]);
        for(k=i;k<=jup;k++){
          groupmean[k] = weightmean;
        }
        jup++;
        if(jup == groupn.length()){
          jup = groupn.length()-1;
          break;
        }
      }
      jdown=i-1;
      while(groupmean[i] < groupmean[jdown]){
        weightmean = (cumsum[jup+1]-cumsum[jdown])/(cumn[jup+1]-cumn[jdown]);
        for(k=jdown;k<=jup;k++){
          groupmean[k] = weightmean;
        }
        jdown--;
      }
    }
  }
  R_Free(cumsum);
  R_Free(cumn);
  return groupmean;
}
