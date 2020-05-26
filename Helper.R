library(Rcpp)
library(RcppArmadillo)
cppFunction(depends = "RcppArmadillo", 'arma::mat absDiff(arma::mat m1, arma::mat m2) {
  int r = m1.n_rows; 
  int c = m1.n_cols;
  if (!(r == m2.n_rows && c ==m2.n_cols)) {
    stop("m1 and m2 must be of the same dimension");
  }
    
   arma::mat dest(r,c);
   for(int i=0; i < r; i++) {
    
    for(int j=0; j < c; j++) {
      dest(i, j) = abs(m1(i,j) - m2(i,j));
    }
    
   }
   return dest;
   
  
}')

cppFunction(depends = "RcppArmadillo",
            "int countNonZero(arma::mat inputMatrix) {
            arma::vec v = inputMatrix.elem(find(inputMatrix > 0));
return v.size();  
}")