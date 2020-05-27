#include <RcppArmadillo.h>

/**
 * @devised by A Fast Parallel Algorithm for Thinning Digital Patterns (1984, March).
 * T.Y.Zhang, C.Y. Shen. Retrieved 22:37, May 25, 2020, from http://www-prima.inrialpes.fr/perso/Tran/Draft/gateway.cfm.pdf
 * @original author in R Jake Drew: Image Thinning using R. (2013, April 28), from https://blog.jakemdrew.com/2013/04/28/image-thinning-using-r./. 
 * Retrived 22:37, May 25, 2020.
 * @author reimplementing in Rcpp Yunheum Dan Seol <yunheum.seol@mail.mcgill.ca> <yunheum.seol.conseolting@gmail.com>
 */

using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat abs_diff(const arma::mat& m1, const arma::mat& m2) {
  arma::uword r = m1.n_rows;
  arma::uword c = m1.n_cols;

  if (!( r == m2.n_rows && c == m2.n_cols)) {
   stop("The dimensions of m1 and m2 must m");
  }

  arma::mat dest(r,c);

  for (arma::uword i=0; i < r; i++) {
  
    for (arma::uword j=0; j < c; j++) {
      dest(i, j) = abs(m1(i, j) - m2(i, j));
    }

  }

  return dest;
}

//[[Rcpp::export]]
int countNonZero(const arma::mat& mtx) {
  arma::vec v = mtx.elem(find(mtx > 0));
  //careful with image dimension!!!
  return (int) v.size();
}

//[[Rcpp::export]]
arma::mat thinningIteration(const arma::mat& image, int iter) {
  arma::mat imageInput = image;
  arma::uword r = imageInput.n_rows - 1;
  arma::uword c = imageInput.n_cols - 1;
  for (arma::uword i = 1; i < r; i++) {
    for (arma::uword j = 1; j < c; j++) {
      double p2 = imageInput(i-1, j);
      double p3 = imageInput(i-1, j+1);
      double p4 = imageInput(i, j+1);
      double p5 = imageInput(i+1, j+1);
      double p6 = imageInput(i+1, j);
      double p7 = imageInput(i+1, j-1);
      double p8 = imageInput(i, j-1);
      double p9 = imageInput(i-1, j-1);
      int A = ((int) (((int) p2) == 0 && ((int) p3) == 1)) 
        + ((int) (((int) p3) == 0 &&  ((int) p4) == 1))
        + ((int) (((int) p4) == 0 && ((int) p5) == 1))
        + ((int) (((int) p5) == 0 && ((int) p6) == 1))
        + ((int) (((int) p6) == 0 && ((int) p7) == 1))
        + ((int) (((int) p7) == 0 && ((int) p8) == 1))
        + ((int) (((int) p8) == 0 && ((int) p9) == 1))
        + ((int) (((int) p9) == 0 && ((int) p2) == 1));
      int B = (int) p2 + (int) p3 + (int) p5 + (int) p6 + (int) p7 + (int) p8 + (int) p9;
      
      int m1, m2;
      if (iter == 0) {
        m1 = (int) p2 * (int) p4 * (int) p6;
        m2 = (int) p4 * (int) p6 * (int) p8;
      } else {
        m1 = (int) p2 * (int) p4 * (int) p8;
        m2 = (int) p2 * (int) p6 * (int) p8;
      }

      if (A == 1 && (B >= 2 && B <= 6) && m1 == 0 && m2 == 0) {
        imageInput(i, j) = 0;
      }
    }
  }
  return imageInput;
}

//[[Rcpp::export]]
arma::mat thinImage(const arma::mat& imageMatrix) {
  arma::mat im = imageMatrix;
  arma::mat prev = imageMatrix;
  arma::mat diff;
  while(true) {
    im = thinningIteration(im, 0);
    im = thinningIteration(im, 1);
    diff = abs_diff(im, prev);
    prev = im;
    
    if (countNonZero(diff) == 0) {
      break;
    }
  }
  return im;
}

