#include <RcppArmadillo.h>

/**
 * A method that takes an image and returns a data frame of [x_coords, y_coords] 
 * from a binary image where image[x_coord, y_coord] = 1 for x_coord in x_coords, y_coord in y_coords
 * @author Yunheum Dan Seol <yunheum.seol@mail.mcgill.ca> <yunheum.seol.conseolting@gmail.com>
 */

using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
DataFrame getCurvePoints(arma::mat image) {
  IntegerVector xCoords;
  IntegerVector yCoords;
  arma::uword r = image.n_rows;
  arma::uword c = image.n_cols;

  for (arma::uword i = 0; i < r; i++) {
    for (arma::uword j = 0; j < c; j++) {
      if (image(i, j)) {
        // integer casting -- be careful when processing a large image!
        xCoords.push_back((int) i);
        yCoords.push_back((int) j);
      }
    }
  }
  DataFrame coords = DataFrame::create(Named("x_coords")=xCoords,
      Named("y_coords")=yCoords);
  return coords;
}

