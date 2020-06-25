#include <RcppArmadillo.h>
#include <deque>
#include <tuple>
#include <algorithm>
#include <cmath>
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
  std::deque<std::tuple<int, int>> container;
  for (arma::uword i = 0; i < r; i++) {
    for (arma::uword j = 0; j < c; j++) {
      if (image(i, j)) {
        // integer casting -- be careful when processing a large image!
        int i1 = (int) i;
        int j1 = (int) j;
        container.emplace_back(i1, j1);
      }
    }
  }

  std::tuple<int, int> head = container.front();
  xCoords.push_back(std::get<0>(head));
  yCoords.push_back(std::get<1>(head));
  container.pop_front();

  while(!container.empty()) {
    int i = xCoords[xCoords.length()-1];
    int j = yCoords[yCoords.length()-1];
    std::sort(container.begin(), container.end(), [&i, &j](const std::tuple<int, int> a, const std::tuple<int, int> b) {
          int a1 = std::get<0>(a);
          int a2 = std::get<1>(a);

          int b1 = std::get<0>(b);
          int b2 = std::get<1>(b);
          
          return (std::pow(a1-i, 2) + std::pow(a2-j, 2)) < (std::pow(b1-i, 2) + std::pow(b2-j, 2));

        });
    std::tuple<int, int> closest = container.front();
    xCoords.push_back(std::get<0>(closest));
    yCoords.push_back(std::get<1>(closest));
    container.pop_front();
  }

  DataFrame coords = DataFrame::create(Named("x_coords")=xCoords,
      Named("y_coords")=yCoords);
  return coords;
}
