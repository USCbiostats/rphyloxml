// [[Rcpp::plugins(cpp11)]]

#include <Rcpp.h>
#include <unordered_map>
using namespace Rcpp;


// [[Rcpp::export]]
Rcpp::XPtr< std::unordered_map< int,int > > new_table(int i) {

  // Creating a map pointer
  std::unordered_map< int, int >* x = new std::unordered_map< int, int >;

  // Creating the R external pointer
  Rcpp::XPtr< std::unordered_map< int , int > > p(x, true);

  return p;
}


// [[Rcpp::export]]
Rcpp::XPtr< std::unordered_map< int , int > > set_element_in_table(
    int key,
    int value,
    Rcpp::XPtr< std::unordered_map< int , int > > table) {

  // We access the elements by square brackets
  (*table)[key] = value;

  return table;
}


// [[Rcpp::export]]
int get_element_from_table(
    int key,
    Rcpp::XPtr< std::unordered_map< int , int > > table) {

  // The find member function allows us to access to the elements
  // The 'second' member returns the value (the 'first' the key)
  // More on this function here:
  // http://en.cppreference.com/w/cpp/container/map/find
  return table->at(key);
}

/***R

# Creating the object
tab <- new_table(1)

# Setting values
tab <- set_element_in_table(key = 10000, value=200, tab)
tab <- set_element_in_table(key = -1, value=500, tab)

# Getting values
get_element_from_table(key = 10000, tab)
get_element_from_table(key = -1, tab)
get_element_from_table(key = 0, tab)

*/


