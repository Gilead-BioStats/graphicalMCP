#include <Rcpp.h>
using namespace Rcpp;

// str : Stores input string
// curr : Stores current subset
// index : Index in current subset, curr
// [[Rcpp::export]]
List power_set_cpp2(std::string str, int index = -1, std::string curr = "")
{
    int n = str.length();
 
    // base case
    if (index == n)
        return 0;
 
    // First print current subset
    curr << "\n";

    int pwrset_card = pow(2,n_els); 
    List out(pwrset_card);          
    out[0] = StringVector::create(); 
 
    // Try appending remaining characters
    // to current subset
    for (int i = index + 1; i < n; i++) {
 
        curr += str[i];
        out[i] = curr;
        power_set_cpp2(str, i, curr);
 
        // Once all subsets beginning with
        // initial "curr" are printed, remove
        // last character to consider a different
        // prefix of subsets.
        curr.erase(curr.size() - 1);
    }

    return curr;
}
