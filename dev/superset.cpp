#include <Rcpp.h>
#include <Math.h>
using namespace Rcpp;

// [[Rcpp::export]]
List pwr_set_cpp(CharacterVector els) {
    
    int n_els = els.size();         
    int pwrset_card = pow(2,n_els); 
    List out(pwrset_card);          
    out[0] = StringVector::create(); 
    CharacterVector tmp;            
    int counter = 0;
    for (int i=0; i < n_els; ++i) {
        int cnt2 = counter;            // capture counter state
        for (int j =0; j <= counter; ++j) {
            cnt2++;                   // capture counter + j steps
            tmp = as<StringVector>(out[j]);
            tmp.push_back(as<std::string>(els[i]));
            out[cnt2] = tmp;
            
        }
        counter = cnt2;             // update counter state
    }
    return out;
}
