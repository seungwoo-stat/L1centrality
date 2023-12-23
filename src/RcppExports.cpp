// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// dhatestimate
NumericVector dhatestimate(NumericVector groupn, NumericVector groupmean);
RcppExport SEXP _L1centrality_dhatestimate(SEXP groupnSEXP, SEXP groupmeanSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type groupn(groupnSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type groupmean(groupmeanSEXP);
    rcpp_result_gen = Rcpp::wrap(dhatestimate(groupn, groupmean));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_L1centrality_dhatestimate", (DL_FUNC) &_L1centrality_dhatestimate, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_L1centrality(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
