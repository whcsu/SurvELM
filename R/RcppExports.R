# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

bjimpute <- function(y, cen, x, inibeta) {
    .Call(`_SurvELM_bjimpute`, y, cen, x, inibeta)
}

mm_mult <- function(x, y) {
    .Call(`_SurvELM_mm_mult`, x, y)
}

testDFtoNM <- function(x) {
    .Call(`_SurvELM_testDFtoNM`, x)
}

scaleNM <- function(x1) {
    .Call(`_SurvELM_scaleNM`, x1)
}

kernmat <- function(xtrain, kernel_type, kernel_para, xtest) {
    .Call(`_SurvELM_kernmat`, xtrain, kernel_type, kernel_para, xtest)
}

