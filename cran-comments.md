## Resubmission

This is a resubmission. We got two comments from the CRAN:

1.  If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file (...)

-   We have a paper describing the methods in this package. However, the paper is under review process and cannot be found on the web, nor does it have an ISBN. Hence, we would add them to the description field of our DESCRIPTION in later versions, when the paper is published.

2.  You write information messages to the console that cannot be easily suppressed. It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object. Instead of print()/cat() rather use message()/warning() or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console. (except for print, summary, interactive functions) -> R/L1centMDS.R

-   We changed as suggested. Every console message can be suppressed when `verbose = FALSE`.

## R CMD check results

0 errors | 0 warnings | 1 note

  checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Seungwoo Kang <kangsw0401@snu.ac.kr>’
  
  New submission

  We used local machine; macOS x86_64-apple-darwin17.0 (64-bit) This is a new release.

## Downstream dependencies

There are currently no downstream dependencies for this package.
