## R CMD check results

0 errors | 0 warnings | 0 notes

* Prior versions' vignette failures were caused by an api 
  call through `tidycensus`, but I have replaced the 
  `tidycensus` call with a locally stored file, so that
  should no longer be an issue.