library(RMark)

##be sure to run "R_mark_formatting.R" prior

run.woodcock=function()
{
  ##processes data into MARK format
  wc.processed=process.data(woodcock, model='Known', groups=c("sex", "tag"))
    ##wc.processed$nocc <- number of occasions
  
  ##generates design matrix
  wc.ddl = make.design.data(wc.processed)

   
  
}
