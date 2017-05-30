library(RMark)

##be sure to run "R_mark_formatting.R" prior
MarkPath="C:/"

######################################
str(woodcock)

######################################


run.woodcock=function()
{
  ##processes data into MARK format
  wc.processed=process.data(woodcock, model='Known', groups=c("sex"))
  ##wc.processed$nocc <- number of occasions
  
  ##generates design matrix
  wc.ddl = make.design.data(wc.processed)
  
  nm = model.matrix(~1, wc.ddl$S)
  sm = model.matrix(~sex, wc.ddl$S)
  
  ##specify variables for models
  S.dot=list(formula=~1)
  S.sex=list(formula=~sex)
  S.weight=list(formula=~weight) 
  S.age=list(formula=~age)
  S.tag=list(formula=~Tag)
  S.sex.weight=list(formula=~sex+weight)
  S.w.age = list(formula=~w.age)
  
  
  ##create model
  w.cml=create.model.list("Known")
  
  wc.results=mark.wrapper(w.cml, data=wc.processed, ddl=wc.ddl)

  return(wc.results)

}

wc.results=run.woodcock()
