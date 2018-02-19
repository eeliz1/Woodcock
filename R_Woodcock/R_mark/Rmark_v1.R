library(RMark)

##be sure to run "R_mark_formatting.R" prior
MarkPath="C:/Program Files (x86)/MARK"

######################################
str(woodcock)

######################################

reset = woodcock
woodcock = reset

sub1 = woodcock[which(woodcock$site=="SHER"),]
sub2 = woodcock[which(woodcock$site=="DERI"),]
sub3 = woodcock[which(woodcock$site=="TENS"),]

##change woodcock to sub1-3 and run each one once
woodcock = sub3


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
  #S.site = list(formula=~site)
  #S.site.sex = list(formula=~site+sex)
  
  
  ##create model
  w.cml=create.model.list("Known")
  
  wc.results=mark.wrapper(w.cml, data=wc.processed, ddl=wc.ddl)

  return(wc.results)

}

wc.results=run.woodcock()



orig = wc.results
orig$S.sex

##reults after running sub1
resSHER = wc.results
resSHER$S.sex

##results after running sub2
resDERI = wc.results
resDERI$S.dot


##results after running sub3
resTENS = wc.results
resTENS$S.dot




