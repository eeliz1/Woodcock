library(RMark)

##be sure to run "R_mark_formatting.R" prior
MarkPath="C:/"

######################################
str(woodcock)

######################################

run.woodcock=function()
{
  ##processes data into MARK format
  wc.processed=process.data(woodcock, model='Known', groups=c("sex", "site")) #,"site"
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
  S.site = list(formula=~site)
  S.site.sex = list(formula=~site+sex)
  S.sitebysex = list(formula=~site*sex)
  S.tag = list(formula=~tag)
  S.tag.sex= list(formula=~tag+sex)
  
  
  ##create model
  w.cml=create.model.list("Known")
  
  wc.results=mark.wrapper(w.cml, data=wc.processed, ddl=wc.ddl)

  return(wc.results)
  real.wc.results = extract.mark.output(wc.results, model, adjust, realvcv = FALSE, vcvfile)

}

wc.results=run.woodcock()
wc.results.2= run.woodcock()
wc.results.3 = run.woodcock()



df1=woodcock[which(woodcock$site=="DERI"),]
df2=woodcock[which(woodcock$site=="PALM"),]
df3=woodcock[which(woodcock$site=="BOYC"),]
df4=woodcock[which(woodcock$site=="YANC"),]
df5=woodcock[which(woodcock$site=="SHER"),]
df6=woodcock[which(woodcock$site=="TENS"),]

