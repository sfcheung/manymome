# Covariance/imputation sensitivity; does not change package defaults.
args <- commandArgs(TRUE)
out <- args[1]; dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(args[2]=='summarize') {
  r <- do.call(rbind,lapply(list.files(out,'^worker-[0-9]+[.]csv$',full.names=TRUE),read.csv))
  write.csv(r,file.path(out,'replicates.csv'),row.names=FALSE)
  tab <- do.call(rbind,lapply(split(r,interaction(r$scenario,r$method,r$covariance,drop=TRUE)),function(d) {
    v <- d[is.finite(d$estimate)&is.finite(d$lo)&is.finite(d$hi),]
    p <- mean(v$lo<=.2 & v$hi>=.2); nn <- nrow(v); z <- qnorm(.975)
    center <- (p+z*z/(2*nn))/(1+z*z/nn)
    half <- z*sqrt(p*(1-p)/nn+z*z/(4*nn*nn))/(1+z*z/nn)
    data.frame(scenario=d$scenario[1],n=d$n[1],high=d$high[1],method=d$method[1],
               covariance=d$covariance[1],attempted=nrow(d),successful=nn,
               bias=mean(v$estimate-.2),coverage=p,lower=center-half,upper=center+half,
               width=mean(v$hi-v$lo))
  }))
  rownames(tab)<-NULL;write.csv(tab,file.path(out,'summary.csv'),row.names=FALSE)
  print(tab);quit(save='no')
}
pkgload::load_all('.',quiet=TRUE)
worker <- as.integer(args[2]); workers <- as.integer(args[3])
jobs <- expand.grid(scenario=c(3,4,5),method=c('pmm','norm'),replicate=1:100,
                     stringsAsFactors=FALSE)
jobs <- jobs[(seq_len(nrow(jobs))-1) %% workers == worker-1,,drop=FALSE]
res <- list()
coef_mi <- methods::getMethod('coef','lavaan.mi',where=asNamespace('lavaan.mi'))
vcov_mi <- methods::getMethod('vcov','lavaan.mi',where=asNamespace('lavaan.mi'))
for(j in seq_len(nrow(jobs))) {
  s <- jobs$scenario[j]; i <- jobs$replicate[j]; method <- jobs$method[j]
  n <- if(s==4) 300 else 100; high <- s==5
  set.seed(710000+s*10000+i)
  x<-rnorm(n);z<-rnorm(n);m1<-.5*x+.4*z+rnorm(n)
  m2<-.3*x+.2*z+rnorm(n) # consume the same RNG draws as the expanded study
  y<-.2*x+.4*m1+.2*z+rnorm(n)
  d<-data.frame(x,m1,m2,y,z)
  d$m1[runif(n)<plogis(if(high) .1+.6*z else -.7+.6*z)]<-NA
  d$y[runif(n)<plogis(if(high) -.3+.4*x else -1.4+.4*x)]<-NA
  mice_args<-list(m=20,maxit=5)
  if(method=='norm') mice_args$method<-c(x='',m1='norm',y='norm',z='')
  rows <- tryCatch({
    q<-suppressWarnings(q_simple_mediation(x='x',m='m1',y='y',cov='z',data=d,
       missing='mi',mice_args=mice_args,mi_seed=810000+s*10000+i,
       boot_ci=FALSE,mc_ci=FALSE,progress=FALSE))
    fit<-get_fit(q);cf<-coef_mi(fit,omit.imps=c('no.conv','no.se'))
    labs<-c('m1~x','y~m1');cf<-cf[labs]
    do.call(rbind,lapply(c('scaled','rubin'),function(pool) {
      vv<-vcov_mi(fit,type='pooled',scale.W=pool=='scaled',
                  omit.imps=c('no.conv','no.se'))[labs,labs]
      set.seed(910000+s*10000+i)
      dr<-MASS::mvrnorm(20000,mu=cf,Sigma=vv)
      ci<-quantile(dr[,1]*dr[,2],c(.025,.975),names=FALSE)
      data.frame(scenario=s,replicate=i,n=n,high=high,method=method,
                 covariance=pool,estimate=prod(cf),lo=ci[1],hi=ci[2],error='')
    }))
  },error=function(e) data.frame(scenario=s,replicate=i,n=n,high=high,method=method,
                      covariance=c('scaled','rubin'),estimate=NA,lo=NA,hi=NA,
                      error=conditionMessage(e)))
  res[[length(res)+1]]<-rows
  write.csv(do.call(rbind,res),file.path(out,paste0('worker-',worker,'.csv')),row.names=FALSE)
  cat('job',j,'/',nrow(jobs),'done\n');flush.console()
}
