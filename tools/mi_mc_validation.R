# Run from the repository root with Rscript tools/mi_mc_validation.R OUTPUT_DIR
# Pilot study: 4 scenarios x 50 replications, 20 imputations, 5 iterations.
# Only unstandardized indirect effects are assessed. This is not a release
# guarantee or a substitute for a larger simulation study.
library(manymome)
# Load this checkout, not the installed release.
pkgload::load_all('.', quiet=TRUE)
args <- commandArgs(trailingOnly=TRUE)
out_dir <- if (length(args)) args[1] else tempdir()
dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)
scenarios <- expand.grid(n=c(100,300), a=c(0,.5), stringsAsFactors=FALSE)
# Start a fresh run; existing result files in out_dir are overwritten.
results <- list()
for (s in seq_len(nrow(scenarios))) {
  for (i in 1:50) {
    set.seed(100000+s*1000+i)
    n <- scenarios$n[s]; a <- scenarios$a[s]; b <- .4
    x <- rnorm(n); z <- rnorm(n)
    m <- a*x+.4*z+rnorm(n)
    y <- .2*x+b*m+.2*z+rnorm(n)
    d <- data.frame(x,m,y,z)
    d$m[runif(n)<plogis(-.7+.6*z)] <- NA
    d$y[runif(n)<plogis(-1.4+.4*x)] <- NA
    ans <- tryCatch({
      q <- suppressWarnings(q_simple_mediation(x='x',m='m',y='y',cov='z',data=d,
                missing='mi',mice_args=list(m=20,maxit=5),mi_seed=200000+s*1000+i,
                seed=300000+s*1000+i,boot_ci=FALSE,mc_ci=FALSE,progress=FALSE))
      fit <- get_fit(q)
      cf <- methods::getMethod('coef','lavaan.mi',where=asNamespace('lavaan.mi'))(fit,
                     omit.imps=c('no.conv','no.se'))
      vc <- manymome:::get_vcov(fit)
      set.seed(300000+s*1000+i)
      draws <- MASS::mvrnorm(20000,mu=cf,Sigma=vc)
      # Same joint normal MC generator as do_mc; omit expensive standardized outputs.
      ai <- which(names(cf)=='m~x'); bi <- which(names(cf)=='y~m')
      if(length(ai)!=1 || length(bi)!=1) stop('Unexpected path coefficient names')
      ci <- quantile(draws[,ai]*draws[,bi],c(.025,.975),names=FALSE)
      data.frame(scenario=s,replicate=i,n=n,a=a,true=a*b,
                 estimate=as.numeric(coef(q$ind_out$ustd)),lo=ci[1],hi=ci[2],
                 used=q$mi_info$used,error='')
    },error=function(e) data.frame(scenario=s,replicate=i,n=n,a=a,true=a*b,
                     estimate=NA,lo=NA,hi=NA,used=NA,error=conditionMessage(e)))
    results[[length(results)+1]] <- ans
    if (i %% 10 == 0) {
      write.csv(do.call(rbind,results),file.path(out_dir,'mi-mc-coverage-replicates.csv'),row.names=FALSE)
      cat('scenario',s,'replicate',i,'completed\n'); flush.console()
    }
  }
}
r <- do.call(rbind,results)
summary <- do.call(rbind,lapply(split(r,r$scenario),function(d) {
  ok <- is.finite(d$estimate); v <- d[ok,]; coverage <- mean(v$lo<=v$true & v$hi>=v$true)
  data.frame(n=d$n[1],a=d$a[1],true=d$true[1],attempted=nrow(d),successful=sum(ok),
             bias=mean(v$estimate-v$true),rmse=sqrt(mean((v$estimate-v$true)^2)),
             coverage=coverage,coverage_mcse=sqrt(coverage*(1-coverage)/nrow(v)),
             rejection=mean(v$lo>0 | v$hi<0),mean_width=mean(v$hi-v$lo))
}))
write.csv(summary,file.path(out_dir,'mi-mc-coverage-summary.csv'),row.names=FALSE)
print(summary)
