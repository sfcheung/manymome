# Expanded MI/MC validation; run from repository root.
# Rscript tools/mi_mc_validation_extended.R OUTPUT_DIR WORKER_ID WORKERS
# Rscript tools/mi_mc_validation_extended.R OUTPUT_DIR summarize
args <- commandArgs(TRUE)
out_dir <- args[1]
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
configs <- data.frame(
  id=1:12,
  model=c(rep('simple',7),'parallel','serial','simple','parallel','serial'),
  n=c(100,300,100,300,100,100,100,200,200,200,200,200),
  a=c(0,0,.5,.5,.5,.5,.05,.5,.5,.5,.5,.5),
  imps=c(20,20,20,20,20,5,20,20,20,20,20,20),
  high=c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),
  standardized=c(rep(FALSE,9),TRUE,TRUE,TRUE),
  reps=c(200,200,500,200,100,100,100,100,100,50,50,50))
write.csv(configs,file.path(out_dir,'configuration.csv'),row.names=FALSE)
if (identical(args[2], 'summarize')) {
  files <- list.files(out_dir, '^worker-[0-9]+[.]csv$', full.names=TRUE)
  r <- do.call(rbind,lapply(files,read.csv,na.strings='NA'))
  # Default fixed.x=TRUE conditions on the observed exogenous design.
  # Keep population-standardized targets for sensitivity, but evaluate the
  # standardized interval against its conditional-design target.
  r$population_true <- r$true
  r$target_definition <- "unstandardized population effect"
  for (sid in 10:12) for (ii in unique(r$replicate[r$scenario==sid])) {
    cfg <- configs[sid,]
    set.seed(710000+sid*10000+ii)
    xx <- rnorm(cfg$n); zz <- rnorm(cfg$n)
    ss <- stats::cov(cbind(xx,zz))*(cfg$n-1)/cfg$n
    cx <- if(cfg$model=="simple") .2+.4*cfg$a else if(cfg$model=="parallel")
      .29+.4*cfg$a else .29+.49*cfg$a
    cz <- if(cfg$model=="simple") .36 else if(cfg$model=="parallel") .42 else .456
    residual <- if(cfg$model=="simple") .4^2+1 else if(cfg$model=="parallel")
      .4^2+.3^2+1 else .49^2+.3^2+1
    sy <- sqrt(cx^2*ss[1,1]+cz^2*ss[2,2]+2*cx*cz*ss[1,2]+residual)
    sx <- sqrt(ss[1,1])
    idx <- which(r$scenario==sid & r$replicate==ii)
    raw <- c(m1=cfg$a*.4,m2=.3*.3,m1_m2=cfg$a*.3*.3)
    r$true[idx] <- raw[r$effect[idx]] * ifelse(r$scale[idx] %in% c("stdx","stdxy"),sx,1) /
                               ifelse(r$scale[idx] %in% c("stdy","stdxy"),sy,1)
    r$target_definition[idx] <- ifelse(r$scale[idx]=="ustd",
      "unstandardized population effect", "conditional on observed exogenous design (fixed.x=TRUE)")
  }
  write.csv(r,file.path(out_dir,'replicates.csv'),row.names=FALSE)
  groups <- split(r,interaction(r$scenario,r$effect,r$scale,drop=TRUE))
  sums <- lapply(groups,function(d) {
    v <- d[is.finite(d$estimate)&is.finite(d$lo)&is.finite(d$hi),]
    nn <- nrow(v); covered <- sum(v$lo<=v$true & v$hi>=v$true)
    p <- if(nn) covered/nn else NA_real_; z <- qnorm(.975)
    center <- (p+z*z/(2*nn))/(1+z*z/nn)
    half <- z*sqrt(p*(1-p)/nn+z*z/(4*nn*nn))/(1+z*z/nn)
    data.frame(scenario=d$scenario[1],model=d$model[1],n=d$n[1],imps=d$imps[1],
      effect=d$effect[1],scale=d$scale[1],target_definition=d$target_definition[1],
      mean_true=mean(d$true),attempted=nrow(d),successful=nn,
      bias=mean(v$estimate-v$true),rmse=sqrt(mean((v$estimate-v$true)^2)),
      coverage=p,coverage_lower=center-half,coverage_upper=center+half,
      rejection=mean(v$lo>0|v$hi<0),mean_width=mean(v$hi-v$lo),
      population_target_coverage=mean(v$lo<=v$population_true & v$hi>=v$population_true))
  })
  tab <- do.call(rbind,sums); rownames(tab) <- NULL
  write.csv(tab,file.path(out_dir,'summary.csv'),row.names=FALSE)
  print(tab); quit(save='no')
}
pkgload::load_all('.',quiet=TRUE)
worker <- as.integer(args[2]); workers <- as.integer(args[3])
jobs <- do.call(rbind,lapply(seq_len(nrow(configs)),function(s)
  data.frame(s=s,i=seq_len(configs$reps[s]))))
jobs <- jobs[(seq_len(nrow(jobs))-1L) %% workers == worker-1L,,drop=FALSE]
results <- list()
for (j in seq_len(nrow(jobs))) {
  s <- jobs$s[j]; i <- jobs$i[j]; cfg <- configs[s,]
  set.seed(710000+s*10000+i)
  n <- cfg$n; a <- cfg$a
  x <- rnorm(n); z <- rnorm(n)
  m1 <- a*x+.4*z+rnorm(n)
  m2 <- .3*x+.2*z+if(cfg$model=='serial') .3*m1 else 0
  m2 <- m2+rnorm(n)
  y <- .2*x+.4*m1+.2*z+if(cfg$model=='simple') 0 else .3*m2
  y <- y+rnorm(n)
  d <- data.frame(x,m1,m2,y,z)
  d$m1[runif(n)<plogis(if(cfg$high) .1+.6*z else -.7+.6*z)] <- NA
  d$y[runif(n)<plogis(if(cfg$high) -.3+.4*x else -1.4+.4*x)] <- NA
  if(cfg$model!='simple') d$m2[runif(n)<plogis(-1.4+.4*x)] <- NA
  path_labels <- if(cfg$model=='simple') 'm1' else if(cfg$model=='parallel')
    c('m1','m2') else c('m1','m2','m1_m2')
  truths <- if(cfg$model=='simple') a*.4 else if(cfg$model=='parallel')
    c(a*.4,.3*.3) else c(a*.4,.3*.3,a*.3*.3)
  names(truths) <- path_labels
  vy <- if(cfg$model=='simple') (.2+.4*a)^2+.36^2+.4^2+1 else
    if(cfg$model=='parallel') (.29+.4*a)^2+.42^2+.4^2+.3^2+1 else
      (.29+.49*a)^2+.456^2+.49^2+.3^2+1
  paths <- lapply(path_labels,function(p) switch(p,m1='m1',m2='m2',m1_m2=c('m1','m2')))
  scales <- if(cfg$standardized) c('ustd','stdx','stdy','stdxy') else 'ustd'
  row <- tryCatch({
    fun <- switch(cfg$model,simple=q_simple_mediation,parallel=q_parallel_mediation,
                  serial=q_serial_mediation)
    warnings <- character()
    q <- withCallingHandlers(fun(x='x',m=if(cfg$model=='simple') 'm1' else c('m1','m2'),
       y='y',cov='z',data=d,missing='mi',mice_args=list(m=cfg$imps,maxit=5),
       mi_seed=810000+s*10000+i,seed=910000+s*10000+i,
       boot_ci=FALSE,mc_ci=FALSE,progress=FALSE),warning=function(w) {
         warnings <<- c(warnings,conditionMessage(w)); invokeRestart('muffleWarning')
       })
    fit <- get_fit(q)
    if(cfg$standardized) {
      mc <- do_mc(fit,R=2000,seed=910000+s*10000+i,progress=FALSE)
    } else {
      cf <- methods::getMethod('coef','lavaan.mi',where=asNamespace('lavaan.mi'))(
               fit,omit.imps=c('no.conv','no.se'))
      vc <- manymome:::get_vcov(fit)
      set.seed(910000+s*10000+i)
      draws <- MASS::mvrnorm(20000,mu=cf,Sigma=vc)
    }
    do.call(rbind,lapply(seq_along(paths),function(k) do.call(rbind,lapply(scales,function(scale) {
      standardized_y <- scale %in% c('stdy','stdxy')
      truth <- truths[k]/if(standardized_y) sqrt(vy) else 1
      if(cfg$standardized) {
        eff <- indirect_effect(x='x',m=paths[[k]],y='y',fit=fit,
          ci_type='mc',ci_out=mc,standardized_x=scale %in% c('stdx','stdxy'),
          standardized_y=standardized_y,R=2000,progress=FALSE)
        estimate <- as.numeric(coef(eff)); ci <- as.numeric(confint(eff))
      } else {
        nodes <- c('x',paths[[k]],'y')
        coeff <- paste0(nodes[-1],'~',nodes[-length(nodes)])
        if(!all(coeff %in% names(cf))) stop('Coefficient names differ from expected paths')
        estimate <- prod(cf[coeff]); values <- apply(draws[,coeff,drop=FALSE],1,prod)
        ci <- quantile(values,c(.025,.975),names=FALSE)
      }
      data.frame(scenario=s,replicate=i,model=cfg$model,n=n,imps=cfg$imps,
        effect=path_labels[k],scale=scale,true=unname(truth),estimate=estimate,
        lo=ci[1],hi=ci[2],used=q$mi_info$used,
        warning=paste(unique(warnings),collapse=' | '),error='')
    }))))
  },error=function(e) do.call(rbind,lapply(seq_along(paths),function(k)
       do.call(rbind,lapply(scales,function(scale) data.frame(scenario=s,replicate=i,
         model=cfg$model,n=n,imps=cfg$imps,effect=path_labels[k],scale=scale,
         true=unname(truths[k]/if(scale %in% c('stdy','stdxy')) sqrt(vy) else 1),
         estimate=NA,lo=NA,hi=NA,used=NA,warning='',error=conditionMessage(e)))))))
  results[[length(results)+1L]] <- row
  write.csv(do.call(rbind,results),file.path(out_dir,paste0('worker-',worker,'.csv')),row.names=FALSE)
  cat('worker',worker,'job',j,'/',nrow(jobs),'scenario',s,'replicate',i,'done\n')
  flush.console()
}

