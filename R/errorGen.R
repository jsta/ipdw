#'@name errorGen
#'@title Generate interpolation error stats from validation datasets
#'@author Joseph Stachelek
#'@param finalraster RasterLayer object
#'@param validation.spdf SpatialPointsDataFrame
#'@param validation.data data.frame
#'@param plot logical. Plot comparison?
#'@param title Plot labels
#'@return List of error statistics
#'@export
#'@examples
#'validation.data<-data.frame(rnorm(10,mean=0.2,sd=1))
#'names(validation.data)<-c("validation")
#'validation.spdf<-validation.data
#'validation.data<-as.numeric(unlist(validation.data))
#'xy<-data.frame(x=c(0:9),y=rep(1,10))
#'coordinates(validation.spdf)<-xy
#'
#'m<-matrix(NA,1,10)
#'out.ras<-raster(m,xmn=0,xmx=ncol(m),ymn=0,ymx=nrow(m))
#'out.ras[]<-validation.data+rnorm(ncell(out.ras),mean=0.01,sd=0.2)
#'
#'valid.stats<-errorGen(out.ras,validation.spdf,validation.data,plot=TRUE,title="Validation Plot")
#'valid.stats

'errorGen'<-function(finalraster,validation.spdf,validation.data,plot=FALSE,title=""){
  
  predicted<-extract(finalraster,validation.spdf)
  
  mvp<-data.frame(cbind(validation.data,predicted))
  fit<-lm(mvp[,2]~mvp[,1])
  r2<-round(summary(fit)$r.squared,2)
  rmse<-round(sqrt(mean((mvp[,1]-mvp[,2])^2,na.rm=TRUE)),2)
  logmse<-round(log(mean((mvp[,1]-mvp[,2])^2,na.rm=TRUE)),2)
  pe<-round(100*mean(mvp[,1]/mvp[,2],na.rm=TRUE),2)
  stats<-data.frame(c("r2",r2,"rmse", rmse,"pe",pe,"logmse",logmse))
  names(stats)<-c("validation stats")
  
  stats<-list(stats,mvp)
  
  
  #optional plotting
  if(plot==TRUE){
    plot(mvp[,1],mvp[,2],ylim=range(mvp[,2],na.rm=T),xlim=range(mvp[,1],na.rm=T),ylab="Interpolated",xlab="Measured",main=title)
    abline(fit,col="red")
    abline(a=0,b=1)
  }
  
  return(stats)
}