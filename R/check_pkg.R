

check_pkg<- function(pkg,install=F ) {
          
    if(!require(pkg)) {
      if(install!=F){
      message("installing the 'readr' package")
      install.packages("readr")
      }
      if(install ==F){
      }
      stop(paste0("the '",pkg,"' package needs to be installed first"))
    }
      
}
