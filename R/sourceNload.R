#' Load all scripts or objects of a folder
#' 
#'
#' @param path The string path where the files to be loaded are stored. Default current directory "."
#' @param type String that can be either 'script' or 'object'
#' @param ending String with the file extension. It is assumed that scripts will end in .R and objects in .RDATA
#'
#' @return Simply loads everything that founds into the global environment
#'
#' @export
#'

sourceNload<-function(path='.', 	type="script", 	ending=if(type=="script"){ending=".R"}else if(type=="object"){ending=".RDATA"}else{ending=NULL} 	) {

if(type=="script"){
	file.sources = list.files(pattern=ending,path=path)
	sapply(paste0(path,"/",file.sources),FUN=source)
}
else if(type=="object")
{
	data.sources = list.files(pattern=ending,path=path)
	sapply(paste0(path,"/",data.sources),load,.GlobalEnv)
}

}
