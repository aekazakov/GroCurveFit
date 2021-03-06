library(jsonlite)
library(raster)

tryCatch({
    source("./lib/GroCurveFit/GroCurveFitImpl.r")
    module_name <- 'GroCurveFit'
    args <- commandArgs(trailingOnly = TRUE)
    token <- args[3]
    input <- fromJSON(args[1], flatten=TRUE, simplifyDataFrame=FALSE, 
        simplifyVector=TRUE, simplifyMatrix=TRUE)
    params <- input[["params"]]
    if (class(params) != "list") {
        input <- fromJSON(args[1], flatten=TRUE, simplifyDataFrame=FALSE, 
            simplifyVector=FALSE, simplifyMatrix=FALSE)
        params <- input[["params"]]
    }
    method <- input[["method"]]
    func <- methods[[method]]
    if ( is.null(func) ) {
        stop(paste("ERROR: Function wasn't found: ", method))
    }
    config_file <- Sys.getenv("KB_DEPLOYMENT_CONFIG")
    config <- readIniFile(config_file, aslist=TRUE)[[module_name]]
    method_name <- sub(".*\\.", "", method)
    prov_action <- list(service=unbox(module_name), method=unbox(method_name), 
        method_params=params)
    context <- list(token=token, config=config, provenance=list(prov_action))
    params[[length(params)+1]] <- context
    ret <- do.call(func, params)
    output <- toJSON(list(version=unbox("1.1"),result=list(ret)))
    write(output, file=args[2])
}, error = function(err) {
    msg <- paste("ERROR: ", err)
    print(msg)
    output <- toJSON(list(version=unbox("1.1"),error=list(error=unbox(""),
        name=unbox("JSONRPCError"),code=unbox(-32603),message=unbox(msg))))
    write(output, file=args[2])
}, finally = {
})
