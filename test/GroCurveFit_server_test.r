library(raster)
library(jsonlite)
library(testthat)

token <- NULL
config <- NULL
ws_client <- NULL
ws_name <- NULL
context <- NULL

get_test_token <- function() {
    if (is.null(token)) {
        token <<- Sys.getenv("KB_AUTH_TOKEN")
    }
    return(token)
}

get_test_config <- function() {
    if (is.null(config)) {
        config_file <- Sys.getenv("KB_DEPLOYMENT_CONFIG")
        config <<- readIniFile(config_file, aslist=TRUE)[['GroCurveFit']]
    }
    return(config)
}

get_test_context <- function() {
    if (is.null(context)) {
        context <<- list(token=get_test_token(), config=get_test_config(),
            provenance=list(list(service=unbox("GroCurveFit"),
                method=unbox("please_never_use_it_in_production"), 
                method_params=list(unbox("no-args")))))
    }
    return(context)
}

get_test_ws_client <- function() {
    if (is.null(ws_client)) {
        ws_url <- get_test_config()[['workspace-url']]
        ws_client <<- WorkspaceClient(ws_url, get_test_token())
    }
    return(ws_client)
}

get_test_ws_name <- function() {
    if (is.null(ws_name)) {
        suffix <- round(as.numeric(Sys.time()) * 1000)
        ws_name_loc <- paste("test_GroCurveFit_",as.character(suffix),sep="")
        tryCatch({
            get_test_ws_client()[['create_workspace']](list(workspace=unbox(ws_name_loc)))
        }, error = function(err) {
            print(paste("WARNING: ", err))
        })
        ws_name <<- ws_name_loc
    }
    return(ws_name)
}

test_cleanup <- function() {
    if (!is.null(ws_name)) {
        tryCatch({
            get_test_ws_client()[['delete_workspace']](list(workspace=unbox(ws_name)))
            ws_name <<- NULL
            print("Test workspace was deleted")
        }, warning = function(war) {
            print(paste("WARNING: ", war))
        }, error = function(err) {
            print(paste("ERROR: ", err))
        }, finally = {
        })
    }
}

source("./lib/GroCurveFit/GroCurveFitImpl.r")
tryCatch({
#    obj_name <- "contigset.1"
#    contig = list(id=unbox('1'), length=unbox(10), md5=unbox('md5'), sequence=unbox('agcttttcat'))
#    obj = list(contigs=list(contig), id=unbox('id'), md5=unbox('md5'), name=unbox('name'), source=unbox('source'),
#            source_id=unbox('source_id'), type=unbox('type'))
#    get_test_ws_client()[['save_objects']](list(workspace=unbox(get_test_ws_name()), objects=list(list(
#            type=unbox('KBaseGenomes.ContigSet'), name=unbox(obj_name), data=obj))))
#    ret <- methods$GroCurveFit.count_contigs(get_test_ws_name(), obj_name, get_test_context())
#    contig_count <- ret[['contig_count']]
#    expect_equal(as.numeric(contig_count), 1)
#    
    print("Testing fit_growth_curve")
#    obj_real_name <- "growth-test-20160205-01"
	obj_real_name <- "growth-statseries-20160211-1"
    ws_real_name <- "aktest:1454614449601"
    test_obj_name <- "growth-test-parameters-20160211-01"
    method_id <- "s"
    ret <- methods$GroCurveFit.fit_growth_curve(ws_real_name, obj_real_name, test_obj_name, method_id, get_test_context())
#    print(toJSON(ret))
    ret_object_name <- ret[['output_object']][[1]]
#    print(toJSON(ret_object_name))
    expect_equal(ret_object_name, test_obj_name)

}, finally = {
    test_cleanup()
})
