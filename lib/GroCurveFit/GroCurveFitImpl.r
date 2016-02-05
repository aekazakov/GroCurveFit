#BEGIN_HEADER
library(jsonlite)
library(grofit)
#END_HEADER

methods <- list()

#BEGIN_CONSTRUCTOR
source("./lib/Workspace/WorkspaceClient.r")
#END_CONSTRUCTOR

methods[["GroCurveFit.count_contigs"]] <- function(workspace_name, contigset_id, context) {
    #BEGIN count_contigs
    token <- context[['token']]
    provenance <- context[['provenance']]
    ws_url <- context[['config']][['workspace-url']]
    ws_client <- WorkspaceClient(ws_url, token)
    ref <- unbox(paste(workspace_name,"/",contigset_id, sep=""))
    object_identity <- list(ref=unbox(ref))
    object_data <- ws_client$get_objects(list(object_identity))[[1]]
    data <- object_data[['data']]
    contigs <- data[['contigs']]
    contig_count <- unbox(length(contigs))
    return(list(contig_count=contig_count, provenance=provenance))
    #END count_contigs
}

methods[["GroCurveFit.fit_growth_curve"]] <- function(workspace_name, growth_matrix_id, context) {
    #BEGIN fit_growth_curve
    token <- context[['token']]
    provenance <- context[['provenance']]
    ws_url <- context[['config']][['workspace-url']]
    ws_client <- WorkspaceClient(ws_url, token)
    ref <- unbox(paste(workspace_name,"/",growth_matrix_id, sep=""))
    object_identity <- list(ref=unbox(ref))
    object_data <- ws_client$get_objects(list(object_identity))[[1]]
    growth_matrix_obj <- object_data[['data']]
	data <- growth_matrix_obj[['data']]
    col_ids <- data[['col_ids']]
    row_ids <- data[['row_ids']]
    timepoints_number <- length(row_ids)
    samples_number <- length(col_ids)
    
    values <- data[['values']]
    print("Making time matrix")
    timepoints <- numeric(timepoints_number)
#    print (timepoints)
    metadata <- growth_matrix_obj[['metadata']]
    row_metadata <- metadata[['row_metadata']]

	for (row_num in 1:length(row_ids)) {		
		row_name <- row_ids[row_num][[1]]
		row_md_entries <- row_metadata[[row_name]]
		for (row_md_pos in 1:length(row_md_entries)) {
			md_entry <- row_md_entries[[row_md_pos]]
			if (md_entry[["entity"]] == "TimeSeries")  {
				if (md_entry[["property_name"]] == "Time") {
					timepoints[row_num] <- as.numeric(md_entry[["property_value"]])
				}
			}
		}
	}
#    print(timepoints)
    time <- t(matrix(rep(timepoints, samples_number), c(timepoints_number, samples_number)))
#    print(time)
    
    print("Making data frame")
#    print (values)
    values_t = do.call(cbind, values)
#    print (values_t)

    data = as.data.frame(matrix(ncol=timepoints_number+3, nrow=samples_number))
    
    for (i in 1:samples_number) {
    	data[i,1] <- col_ids[i][[1]]
    }

	description = "No description"
    for (i in 1:samples_number) {
    	data[i,2] <- description
    }

	concentration = 0    
    for (i in 1:samples_number) {
    	data[i,3] <- concentration
    }
    
    for (i in 1: timepoints_number){
		data_col_i <- numeric(samples_number)
    	for (j in 1:samples_number) {
    		data_col_i[j] <- as.numeric(values_t[j,i])
    	}
   	    data[, i+3] = data_col_i
    	
	}
 #   print(data)
    
    print("Running grofit")
    result <- gcFit(time,data, control=grofit.control(fit.opt="b",suppress.messages = TRUE))
#	print(summary(result))
#	print(result)
	result_frame <- summary(result)
	ret_data <- result_frame[,c("TestId","mu.model", "lambda.model", "A.model", "integral.model")]

	for (i in 1:samples_number) {
		if (is.na(ret_data[i,"mu.model"])) ret_data[i,"mu.model"] <- "NA"
		if (is.na(ret_data[i,"lambda.model"])) ret_data[i,"lambda.model"] <- "NA"
		if (is.na(ret_data[i,"A.model"])) ret_data[i,"A.model"] <- "NA"
		if (is.na(ret_data[i,"integral.model"])) ret_data[i,"integral.model"] <- "NA"
	}
	
	colnames(ret_data) <- c("sample_id", "mu", "lambda", "a", "integral")
	print(toJSON(ret_data))
		
    return(list(growth_parameters=ret_data, provenance=provenance))
    #END fit_growth_curve
}
