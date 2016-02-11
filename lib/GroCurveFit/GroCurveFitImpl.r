#BEGIN_HEADER
library(jsonlite)
library(grofit)
#END_HEADER

methods <- list()

#BEGIN_CONSTRUCTOR
source("./lib/Workspace/WorkspaceClient.r")

#END_CONSTRUCTOR

methods[["GroCurveFit.fit_growth_curve"]] <- function(workspace_name, growth_matrix_id, parameters_obj_name, context) {
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
    
    values <- data[['values']]
    print("Making time matrix")
    timepoints <- numeric(timepoints_number)
    metadata <- growth_matrix_obj[['metadata']]
    row_metadata <- metadata[['row_metadata']]
    matrix_metadata <- metadata[['matrix_metadata']]

# find type of values 
	matrix_type <- 'RawValues'
	for (entry_num in 1:length(matrix_metadata)){
		if ((matrix_metadata[[entry_num]][["category"]] == "Measurement") && (matrix_metadata[[entry_num]][["property_value"]] == "StatValues")) {
			matrix_type = "StatValues"
		}
	}

# make list of column indices with growth data
	samples_indices <- numeric()
	if (matrix_type == "StatValues") {
		col_metadata <- metadata[['column_metadata']]
		for (col_num in 1:length(col_ids)) {		
			col_name <- col_ids[[col_num]][[1]]
			col_md_entries <- col_metadata[[col_name]]
			for (col_md_pos in 1:length(col_md_entries)) {
				col_md_entry <- col_md_entries[[col_md_pos]]
				if ((col_md_entry[["category"]] == "Measurement") && (col_md_entry[["property_name"]] == "ValueType") && (col_md_entry[["property_value"]] == "Average")) {					
					samples_indices <- c (samples_indices, col_num)
				}
			}
		}
	} else {
		samples_indices <- seq(1,length(col_ids))	
	}
    samples_number <- length(samples_indices)

# make list of timepoints
	for (row_num in 1:length(row_ids)) {		
		row_name <- row_ids[[row_num]][[1]]
		row_md_entries <- row_metadata[[row_name]]
		for (row_md_pos in 1:length(row_md_entries)) {
			md_entry <- row_md_entries[[row_md_pos]]
			if ((md_entry[["category"]] == "TimeSeries") && (md_entry[["property_name"]] == "Time")) {
				timepoints[row_num] <- as.numeric(md_entry[["property_value"]])
			}
		}
	}
    time <- t(matrix(rep(timepoints, samples_number), c(timepoints_number, samples_number)))
    
    print("Making data frame")
    values_t = do.call(cbind, values)

    data = as.data.frame(matrix(ncol=timepoints_number+3, nrow=samples_number))
    
    for (i in 1:samples_number) {
    	data[i,1] <- col_ids[samples_indices[i]][[1]]
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
    		data_col_i[j] <- as.numeric(values_t[samples_indices[j],i])
    	}
   	    data[, i+3] = data_col_i
    	
	}
 	grofit_control <- grofit.control(fit.opt="b",suppress.messages = TRUE, interactive = FALSE)
    print("Running grofit")

    result <- gcFit(time,data, control=grofit_control)
    print ("grofit finished")
	print("Creating output object")
	result_frame <- summary(result)
	ret_data <- result_frame[,c("TestId","mu.model", "lambda.model", "A.model", "integral.model")]

	for (i in 1:samples_number) {
		if (is.na(ret_data[i,"mu.model"])) ret_data[i,"mu.model"] <- "NA"
		if (is.na(ret_data[i,"lambda.model"])) ret_data[i,"lambda.model"] <- "NA"
		if (is.na(ret_data[i,"A.model"])) ret_data[i,"A.model"] <- "NA"
		if (is.na(ret_data[i,"integral.model"])) ret_data[i,"integral.model"] <- "NA"
	}
	
	colnames(ret_data) <- c("mtx_column_id", "growth_rate", "lag_phase", "max_growth", "area_under_curve")
	output_obj = list(matrix_id = unbox(growth_matrix_id), parameters = ret_data)

#	print(toJSON(output_obj))
	
	ws_output <- ws_client$save_objects(list(workspace=unbox(workspace_name), objects=list(list(
            type=unbox('KBaseEnigmaMetals.GrowthMatrixParameters'), name=unbox(parameters_obj_name), data=output_obj))))

	print(toJSON(ws_output))
	
	ret <- unbox(ws_output[[1]][[2]])
		
    return(list(output_object=ret, provenance=provenance))

    #END fit_growth_curve
}

