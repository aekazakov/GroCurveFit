#BEGIN_HEADER
library(jsonlite)
library(grofit)
#END_HEADER

methods <- list()

#BEGIN_CONSTRUCTOR
source("./lib/Workspace/WorkspaceClient.r")

#END_CONSTRUCTOR

methods[["GroCurveFit.fit_growth_curve"]] <- function(workspace_name, growth_matrix_id, parameters_obj_name, fit_method, context) {
    #BEGIN fit_growth_curve
    token <- context[['token']]
    provenance <- context[['provenance']]
    ws_url <- context[['config']][['workspace-url']]
    ws_client <- WorkspaceClient(ws_url, token)
    ref <- unbox(paste(workspace_name,growth_matrix_id, sep="/"))
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
			matrix_type <- "StatValues"
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

	description <- "No description"
    for (i in 1:samples_number) {
    	data[i,2] <- description
    }

	concentration <- 0    
    for (i in 1:samples_number) {
    	data[i,3] <- concentration
    }
    
    for (i in 1: timepoints_number){
		data_col_i <- numeric(samples_number)
    	for (j in 1:samples_number) {
    		data_col_i[j] <- as.numeric(values_t[samples_indices[j],i])
    	}
   	    data[, i+3] <- data_col_i
    	
	}
	
	if (fit_method == "g") {
		fit_method <- "m"
		fit_models <- c("gompertz")
	} else if (fit_method == "e") {
		fit_method <- "m"
		fit_models <- c("gompertz.exp")
	} else if (fit_method == "l") {
		fit_method <- "m"
		fit_models <- c("logistic")
	} else if (fit_method == "r") {
		fit_method <- "m"
		fit_models <- c("richards")
	} else if (fit_method == "m") {
		fit_method <- "m"
		fit_models <- c("gompertz.exp")
	} else if (fit_method == "s") {
		fit_method <- "s"
		fit_models <- c("logistic","richards","gompertz", "gompertz.exp")
	} else if (fit_method == "b") {
		fit_method <- "b"
		fit_models <- c("logistic","richards","gompertz", "gompertz.exp")
	}
 	grofit_control <- grofit.control(fit.opt = fit_method, model.type = fit_models, suppress.messages = TRUE, interactive = FALSE)
    print("Running grofit")

    result <- gcFit(time,data, control=grofit_control)
    print ("grofit finished")
	print("Creating output object")
	result_frame <- summary(result)
	
	# use 9 columns from result_frame: "used.model", "mu.model", "lambda.model", "A.model", "integral.model", "mu.spline", "lambda.spline", "A.spline", "integral.spline"

	ret_data <- as.data.frame(matrix(ncol=6, nrow=samples_number))
	
	colnames(ret_data) <- c("mtx_column_id", "method", "growth_rate", "lag_phase", "max_growth", "area_under_curve")
	
	for (i in 1:samples_number) {
		ret_data[i,"mtx_column_id"] <- col_ids[samples_indices[i]][[1]]
		if (is.na(result_frame[i,"mu.model"])|| is.na(result_frame[i,"lambda.model"]) || is.na(result_frame[i,"A.model"]) || is.na(result_frame[i,"integral.model"])) {
			if (is.na(result_frame[i,"mu.spline"])|| is.na(result_frame[i,"lambda.spline"]) || is.na(result_frame[i,"A.spline"]) || is.na(result_frame[i,"integral.spline"])) {
				ret_data[i,"method"] <- "NA"
				ret_data[i,"growth_rate"] <- 0.0
				ret_data[i,"lag_phase"] <- 0.0
				ret_data[i,"max_growth"] <- 0.0
				ret_data[i,"area_under_curve"] <- 0.0
			} else {
				ret_data[i,"method"] <- "spline"
				ret_data[i,"growth_rate"] <- as.numeric(result_frame[i, "mu.spline"])
				ret_data[i,"lag_phase"] <- as.numeric(result_frame[i, "lambda.spline"])
				ret_data[i,"max_growth"] <- as.numeric(result_frame[i, "A.spline"])
				ret_data[i,"area_under_curve"] <- as.numeric(result_frame[i, "integral.spline"])
			} 
		} else {
			ret_data[i,"method"] <- paste (result_frame[i, "used.model"], "model", sep = " ", collapse = NULL)
			ret_data[i,"growth_rate"] <- as.numeric(result_frame[i, "mu.model"])
			ret_data[i,"lag_phase"] <- as.numeric(result_frame[i, "lambda.model"])
			ret_data[i,"max_growth"] <- as.numeric(result_frame[i, "A.model"])
			ret_data[i,"area_under_curve"] <- as.numeric(result_frame[i, "integral.model"])
		}		
	}
	
	
	output_obj <- list(matrix_id = ref, parameters = ret_data)

	print(toJSON(output_obj))
	print("Saving output object to workspace")
	
	ws_output <- ws_client$save_objects(list(workspace=unbox(workspace_name), objects=list(list(
            type=unbox('KBaseEnigmaMetals.GrowthParameters'), name=unbox(parameters_obj_name), data=output_obj))))

	ret <- unbox(ws_output[[1]][[2]])
	print("Done")
		
    return(list(output_object=ret, provenance=provenance))

    #END fit_growth_curve
}

