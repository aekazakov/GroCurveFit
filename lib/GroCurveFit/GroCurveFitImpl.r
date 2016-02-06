#BEGIN_HEADER
library(jsonlite)
library(testthat)
library(grofit)
#END_HEADER

methods <- list()

#BEGIN_CONSTRUCTOR
source("./lib/Workspace/WorkspaceClient.r")

gcFitModel2 <-
function(time, data, gcID ="undefined", control=grofit.control())
{

print("Start gcFitModel2")
# /// check input parameters
if (is(control)!="grofit.control") stop("control must be of class grofit.control!")
if (!control$fit.opt%in%c("m","b")) stop("Fit option is not set for a model fit. See grofit.control()")

# /// conversion to handle even data.frame inputs
time <- as.vector(as.numeric(as.matrix(time)))
data    <- as.vector(as.numeric(as.matrix(data)))

# /// check length of input data
if (length(time)!=length(data)) stop("gcFitModel: length of input vectors differ!")

# /// determine which values are not valid
bad.values <- (is.na(time))|(time<0)|(is.na(data))|(data<0)|(!is.numeric(time))|(!is.numeric(data))

# /// remove bad values or stop program
if (TRUE%in%bad.values)
{
   if (control$neg.nan.act==FALSE)
   {
      time    <- time[!bad.values]
      data    <- data[!bad.values]
   }
   else{
   stop("Bad values in gcFitModel")
   }
}

# /// check if there are enough data points
if (length(data)<5){
	warning("gcFitModel: There is not enough valid data. Must have at least 5 unique values!")
        gcFitModel   <- list(raw.time = time, raw.data = data, gcID = gcID, fit.time = NA, fit.data = NA, parameters = list(A=NA, mu=NA, lambda=NA, integral=NA), model = NA, nls = NA, reliable=NULL, fitFlag=FALSE, control = control)
	class(gcFitModel) <- "gcFitModel"
        return(gcFitModel)
}
else{
	# /// apply transformation
	if (control$log.x.gc==TRUE){ time <- log(1+time)}
	if (control$log.y.gc==TRUE){ data <- log(1+data)}
	
	# fitting parametric growth curves
	y.model     <- NULL
	bestAIC     <- NULL
	best        <- NULL
	used        <- NULL
	
	# starting values for parametric fitting from spline fit
	nonpara     <- gcFitSpline(time, data, control)
	mu.low      <- nonpara$parametersLowess$mu
	lambda.low  <- nonpara$parametersLowess$lambda
	A.low       <- nonpara$parametersLowess$A

	# /// determine length of model names
	l               <- 10
	for (i in 1:length(control$model.type)){
		l[i] <- nchar((control$model.type)[i])
	}
	lmax <- max(l)
	
	# /// loop over all parametric models requested
		print("loop over all parametric models requested")

	for (i in 1:length(control$model.type)){
		if (control$suppress.messages==FALSE){
			cat(paste("--> Try to fit model", (control$model.type)[i]))
		}
		initmodel    <- paste("init",(control$model.type)[i],sep="")
		formulamodel <- as.formula(paste("data ~ ", (control$model.type)[i], "(time, A, mu, lambda, addpar)", sep=""))
		if ( (exists((control$model.type)[i])) && (exists(initmodel))){
			init.model  <- do.call(initmodel, list(y=data, time=time, A=A.low, mu=mu.low, lambda=lambda.low))
			try(y.model <- nls(formulamodel, start=init.model), silent = TRUE)
			if(!(TRUE%in%is.null(y.model))){
				AIC       <- AIC(y.model)
			}

		if (control$suppress.messages==FALSE){
			if (class(y.model)=="nls"){
				if (y.model$convInfo$isConv==TRUE){
					message(paste(rep(".",lmax+3-l[i])), " OK")	
				}
				else{
					warning(paste(rep(".",lmax+3-l[i]), " nls() failed to converge with stopCode ", as.character(y.model$convInfo$stopCode)))
				}
			}
			else{
                                        message(paste(rep(".",lmax+3-l[i]))," ERROR in nls(). For further information see help(gcFitModel)")
			}
		}
			if (FALSE%in%is.null(AIC)){
				if (is.null(best) || AIC<bestAIC){
				bestAIC <- AIC
				best    <- y.model
				used    <- (control$model.type)[i]
			}
			}
		} # of if ( (exists((control$model.type)[i])) ...
		else{
		cat((control$model.type)[i])
		cat("\n")
		cat(initmodel)
		cat("\n")
		stop("The model definition above does not exist! Spelling error?")
		}
y.model <- NULL
	}
	
	if (control$suppress.messages==FALSE) cat("\n") 
	
	print("extract parameters from data fit")
	# /// extract parameters from data fit
	if(is.null(best)==FALSE){
		Abest      <- summary(best)$parameters["A",1:2]
		mubest     <- summary(best)$parameters["mu",1:2]
		lambdabest <- summary(best)$parameters["lambda",1:2]
		fitFlag    <- TRUE
		if (length(time)==length(as.numeric(fitted.values(best)))){
			Integralbest <- low.integrate(time, as.numeric(fitted.values(best)) )
		}
		else{
			Integralbest <- NA
		}
	}
	else{
		warning("gcFitModel: Unable to fit this curve parametrically!")
		Abest        <- c(NA,NA)
		mubest       <- c(NA,NA)
		lambdabest   <- c(NA,NA)
		Integralbest <- NA
		fitFlag      <- FALSE
	}
} # end of else if(length(y)<5)
print("Create gcFitModel output")

gcFitModel <- list(raw.time = time, raw.data = data, gcID = gcID, fit.time = time, fit.data = as.numeric(fitted.values(best)), parameters = list(A=Abest, mu=mubest, lambda=lambdabest, integral=Integralbest), model = used, nls = best, reliable=NULL, fitFlag=fitFlag, control = control)

print("End gcFitModel2")
class(gcFitModel) <- "gcFitModel"

gcFitModel
}

gcFit2 <-
function(time, data, control=grofit.control())
{

print("Start gcFit2")
# /// check input parameters
if (is(control)!="grofit.control") stop("control must be of class grofit.control!")

# /// check number of datasets
if ( (dim(time)[1])!=(dim(data)[1]) ) stop("gcFit: Different number of datasets in data and time")

# /// check fitting options
if (!is.element(control$fit.opt, c("s","m","b"))){
    warning("fit.opt must be set to 's', 'm' or 'b'. Changed to 'b'!")
    fit.opt="b"
}

# /// Initialize some parameters
out.table       <- NULL
used.model      <- NULL
fitpara.all     <- list()
fitnonpara.all  <- list()
boot.all        <- list()
fitted.param    <- NULL
fitted.nonparam <- NULL
bootstrap.param <- NULL

# /// loop over all wells
for (i in 1:dim(data)[1]){
	print("Start main loop iteration")

	# /// conversion, to handle even data.frame inputs
	acttime    <- as.numeric(as.matrix(time[i,]))
	actwell <- as.numeric(as.matrix((data[i,-1:-3])))
	gcID    <- as.matrix(data[i,1:3])

	if ((control$suppress.messages==FALSE)){
		cat("\n\n")
		cat(paste("= ", as.character(i), ". growth curve =================================\n", sep=""))
		cat("----------------------------------------------------\n")
	}

	print("Run gcFitModel")

	# /// Parametric fit
	if ((control$fit.opt=="m") || (control$fit.opt=="b")){
		print("fitpara          <- gcFitModel(acttime, actwell, gcID, control)")
		fitpara          <- gcFitModel2(acttime, actwell, gcID, control)
		print("fitpara.all[[i]] <- fitpara")
		fitpara.all[[i]] <- fitpara
	}
	else{
	# /// generate empty object
		print("fitpara          <- list(raw.x = acttime, raw.y = actwell, gcID = gcID, fit.x = NA, fit.y = NA, parameters = list(A=NA, mu=NA, lambda=NA, integral=NA), model = NA, nls = NA, reliable=NULL, fitFlag=FALSE, control = control)")
		fitpara          <- list(raw.x = acttime, raw.y = actwell, gcID = gcID, fit.x = NA, fit.y = NA, parameters = list(A=NA, mu=NA, lambda=NA, integral=NA),
					  model = NA, nls = NA, reliable=NULL, fitFlag=FALSE, control = control)
		print("class(fitpara)   <- \"gcFitModel\"")
		class(fitpara)   <- "gcFitModel"
		print("fitpara.all[[i]] <- fitpara")
		fitpara.all[[i]] <- fitpara
	}
	
	print("Run gcFitSpline")
	# /// Non parametric fit
	if ((control$fit.opt=="s") || (control$fit.opt=="b")){
		nonpara             <- gcFitSpline(acttime, actwell, gcID, control)
		fitnonpara.all[[i]] <- nonpara
	}
	else{
	# /// generate empty object
		nonpara             <- list(raw.x = acttime, raw.y = actwell, gcID = gcID, fit.x = NA, fit.y = NA, parameters = list(A= NA, mu=NA, lambda=NA, integral=NA),
					    parametersLowess=list(A= NA, mu=NA, lambda=NA), spline = NA, reliable=NULL, fitFlag=FALSE, control = control)
		class(nonpara)      <- "gcFitSpline"
		fitnonpara.all[[i]] <- nonpara
	}
	
	print("Run plotting")
	
	# /// plotting stuff
	wellname <- paste(as.character(data[i,1]), as.character(data[i,2]),as.character(data[i,3]), sep="-")
	
	if ((control$interactive==TRUE)){
		if (fitpara$fitFlag==TRUE){
			plot(fitpara,colData=1, colModel=1, cex=1.5)
			plot(nonpara,add=TRUE, raw=FALSE, colData=0, colSpline=2 , cex=1.5)
		}
		else{
			plot(nonpara,add=FALSE, raw=TRUE, colData=1, colSpline=2 , cex=1.5)
		}
		title(wellname)
		
		# /// add legend and title
		if (control$fit.opt=="m") legend(x="bottomright", legend=fitpara$model, col="black", lty=1)
		if (control$fit.opt=="s") legend(x="bottomright", legend="spline fit", col="red", lty=1)
		if (control$fit.opt=="b") legend(x="bottomright", legend=c(fitpara$model,"spline fit"), col=c("black","red"), lty=c(1,1))
	}

	# /// here a manual reliability tag is set in the interactive mode
	reliability_tag<-NA
	if(control$interactive==TRUE){
		answer <- readline("Are you satisfied (y/n)?")
		if (substr(answer, 1, 1) == "n"){
			cat("\n Tagged this well as unreliable !\n\n")
			reliability_tag              <- FALSE
			fitpara.all[[i]]$reliable    <- FALSE
			fitnonpara.all[[i]]$reliable <- FALSE
		}
		else{
			reliability_tag              <- TRUE
			fitpara.all[[i]]$reliable    <- TRUE
			fitnonpara.all[[i]]$reliable <- TRUE
			cat("Well was (more ore less) o.k.\n")
		}# of if (substr(answer, 1, 1) == "n")
	}# of if(control$interactive==TRUE){
	else{
	reliability_tag <- TRUE
	}

	if (control$interactive==TRUE) graphics.off()

	print("Run gcBootSpline")

	# /// Beginn Bootstrap
	if ((control$nboot.gc > 0) && (reliability_tag==TRUE)){
		bt            <- gcBootSpline(acttime, actwell, gcID, control)
		boot.all[[i]] <- bt	
	} # /// end of if (control$nboot.gc ...)
        else{
        # /// create empty gcBootSpline  object
        	bt            <- list(raw.x=acttime, raw.y=actwell, gcID =gcID, boot.x=NA, boot.y=NA, boot.gcSpline=NA,
				   lambda=NA, mu=NA, A=NA, integral=NA, bootFlag=FALSE, control=control)
		class(bt)     <- "gcBootSpline"
		boot.all[[i]] <- bt	
        }

	print("Create output table")
	# create output table
	description     <- data.frame(TestId=data[i,1], AddId=data[i,2],concentration=data[i,3], reliability=reliability_tag, used.model=fitpara$model, log.x=control$log.x.gc, log.y=control$log.y.gc, nboot.gc=control$nboot.gc)

	fitted          <- cbind(description, summary(fitpara), summary(nonpara), summary(bt))
	
	out.table       <- rbind(out.table, fitted)
	print("End main loop iteration")

} # /// of for (i in 1:dim(y)[1])

gcFit           <- list(raw.time = time, raw.data = data, gcTable = out.table, gcFittedModels = fitpara.all, gcFittedSplines = fitnonpara.all, gcBootSplines = boot.all, control=control)
print("End gcFit2")

class(gcFit)    <- "gcFit"
gcFit



} # /// end of function

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
			if (md_entry[["category"]] == "TimeSeries")  {
				if (md_entry[["property_name"]] == "Time") {
					timepoints[row_num] <- as.numeric(md_entry[["property_value"]])
				}
			}
		}
	}
#    print(timepoints)
    time <- t(matrix(rep(timepoints, samples_number), c(timepoints_number, samples_number)))
    print(time)
    
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
    print(data)
    
    print(packageVersion("grofit"))
    print(sessionInfo())

	print("-------Time--------")
	print(typeof(time))
	print(class(time))
#	print(sapply(time, class))
#	print(sapply(time, attributes))
	print(attributes(time))
	print(names(time))
	print (toJSON(time))
	print("-------Data--------")
	print(typeof(data))
	print(class(data))
#	print(sapply(data, class))
#	print(sapply(data, attributes))
	print(attributes(data))
	print(names(data))
	print (toJSON(data))
	print("------Control-------")
	grofit_control <- grofit.control(fit.opt="b",suppress.messages = FALSE, interactive = FALSE)
	print (grofit_control)
	print("-------------------")

    print("Running grofit")
    print(sessionInfo(package="grofit"))

    result <- gcFit2(time,data, control=grofit_control)
    print ("grofit finished")
	print(summary(result))
	print("Creating output object")
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


