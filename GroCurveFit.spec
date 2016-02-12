/*
A KBase module: GroCurveFit
This module contains one small method - fit_growth_curve.
*/

module GroCurveFit {

    /*
        A string representing a GrowthMatrix id.
    */
    
    typedef string growth_matrix_id;
    
    /*
        A string representing a workspace name.
    */
    typedef string workspace_name;
    
    /*
	A string representing final object name
    */
    typedef string parameters_obj_name;

    /* 
	Parameters of a single growth curve
    */

	typedef structure{
    	string mtx_column_id;
		string method;
    	float growth_rate;
        float lag_phase;
        float max_growth;
        float area_under_curve;
	} GrowthCurveParameters;

    /* 
	Parameters of all growth curves for a GrowthMatrix
    */

	typedef structure{
    	string matrix_id;
    	list <GrowthCurveParameters> parameters;
	} GrowthParameters;

    
    /*
        Returns growth matrix parameters
        growth_matrix_id - the GrowthMatrix to fit.
    */

    funcdef fit_growth_curve(workspace_name,growth_matrix_id,parameters_obj_name,string fit_method) returns (string output_object) authentication required;
};
