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
    
    typedef structure {
    	string sample_id;
    	float mu;
        float lambda;
        float a;
        float integral;
    } GrowthCurveParameters;

    
    /*
        Returns growth curve parameters
        contigset_id - the ContigSet to count.
    */

    funcdef fit_growth_curve(workspace_name,growth_matrix_id) returns (list <GrowthCurveParameters> growth_parameters) authentication required;
};
