/*
A KBase module: GroCurveFit
This sample module contains one small method - filter_contigs.
*/

module GroCurveFit {
    /*
        A string representing a ContigSet id.
    */
    typedef string contigset_id;
    
    typedef string growth_matrix_id;
    
    /*
        A string representing a workspace name.
    */
    typedef string workspace_name;
    
    typedef structure {
        int contig_count;
    } CountContigsResults;
    
    typedef structure {
    	string sample_id;
    	float mu;
        float lambda;
        float a;
        float integral;
    } GrowthCurveParameters;

    
    /*
        Count contigs in a ContigSet
        contigset_id - the ContigSet to count.
    */
    funcdef count_contigs(workspace_name,contigset_id) returns (CountContigsResults) authentication required;

    funcdef fit_growth_curve(workspace_name,growth_matrix_id) returns (list <GrowthCurveParameters> growth_parameters) authentication required;
};
