apitools
======

Tools for using the APIs for the U.S. Bureau of Economic Analysis (BEA -- www.bea.gov) and the Census Bureau (www.census.gov).

For information about the BEA API, see http://www.bea.gov/API/bea_web_service_api_user_guide.htm.
For details on BEA NIPA datasets see:
   NIPA:  http://www.bea.gov/iTable/index_nipa.cfm
   NIPA underlying detail: http://www.bea.gov/iTable/index_UD.cfm
   
Basic steps for BEA data:
1) BEA_DSlist() to get a list of available datasets
2) Decide upon a dataset of interest
3) BEA_DSparams() to get the parameters needed for the dataset of interest
4) If you need information about allowable parameters, use BEA_ParamVals()

Here are the steps in action

   

