*******************************************************************************************************
*	Project: SDoH		
*   Purpose: 
*     1. PCA
*     2. Cronbach Alpha
*     3. Correlation of SDOH with WBI

*  any the directory for data files need to be updated 
*  generate one sdoh dataset (17 items+1 fitps=18)
********************************************************************************************************;


options nofmterr nodate nocenter ;

proc datasets library=work kill;
run;quit;

/*
Get the SDOH items with raw values

need to update the directory
*/

proc import datafile="sdoh_county_2020_covid_full.xlsx" out=sdoh_county dbms=xlsx replace;
    getnames=yes;
run; /*3142, 18*/

%let sdoh_fa=lablack1_prop lakids1_prop laseniors1_prop;
%let sdoh_ht=pct_home_vl_500K_up_e home_val_inc_ratio_30_39_e pct_pub_trans_e;
%let sdoh_ra=LATracts20 libraries_per_10000 institutions_per_10000 Percent65UpWorked_e;
%let sdoh_es=pct_ins_19_64_pub_e unem_rt_20_64_e percentome_below_e percent_in_force_e;
%let sdoh_ha=MDs OBGYN  Pediatrics;

proc contents data=sdoh_county varnum; run;

proc means data=sdoh_county n nmiss min p25 p50 p75 max mean std; var 
&sdoh_fa &sdoh_ht &sdoh_ra &sdoh_es &sdoh_ha; 
run;

/* 
standardized all items into mean=50, SD=10, 
and truncated values into 0-100
and reversed code some items
*/

PROC STANDARD DATA=sdoh_county MEAN=50 STD=10 OUT=sdoh_county_std;
  VAR &sdoh_fa &sdoh_ht &sdoh_ra &sdoh_es &sdoh_ha; run;
data sdoh_county_std; set sdoh_county_std;
array x(*) &sdoh_fa &sdoh_ht &sdoh_ra &sdoh_es &sdoh_ha;
do i=1 to dim(x);
	if x(i)=. then x(i)=.;
	else if x(i)<0 then x(i)=0;
	else if x(i)>100 then x(i)=100;
end;
drop i;
run;

data sdoh_county_std_r; set sdoh_county_std;
pct_ins_19_64_pub_e	=100-pct_ins_19_64_pub_e;
unem_rt_20_64_e		=100-unem_rt_20_64_e;
percentome_below_e	=100-percentome_below_e;

lakids1_prop		=100-lakids1_prop;
lablack1_prop		=100-lablack1_prop;
laseniors1_prop		=100-laseniors1_prop;
run;

proc means data=sdoh_county_std_r n nmiss min p25 p50 p75 max mean std; 
var &sdoh_fa &sdoh_ht &sdoh_ra &sdoh_es &sdoh_ha; run;


/*
Table S1. SDOH Indices - Principal Component Analysis Promax Rotated Factor Pattern (N=3,142) 
Table S2. Reliability and Concurrent Validity with the Well-Being Index (N=3,142) 

calculated cronbach's alpha, and each subdomains

Healthcare access: 			md obgyn peds
Food access: 				black1 kid1 senior1
Housing and Transportation:	home500k homeinc3 pubtrns
Resource Access:			tract20 Library20 instit20 work65u
Economic Security:			ins19_64 uemp20_64 inc_pov laborfc
*/


title "sdoh";
proc corr data=sdoh_county_std_r alpha;
var &sdoh_fa &sdoh_ht &sdoh_ra &sdoh_es &sdoh_ha;
run; /*Cronbach Coefficient Alpha=0.78*/

title "Food Access";
proc corr data=sdoh_county_std_r alpha; var &sdoh_fa; run; /*Food Access 				Alpha=0.93*/
title "Housing and Transportation";
proc corr data=sdoh_county_std_r alpha; var &sdoh_ht; run; /*Housing and Transportation Alpha=0.69*/
title "Resource Access";
proc corr data=sdoh_county_std_r alpha; var &sdoh_ra; run; /*Resource Access 			Alpha=0.70*/
title "Economic Security";
proc corr data=sdoh_county_std_r alpha; var &sdoh_es; run; /*Economic Security 			Alpha=0.82*/
title "Healthcare Access";
proc corr data=sdoh_county_std_r alpha; var &sdoh_ha; run; /*Healthcare Access 			Alpha=0.92*/

 
/*PCA*/

proc factor data=sdoh_county_std_r
simple
method=prin
nfact=5
priors=one
/*plot=all*/
rotate=promax
round
flag=0.4
msa
;
var 
&sdoh_fa &sdoh_ht &sdoh_ra &sdoh_es &sdoh_ha
;
run;


/*calculate the sdoh and subscale scores*/


data sdoh_county_std_r_score; set sdoh_county_std_r;
array food_ (*) &sdoh_fa;
food=mean(of food_(*));

array Hous_Trans_(*) &sdoh_ht;
Hous_Trans=mean(of Hous_Trans_(*));

array Resource_(*) &sdoh_ra;
Resource=mean(of Resource_(*));

array Economic_(*) &sdoh_es;
Economic=mean(of Economic_(*));

array Healthcare_(*) &sdoh_ha;
Healthcare=mean(of Healthcare_(*));

array sdoh_(*) food  Hous_Trans  Resource  Economic  Healthcare;
sdoh=mean(of sdoh_(*));

*keep stcofips thriving  economic  healthcare  volupopu  accessibility sdoh;
run;


/*******************************************************************************

This will not work since WBI data does not provide.

get WBI score

*******************************************************************************/


proc import datafile="county_ranking_2020.xlsx" 
out=county_ranking_2020 dbms=xlsx replace;
    getnames=yes;
run; /*3142, 48*/


proc contents data=county_ranking_2020 varnum; run;
proc means data=county_ranking_2020 n nmiss min p25 p50 p75 max mean std; var WELL_BEING_INDEX; run;

data county_wbi_2020;set county_ranking_2020;
keep StCoFIPS WELL_BEING_INDEX;
run;


proc sort data=sdoh_county_std_r_score; by StCoFIPS;
proc sort data=county_wbi_2020; by StCoFIPS;
data all; merge sdoh_county_std_r_score county_wbi_2020; by StCoFIPS;
run;



/*
Table S2. Reliability and Concurrent Validity with the Well-Being Index (N=3,142) 
*/

/*spearman corr of wbi with sdoh and subdomains*/


proc means data=all n nmiss min mean max std; 
var WELL_BEING_INDEX sdoh food Hous_Trans Resource Economic Healthcare;
run;

proc corr data=all spearman;
var sdoh food Hous_Trans Resource Economic Healthcare ; with WELL_BEING_INDEX;
run;
/*
 					sdoh      food     Hous_Trans    Resource    Economic Healthcare 
WELL_BEING_INDEX  0.36846    0.09314   0.26779        0.07535  0.43620     0.17803 
                  <.0001     <.0001    <.0001         <.0001   <.0001      <.0001 
                  3142       3142      3142           3142     3142        3141 

*/
