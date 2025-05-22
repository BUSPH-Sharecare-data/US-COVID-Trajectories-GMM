*******************************************************************************************************
*	Project: SDoH		
*	PI: KIM
*	Programmer: Pengsheng Ni 
* 	Date: 
*         2/10/2025
*         3/7/25: meeting
           Kim investigated the correct datasets for which to pull the SDOH information 
           and it is documented here: “//Ad.bu.eud/bumcfiles/SPH\Projects\Sharecare\05Data_Management\02Data_Definition\Spatial” 
           and the file name is “SDOH 17 items and demog-designation -5 vars covid paper”
           1) Ni: Pull the appropriate variables from the December 2022 and December 2023 datasets as specified in 
              the spreadsheet (columns B and C) for all 17 SDOH items.
           2) Ni: Please revalidate the SDOH measures using this updated SDOH and the 2020
           3) Ni: Please use the 2020 WBI data for validation located here: 
              “\\Ad.bu.edu\bumcfiles\bedacfiles\SharecareData\To_Share\Annual_CWBI_Rankings\County\Excel” filename: county_ranking_2020.xlsx - 
              Ni, please check this matches the file you used previously – also I don’t see this information pulled into the covid_trajectory_analysis_SciRep r program… that is okay, 
              we don’t need to provide that since it is the WBI
*		
*   Puropose: 
*     1. PCA
*     2. Cronbach Alpha
*     3. Correlation of SDOH with WBI
********************************************************************************************************;


options nofmterr nodate nocenter ;

proc datasets library=work kill;
run;quit;

/*Get the SDOH items*/


proc import datafile="\\Ad.bu.edu\bumcfiles\SPH\Projects\SC\05Reports_and_Findings\Internal\02ShareCare\Data_Share_Dec2022\SDoH_Vars_COUNTY_Dec2022_Deliverable.xlsx" out=sdoh_county_dec2022 dbms=xlsx replace;
    getnames=yes;
run; /*3142, 76*/

%let sdoh_fa=lablack1_prop lakids1_prop laseniors1_prop;
%let sdoh_ht=pct_home_vl_500K_up_e home_val_inc_ratio_30_39_e pct_pub_trans_e;
%let sdoh_ra=LATracts20 libraries_per_10000 institutions_per_10000 Percent65UpWorked_e;
%let sdoh_es=pct_ins_19_64_pub_e unem_rt_20_64_e percentome_below_e percent_in_force_e;

data sdoh_county_dec2022_short; set sdoh_county_dec2022;
keep stcofips 
&sdoh_fa &sdoh_ht &sdoh_ra &sdoh_es 
;
run; /*3142, 15*/


proc import datafile="\\Ad.bu.edu\bumcfiles\SPH\Projects\SC\05Reports_and_Findings\Internal\02ShareCare\Data_Share_Dec2023\SDoH_Vars_COUNTY_Dec2023_Deliverable.xlsx" out=sdoh_county_dec2023 dbms=xlsx replace;
    getnames=yes;
run; /*3142, 76*/

%let sdoh_ha=MDs OBGYN  Pediatrics;

data sdoh_county_dec2023_short; set sdoh_county_dec2023;
keep stcofips 
&sdoh_ha
;
run; /*3142, 4*/

proc sort data=sdoh_county_dec2022_short; by stcofips;
proc sort data=sdoh_county_dec2023_short; by stcofips;
data sdoh_county; merge sdoh_county_dec2022_short sdoh_county_dec2023_short; by stcofips;
run; /*3142, 18*/

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
libname in '\\ad.bu.edu\bumcfiles\bedacfiles\SharecareData\National\04Analytic_Data\2023\';

data county_sdoh; set in.sdoh_2023_county_an_20240306;
proc contents data=county_sdoh varnum; run;
data county_sdoh_; set county_sdoh; keep fips_code--instit20;
run;

proc means data=county_sdoh_ nmiss n mean min max p25 p75; var home500k--instit20;
run;
*/

/*
Table S1. SDOH Indices - Principal Component Analysis Promax Rotated Factor Pattern (N=3,142) 
Table S2. Reliability and Concurrent Validity with the Well-Being Index (N=3,142) 
*/

/*
calculated cronbach's alpha, and each subdomains

Healthcare access: 			md obgyn peds
Food access: 				black1 kid1 senior1
Housing and Transportation:	home500k homeinc3 pubtrns
Resource Access:			tract20 Library20 instit20 work65u
Economic Security:			ins19_64 uemp20_64 inc_pov laborfc
*/

ods pdf file="\\Ad.bu.edu\bumcfiles\SPH\Projects\SC\06Methods_and_Specifications\05CWBI2020\Internal\03Analytic\ModeAdj_Scoring_Ranks\covid19\07122021\PublishCodes\output\CronhachAlpha_sdoh.pdf";

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

ods pdf close;

/*PCA*/
 
ods excel file="\\Ad.bu.edu\bumcfiles\SPH\Projects\SC\06Methods_and_Specifications\05CWBI2020\Internal\03Analytic\ModeAdj_Scoring_Ranks\covid19\07122021\PublishCodes\output\pca_sdoh.xlsx";

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

ods excel close;

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

data sdoh_county_score; set sdoh_county_std_r_score;
fips_code=stcofips; keep fips_code sdoh;
run;

/*export sdoh score*/

proc export data=sdoh_county_score
outfile="\\ad.bu.edu\bumcfiles\bedacfiles\SCData\cWBIData\2020\sdoh_county_2020_updatedcovid.xlsx"
dbms=xlsx  replace;
putnames=yes;
run;



/*export for confirmatory factor analysis*/

data sdoh_cfa_data; set sdoh_county_std_r_score;
array x(*) home_val_inc_ratio_30_39_e--sdoh;
do i=1 to dim(x);
	if x(i)=. then x(i)=-999;
end;
drop i StCoFIPS;
run;
proc contents data=sdoh_cfa_data varnum; run;
proc export data=sdoh_cfa_data
outfile="\\ad.bu.edu\bumcfiles\SPH\Projects\SC\06Methods_and_Specifications\05CWBI2020\Internal\03Analytic\ModeAdj_Scoring_Ranks\covid19\07122021\PublishCodes\cfa\sdoh.csv"
dbms=csv  replace;
putnames=no;
run;

/*
Fit index for CFA

CFI:  0.892
RMSEA:0.080
*/

/*
get WBI score

Please use the 2020 WBI data for validation located here: 
“\\Ad.bu.edu\bumcfiles\bedacfiles\SharecareData\To_Share\Annual_CWBI_Rankings\County\Excel” 
filename: county_ranking_2020.xlsx - 
Ni, please check this matches the file you used previously – 
also I don’t see this information pulled into the covid_trajectory_analysis_SciRep r program…
that is okay, we don’t need to provide that since it is the WBI
*/


proc import datafile="\\Ad.bu.edu\bumcfiles\bedacfiles\SharecareData\To_Share\Annual_CWBI_Rankings\County\Excel\county_ranking_2020.xlsx" 
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


/*input SDOHi 2023 county data


libname in_sdoh '\\ad.bu.edu\bumcfiles\bedacfiles\SharecareData\National\04Analytic_Data\2023\';

data sdoh_; set in_sdoh.county_scores_items_2023_an;
length fips_code $7.;
fips_code=STCOFIPS;
RUN;

proc contents data=sdoh varnum; run;

data a1; set county_wbi_2020; keep StCoFIPS WELL_BEING_INDEX_; WELL_BEING_INDEX_=WELL_BEING_INDEX;
run;

proc sort data=a1; by StCoFIPS;
proc sort data=sdoh_; by StCoFIPS;
data tmp; merge a1 sdoh_; by StCoFIPS;
run;

check the correlation of 2020 wbi and 2023 wbi:

proc corr spearman data=tmp; var  WELL_BEING_INDEX_ WELL_BEING_INDEX;
run;

r=0.37

proc sgplot data=tmp; 
scatter x=WELL_BEING_INDEX_ y=WELL_BEING_INDEX;
quit;


*/


/*
Table S2. Reliability and Concurrent Validity with the Well-Being Index (N=3,142) 
*/

/*spearman corr of wbi with sdoh and subdomains*/


proc means data=all n nmiss min mean max std; 
var WELL_BEING_INDEX sdoh food Hous_Trans Resource Economic Healthcare;
run;
ods pdf file="\\Ad.bu.edu\bumcfiles\SPH\Projects\SC\06Methods_and_Specifications\05CWBI2020\Internal\03Analytic\ModeAdj_Scoring_Ranks\covid19\07122021\PublishCodes\output\Correlation_sdoh_wbi.pdf";
proc corr data=all spearman;
var sdoh food Hous_Trans Resource Economic Healthcare ; with WELL_BEING_INDEX;
run;
ods pdf close;
/*
 					sdoh      food     Hous_Trans    Resource    Economic Healthcare 
WELL_BEING_INDEX  0.36846    0.09314   0.26779        0.07535  0.43620     0.17803 
                  <.0001     <.0001    <.0001         <.0001   <.0001      <.0001 
                  3142       3142      3142           3142     3142        3141 

*/
