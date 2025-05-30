/************************************************************************

Create COVID incidence rate in week by three period

	March 15 - June 14: starts and ends on a Sunday, comes out to 13 weeks and 1 day. Round to 13 weeks.
	June 15 - August 14: starts on a Monday and ends on a Friday. 8 weeks and 5 days. Round to 9 weeks.
	August 15 - Nov. 2: starts on a Saturday and ends on a Sunday. 11 weeks and 2 days. Round to 11 weeks.

************************************************************************/

proc datasets library=work kill;
run;
quit;

/*
Get the valid fips codes from urban rural data N=3142

need to update the directory using the dataset found in github
*/

proc import datafile="County_stcofips_2018v3.xlsx" out=fips_old dbms=xlsx replace;
    getnames=yes;
run; /*3142*/

data fips_old; retain fips_code fips; set fips_old; fips=stcofips+0; keep fips fips_code; fips_code=stcofips; run; /*3142*/


/*
COVID data from: 
	https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv

The periods we defined as :  
period-1: 3/15/2020~6/14/2020; for period-1, it would be 13 weeks and the extra 1 days
											 starts and ends on a Sunday, comes out to 13 weeks and 1 day. Round to 13 weeks.
period-2: 6/15/2020~8/14/2020; for period-2, it would be 8 weeks and 4 extra days
											 starts on a Monday and ends on a Friday. 8 weeks and 5 days. Round to 9 weeks.
period-3: 8/15/2020~11/2/2020. for period-3, it would be 11 weeks and 2 extra days
											 starts on a Saturday and ends on a Sunday. 11 weeks and 2 days. Round to 11 weeks.
*/

%let file="time_series_covid19_confirmed_US.csv";

PROC IMPORT OUT=data_in_web_
    DATAFILE=&file
    DBMS=CSV
    REPLACE;
    GETNAMES=YES;
RUN; /*3342*/


/*
For:
	fips: 2261 
	split into 2 counties in year 2020
	fips: 2063, 2066 

JHS dataset keeps all 3 fips, and numbers in 2261 fips are all 0; 
so we combine 2063, 2066  and create new record for 2261 
then it would keep the # of counties as 3142, it will align # of counties (3142) in 3 datasets together.
*/

data tmp; set data_in_web_;
where fips in (2063 2066 2261);
run;

data t1; set tmp; keep fips _1_22_20--_3_9_23; where fips in (2063 2066); 
data t2; set tmp; keep fips Province_State; where fips in (2261); 
proc means data=t1; output out=t1_ sum=; var _1_22_20--_3_9_23; 
data t3; merge t2 t1_; drop _type_ _freq_; run;

data data_in_web; set data_in_web_; if fips in (2063 2066 2261) then delete; run;
data data_in_web; set data_in_web t3; run; /*3340*/

proc sql;
create table state as
select unique(Province_State) from data_in_web;
quit;/*58*/

proc sql;
create table FIPS as
select unique(FIPS) from data_in_web;
quit; /*3331*/


/*merge with valid fips list*/
proc sort data=fips_old; by fips; run;
proc sort data=data_in_web; by fips; run;
data data_in_web_clean; merge fips_old(in=t) data_in_web; 
by fips; if t; run; /*3142*/


/*period - 1: 3/15/2020~6/14/2020;*/
data period_1; set data_in_web_clean; keep fips _3_14_20--_6_14_20; run; 
/*
keep one previous date to count incidence for the first week
decided on 3/6/2025: 
	March 15 - June 14: starts and ends on a Sunday, comes out to 13 weeks and 1 day. Round to 13 weeks.
	we just keep exact 7 days going forward to calcuate incidence rate: 3/21/2025-3/14/2025 1st week
	last week will have 8 days.
	we need to modify the week to reflect the more days in last week
*/

proc contents data=period_1 varnum; run;

proc sort data=period_1; by fips;
proc transpose data=period_1 out=period_1_; by fips; run;
data period_1_; set period_1_; 
month=input(scan(_name_,1,'_'),8.);
day=input(scan(_name_,2,'_'),8.);
year=input(scan(_name_,3,'_'),8.);
date=mdy(month,day,year);
week_numbweek=int((date-2)/7)-3140;
if date=21988 then week_numbweek=0; /* make the last day before period as week=0 */
if date=22080 then week_numbweek=13; /*the last day into previous week*/
run;

proc sort data=period_1_; by fips week_numbweek date;
data period_1_1ast; set period_1_; by fips week_numbweek date; if last.week_numbweek; keep fips col1 week_numbweek;
data period_1_1ast; set period_1_1ast; _name_=week_numbweek;
proc transpose data=period_1_1ast out=period_1_1ast_; var col1; by fips; run;
data period_1_1ast_; set period_1_1ast_;
array first(*) _0-_12;
array last(*)_1--_13;
array ir(*) w1-w13;
do i=1 to dim(first);
	ir(i)=last(i)-first(i);
end;
drop i;
run;

proc print data=period_1_1ast_; var _0 _1 w1; run;

proc transpose data=period_1_1ast_ out=period_1_long; by fips; var w1-w13; run;


/*period - 2: 6/15/2020~8/14/2020;*/
data period_2; set data_in_web_clean; keep fips _6_14_20--_8_14_20; run;
/*
keep one previous date to count incidence for the first week
decided on 3/6/2025: 
	June 15 - August 14: starts on a Monday and ends on a Friday. 8 weeks and 5 days. Round to 9 weeks.
	we just keep exact 7 days going forward to calcuate incidence rate: 6/14/2025-8/14/2025 1st week
	last week will have 5 days.
*/

proc contents data=period_2 varnum; run;

proc sort data=period_2; by fips;
proc transpose data=period_2 out=period_2_; by fips; run;
data period_2_; set period_2_; 
month=input(scan(_name_,1,'_'),8.);
day=input(scan(_name_,2,'_'),8.);
year=input(scan(_name_,3,'_'),8.);
date=mdy(month,day,year);
week_numbweek=int((date-3)/7)-3153;
if date=22080 then week_numbweek=0; /* make the last day before period as week=0 */
run;

proc sort data=period_2_; by fips week_numbweek date;
data period_2_1ast; set period_2_; by fips week_numbweek date; if last.week_numbweek; keep fips col1 week_numbweek;run;
data period_2_1ast; set period_2_1ast; _name_=week_numbweek;
proc transpose data=period_2_1ast out=period_2_1ast_; var col1; by fips; run;
data period_2_1ast_; set period_2_1ast_;
array first(*) _0-_8;
array last(*)_1--_9;
array ir(*) w1-w9;
do i=1 to dim(first);
	ir(i)=last(i)-first(i);
end;
drop i;
run;

proc print data=period_2_1ast_; var _0 _1 w1; run;

proc transpose data=period_2_1ast_ out=period_2_long; by fips; var w1-w9; run;


/*period - 3: 8/15/2020~11/2/2020;*/
data period_3; set data_in_web_clean; keep fips _8_14_20--_11_2_20; run; 
/*
keep one previous date to count incidence for the first week
decided on 3/6/2025: 
	August 15 - Nov. 2: starts on a Saturday and ends on a Sunday. 11 weeks and 2 days. Round to 11 weeks.
	last week will have 9 days.
	we need to modify the week to reflect the more days in last week
*/


proc contents data=period_3 varnum; run;

proc sort data=period_3; by fips;
proc transpose data=period_3 out=period_3_; by fips; 
data period_3_; set period_3_; 
month=input(scan(_name_,1,'_'),8.);
day=input(scan(_name_,2,'_'),8.);
year=input(scan(_name_,3,'_'),8.);
date=mdy(month,day,year);
week_numbweek=int((date-1)/7)-3162;
if date=22141 then week_numbweek=0; /*make the last day before period as week=0*/
if date in (22219, 22220, 22221) then week_numbweek=11; /*make the last 3 days as previous week */
run;

proc sort data=period_3_; by fips week_numbweek date;
data period_3_1ast; set period_3_; by fips week_numbweek date; if last.week_numbweek; keep fips col1 week_numbweek;
data period_3_1ast; set period_3_1ast; _name_=week_numbweek;
proc transpose data=period_3_1ast out=period_3_1ast_; var col1; by fips; run;
data period_3_1ast_; set period_3_1ast_;
array first(*) _0-_10;
array last(*)_1--_11;
array ir(*) w1-w11;
do i=1 to dim(first);
	ir(i)=last(i)-first(i);
end;
drop i;
run;

proc print data=period_3_1ast_; var _0 _1 w1; run;

proc transpose data=period_3_1ast_ out=period_3_long; by fips; var w1-w11; run;



/*
denominator: population size for each county

updated the directory
*/

%let popu="JHU_Centers_for_Civic_Impact_Covid-19_County_Cases__Daily_Update_11032020.csv";

PROC IMPORT OUT=data_in_popu
    DATAFILE=&popu
    DBMS=CSV
    REPLACE;
    GETNAMES=YES;
RUN; /*955240*/

proc freq data=data_in_popu; tables fips; run;

proc sql;
create table tmp as
select fips, mean(population) as mean, std(population) as sd from data_in_popu group by fips;
quit; /*3331*/
data tmp; set tmp; if fips<1000 then delete; run; /*not valid fips, left N= 3326*/
proc means data=tmp; var sd; run; /*population are invariance across time with in county, and mean is the popultion size*/

/*remove population=0 and merge with valid fips list*/

data popu; set tmp; if mean=0 then delete; drop sd; run; /*county: 3220*/
proc sort data=fips_old; by fips; run;
proc sort data=popu; by fips; run;
data popu_clean; merge fips_old(in=t) popu; 
by fips; if t; run; /*3142*/



/*
combined popu size and weekly new cases and calculate incidence rate
*/

proc sort data=popu_clean; by fips; run;
proc sort data=Period_1_long; by fips; run;
proc sort data=Period_2_long; by fips; run;
proc sort data=Period_3_long; by fips; run;

data Period_1_long_; merge popu_clean Period_1_long; by fips; run;
data Period_2_long_; merge popu_clean Period_2_long; by fips; run;
data Period_3_long_; merge popu_clean Period_3_long; by fips; run;

data Period_1_long_; set Period_1_long_; ci=col1/mean;
data Period_2_long_; set Period_2_long_; ci=col1/mean;
data Period_3_long_; set Period_3_long_; ci=col1/mean;
run;

proc means data=Period_1_long_ n min p5 p25 p50 p75 p95 max; var ci; run;
proc means data=Period_2_long_ n min p5 p25 p50 p75 p95 max; var ci; run;
proc means data=Period_3_long_ n min p5 p25 p50 p75 p95 max; var ci; run;

/*
t1: 40846
t2: 28278
t3: 34562

*/


/* clean the number of negative incidence rate*/

proc sql; select count(*) from Period_1_long_ where ci<0; quit; /*316*/
proc sql; select count(*) from Period_2_long_ where ci<0; quit; /*156*/
proc sql; select count(*) from Period_3_long_ where ci<0; quit; /*107*/

/*
left with value indicdence rate

t1: 40530
t2: 28122
t3: 34455

*/
data _t1; set Period_1_long_; if ci<0 then delete;
data _t2; set Period_2_long_; if ci<0 then delete;
data _t3; set Period_3_long_; if ci<0 then delete;
run;


proc sql; create table fips_code1 as select unique(fips_code) from _t1; quit;/*3142*/
proc sql; create table week1 as select unique(_name_) from _t1;quit; /*13 weeks*/
 
proc sql; create table fips_code2 as select unique(fips_code) from _t2; quit; /*3142*/
proc sql; create table week2 as select unique(_name_) from _t2;quit; /*9 weeks*/
 
proc sql; create table fips_code3 as select unique(fips_code) from _t3; quit;/*3142*/
proc sql; create table week3 as select unique(_name_) from _t3;quit; /*11 weeks*/
 


/*
Finial datasets for sttp two

1. convert week start from 0
2. incidence rate multiple by 100000
*/


data Ir_v1_1; set _t1; ci100000=ci*100000;
weekn=substr(_name_,2,2)-1; drop fips;
run;

data Ir_v2_1; set _t2; ci100000=ci*100000;
weekn=substr(_name_,2,2)-1; drop fips;
run;

data Ir_v3_1; set _t3; ci100000=ci*100000;
weekn=substr(_name_,2,2)-1; drop fips;
run;

proc means data=Ir_v1_1; var ci100000 weekn;
proc means data=Ir_v2_1; var ci100000 weekn;
proc means data=Ir_v3_1; var ci100000 weekn;
run;

/*
final datasets

update the directories

export 3 datasets to csv files
*/


proc export data=Ir_v1_1
outfile="Ir_v1_1.csv" replace
dbms=csv;
run;

proc export data=Ir_v2_1
outfile="Ir_v2_1.csv" replace
dbms=csv;
run;

proc export data=Ir_v3_1
outfile="Ir_v3_1.csv" replace
dbms=csv;
run;



/*
need to calculate incidence rate between 3/15/2020~11/2/2020
calcuate incidence rates and by periods
*/

/*get new cases within observed periods*/
data period_all; set data_in_web_clean; keep fips _3_14_20 _6_14_20 _8_14_20 _11_2_20 newcases_all newcases_p1 newcases_p2 newcases_p3; 
newcases_all=_11_2_20-_3_14_20;
newcases_p1=_6_14_20-_3_14_20;
newcases_p2=_8_14_20-_6_14_20;
newcases_p3=_11_2_20-_8_14_20;
run; 

proc sort data=popu_clean; by fips; run;
proc sort data=period_all; by fips; run;

data period_all_; merge popu_clean period_all; by fips; run;
data period_all_; set period_all_; 
IncidenceRate=newcases_all/mean*100000;
newcases_p1=newcases_p1/mean*100000;
newcases_p2=newcases_p2/mean*100000;
newcases_p3=newcases_p3/mean*100000;
Population=mean;
run;

proc means data=period_all_ sum; var Population; run;
proc means data=period_all_; var IncidenceRate newcases_p1 newcases_p2 newcases_p3; run;

proc print data=period_all_;  where newcases_p2<0; run;
/*treat 1 case negative as missing */

data period_all_; set period_all_; if newcases_p2<0 then newcases_p2=.; run;
proc means data=period_all_; var IncidenceRate newcases_p1 newcases_p2 newcases_p3; run;

/*
update the directory
*/

proc export data=period_all_
outfile="Covid19_County_Cases__Daily_Update_11022020.csv" replace
dbms=csv;
run;
