--Create table for FL_insurance_sample
create table FL_insurance_sample (
    [policyID] int,
    [statecode] varchar(2),
    [county] varchar(256),
    [eq_site_limit] int,
    [hu_site_limit] int,
    [fl_site_limit] int,
    [fr_site_limit] int,
    [tiv_2011] int,
    [tiv_2012] int,
    [eq_site_deductible] int,
    [hu_site_deductible] int,
    [fl_site_deductible] int,
    [fr_site_deductible] int,
    [point_latitude] int,
    [point_longitude] int,
    [line] varchar(256),
    [construction] varchar(256),
    [point_granularity] int
);
--Import FL_insurance_sample into newly created table
.mode csv
.import FL_insurance_sample.csv FL_insurance_sample
.schema
--Print out first 10 rows of data set
SELECT * FROM FL_insurance_sample LIMIT 10;
--List which counties are in the sample (i.e. list unique values of the county variable)
SELECT county FROM FL_insurance_sample GROUP BY county;
--Compute the average property appreciation from 2011 to 2012 (i.e. compute the mean of tiv_2012 - tiv_2011)
SELECT AVG(tiv_2012 - tiv_2011) FROM FL_insurance_sample;
--Create a frequency table of construction variable to see what fraction of buildings are made out of wood or some other
-- material
SELECT construction, COUNT(*) FROM FL_insurance_sample GROUP BY construction;
