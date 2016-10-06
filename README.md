# AMSanalysis
Analysis of radiocarbon data from AMS instruments.

This program will use Shiny as a frontend to do data analysis for AMS systems.
Multiple AMS systems will be handled by individual modules which will handle
acquiring the run data for each system and exporting it to a universal data 
frame which will be the input for the main analysis part of the program.

At that point the program will allow assigning analysis batches, standards, and
blanks. Also, the program will plot 13C/12C and 14C/(12C or 13C) values and 
allow the operator to remove bad measurment runs and outliers. Then standards 
techniques will be employed to derive the d13C and pMC values for every sample. 
Normalization will be done by using the nearest six standard runs to the run 
being analyzed and weighting each of those standard runs by the inverse square 
of the difference in 13C current between the standard run and the sample run.

Finally, the program will allow export of the results and relevent analysis 
parameters to a relational database and also create a pdf report.

## Modular Data Import
Two modules will be initially created, one to handle NEC CAMS runlogs and one 
to handle the BioMICADAS database results. The modules will essentially be just 
functions to handle the necessary import and transform procedures to produce 
the universal run data frame. This will have the form:

| Field     | Type       | Description                               |
| --------- | ---------- | ----------------------------------------- |
| Run       | Char       | Unique run number.                        |
| Pos       | Int        | Position in sample wheel or magazine.     |
| Label     | Char       | Sample label or identifier.               |
| SmType    | Char       | Sample type, e.g. OX2, Blank, UNK         |
| Cycles    | Int        | Number of cycles in the run.              |
| DateTime  | POSIXct    | Date and time that run completed.         |
| le12C     | Double     | The low energy 12C- current [uA].         |
| he12C     | Double     | The high energy 12C+ current [uA].        |
| he13C     | Double     | The high energy 13C+ current [nA].        |
| Trans     | Double     | 12C transmission [%].                     |
| Cnt14C    | Int        | The deadtime corrected 14C counts.        |
| he13.12   | Double     | The 13C/12C ratio.                        |
| he14.12   | Double     | The 14C/12C ratio.                        |
| he14.13   | Double     | The 14C/13C ratio.                        |


## Removal of Outliers and Bad Measurement Data
The program will plot the 13C/12C ratio and 14C/13C ratio for each individual 
sample run. The plots will show the 1 sigma and 2 sigma lines and allow the 
operator to make determinations about bad data points which need to be removed 
prior to analysis.

At some point, functionality will be added to look at spectrometer parameters 
during individual runs to see if there is probable cause for removing outlier 
data points.

At a future point, much of this will be automated by the program such that 
little to no operator interaction is necessary to analyze data.


## Normalization
Normalization occurs after 14C/13C subtraction of the machine blank. 
Normalization of the sample data is achieved by using the nearest 6 standard 
runs in time to the sample run being normalized (both for d13C and pMC). Then 
the normalization by those standard runs will be weighted by the inverse square 
of the difference between the standard run's 13C current and the sample run's 
13C current. This will give heavier weighting to standard runs with current 
more similar to the sample's.

After the nearest standard runs and their weighting have been obtained the d13C 
value will be determined first. This is accomplished by calculating a weighted 
mean correction factor from the nearest standard runs. The correction factor is 
then applied to the sample run's 13C/12C value and then a d13C value is 
calculated from the corrected 13C/12C.

Once corrected d13C values are determined, then the pMC will be calculated by 
similar normalization. d13C normalized 14C/13C values are calculated and a 
weighted mean of these is determined for the standard runs which is then 
applied to the final normalization of the sample run's 14C/13C value to obtain 
the pMC.


## Final Values
Final values of d13C and pMC are then determined for each sample by using an 
error weighted mean of the run values. The weighting is the inverse of the 
error (so smaller errors receive higher weighting). Then process blank values 
are subtracted from the final pMC values using an equal mass approximation, 
unless carbon masses are provided in which case a mass balance subtraction is 
used.

Final results must then be approved by the operator and the results are stored 
in a database.


## Reporting
PDF and LaTEX software must be installed on the shiny server. Then a PDF report 
can be generated with appropriate headers and footers with the data in a table. 
The PDF can be saved and printed as with any other PDF. The report will contain 
d13C values, pMC values, and either dpm/gC or Age depending on sample origin, 
with all associated errors.