# z/OS Training

## Job Control Language (JCL) and z/OS Utilities samples

#### Sequential Data Sets and the DD Statement

- **[HELLO.(HELLO.**. First runnable job - Hello, World!

- **[ALLOSEQ.(ALLOSEQ.**. Use DD statements with IEFBR14 to allocate and catalog physical sequential data sets (PS). 

- **[UNALSEQ.(UNALSEQ.**. Use DD statements with IEFBR14 to uncatalog and unallocate physical sequential data sets (PS). 

#### Partitioned Data Sets

- **[ALLOPDSE.(ALLOPDSE.**. Allocate a PDSE to be used as a source library.

- **[ALLOLIB.(ALLOLIB.**. Allocate a PDSE to be used as a Program Library.

#### Copying Sequential Data Sets with IEBGENER

- **[CPSYSOUT.(CPSYSOUT.**. Copy in-line data to SYSOUT.

- **[CPTONEW.(CPTONEW.**. Create a new sequential data set and copy in-line data to it.

- **[CPREFDD.(CPREFDD.**. Specify the REFDD parameter on the SYSUT2 DD statement to re-use DCB information from an existing data set declared in the same job.

- **[DEFMODEL.(DEFMODEL.**. Catalog a model DSCB.

- **[CPMODEL.(CPMODEL.**. Specify a model DSCB on the SYSUT2 DD statement for a new data set.

- **[CPDELETE.(CPDELETE.**. Use DD parameters to effect a "move" or "rename" of a sequential data set using EIBGENER.

- **[CPEDIT.(CPEDIT.**. Use EIBGENER to edit/transform input data.

#### Copying Partitioned Data Sets with EIBCOPY

- **[ALLOPDSE.(ALLOPDSE.**. Create PDSE with IEFBR14.

- **[COPYPDSE.(COPYPDSE.**. Create and load PDSE with IEBCOPY.

#### Working with Generation Data Groups (GDGs)

- **[CREGDG.(CREGDG.**. Create a GDG using IDCAMS and a model DSCB using IEFBR14.

- **[GDGLD1.(GDGLD1.**. Create and load a Generation Data Set (GDS).

- **[GDGLD2.(GDGLD2.**. Create and load another GDS.

- **[LISTGDG.(LISTGDG.**. List individual and concatenated GDSs using IEBGENER.

#### Sorting and Merging Data Sets with DFSORT

- **[SORT1.(SORT1.**. Sort a data set on a single field.

- **[SORT2.(SORT2.**. Sort a data set on multiple fields.

- **[SORT3.(SORT3.**. Include selected records for sorting.

- **[SORT4.(SORT4.**. Reformat output records.

- **[SORT5.(SORT5.**. Use symbols with DFSORT.

- **[SORT6.(SORT6.**. Merge three data sets.

- **[SORT7.(SORT7.**. Sort three data sets concatenated as one.

#### Defining and Using JCL Procedures, temporary data sets 

- **[SORT8.(SORT8.**. Define an in-line JCL procedure.

- **[SORT9.(SORT9.**. Create and pass temporary data sets.

- **[SORT10.(SORT10.**. Define a catalogued JCL procedure.

- **[SORTIT.(procs/SORTIT.**. Catalogued procedure to do a sort.

#### Conditional Step Execution 

- **[COND_1.(COND_1.**. Sample JCL using COND=EVEN.

- **[COND_ONLY_1.(COND_ONLY_1.**. Sample JCL using COND=ONLY, first step does not fail.

- **[COND_ONLY_2.(COND_ONLY_2.**. Sample JCL using COND=ONLY, first step fails.

- **[COND_STEP_REF.(COND_STEP_REF.**. Sample JCL using COND referring to the condition code from a particular step.

- **[COND_MULTIPLE.(COND_MULTIPLE.**. Sample JCL using COND with multiple conditions specified.

- **[IFELSE_MULTIPLE.(IFELSE_MULTIPLE.**. Sample JCL using IF/ELSE with multiple conditions specified.

#### Defining VSAM objects with IDCAMS

- **[CREAIX.(CREAIX.**. Delete and define an Alternate Index and Path and build the index using IDCAMS.

- **[LOADKSDS.(CREAIX.**. Ensure newly-defined KSDS is not empty before attempting BLDINDEX. Demonstrates the IDCAMS REPRO command.

- **[CREESDS.(CREESDS.**. Delete and define an Entry-Sequenced Data Set (ESDS) using IDCAMS.

- **[CREKSDS1.(CREKSDS1.**. Delete and define a Key-Sequenced Data Set (KSDS) using IDCAMS.

- **[CREKSDS2.(CREKSDS2.**. Delete and define a Key-Sequenced Data Set (KSDS) using IEFBR14 and a DD statement.

- **[CRELDS.(CRELDS.**. Delete and define a Linear Data Set (LDS) using IDCAMS.

- **[CRERRDS.(CRERRDS.**. Delete and define a Relative Record Data Set (RRDS) using IDCAMS.

#### Job Restart 

- **[RESTART.(RESTART.**. Sample JCL demonstrating the RESTART parameter of the JOB statement.

#### JCL for COBOL Examples

- **[DATES.(DATES.**. JCL to run the DATES program, including a SYSIN data set needed for the ACCEPT statements in DATES.cbl.

- **[CREESFL.(CREESFL.**. Delete, define, and load an Entry Sequenced Data Set (ESDS) for the COBOL sample program ESDS1.cbl.

- **[CREKSFL.(CREKSFL.**. Delete, define, and load a Key Sequenced Data Set (KSDS) for the COBOL sample program KSDS1.cbl.

- **[CRERRFL.(CRERRFL.**. Delete, define, and load a Relative Record Data Set (RRDS) for the COBOL sample program RRDS1.cbl.

- **[ESDS1.(ESDS1.**. Run the COBOL sample program ESDS1.cbl.

- **[KSDS1.(KSDS1.**. Run the COBOL sample program KSDS1.cbl.

- **[RRDS1.(RRDS1.**. Run the COBOL sample program RRDS1.cbl.

