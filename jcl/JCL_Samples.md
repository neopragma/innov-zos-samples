# z/OS Training

## Job Control Language (JCL) and z/OS Utilities samples

#### Sequential Data Sets and the DD Statement

- **[HELLO.jcl](jcl/HELLO.jcl)**. First runnable job - Hello, World!

- **[ALLOSEQ.jcl](jcl/ALLOSEQ.jcl)**. Use DD statements with IEFBR14 to allocate and catalog physical sequential data sets (PS). 

- **[UNALSEQ.jcl](jcl/UNALSEQ.jcl)**. Use DD statements with IEFBR14 to uncatalog and unallocate physical sequential data sets (PS). 

#### Partitioned Data Sets

- **[ALLOPDSE.jcl](jcl/ALLOPDSE.jcl)**. Allocate a PDSE to be used as a source library.

- **[ALLOLIB.jcl](jcl/ALLOLIB.jcl)**. Allocate a PDSE to be used as a Program Library.

#### Copying Sequential Data Sets with IEBGENER

- **[CPSYSOUT.jcl](jcl/CPSYSOUT.jcl)**. Copy in-line data to SYSOUT.

- **[CPTONEW.jcl](jcl/CPTONEW.jcl)**. Create a new sequential data set and copy in-line data to it.

- **[CPREFDD.jcl](jcl/CPREFDD.jcl)**. Specify the REFDD parameter on the SYSUT2 DD statement to re-use DCB information from an existing data set declared in the same job.

- **[DEFMODEL.jcl](jcl/DEFMODEL.jcl)**. Catalog a model DSCB.

- **[CPMODEL.jcl](jcl/CPMODEL.jcl)**. Specify a model DSCB on the SYSUT2 DD statement for a new data set.

- **[CPDELETE.jcl](jcl/CPDELETE.jcl)**. Use DD parameters to effect a "move" or "rename" of a sequential data set using EIBGENER.

- **[CPEDIT.jcl](jcl/CPEDIT.jcl)**. Use EIBGENER to edit/transform input data.

#### Copying Partitioned Data Sets with EIBCOPY

- **[ALLOPDSE.jcl](jcl/ALLOPDSE.jcl)**. Create PDSE with IEFBR14.

- **[COPYPDSE.jcl](jcl/COPYPDSE.jcl)**. Create and load PDSE with IEBCOPY.

#### Working with Generation Data Groups (GDGs)

- **[CREGDG.jcl](jcl/CREGDG.jcl)**. Create a GDG using IDCAMS and a model DSCB using IEFBR14.

- **[GDGLD1.jcl](jcl/GDGLD1.jcl)**. Create and load a Generation Data Set (GDS).

- **[GDGLD2.jcl](jcl/GDGLD2.jcl)**. Create and load another GDS.

- **[LISTGDG.jcl](jcl/LISTGDG.jcl)**. List individual and concatenated GDSs using IEBGENER.

#### Sorting and Merging Data Sets with DFSORT

- **[SORT1.jcl](jcl/SORT1.jcl)**. Sort a data set on a single field.

- **[SORT2.jcl](jcl/SORT2.jcl)**. Sort a data set on multiple fields.

- **[SORT3.jcl](jcl/SORT3.jcl)**. Include selected records for sorting.

- **[SORT4.jcl](jcl/SORT4.jcl)**. Reformat output records.

- **[SORT5.jcl](jcl/SORT5.jcl)**. Use symbols with DFSORT.

- **[SORT6.jcl](jcl/SORT6.jcl)**. Merge three data sets.

- **[SORT7.jcl](jcl/SORT7.jcl)**. Sort three data sets concatenated as one.

#### Defining and Using JCL Procedures, temporary data sets 

- **[SORT8.jcl](jcl/SORT8.jcl)**. Define an in-line JCL procedure.

- **[SORT9.jcl](jcl/SORT9.jcl)**. Create and pass temporary data sets.

- **[SORT10.jcl](jcl/SORT10.jcl)**. Define a catalogued JCL procedure.

- **[SORTIT.jcl](procs/SORTIT.jcl)**. Catalogued procedure to do a sort.

#### Conditional Step Execution 

- **[COND_1.jcl](jcl/COND_1.jcl)**. Sample JCL using COND=EVEN.

- **[COND_ONLY_1.jcl](jcl/COND_ONLY_1.jcl)**. Sample JCL using COND=ONLY, first step does not fail.

- **[COND_ONLY_2.jcl](jcl/COND_ONLY_2.jcl)**. Sample JCL using COND=ONLY, first step fails.

- **[COND_STEP_REF.jcl](jcl/COND_STEP_REF.jcl)**. Sample JCL using COND referring to the condition code from a particular step.

- **[COND_MULTIPLE.jcl](jcl/COND_MULTIPLE.jcl)**. Sample JCL using COND with multiple conditions specified.

- **[IFELSE_MULTIPLE.jcl](jcl/IFELSE_MULTIPLE.jcl)**. Sample JCL using IF/ELSE with multiple conditions specified.

#### Defining VSAM objects with IDCAMS

- **[CREAIX.jcl](jcl/CREAIX.jcl)**. Delete and define an Alternate Index and Path and build the index using IDCAMS.

- **[LOADKSDS.jcl](jcl/CREAIX.jcl)**. Ensure newly-defined KSDS is not empty before attempting BLDINDEX. Demonstrates the IDCAMS REPRO command.

- **[CREESDS.jcl](jcl/CREESDS.jcl)**. Delete and define an Entry-Sequenced Data Set (ESDS) using IDCAMS.

- **[CREKSDS1.jcl](jcl/CREKSDS1.jcl)**. Delete and define a Key-Sequenced Data Set (KSDS) using IDCAMS.

- **[CREKSDS2.jcl](jcl/CREKSDS2.jcl)**. Delete and define a Key-Sequenced Data Set (KSDS) using IEFBR14 and a DD statement.

- **[CRELDS.jcl](jcl/CRELDS.jcl)**. Delete and define a Linear Data Set (LDS) using IDCAMS.

- **[CRERRDS.jcl](jcl/CRERRDS.jcl)**. Delete and define a Relative Record Data Set (RRDS) using IDCAMS.

#### Job Restart 

- **[RESTART.jcl](jcl/RESTART.jcl)**. Sample JCL demonstrating the RESTART parameter of the JOB statement.

#### JCL for COBOL Examples

- **[DATES.jcl](jcl/DATES.jcl)**. JCL to run the DATES program, including a SYSIN data set needed for the ACCEPT statements in DATES.cbl.

- **[CREESFL.jcl](jcl/CREESFL.jcl)**. Delete, define, and load an Entry Sequenced Data Set (ESDS) for the COBOL sample program ESDS1.cbl.

- **[CREKSFL.jcl](jcl/CREKSFL.jcl)**. Delete, define, and load a Key Sequenced Data Set (KSDS) for the COBOL sample program KSDS1.cbl.

- **[CRERRFL.jcl](jcl/CRERRFL.jcl)**. Delete, define, and load a Relative Record Data Set (RRDS) for the COBOL sample program RRDS1.cbl.

- **[ESDS1.jcl](jcl/ESDS1.jcl)**. Run the COBOL sample program ESDS1.cbl.

- **[KSDS1.jcl](jcl/KSDS1.jcl)**. Run the COBOL sample program KSDS1.cbl.

- **[RRDS1.jcl](jcl/RRDS1.jcl)**. Run the COBOL sample program RRDS1.cbl.

