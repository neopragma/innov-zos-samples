# CICS/COBOL guidelines for Copilot 

## COBOL program general instructions 

- Generate a command-level COBOL program for the CICS environment.
- Program name for Program-Id in all upper case.
- Generate CRUD functionality for a VSAM KSDS data store.
- Assume a simple record layout for example purposes.

## BMS mapset general instructions

- Generate a BMS mapset source file in assembler.
- Name the mapset CRUDMS.
- Name the map CRUDM.
- Define fields as appropriate to match the VSAM record layout.
<!-- - The first enterable field is a two-character ACTION field -->
<!-- - The second enterable field is the record key field. -->
- The first enterable field is the record key field.
- Save the mapset source file as cics/CRUDMS.asm.

## Program organization and coding style 

- Use pseudoconversational design.
- Use containers and channels rather than commarea. 
- For user-defined names, use mixed case starting with upper case and separate words with a dash.
- For COBOL and CICS reserved words, use upper case.
- Assume a COBOL copybook for map CRUDM exists and use it in the program.

## User interaction instructions

- User selects the program function by pressing an associated Attention Identifier key as follows:
  - PF1 = display usage help for the program 
  - PF2 = save changes
  - PF3 = save changes (if applicable) and exit
  - PF4 = display input form to add a new record
  - PF5 = delete currently-displayed record
  - PF7 = browse backward 
  - PF8 = browse forward 
  - PF11 = cancel unsaved changes
  - PF12 = exit without saving changes 

<!-->
## User interaction instructions

- User selects the program function by entering a value in the ACTION field and pressing the Enter key.
- ACTION values are as follows:
  - H = display usage help for the program 
  - S = save changes
  - SX = save changes (if applicable) and exit
  - A = display input form to add a new record
  - D = delete currently-displayed record
  - P = browse backward 
  - N = browse forward 
  - C = cancel unsaved changes 
  - X = exit without saving changes 
-->

## Procedure Division coding conventions

- Do not use sections.
- Follow general Structured Programming conventions.
- Do not use GO TO statements.
- To clear the output map area, move low-values to the 01-level output map name.
- Check EIBRESP and EIBRESP2 rather than using HANDLE CONDITION.
- Do not terminate individual statements with a period. 
- Include a period in column 12 on a separate line at the end of each paragraph.
- Use explicit scope terminators where applicable (e.g., END-IF).
- Organize paragraphs with mainline logic first, then paragraphs that are performed from the mainline logic, then paragraphs that are performed from the second-level logic, etc. 
- Prefix each paragraph name with a four-digit numerical value followed by a dash. Number the paragraphs in ascending order based on their position in the source file. Group similar functionality under the same thousands-level number; e.g. I/O logic in the 6000- range, container managerment in the 7000- range, etc. 