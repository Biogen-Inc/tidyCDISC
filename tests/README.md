# TABLE GENERATOR UNIT TESTS

### DATA PROCESSING
1)	Take ADVS and BDS [ADVS], pivot BDS by PARAMCD merge by USUBJID
2)	Distinct number of USUBJID for total in header
3)	Distinct number of USUBJID for total in header [COUNTRY == CANADA]
4)	Distinct number of USUBJID for total in header [GROUP BY TRT01P]
5)	Distinct number of USUBJID for total in header [GROUP BY TRT01P, COUNTRY == CANADA]

### MEAN BLOCK
6)	Mean of PARAMCD (provide AVISIT) 
a.	[ROW == DIABP,  AVISIT == Week 12]
7)	Mean of PARAMCD (provide AVISIT) with filter
a.	[ROW == DIABP,  AVISIT == Week 12, COUNTRY == CANADA]
8)	Mean of ADSL 
a.	[ROW == AGE]
9)	Mean of ADSL with Filter
a.	[ROW == AGE, COUNTRY == CANADA]
10)	Mean of PARAMCD (provide AVISIT), group by COLUMN
a.	[ROW == DIABP, COLUMN == TRT01P]
11)	Mean of PARAMCD (provide AVISIT), group by COLUMN with Filter
a.	[[ROW == DIABP, COLUMN == TRT01P, COUNTRY == CANADA]
    12)	Mean of ADSL, group by COLUMN
a.	[ROW == AGE, COLUMN == TRT01P]
13)	Mean of ADSL, group by COLUMN with filter
a.	[ROW == AGE, COLUMN == TRT01P, COUNTRY == CANADA]

### FREQUENCY BLOCK
14)	Frequency of ROW
a.	[ROW == SEX,]
15)	Frequency of ROW with filter
a.	[ROW == SEX, COUNTRY == CANADA]
16)	Frequency of ROW, group by COLUMN
a.	[ROW == SEX, COLUMN == TRT01P]
17)	Frequency of ROW, group by COLUMN, with Filter
a.	[ROW == SEX, COLUMN == TRT01P,  FILTER == CANADA]

### T-TEST
18)	PARAMCD
a.	[ROW == DIABP, COLUMN == TRT01P]
19)	PARAMCD with Filter
a.	[ROW == DIABP, COLUMN == TRT01P, COUNTRY == CANADA]
20)	ADSL
a.	[ROW == AGE, COLUMN == TRT01P]
21)	ADSL with Filter
a.	[ROW == AGE, COLUMN == TRT01P, COUNTRY == CANADA]

### CHANGE FROM BASELINE
22)	PARAMCD and AVISIT
a.	[ROW == DIABP, COLUMN == TRT01P]
23)	PARAMCD and AVISIT with Filter
a.	[ROW == DIABP, COLUMN == TRT01P]
24)	PARAMCD and AVISIT and COLUMN
a.	[ROW == AGE, COLUMN == TRT01P]
25)	PARAMCD and AVISIT and COLUMN with Filter
a.	[ROW == AGE, COLUMN == TRT01P, COUNTRY == CANADA]
