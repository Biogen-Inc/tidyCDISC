# TABLE GENERATOR UNIT TESTS
All R data frames are created using `tests/data/test_data.RDS` and are compared to SAS files with corresponding numbers in `tests/data/test_outputs`

### DATA PROCESSING
- [ ] 1)	Take ADVS and BDS [ADVS], pivot BDS by PARAMCD merge by USUBJID
- [X] 2)	Distinct number of USUBJID for total in header
- [X] 3)	Distinct number of USUBJID for total in header [COUNTRY == CANADA]
- [X] 4)	Distinct number of USUBJID for total in header [GROUP BY TRT01P]
- [X] 5)	Distinct number of USUBJID for total in header [GROUP BY TRT01P, COUNTRY == CANADA]

### MEAN BLOCK
- [X] 6)	Mean of PARAMCD (provide AVISIT) [ROW == DIABP,  AVISIT == Week 12]
- [X] 7)	Mean of PARAMCD (provide AVISIT) with filter [ROW == DIABP,  AVISIT == Week 12, COUNTRY == CANADA]
- [X] 8)	Mean of ADSL [ROW == AGE]
- [X] 9)	Mean of ADSL with Filter [ROW == AGE, COUNTRY == CANADA]
- [X] 10)	Mean of PARAMCD (provide AVISIT), group by COLUMN [ROW == DIABP, COLUMN == TRT01P]
- [X] 11)	Mean of PARAMCD (provide AVISIT), group by COLUMN with Filter [[ROW == DIABP, COLUMN == TRT01P, COUNTRY == CANADA]
- [X] 12)	Mean of ADSL, group by COLUMN [ROW == AGE, COLUMN == TRT01P]
- [X] 13)	 Mean of ADSL, group by COLUMN with filter [ROW == AGE, COLUMN == TRT01P, COUNTRY == CANADA]

### FREQUENCY BLOCK
- [ ] 14)	**off by 1** Frequency of ROW [ROW == SEX,]
- [ ] 15)	**off by 1** Frequency of ROW with filter [ROW == SEX, COUNTRY == CANADA]
- [ ] 16)	**off by 1** Frequency of ROW, group by COLUMN [ROW == SEX, COLUMN == TRT01P]
- [ ] 17)	**off by 1** Frequency of ROW, group by COLUMN, with Filter [ROW == SEX, COLUMN == TRT01P,  FILTER == CANADA]

### T-TEST
- [ ] 18)	PARAMCD [ROW == DIABP, COLUMN == TRT01P]
- [ ] 19)	PARAMCD with Filter [ROW == DIABP, COLUMN == TRT01P, COUNTRY == CANADA]
- [ ] 20)	ADSL [ROW == AGE, COLUMN == TRT01P]
- [ ] 21)	ADSL with Filter [ROW == AGE, COLUMN == TRT01P, COUNTRY == CANADA]

### CHANGE FROM BASELINE
- [X] 22)	PARAMCD and AVISIT [ROW == DIABP, COLUMN == TRT01P]
- [X] 23)	PARAMCD and AVISIT with Filter [ROW == DIABP, COLUMN == TRT01P]
- [X] 24)	PARAMCD and AVISIT and COLUMN [ROW == DIABP, COLUMN == TRT01P]
- [X] 25)	PARAMCD and AVISIT and COLUMN with Filter [ROW == DIABP, COLUMN == TRT01P, COUNTRY == CANADA]
