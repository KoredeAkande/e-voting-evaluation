This replication package contains files that can be used to replicate the analysis done in "Voting Made Safe and Easy: The Impact of e-voting on Citizen Perceptions", Political Science Research and Methods 1(1): 117-37.

The replication package includes the following files:

(1) salta_data.Rdata      R data file containing the survey data used in the analysis

(2) script1_recoding.R    R code for recoding variables included in the analysis. Depends on salta_data.Rdata and produces datamatch.Rdata

(3) datamatch.Rdata       R data file containing the recoded data set produced by script1_recoding.R

(4) script2_analysis.R    R code for replicating all the tables included in the paper (Tables 1-5). Depends on datamatch.Rdata and produces datamatched.Rdata

(5) m.out.Rdata           MatchIt object produced by script2_analysis.R

(6) datamatched.Rdata     R data file containing the matched data set produced by script2_analysis.R

(7) script3_multilev.R    R and JAGS code for replicating Figure A4 and Table A4 of the Supplementary Materials, Online Appendix 4. Depends on datamatched.Rdata and produces model.bug and coda.samples.Rdata

(8) model.bug             JAGS code for estimating the multilevel models discussed in the Supplementary Materials, Online Appendix 4
