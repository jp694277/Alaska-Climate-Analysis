PROC IMPORT OUT= climate  DATAFILE= "C:\Users\user\Documents\多變量資料分析\climate.xls"
 DBMS=xls REPLACE;
 GETNAMES=YES;
RUN;

/*1～17變數，跑集群分析 且利用平均法做*/
PROC CLUSTER DATA=climate S STANDARD METHOD=AVERAGE
RMSSTD RSQUARE OUTTREE=TREE;
var  AO	AO_wint	AO_summ	NPI 	NPI_spring 	NPI_winter	Temp 	Temp_summ	Temp_wint	Rain	Rain_summ	Rain_wint	Ice	Ice_JanJul	Ice_OctDec	IceCover	IceFreeDays;
RUN;

PROC PLOT DATA=TREE;
PLOT _SPRSQ_*_NCL_='*'   _RSQ_*_NCL_='@' / Overlay HAXIS=0 to 16 BY 4;
RUN;

PROC TREE DATA=TREE OUT=TREEOUT NCLUSTERS=4;
COPY  AO	AO_wint	AO_summ	NPI 	NPI_spring 	NPI_winter	Temp 	Temp_summ	Temp_wint	Rain	Rain_summ	Rain_wint	Ice	Ice_JanJul	Ice_OctDec	IceCover	IceFreeDays;
RUN;

PROC SORT DATA=TREEOUT; BY CLUSTER;

PROC PRINT DATA=TREEOUT; 
VAR CLUSTER;
PROC means DATA=TREEOUT; 
var  AO	AO_wint	AO_summ	NPI 	NPI_spring 	NPI_winter	Temp 	Temp_summ	Temp_wint	Rain	Rain_summ	Rain_wint	Ice	Ice_JanJul	Ice_OctDec	IceCover	IceFreeDays;
By cluster;
output out=means mean=  AO	AO_wint	AO_summ	NPI 	NPI_spring 	NPI_winter	Temp 	Temp_summ	Temp_wint	Rain	Rain_summ	Rain_wint	Ice	Ice_JanJul	Ice_OctDec	IceCover	IceFreeDays;
RUN;

/*非階層法*/
PROC fastclus radius=0 replace=full  data=HW4_1out2 out=cluster seed=means maxc=4   maxiter=30   
 list distance out=clus1 ;
var  AO	AO_wint	AO_summ	NPI 	NPI_spring 	NPI_winter	Temp 	Temp_summ	Temp_wint	Rain	Rain_summ	Rain_wint	Ice	Ice_JanJul	Ice_OctDec	IceCover	IceFreeDays;
TITLE2 'Obtaining three clusters with the data in the order given';
RUN;



/*集群的MANOVA*/
 PROC GLM data=cluster;
 CLASS cluster;
 MODEL  AO	AO_wint	AO_summ	NPI 	NPI_spring 	NPI_winter	Temp 	Temp_summ	Temp_wint	Rain	Rain_summ	Rain_wint	Ice	Ice_JanJul	Ice_OctDec	IceCover	IceFreeDays= cluster;
 MANOVA H=cluster/PRINTE ;
 RUN;


/*PCA*/
proc princomp data=climate out=HW4_1out2;
var  AO	AO_wint	AO_summ	NPI 	NPI_spring 	NPI_winter	Temp 	Temp_summ	Temp_wint	Rain	Rain_summ	Rain_wint	Ice	Ice_JanJul	Ice_OctDec	IceCover	IceFreeDays;
proc print;
var  AO	AO_wint	AO_summ	NPI 	NPI_spring 	NPI_winter	Temp 	Temp_summ	Temp_wint	Rain	Rain_summ	Rain_wint	Ice	Ice_JanJul	Ice_OctDec	IceCover	IceFreeDays prin1 prin2;
run;

 /*列出變數1～17經過主成份分析後所得兩組分數*/
  PROC PRINT DATA=HW4_1out2  ; 
 VAR PRIN1-PRIN2;
TITLE2 'VALUES OF THE FIRST 2 PRINCIPAL COMPONENT SCORES';
 RUN;

/*1～17變數經過主成份後，跑集群分析 且利用平均法做*/
PROC CLUSTER DATA=HW4_1out2 S STANDARD METHOD=AVERAGE
RMSSTD RSQUARE OUTTREE=TREE;
VAR PRIN1-PRIN2;
RUN;

PROC PLOT DATA=TREE;
PLOT _SPRSQ_*_NCL_='*'   _RSQ_*_NCL_='@' / Overlay HAXIS=0 to 16 BY 4;
RUN;

PROC TREE DATA=TREE OUT=TREEOUT NCLUSTERS=4;
COPY PRIN1-PRIN2;
RUN;

PROC SORT DATA=TREEOUT; BY CLUSTER;

PROC PRINT DATA=TREEOUT; 
VAR CLUSTER;
PROC means DATA=TREEOUT; 
VAR PRIN1 PRIN2;
By cluster;
output out=means mean=PRIN1 PRIN2;
RUN;

/*非階層法*/
PROC fastclus radius=0 replace=full  data=HW4_1out2 out=cluster seed=means maxc=4   maxiter=30   
 list distance out=clus1 ;
var PRIN1-PRIN2;
TITLE2 'Obtaining three clusters with the data in the order given';
RUN;



/*集群的MANOVA*/
 PROC GLM data=cluster;
 CLASS cluster;
 MODEL prin1 prin2 = cluster;
 MANOVA H=cluster/PRINTE ;
 RUN;


 /*FA*/
 PROC FACTOR data=climate METHOD=PRINCIPAL SCREE   NFACT=2 ROTATE=VARIMAX S C EV RES REORDER DATA=climate  
   SCORE OUT=SCORES;
var  AO	AO_wint	AO_summ	NPI 	NPI_spring 	NPI_winter	Temp 	Temp_summ	Temp_wint	Rain	Rain_summ	Rain_wint	Ice	Ice_JanJul	Ice_OctDec	IceCover	IceFreeDays;
 RUN;

 PROC FACTOR METHOD=ML NFACT=2 ROTATE=VARIMAX S C EV RES REORDER DATA=climate;
var  AO	AO_wint	AO_summ	NPI 	NPI_spring 	NPI_winter	Temp 	Temp_summ	Temp_wint	Rain	Rain_summ	Rain_wint	Ice	Ice_JanJul	Ice_OctDec	IceCover	IceFreeDays;
  RUN;

 PROC PRINT DATA=SCORES;
  VAR FACTOR1-FACTOR2;
 RUN;

  /*從1～17萃取的因子跑集群分析*/
PROC CLUSTER DATA=SCORES S STANDARD METHOD=AVERAGE 
RMSSTD RSQUARE OUTTREE=TREE2;
VAR FACTOR1-FACTOR2;
RUN;

PROC PLOT DATA=TREE2;
PLOT _SPRSQ_*_NCL_='*'   _RSQ_*_NCL_='@' / Overlay HAXIS=0 to 16 BY 4;
RUN;

PROC TREE DATA=TREE2 OUT=TREE2OUT NCLUSTERS=4;
COPY FACTOR1-FACTOR2;
RUN;

PROC SORT DATA=TREE2OUT; BY CLUSTER;

PROC PRINT DATA=TREE2OUT; 
VAR CLUSTER;

PROC means DATA=TREE2OUT; 
VAR  FACTOR1-FACTOR2;
By CLUSTER;
output out=means mean= FACTOR1-FACTOR2;
RUN;

/*非階層法*/
PROC fastclus radius=0 replace=full  data=SCORES out=cluster seed=means maxc=8   maxiter=30   
 list distance out=clus2 ;
var  FACTOR1-FACTOR2;
run;

/*集群的MANOVA*/
 PROC GLM data=cluster;
 CLASS cluster;
 MODEL  FACTOR1-FACTOR2 = cluster;
 MANOVA H=cluster/PRINTE ;
 RUN;
