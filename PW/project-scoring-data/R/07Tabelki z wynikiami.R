> xtable(stats_cfs_models, caption="Basic statistics for models with features from CFS method")
% latex table generated in R 3.1.3 by xtable 1.7-4 package
% Wed Jun 10 18:32:00 2015
\begin{table}[ht]
\centering
\begin{tabular}{rlrrrrrr}
\hline
& Label & ACC & TPR & SPC & PPV & F1 & FDR \\ 
\hline
1 & AIC Logistic Regression & 0.76 & 0.82 & 0.60 & 0.83 & 0.83 & 0.17 \\ 
2 & Logistic Regression & 0.75 & 0.76 & 0.66 & 0.93 & 0.84 & 0.07 \\ 
3 & K-NN & 0.76 & 0.79 & 0.59 & 0.90 & 0.84 & 0.10 \\ 
4 & Linear Dyscriminant Analysis & 0.78 & 0.83 & 0.61 & 0.87 & 0.85 & 0.13 \\ 
5 & Quadric Dyscriminant Analysis & 0.80 & 0.84 & 0.65 & 0.88 & 0.86 & 0.12 \\ 
\hline
\end{tabular}
\caption{Basic statistics for models with features from CFS method} 
\end{table}



> xtable(stats_consistency_models, caption="Basic statistics for models with features from consistency method")
% latex table generated in R 3.1.3 by xtable 1.7-4 package
% Wed Jun 10 18:32:02 2015
\begin{table}[ht]
\centering
\begin{tabular}{rlrrrrrr}
\hline
& Label & ACC & TPR & SPC & PPV & F1 & FDR \\ 
\hline
1 & AIC Logistic Regression & 0.81 & 0.83 & 0.72 & 0.90 & 0.87 & 0.10 \\ 
2 & Logistic Regression & 0.80 & 0.83 & 0.71 & 0.90 & 0.86 & 0.10 \\ 
3 & K-NN & 0.79 & 0.83 & 0.63 & 0.88 & 0.86 & 0.12 \\ 
4 & Linear Dyscriminant Analysis & 0.81 & 0.84 & 0.70 & 0.91 & 0.87 & 0.09 \\ 
5 & Quadric Dyscriminant Analysis & 0.79 & 0.83 & 0.64 & 0.88 & 0.86 & 0.12 \\ 
\hline
\end{tabular}
\caption{Basic statistics for models with features from consistency method} 
\end{table}



> xtable(stats_best_models, caption="Basic statistics for models with features from filtered best method")
% latex table generated in R 3.1.3 by xtable 1.7-4 package
% Wed Jun 10 18:32:02 2015
\begin{table}[ht]
\centering
\begin{tabular}{rlrrrrrr}
\hline
&  \\ 
\hline
1 & Logistic Regression & 0.81 & 0.84 & 0.72 & 0.90 & 0.87 & 0.10 \\ 
2 & K-NN & 0.79 & 0.82 & 0.66 & 0.90 & 0.86 & 0.10 \\ 
3 & Linear Dyscriminant Analysis & 0.80 & 0.84 & 0.66 & 0.89 & 0.86 & 0.11 \\ 
4 & Quadric Dyscriminant Analysis & 0.79 & 0.86 & 0.63 & 0.86 & 0.86 & 0.14 \\ 
\hline
\end{tabular}
\caption{Basic statistics for models with features from filtered best method} 
\end{table}




> xtable(stats_full_models, caption="Basic statistics for models with all features")
% latex table generated in R 3.1.3 by xtable 1.7-4 package
% Wed Jun 10 18:33:35 2015
\begin{table}[ht]
\centering
\begin{tabular}{rlrrrrrr}
\hline
& Label & ACC & TPR & SPC & PPV & F1 & FDR \\ 
\hline
1 & Boosting & 0.79 & 0.81 & 0.69 & 0.90 & 0.85 & 0.10 \\ 
2 & Random Forest & 0.76 & 0.78 & 0.67 & 0.91 & 0.84 & 0.09 \\ 
3 & AIC Logistic regression & 0.78 & 0.82 & 0.65 & 0.88 & 0.85 & 0.12 \\ 
4 & Logistic regression & 0.78 & 0.82 & 0.67 & 0.88 & 0.85 & 0.12 \\ 
5 & K-NN & 0.80 & 0.84 & 0.67 & 0.89 & 0.87 & 0.11 \\ 
6 & Linear Dyscryminant Analysis & 0.78 & 0.83 & 0.63 & 0.88 & 0.85 & 0.12 \\ 
7 & Quadric Dyscriminant Analysis & 0.78 & 0.87 & 0.59 & 0.82 & 0.84 & 0.18 \\ 
\hline
\end{tabular}
\caption{Basic statistics for models with all features} 
\end{table}
> 
  
  > xtable(score_card, caption="Scorecard table")
% latex table generated in R 3.1.3 by xtable 1.7-4 package
% Wed Jun 10 18:36:04 2015
\begin{table}[ht]
\centering
\begin{tabular}{rlr}
\hline
& Variable & Value \\ 
\hline
1 & (Intercept) & 511.93 \\ 
2 & `CHK\_ACCT$>$=200 DM` & -30.32 \\ 
3 & `CHK\_ACCT0-200 DM` & -14.95 \\ 
4 & `CHK\_ACCTno checking account` & -50.64 \\ 
5 & `DURATION\_TO\_AGE01 DURATION\_TO\_AGE $<$= 1.21875` & 26.56 \\ 
6 & `DURATION\_TO\_AGE02 DURATION\_TO\_AGE $<$= 3` & 67.92 \\ 
7 & `HISTORYcritical account/ other credits existing (not at this bank)` & -52.15 \\ 
8 & `HISTORYdelay in paying off in the past` & -37.21 \\ 
9 & `HISTORYexisting credits paid back duly till now` & -36.26 \\ 
10 & `HISTORYno credits taken/ all credits paid back duly` & -6.03 \\ 
11 & `DURATION01 DURATION $<$= 33` & 12.13 \\ 
12 & `DURATION02 DURATION $<$= 72` & 2.37 \\ 
13 & `AMOUNT01 AMOUNT $<$= 3913` & -59.83 \\ 
14 & `AMOUNT02 AMOUNT $<$= 7824` & 14.85 \\ 
15 & `AMOUNT03 AMOUNT $<$= 18424` & 47.81 \\ 
16 & `SAVINGS\_ACCT$>$=1000 DM` & -28.61 \\ 
17 & `SAVINGS\_ACCT100-500 DM` & -1.22 \\ 
18 & `SAVINGS\_ACCT500-1000 DM` & -14.45 \\ 
19 & `SAVINGS\_ACCTunknown/ no savings account` & -30.90 \\ 
20 & `PURPOSEcar (new)` & 16.19 \\ 
21 & `PURPOSEcar (used)` & -20.29 \\ 
22 & PURPOSEeducation & 31.75 \\ 
23 & `PURPOSEfurniture/equip\_domestic applia` & 11.91 \\ 
24 & PURPOSEothers & -10.23 \\ 
25 & `PURPOSEradio/television` & -4.87 \\ 
26 & PURPOSErepairs & 12.99 \\ 
27 & PURPOSEretraining & -50.63 \\ 
28 & `AMOUNT\_TO\_DURATION01 AMOUNT\_TO\_DURATION $<$= 250.944444444444` & -24.12 \\ 
29 & `AMOUNT\_TO\_DURATION02 AMOUNT\_TO\_DURATION $<$= 291.583333333333` & -48.23 \\ 
30 & `AMOUNT\_TO\_DURATION03 AMOUNT\_TO\_DURATION $<$= 358.090909090909` & -54.94 \\ 
31 & `AMOUNT\_TO\_DURATION04 AMOUNT\_TO\_DURATION $<$= 2482.66666666667` & 3.82 \\ 
\hline
\end{tabular}
\caption{Scorecard table} 
\end{table}  
  