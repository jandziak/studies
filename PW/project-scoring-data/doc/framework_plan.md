

# Introduction 

*Why was the study undertaken? What was the research question, the tested hypothesis or the purpose of the research?*

Purposes of the research: 

1. Find and describe relations in the data (relations bewteen explanatory variables and response variable, relations bewteen explanatory variables). 

2. Compare different methods / algorithms to perform exploratory data analysis and predictive data analysis. 

3. Provide a summary of the analysis, containing suggestions of practical appliance and remarks regarding possible further research.  





# Methods

*When, where, and how was the study done? What materials were used or who was included in the study groups (patients, etc.)?*


## Data formattng

- recoding selected variables, saving as factor
- changing variables type from int to num
- recoding response variable
- renaming columns


## Data pre-processing 

- search for detecting missing, corrupt and invalid data (basic summaries, graphical tools)
- creating derived variables
- binning countinuous variables
- correcting bins (levels) of categorical variables (combining levels if needed)
- recoding variables to WoE

Also:

- visualizations of IV of variables binned in different ways 
- visualizations of IV of binned variables, WoE of selected binned variables 



## Data modelling 

**Deadline**: 31. maja (kody + analiza + raport)


### Klasyfikacja wraz z oceną dokładności [Janek]

* regresja logistyczna 

    - podejście standardowe i podejście cost-sensitive (uwzględnienie różnych kosztów różnych błędów, tj. więcej nas kosztuje, jak damy złemu klientowi kredyt niż odwrotnie; hasła: cost-matrix, cost-sensitive learning)
    - porównanie modeli budowanych z cechami skategoryzowanymi (typu factor) i cechami numerycznymi (cechy skategoryzowane przekodowane do WOE)
    - porównanie performance modeli
    
* drzewo klasyfikacyjne (1 drzewo, random forest)

* LDA, QDA

* zastosowanie wybranej metody redukcji wymiaru w połączeniu z klasyfikacją (np. na metodzie, która da najlepsze wyniki z powyższych)



### Analiza skupień wraz z oceną poprawności [Marta]

(uzupełnię)



# Results

*What answer was found to the research question; what did the study find? Was the tested hypothesis true?*





# Discussion

*What might the answer imply and why does it matter? How does it fit in with what other researchers have found? What are the perspectives for future research?*



