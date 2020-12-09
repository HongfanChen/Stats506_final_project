# Stats506_final_project
This is my final project for stats 506 Fall 2020.

## Question

> **Do commercial buildings open 24 hours have larger average work forces than those that are not?  Does this depend on square footage category and principal activity of the building?**   

## DATA and Variables

* DATASET: 2012 US Commercial Building Energy Consumption Survey([CBECS](https://www.eia.gov/consumption/commercial/data/2012/index.php?view=microdata)) data. 

For more information about data, please click [documentation](https://www.eia.gov/consumption/commercial/data/2012/pdf/user_guide_public_use_aug2016.pdf)  

For a brief review, please read the [write up](https://raw.githack.com/HongfanChen/Stats506_final_project/main/final_project_Hongfan.html).  

**WARNING** : The **table** produced by `DT:datatable` sometimes **CANNOT** show on Github html viewer. So if you are looking for something more detailed, please download the `final_project_Hongfan.html` and open it locally.

* **Variables**

| Variable Name | Description |
| :----: | :---- |
| `PUBID` | Building identifier |
| `FINALWT` | Final full sample building weight | 
| `FINALWT1 - FINALWT197` | Final replicate weight |
| `NWKER` | Number of employees |
| `OPEN24` | Open 24 hours a day |
| `SQFTC` | Square footage category |
| `PBA` | Principal building activity |

## Code Structure

* data folder contains the data used in analysis.
* graph folder contains the graph output.
* proposal folder contains the initial proposal proved by Dr.Henderson
* final.R is the R scripts file that does the most part of the analysis.
* final_project_Hongfan.Rmd is the Rmarkdown file that produces the writeup.

