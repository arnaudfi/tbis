# tbis


This R package provide Tools for Business Indirect Sampling (_tbis_). 

The method is valid only if the initial business sampling is a _stratified simple sampling_ (the usual in INSEE business survey).

See [JMS 2022](http://jms-insee.fr/jms2022s30_3/) (french paper) for details on the methodology. 


To use it, write in the R console : `remotes::install_github("arnaudfi/tbis")`

On AUS V3 (INSEE) :
- Téléchargez les sources ; 
- Mettez les dans une archive tbis.tar.gz accessible sur AUS (par exemple U:/tbis.tar.gz);
- Executez install.packages("U:/tbis.tar.gz", repos = NULL, type="source")

Si vous avez accès à V:/Pise-DS, vous pouvez plus simplement executer install.packages("V:/PISE-DS/note restructurations et compagnie/Mad FonctionsR/tbis.tar.gz", repos = NULL, type="source")
