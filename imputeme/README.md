# Detail description of data submission

After upload of data, a central aspect of the analysis engine is the performance of an imputation-process. It is better described in the [main readme-file](https://github.com/lassefolkersen/impute-me#part-3-imputation-algorithm-description). However, of interest in this module is to highlight the differences in handling of sensitive data in accordance with terms of use and GDPR.

![Schematics](2018-09-20_screenshot_1.png)

As is shown, the following components are deleted 14 days after processing:
 * email
 * filename
 * input-data
 * imputed-data
 
Remaining data consist of all the derived data, such as diseases-gene-scores. Although this includes some genotypes, it is not considered personally traceable because these genotypes cannot be used to track people systematically, e.g. using software like [GEDmatch](https://www.gedmatch.com).