There are 2 changes and 1 bugfix (see NEWS.md).

---

## Test environments
* Ubuntu 20.04, R 4.4.1
* Microsoft Windows Server 2022, R 4.4.1
* MacOS, R 4.4.1

## R CMD check results

There were no ERRORs or WARNINGs. 

## Downstream dependencies

- Achilles, allofus, CohortAlgebra, CommonDataModel, FeatureExtraction, CohortAlgebra, DatabaseConnector, DrugExposureDiagnostics, TreatmentPatterns, CDMConnector, and DrugExposureDiagnostics depend on SqlRender, and have been tested with this new version. No problems were found. 

- CommonDataModel has been updated since the last attempt to release this new version. It no longer contains the offending unit test, and passes R check with this new version.