Some of the CRAN testing environments do not meet the documented system requirements of this package (specifically, the Solaris environment is running a Java version that was discontinued years ago). We are putting a lot of effort in making our many unit tests pass on these environments. It would be helpful if instead the testing environments were updated or discontinued.

---

## Test environments
* Ubuntu 20.03, R 4.1.1
* Microsoft Windows Server 2019, R 4.1.1
* MacOS, R 4.1.1
* Windows 10, R 4.1.1

## R CMD check results

There were no ERRORs or WARNINGs. 

## Downstream dependencies

- DatabaseConnector and Eunomia depend on SqlRender, and have been tested with this new version. No problems were found.