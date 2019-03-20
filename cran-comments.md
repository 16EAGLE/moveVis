## Test environments

* Ubuntu 18.04.2 LTS, R 3.5.2 (local)
* Ubuntu 14.04.5 LTS, R-release (travis)
* Ubuntu 14.04.5 LTS, R-devel (travis)
* Windows Server 2008, R 3.5.3 (winbuilder)
* Windows Server 2008, R 3.6.0 (winbuilder)
* Windows Server 2008 R2 SP1, R-release (r-hub)
* Windows Server 2008 R2 SP1, R-devel (r-hub)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a resubmission after the CRAN auto-check service rejected the former submission due to a NOTE on examples running longer than 10 seconds.
* Thus, time-consuming examples have been excluded from being checked using `\donttest`.