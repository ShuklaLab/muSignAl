# muSignAl: <ins>mu</ins>ltiple <ins>Sign</ins>ature <ins>Al</ins>gorithm

An R package for detecting multiple signatures with similar predictive performance.

*Code author/maintainer: [@bodhayan](https://github.com/bodhayan)*

**Visit us at:** [Shukla Lab](https://shuklalab.github.io/)

![GitHub](https://img.shields.io/github/license/ShuklaLab/muSignAl)
![GitHub R package version](https://img.shields.io/github/r-package/v/ShuklaLab/muSignAl)

## Requirements

muSignAl was coded in R v3.6, and require a couple of packages (`glmnet` and `pROC`) to be pre-installed, which can be installed as below:

```
install.packages(c("glmnet","pROC"))
```

## Installation

To install the latest version of muSignAl, please follow the steps:

```
install.packages("devtools")
devtools::install_github("ShuklaLab/muSignAl")
```

To update the package just run the same command again.

## Sample run

A test script (`test.R`), ran on a sample dataset (`AD.xlsx`) and its final output (`sign_cvd2.xlsx`) are present in the `example` folder.

## Contributing and licensing
[MIT](https://choosealicense.com/licenses/mit/) License is free license software and grants the software end user rights such as copying, modifying, merging, distributing, etc.

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## Publication
If you're using our tool and/or dataset, please cite:

[Prasad B, Bjourson AJ, Shukla P. muSignAl: An algorithm to search for multiple omic signatures with similar predictive performance. Proteomics. 2023 Jan;23(2):e2200252. doi: 10.1002/pmic.202200252. Epub 2022 Oct 3. PMID: 36076312.](https://doi.org/10.1002/pmic.202200252)
***
*Last updated on: 18 Jan 2023*
