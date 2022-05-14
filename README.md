# muSignAl: <ins>mu</ins>ltiple <ins>Sign</ins>ature <ins>Al</ins>gorithm
An R package for detecting multiple signatures with similar predictive performance.

**Visit us at:** [https://github.com/ShuklaLab/muSignAl](https://github.com/ShuklaLab/muSignAl)

## Requirements

muSignAl requires a couple of R packages to be pre-installed, which can be installed as below:

```
install.packages("glmnet")
install.packages("pROC")
```

## Installation

To install the latest version of muSignAl, please follow the steps:

```
install.packages("devtools")
devtools::install_github("ShuklaLab/muSignAl")
```

To update the package just run the `install_github("ShuklaLab/muSignAl")` command again.

A test script (`test.R`), ran on a sample dataset (`AD.xlsx`) and its final output (`sign_cvd2.xlsx`) are present in the `example` folder.

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## License
[MIT](https://choosealicense.com/licenses/mit/)
