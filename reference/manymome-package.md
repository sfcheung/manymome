# manymome: Mediation, Moderation and Moderated-Mediation After Model Fitting

Computes indirect effects, conditional effects, and conditional indirect
effects in a structural equation model or path model after model
fitting, with no need to define any user parameters or label any paths
in the model syntax, using the approach presented in Cheung and Cheung
(2024)
[doi:10.3758/s13428-023-02224-z](https://doi.org/10.3758/s13428-023-02224-z)
. Can also form bootstrap confidence intervals by doing bootstrapping
only once and reusing the bootstrap estimates in all subsequent
computations. Supports bootstrap confidence intervals for standardized
(partially or completely) indirect effects, conditional effects, and
conditional indirect effects as described in Cheung (2009)
[doi:10.3758/BRM.41.2.425](https://doi.org/10.3758/BRM.41.2.425) and
Cheung, Cheung, Lau, Hui, and Vong (2022)
[doi:10.1037/hea0001188](https://doi.org/10.1037/hea0001188) . Model
fitting can be done by structural equation modeling using lavaan() or
regression using lm().

## See also

Useful links:

- <https://sfcheung.github.io/manymome/>

- Report bugs at <https://github.com/sfcheung/manymome/issues>

## Author

**Maintainer**: Shu Fai Cheung <shufai.cheung@gmail.com>
([ORCID](https://orcid.org/0000-0002-9871-9448))

Authors:

- Sing-Hang Cheung ([ORCID](https://orcid.org/0000-0001-5182-0752))

Other contributors:

- Rong Wei Sun ([ORCID](https://orcid.org/0000-0003-0034-1422))
  \[contributor\]
