<br>

<div align="center">

  ![Logotype](./guides/logotype.png)

  <p align="center">
    <strong>Epimight is a powerful R epidemiology analysis pipeline</strong>
  </p>

  [![Docker Image Version](https://img.shields.io/docker/v/biopsyk/epimight?sort=semver&logo=docker)](https://hub.docker.com/r/biopsyk/epimight)

</div>

## Features

- 🔋 **Batteris included**: Implements cumulative incidence, heritability and genetic correlation, along with flexible stratification support and meta analysis (fixed and random model)
- 📚 **Well documented**: Step-by-step guides, data format specifications and example code makes sure you can get started quickly.
- 🧩 **Modular**: Well defined boundaries and separation of concerns makes sure you are free to pick which parts you deem relevant and that you can integrate them into your project effortlessly.
- 💪 **Robust**: A rigorous test suite makes sure the package works as expected and careful input arguments validation makes sure the package is used correctly.

## Quick Start 🚀

1. Create a directory to work in
2. Enter the newly created directory and run all commands inside this directory in the following steps
3. Download [this R script](./guides/genetic-correlation/guide-yob.R), name it `generate-genetic-correlation.R`
4. Download [this TTE test data for disorder 1](./guides/data/tte_SCZ_FS.csv), name it `tte_SCZ_FS.csv`
5. Download [this TTE test data for disorder 2](./guides/data/tte_CAD_FS.csv), name it `tte_CAD_FS.csv`
6. Run `singularity shell docker://biopsyk/epimight:latest`
7. Inside the singularity shell, run `Rscript generate-genetic-correlation.R`

You should see genetic correlations of two disorders that has been stratified by birth year and
meta-analyzed using a fixed and random model.

Now you have everything needed to run the example code provided in these step-by-step guides (reading them in order is recommended):

- [Guide: estimate cumulative incidence of disorder](./guides/cumulative-incidence/guide.org)
- [Guide: estimate heritability of disorder](./guides/heritability/guide.org)
- [Guide: estimate heritability of disorder stratified by year of birth](./guides/heritability/guide-yob.org)
- [Guide: estimate genetic correlation between two disorders](./guides/genetic-correlation/guide.org)
- [Guide: estimate genetic correlation between two disorders stratified by year of birth](./guides/genetic-correlation/guide-yob.org)

You can also view the reference documentation for each of the analysis components here:

- [Reference: Cumulative incidence](./guides/cumulative-incidence/index.org)
- [Reference: Heritability](./guides/heritability/index.org)
- [Reference: Genetic correlation](./guides/genetic-correlation/index.org)

## Support 💬

If you have any questions, suggestions, or need assistance, please open a GitHub issue.
