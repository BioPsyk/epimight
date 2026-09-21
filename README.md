<br>

<div align="center">

  ![Logotype](./guides/logotype.png)

  <p align="center">
    <strong>Epimight is a powerful epidemiology analysis pipeline written in R</strong>
  </p>

  [![Docker Image Version](https://img.shields.io/docker/v/biopsyk/epimight?sort=semver&logo=docker)](https://hub.docker.com/r/biopsyk/epimight)

</div>

## Features

- 🔋 **Batteris included**: An easy to use pipeline that takes care of bias adjustments, cumulative incidence, heritability, genetic correlation, stratification and meta analysis (fixed and random model).
- 📚 **Well documented**: Step-by-step guides, data format specifications and example code makes sure you can get started quickly.
- 🧩 **Modular**: The pipeline consists of components that can be swapped out or used independently.
- 💪 **Robust**: A rigorous test suite makes sure the package works as expected and careful input arguments validation makes sure the package is used correctly.

## Quick Start 🚀

Create a directory to work in and perform the following steps inside that direction:

1. Download [this R script](./guides/pipeline/guide.R), name it `run-epimight.R`
2. Download [this time-to-event data](./tests/data/pipeline-tte.csv), name it `pipeline-tte.csv`
3. Run `singularity shell docker://biopsyk/epimight:latest`
4. Inside the singularity shell, run `Rscript run-epimight.R`

You should see the following output in the console:

```
|      rg |     se |     l95 |     u95 |
|---------+--------+---------+---------|
| -0.0729 | 0.0268 | -0.1254 | -0.0205 |
```

This guide explains how the pipeline works and what each line of code in `run-epimight.R` does:

- [Guide: running the pipeline](./guides/pipeline/guide.org)

## Support 💬

If you have any questions, suggestions, or need assistance, please open a GitHub issue.
