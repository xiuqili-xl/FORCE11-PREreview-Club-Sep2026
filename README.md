# FORCE11 PREreview Club Sep 2026

This repository contains exploratory code written as part of the September 2026 FORCE11 PREreview Club review of:

> Yin H, Ahn W, Forster PM, Rust R. 2026. Tracking claim changes from preprint to publication across 72,644 biomedical studies using large language models. Version 2. bioRxiv. doi:[10.64898/2026.06.30.735556](https://doi.org/10.64898/2026.06.30.735556)

We also explored the data and code shared by the authors in the `rustlab1/PreprintPaperTracker`repository.

> Yin H, Ahn W, Forster PM, Rust R. Tracking claim changes from preprint to publication across 72,644 biomedical studies using large language models [Computer software]. https://github.com/rustlab1/PreprintPaperTracker

Our collaborative review will be linked here once it is available:

> **PREreview:** [link forthcoming]


<br>

## Scope and limitations

> [!IMPORTANT] 
> The code in this repository was written to quickly evaluate the data during our preprint review process. It is exploratory and is *not* intended to serve as a reproducible analysis workflow.

Some scripts retrieve live data from the bioRxiv API or files hosted in the authors’ GitHub repository. Because those sources may change over time, rerunning the scripts may not reproduce the exact results obtained during the review. The repository also does not provide a fully specified computational environment or an automated execution pipeline.


<br>

## Exploratory analyses

The analyses in this repository were used to:

* inspect the authors’ corpus of 72,644 matched preprint–publication pairs
* recreate selected figures in the preprint
* compare the manuscript corpus with preprint–publication data retrieved from the bioRxiv `/pubs/` API
* inspect bioRxiv `/details/` metadata, including preprint version information
* explore the distribution of preprint-to-publication delays using both the authors corpus and data from the bioRxiv `/pubs/` API

These analyses were conducted to inform the review. They should not be interpreted as a complete reanalysis or independent reproduction of the preprint.


<br>

## File/folder description

* `code/manuscript-data-viz.R` recreates selected manuscript figures using the authors’ shared corpus
* `code/bioRxiv-data-pubs.R` retrieves preprint–publication pairs through the bioRxiv `/pubs/` endpoint and compares them with the manuscript corpus
* `code/biorxiv-data-details.R` retrieves version-level metadata through the bioRxiv `/details/` endpoint
* `code/preprint-to-publication-delay.R` explores publication-delay distributions in the manuscript corpus and bioRxiv API data
* `data_biorxiv_pubs/` contains quarterly R data files retrieved from the bioRxiv `/pubs/` endpoint
* `data_biorxiv_details/` contains monthly R data files retrieved from the bioRxiv `/details/` endpoint
* `data_processed/` contains derived CSV files used in the exploratory analyses
* `graphs/` contains figures generated during the review analysis


<br>

## Data provenance

The repository uses or derives data from:

* the [preprint and associated materials](https://doi.org/10.64898/2026.06.30.735556) cited above
* the authors’ [PreprintPaperTracker repository](https://github.com/rustlab1/PreprintPaperTracker)
* the [bioRxiv API](https://api.biorxiv.org/)

Users should consult those sources for their applicable licenses, attribution requirements, and current versions. Inclusion of derived or cached data in this repository does not supersede the terms of the original data sources.


<br>

## Use of AI-assisted agents

Codex (GPT 5.6 Sol Light) were used to support portions of the exploratory coding (i.e., functions for extracting data from bioRxiv and PubMed APIs) and documentation in this repository (i.e., `README.md`). Code and text developed by the agent were reviewed and revised by the repository contributors, who remain responsible for the analyses, 
interpretations, and final review. 

<br>

## License
The code in this repository is available under the MIT License.

Data originating from the preprint authors, bioRxiv, or other external sources
remain subject to the licenses and terms specified by those sources.

<br>


