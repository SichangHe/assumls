# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.0.0](https://github.com/SichangHe/assumls/releases/tag/v0.0.0) - 2026-01-01

### Added

- *(perf)* incremental indexing on editor change
- *(generated)* semantic token highlighting
- *(generated)* LSP highlight on hover;
- *(generated)* hover work on tag;dup-definition errors;goto references
- *(syntax)* [**breaking**] `@ASSUME:blah` instead of `@ASSUMES blah` to avoid line breaks;
- *(generated)* LSP/linter;E2E test
- init generated LSP server

### Fixed

- rg exit code;UTF-16;type cast;
- *(ci)* properly install fd&rg
- *(overlay)* track version
- *(completion)* mark suggestion as variable not text to show up in nvim

### Other

- *(metadata)* prepare for release
- more assumptions;better README;
- *(build)* size reduction 6.5M -> 3.8M
- *(ci)* pre-commit clippy fmt
- *(debug)* lack of completion seem nvim problem
- *(memory)* run on current thread tokio;
- basic project + dependency
