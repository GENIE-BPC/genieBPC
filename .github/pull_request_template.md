What changes are involved in this pull request? Is there a GitHub issue corresponding to this pull request? If so, please provide link.

Checklist
- [ ] Make sure all updates from master branch are pulled to branch issuing pull request
- [ ] Confirm package dependencies are installed by running `renv::install()`
- [ ] For bug corrections, check that unit test was added
- [ ] `usethis::use_spell_check()` runs with no spelling errors in documentation
- [ ] R CMD Check runs without errors, warnings, and notes
- [ ] Document changes from this pull request in `NEWS.md` file
- [ ] Increment the version number using `usethis::use_version(which = "dev")`
- [ ] Run `pkgdown::build_site()`. Check the R console for errors, and review the rendered website
- [ ] Code coverage is suitable for any new functions/features. Review coverage with `covr::report()`. Before you run, begin in a fresh R session without any packages loaded and set `Sys.setenv(NOT_CRAN="true")`.
- [ ] Approve and merge pull request
