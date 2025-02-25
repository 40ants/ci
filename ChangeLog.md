<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# ChangeLog

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E18-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.18.0 (2025-02-25)

<a id="added"></a>

### Added

New argument `CHECKOUT-SUBMODULES` was added to jobs. By default it is `NIL`, but if you set this argument to T, then `actions/checkout` action will also download git submodules.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E17-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.17.0 (2025-02-06)

<a id="added"></a>

### Added

Functions for creation jobs now accept two new arguments:

* `STEPS-BEFORE` argument allows to specify a list of steps to be performed before the job. For example, this can be used to install some system packages required for loading `ASDF` systems during the job execution.
* `STEPS-AFTER` argument is the same as previous one, but executes steps after the job.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E16-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.16.0 (2024-12-14)

<a id="added"></a>

### Added

Now dynamic space size can be given for lisp steps.

There are two ways to set it:

```
(build-docs
  :asdf-system "cl-telegram-bot-docs"
  :env (("DYNAMIC_SPACE_SIZE" . "4Gb")))
```
This way it will be applied only to the step of the documentation building,
because [docs-builder script][843b] allows to use
such environment variable.

But if you `CI` process fails to compile the `ASDF` system because of the memory limit,
then you need to set dynamic space size on the earlier state - during "Setup Lisp"
step. For this case an argument `DYNAMIC-SPACE-SIZE` can be given:

```
(build-docs
 :asdf-system "cl-telegram-bot-docs"
 :dynamic-space-size "4gb")
```
<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E15-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.15.0 (2024-03-02)

<a id="new"></a>

### New

* Now you can specify `ENV` argument to [`40ants-ci/workflow:defworkflow`][95ea] and any job. This should be an alist where keys are strings and values are evaluated during GitHub workflow generation phase. Read more in [`Adding env variables`][ff55] section.
* Also, [`40ants-ci/jobs/autotag:autotag`][dcc3] function now ignores `TOKEN-PATTERN` argument if `ENV` argument was given and has `GITHUB_TOKEN` value for whole job.

<a id="backward-incompatible-changes"></a>

### Backward incompatible changes

* When additional keyword arguments to [`40ants-ci/steps/sh:sh`][4d70] function are given, they are transformed into env variables. Previously, their names were taken as is. Now they are uppercased and dash symbols are replaced with underscores.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E14-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.14.0 (2024-02-25)

<a id="changed"></a>

### Changed

All jobs now use setup-lisp@v4 where internal caching was implemented.

Also all jobs were switched to from `actions/checkout@v3` to `actions/checkout@v4` action.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E13-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.13.0 (2023-12-14)

<a id="changed"></a>

### Changed

Jobs now use setup-lisp@v3 action where a fix to quicklisp-client is applied.
This fix makes ql:quickload work with package-inferred systems.

If you want to use this fix in your own environments, you can find
it [here][64c0].

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E12-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.12.0 (2023-12-11)

<a id="changed"></a>

### Changed

Use `secrets.GITHUB_TOKEN` instead of `secrets.DEPLOY_TRIGGER_TOKEN` and set required scopes for the token.
This way you don't have to setup a special secret for each repository or an organization.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E11-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.11.0 (2023-12-01)

<a id="added"></a>

### Added

New job class [`40ants-ci/jobs/autotag:autotag`][1b03] was added.

Use it like this:

```lisp
(defworkflow release
  :on-push-to "master"
  :jobs ((40ants-ci/jobs/autotag:autotag)))
```
and it will search for new semver tags in the ChangeLog.md file and push them to the git.

<a id="changed"></a>

### Changed

Slots `quicklisp` and `lisp` were moved from class [`40ants-ci/jobs/job:job`][17c5] to [`40ants-ci/jobs/lisp-job:lisp-job`][2f4c].

<a id="fixed"></a>

### Fixed

Class [`40ants-ci/jobs/critic:critic`][cd00] was fixed for case when there are multiple critiques to ignore.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E10-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.1 (2023-03-08)

* Fixed installation of the Linter. Now it depends on [`40ants-asdf-system`][d2a8] system.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E10-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.0 (2022-11-10)

* Now Linter does `qlot install --no-deps` and quickloads only those systems, which should be linted.
* Also, [`40ants-ci`][b171] system now inherits from [`40ants-asdf-system`][d2a8] system.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E9-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.9.0 (2022-11-10)

* Fixed warnings about `set-output` and [outdated Node.js versions][22b4] in checkout and cache actions.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E8-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.8.1 (2022-09-18)

* Fixed default value of asdf-systems slot of [`40ants-ci/jobs/linter:linter`][8918] class.
* Also, now linter accepts `CHECK-IMPORTS` argument and is able to warn on unused or missing imports in package-inferred systems.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E8-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.8.0 (2022-03-21)

* Fixed caching on `OSX`. Previously, job failed with
`/Users/runner/.roswell/bin/qlot: line 4: exec: ros: not found` error
if `:cache t` was given to a job running on `OSX` and Roswell was restored from a cache.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E7-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.7.0 (2022-03-13)

* `40ants-ci/jobs/critic:critic` ([`1`][484a] [`2`][cd00]) function's argument `IGNORE-CRITICUES` was
renames to the `IGNORE-CRITIQUES` argument.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E6-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.6.0 (2022-02-21)

* New job type "critic" was added. It advices how to make you Lisp code better.
Learn more about this job type at [`Critic`][371b] section.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E5-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.0 (2022-01-28)

* Move the actions/checkout action from v1 to v2.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E4-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.4.0 (2022-01-28)

* Now multiple jobs of the same type can be listed in the same workflow.
* Also, you can change a job's name using `:NAME` argument.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E3-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.3.0 (2021-10-24)

* Now jobs `40ants-ci/jobs/linter:linter` ([`1`][523a] [`2`][8918]), `40ants-ci/jobs/run-tests:run-tests` ([`1`][e35d] [`2`][6cb7]) and `40ants-ci/jobs/docs:build-docs` ([`1`][13b8] [`2`][1ddb])
support `ASDF-VERSION` argument.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E2-2E2-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.2 (2021-06-18)

* Fixed an occasional failure on `qlot update` inside linter workflow.
Usually it happed when quicklisp distribution was updated and `qlfile.lock`
changed.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E2-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.1 (2021-04-22)

* Linter step was fixed to use default
`ASDF` system if it wasn't specified explicitly.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E2-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.0 (2021-04-15)

* Supported `ERROR-ON-WARNINGS` argument for documentation builder.
* Argument `ASD-SYSTEM` was renamed to `ASDF-SYSTEM`.
* Moved this project's documentation to `40ANTS-DOC` system.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E1-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.1.1 (2021-03-08)

* Fixed the cache key to use `*.asd` files.

<a id="x-2840ANTS-CI-DOCS-2FCHANGELOG-3A-3A-7C0-2E1-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.1.0 (2021-02-26)

* Initial version.


[d2a8]: /home/runner/work/40ants-asdf-system/40ants-asdf-system/docs/build/#x-28-23A-28-2818-29-20BASE-CHAR-20-2E-20-2240ants-asdf-system-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29
[b171]: https://40ants.com/ci/#x-28-23A-28-289-29-20BASE-CHAR-20-2E-20-2240ants-ci-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29
[1b03]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FAUTOTAG-3AAUTOTAG-20CLASS-29
[dcc3]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FAUTOTAG-3AAUTOTAG-20FUNCTION-29
[cd00]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FCRITIC-3ACRITIC-20CLASS-29
[484a]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FCRITIC-3ACRITIC-20FUNCTION-29
[1ddb]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FDOCS-3ABUILD-DOCS-20CLASS-29
[13b8]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FDOCS-3ABUILD-DOCS-20FUNCTION-29
[17c5]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FJOB-3AJOB-20CLASS-29
[8918]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FLINTER-3ALINTER-20CLASS-29
[523a]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FLINTER-3ALINTER-20FUNCTION-29
[2f4c]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FLISP-JOB-3ALISP-JOB-20CLASS-29
[6cb7]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FRUN-TESTS-3ARUN-TESTS-20CLASS-29
[e35d]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FRUN-TESTS-3ARUN-TESTS-20FUNCTION-29
[4d70]: https://40ants.com/ci/#x-2840ANTS-CI-2FSTEPS-2FSH-3ASH-20FUNCTION-29
[95ea]: https://40ants.com/ci/#x-2840ANTS-CI-2FWORKFLOW-3ADEFWORKFLOW-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[371b]: https://40ants.com/ci/#x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-40CRITIC-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[ff55]: https://40ants.com/ci/#x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-40ENV-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[22b4]: https://github.blog/changelog/2022-09-22-github-actions-all-actions-will-begin-running-on-node16-instead-of-node12/
[843b]: https://github.com/40ants/docs-builder
[64c0]: https://github.com/40ants/quicklisp-client-fix

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
