<a id="x-2840ANTS-CI-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# ChangeLog

<a id="x-2840ANTS-CI-2FCHANGELOG-3A-3A-7C0-2E10-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.1 (2023-03-08)

* Fixed installation of the Linter. Now it depends on [`40ants-asdf-system`][d2a8] system.

<a id="x-2840ANTS-CI-2FCHANGELOG-3A-3A-7C0-2E10-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.0 (2022-11-10)

* Now Linter does "qlot install --no-deps" and quickloads only those systems, which should be linted.

* Also, [`40ants-ci`][b171] system now inherits from [`40ants-asdf-system`][d2a8] system.

<a id="x-2840ANTS-CI-2FCHANGELOG-3A-3A-7C0-2E9-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.9.0 (2022-11-10)

* Fixed warnings about `set-output` and [outdated Node.js versions][22b4] in checkout and cache actions.

<a id="x-2840ANTS-CI-2FCHANGELOG-3A-3A-7C0-2E8-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.8.1 (2022-09-18)

* Fixed default value of asdf-systems slot of [`40ants-ci/jobs/linter:linter`][8918] class.

* Also, now linter accepts `CHECK-IMPORTS` argument and is able to warn on unused or missing imports in package-inferred systems.

<a id="x-2840ANTS-CI-2FCHANGELOG-3A-3A-7C0-2E8-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.8.0 (2022-03-21)

* Fixed caching on `OSX`. Previously, job failed with
`/Users/runner/.roswell/bin/qlot: line 4: exec: ros: not found` error
if `:cache t` was given to a job running on `OSX` and Roswell was restored from a cache.

<a id="x-2840ANTS-CI-2FCHANGELOG-3A-3A-7C0-2E7-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.7.0 (2022-03-13)

* `40ants-ci/jobs/critic:critic` ([`1`][484a] [`2`][cd00]) function's argument `IGNORE-CRITICUES` was
renames to the `IGNORE-CRITIQUES` argument.

<a id="x-2840ANTS-CI-2FCHANGELOG-3A-3A-7C0-2E6-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.6.0 (2022-02-21)

* New job type "critic" was added. It advices how to make you Lisp code better.
Learn more about this job type at [`Critic`][240b] section.

<a id="x-2840ANTS-CI-2FCHANGELOG-3A-3A-7C0-2E5-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.0 (2022-01-28)

* Move the actions/checkout action from v1 to v2.

<a id="x-2840ANTS-CI-2FCHANGELOG-3A-3A-7C0-2E4-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.4.0 (2022-01-28)

* Now multiple jobs of the same type can be listed in the same workflow.

* Also, you can change a job's name using `:NAME` argument.

<a id="x-2840ANTS-CI-2FCHANGELOG-3A-3A-7C0-2E3-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.3.0 (2021-10-24)

* Now jobs `40ants-ci/jobs/linter:linter` ([`1`][523a] [`2`][8918]), `40ants-ci/jobs/run-tests:run-tests` ([`1`][6cb7] [`2`][e35d]) and `40ants-ci/jobs/docs:build-docs` ([`1`][1ddb] [`2`][13b8])
support `ASDF-VERSION` argument.

<a id="x-2840ANTS-CI-2FCHANGELOG-3A-3A-7C0-2E2-2E2-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.2 (2021-06-18)

* Fixed an occasional failure on `qlot update` inside linter workflow.
Usually it happed when quicklisp distribution was updated and `qlfile.lock`
changed.

<a id="x-2840ANTS-CI-2FCHANGELOG-3A-3A-7C0-2E2-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.1 (2021-04-22)

* Linter step was fixed to use default
`ASDF` system if it wasn't specified explicitly.

<a id="x-2840ANTS-CI-2FCHANGELOG-3A-3A-7C0-2E2-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.0 (2021-04-15)

* Supported `ERROR-ON-WARNINGS` argument for documentation builder.

* Argument `ASD-SYSTEM` was renamed to `ASDF-SYSTEM`.

* Moved this project's documentation to `40ANTS-DOC` system.

<a id="x-2840ANTS-CI-2FCHANGELOG-3A-3A-7C0-2E1-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.1.1 (2021-03-08)

* Fixed the cache key to use `*.asd` files.

<a id="x-2840ANTS-CI-2FCHANGELOG-3A-3A-7C0-2E1-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.1.0 (2021-02-26)

* Initial version.


[d2a8]: /home/runner/work/40ants-asdf-system/40ants-asdf-system/docs/build/#x-28-23A-28-2818-29-20BASE-CHAR-20-2E-20-2240ants-asdf-system-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29
[b171]: https://40ants.com/ci/#x-28-23A-28-289-29-20BASE-CHAR-20-2E-20-2240ants-ci-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29
[cd00]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FCRITIC-3ACRITIC-20CLASS-29
[484a]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FCRITIC-3ACRITIC-20FUNCTION-29
[1ddb]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FDOCS-3ABUILD-DOCS-20CLASS-29
[13b8]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FDOCS-3ABUILD-DOCS-20FUNCTION-29
[8918]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FLINTER-3ALINTER-20CLASS-29
[523a]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FLINTER-3ALINTER-20FUNCTION-29
[6cb7]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FRUN-TESTS-3ARUN-TESTS-20CLASS-29
[e35d]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FRUN-TESTS-3ARUN-TESTS-20FUNCTION-29
[240b]: https://40ants.com/ci/#x-2840ANTS-CI-3A-3A-40CRITIC-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[22b4]: https://github.blog/changelog/2022-09-22-github-actions-all-actions-will-begin-running-on-node16-instead-of-node12/

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
