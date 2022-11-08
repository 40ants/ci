<a id="x-2840ANTS-CI-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# 40Ants-CI - Github Workflow Generator

[![](https://github-actions.40ants.com/40ants/ci/matrix.svg)][de0b]

![](http://quickdocs.org/badge/ci.svg)

This is a small utility, which can generate GitHub workflows for Common Lisp
projects.

It generates workflow for running tests and building docs. These workflows
use [40ants/run-tests][8469] and [40ants/build-docs][b882]
actions and [`SBLint`][2f94] to check code for compilation errors.

<a id="40-ants-ci-asdf-system-details"></a>

## 40ANTS-CI ASDF System Details

* Version: 0.1.0

* Description: A tool simplify continuous deployment for Common Lisp projects.

* Licence: `BSD`

* Author: Alexander Artemenko

* Homepage: [https://40ants.com/ci/][3f72]

* Source control: [GIT][e681]

<a id="x-2840ANTS-CI-3A-3A-40REASONS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Reasons to Use

* This system hides all entrails related to caching.

* Includes a few ready to use job types.

* Custom job types can be defined and distributed as separate `ASDF` systems.

* You don't have to write `YAML` anymore!

<a id="x-2840ANTS-CI-3A-3A-40QUICKSTART-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Quickstart

This system allows you to define workflows in the lisp code. The best way is to make these
definitions a part of your `ASDF` system. This way [`40ants-ci`][b171] will be able to
automatically understand for which system it builds a workflow.

Each workflow consists of jobs and each job is a number of steps.

There are three predefine types of jobs and you can create your own. Predefined jobs
allows to reuse steps in multiple `CL` libraries.

In next examples, I'll presume you are writing code in a file which is the part
of the package inferred `ASDF` system `EXAMPLE/CI`. A file should have the following header:

```lisp
(defpackage #:example/ci
  (:use #:cl)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/run-tests)
  (:import-from #:40ants-ci/jobs/docs))
```
<a id="x-2840ANTS-CI-3A-3A-40JOB-TYPES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Job Types

<a id="x-2840ANTS-CI-3A-3A-40LINTER-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Linter

The simplest job type is linter. It loads a

```lisp
(defworkflow linter
  :on-pull-request t
  :jobs ((40ants-ci/jobs/linter:linter)))
```
When you'll hit `C-c C-c` on this definition,
it will generate `.github/workflows/linter.yml` with following content:

```json
{
  "name": "LINTER",
  "on": {
    "pull_request": null
  },
  "jobs": {
    "linter": {
      "runs-on": "ubuntu-latest",
      "env": {
        "OS": "ubuntu-latest",
        "QUICKLISP_DIST": "quicklisp",
        "LISP": "sbcl-bin"
      },
      "steps": [
        {
          "name": "Checkout Code",
          "uses": "actions/checkout@v1"
        },
        {
          "name": "Setup Common Lisp Environment",
          "uses": "40ants/setup-lisp@v2",
          "with": {
            "asdf-system": "example"
          }
        },
        {
          "name": "Install SBLint",
          "run": "qlot exec ros install cxxxr/sblint",
          "shell": "bash"
        },
        {
          "name": "Run Linter",
          "run": "qlot exec sblint example.asd",
          "shell": "bash"
        }
      ]
    }
  }
}
```
Here you can see, a few steps in the job:

1. Checkout the code.

2. Install Roswell & Qlot using [40ants/setup-lisp][8de1] action.

3. Install [`SBLint`][2f94].

4. Run linter for `example.asd`.

Another interesting thing is that this workflow automatically uses `ubuntu-latest` `OS`,
`Quicklisp` and `sbcl-bin` Lisp implementation. Later I'll show you how to redefine these settings.

<a id="x-2840ANTS-CI-2FJOBS-2FLINTER-3ALINTER-20CLASS-29"></a>

##### [class](33a6) `40ants-ci/jobs/linter:linter` (lisp-job)

<a id="x-2840ANTS-CI-3A-3A-40CRITIC-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Critic

This job is similar to linter, but instead of `SBL`int it runs
[Lisp Critic][2100].

Lisp Critic is a program which advices how to make you Common Lisp code more
idiomatic, readable and performant. Also, sometimes it might catch logical
errors in the code.

Here is how you can add this job type in your workflow:

```lisp
(defworkflow ci
  :on-pull-request t
  :jobs ((40ants-ci/jobs/critic:critic)))
```
Also, you might combine this job together with others, for example,
with linter:

```lisp
(defworkflow ci
  :on-pull-request t
  :jobs ((40ants-ci/jobs/linter:linter)
         (40ants-ci/jobs/critic:critic)))
```
and they will be executed in parallel. See docs on [`40ants-ci/jobs/critic:critic`][484a] function
to learn about supported arguments.

<a id="x-2840ANTS-CI-3A-3A-40RUN-TESTS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Running Tests

Another interesting job type is `40ants-ci/jobs/run-tests:run-tests` ([`1`][6cb7] [`2`][e35d]).

When using this job type, make sure, your system
runs tests on `(ASDF:TEST-SYSTEM :system-name)` call
and signals error if something went wrong.

```lisp

(defworkflow ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :jobs ((40ants-ci/jobs/run-tests:run-tests
          :coverage t)))
```
Here I've added a few options to the workflow:

* `by-cron` - sets a schedule.

* `on-push-to` - defines a branch or branches to track.

It will generate `.github/workflows/ci.yml` with following content:

```json
{
  "name": "CI",
  "on": {
    "push": {
      "branches": [
        "master"
      ]
    },
    "pull_request": null,
    "schedule": [
      {
        "cron": "0 10 * * 1"
      }
    ]
  },
  "jobs": {

    "run-tests": {
      "runs-on": "ubuntu-latest",
      "env": {
        "OS": "ubuntu-latest",
        "QUICKLISP_DIST": "quicklisp",
        "LISP": "sbcl-bin"
      },
      "steps": [
        {
          "name": "Checkout Code",
          "uses": "actions/checkout@v1"
        },
        {
          "name": "Setup Common Lisp Environment",
          "uses": "40ants/setup-lisp@v2",
          "with": {
            "asdf-system": "example"
          }
        },
        {
          "name": "Run Tests",
          "uses": "40ants/run-tests@v2",
          "with": {
            "asdf-system": "example",
            "coveralls-token": "${{ secrets.github_token }}"
          }
        }
      ]
    }
  }
}
```
The result is similar to the workflow generated for Linter,
but uses [40ants/setup-lisp][59d7] action
at the final step.

Also, I've passed an option `:coverage t` to the job. Thus coverage
report will be uploaded to [Coveralls.io][b60c] automatically.

<a id="x-2840ANTS-CI-3A-3A-40MATRIX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Defining a test Matrix

Lisp has many implementations and can be used on multiple platforms. Thus
it is a good idea to test our software on many combinations of `OS` and lisp
implementations. Workflow generator makes this very easy.

Here is an example of workflow definition with three dimentional matrix.
It not only tests a library under different lisps and `OS`, but also checks
if it works with the latest Quicklisp and Ultralisp distributions:

```lisp
(defworkflow ci
  :on-pull-request t
  :jobs ((run-tests
          :os ("ubuntu-latest"
               "macos-latest")
          :quicklisp ("quicklisp"
                      "ultralisp")
          :lisp ("sbcl-bin"
                 "ccl-bin"
                 "allegro"
                 "clisp"
                 "cmucl")
          :exclude (;; Seems allegro is does not support 64bit OSX.
                    ;; Unable to install it using Roswell:
                    ;; alisp is not executable. Missing 32bit glibc?
                    (:os "macos-latest" :lisp "allegro")))))
```
<a id="x-2840ANTS-CI-3A-3A-40MULTIPLE-JOBS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### Multiple jobs

Besides a build matrix, you might specify a multiple jobs of the same type,
but with different parameters:

```lisp
(defworkflow ci
  :on-push-to "master"
  :on-pull-request t
  :jobs ((run-tests
          :lisp "sbcl-bin")
         (run-tests
          :lisp "ccl-bin")
         (run-tests
          :lisp "allegro")))
```
This will generate a workflow with three jobs: "run-tests", "run-tests-2" and "run-tests-3".

Meaningful names might be specified as well:

```lisp
(defworkflow ci
  :on-push-to "master"
  :on-pull-request t
  :jobs ((run-tests
          :name "test-on-sbcl"
          :lisp "sbcl-bin")
         (run-tests
          :name "test-on-ccl"
          :lisp "ccl-bin")
         (run-tests
          :name "test-on-allegro"
          :lisp "allegro")))
```
Here is how these jobs will look like in the GitHub interface:

![](https://user-images.githubusercontent.com/24827/151619261-2d49e2a6-bc5c-42db-aec5-674d9229a1b0.png)

<a id="x-2840ANTS-CI-3A-3A-40BUILD-DOCS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Building Docs

Third predefined job type is `40ants-ci/jobs/docs:build-docs` ([`1`][1ddb] [`2`][13b8]).
It uses [40ants/build-docs][613f]
action and will work only if your `ASDF` system uses a documentation builder supported by
[40ants/docs-builder][f2be].

To build docs on every push to master, just use this code:

```lisp

(defworkflow docs
  :on-push-to "master"
  :jobs ((40ants-ci/jobs/docs:build-docs)))
```
It will generate `.github/workflows/docs.yml` with following content:

```json

{
  "name": "DOCS",
  "on": {
    "push": {
      "branches": [
        "master"
      ]
    }
  },
  "jobs": {
    "build-docs": {
      "runs-on": "ubuntu-latest",
      "env": {
        "OS": "ubuntu-latest",
        "QUICKLISP_DIST": "quicklisp",
        "LISP": "sbcl-bin"
      },
      "steps": [
        {
          "name": "Checkout Code",
          "uses": "actions/checkout@v1"
        },
        {
          "name": "Setup Common Lisp Environment",
          "uses": "40ants/setup-lisp@v2",
          "with": {
            "asdf-system": "example",
            "qlfile-template": ""
          }
        },
        {
          "name": "Build Docs",
          "uses": "40ants/build-docs@v1",
          "with": {
            "asdf-system": "example"
          }
        }
      ]
    }
  }
}
```
<a id="x-2840ANTS-CI-3A-3A-40CACHING-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Caching

To significantly speed up our tests, we can cache installed Roswell,
Qlot and Common Lisp fasl files.

To accomplish this task, you don't need to dig into GitHub's docs anymore!
Just add one line `:cache t` to your workflow definition:

```lisp
(defworkflow docs
  :on-push-to "master"
  :cache t
  :jobs ((40ants-ci/jobs/docs:build-docs)))
```
Here is the diff of the generated workflow file. It shows steps, added automatically:

```diff
modified   .github/workflows/docs.yml
@@ -20,13 +20,40 @@
           "name": "Checkout Code",
           "uses": "actions/checkout@v1"
         },
+        {
+          "name": "Grant All Perms to Make Cache Restoring Possible",
+          "run": "sudo mkdir -p /usr/local/etc/roswell\n                 sudo chown \"${USER}\" /usr/local/etc/roswell\n                 # Here the ros binary will be restored:\n                 sudo chown \"${USER}\" /usr/local/bin",
+          "shell": "bash"
+        },
+        {
+          "name": "Get Current Month",
+          "id": "current-month",
+          "run": "echo \"::set-output name=value::$(date -u \"+%Y-%m\")\"",
+          "shell": "bash"
+        },
+        {
+          "name": "Cache Roswell Setup",
+          "id": "cache",
+          "uses": "actions/cache@v2",
+          "with": {
+            "path": "qlfile\n                           qlfile.lock\n                           /usr/local/bin/ros\n                           ~/.cache/common-lisp/\n                           ~/.roswell\n                           /usr/local/etc/roswell\n                           .qlot",
+            "key": "${{ steps.current-month.outputs.value }}-${{ env.cache-name }}-ubuntu-latest-quicklisp-sbcl-bin-${{ hashFiles('qlfile.lock') }}"
+          }
+        },
+        {
+          "name": "Restore Path To Cached Files",
+          "run": "echo $HOME/.roswell/bin >> $GITHUB_PATH\n                 echo .qlot/bin >> $GITHUB_PATH",
+          "shell": "bash",
+          "if": "steps.cache.outputs.cache-hit == 'true'"
+        },
         {
           "name": "Setup Common Lisp Environment",
           "uses": "40ants/setup-lisp@v2",
           "with": {
             "asdf-system": "40ants-ci",
             "qlfile-template": ""
-          }
+          },
+          "if": "steps.cache.outputs.cache-hit != 'true'"
         },
         {
```
<a id="x-2840ANTS-CI-3A-3A-40DETAILS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Details

`TODO`: I have to write a few chapters with details on additional job's parameters
and a way how to create new job types.

<a id="x-2840ANTS-CI-3AGENERATE-20FUNCTION-29"></a>

### [function](e732) `40ants-ci:generate` system &key path

Generates GitHub workflow for given `ASDF` system.

This function searches workflow definitions in all packages
of the given `ASDF` system.

If `PATH` argument is not given, workflow files will be written
to .github/workflow/ relarive to the `SYSTEM`.

<a id="x-2840ANTS-CI-2FJOBS-2FRUN-TESTS-3ARUN-TESTS-20FUNCTION-29"></a>

### [function](b652) `40ants-ci/jobs/run-tests:run-tests` &rest rest &key coverage qlfile asdf-system asdf-version os quicklisp lisp exclude custom

Creates a job step of class [`run-tests`][6cb7].

<a id="x-2840ANTS-CI-2FJOBS-2FRUN-TESTS-3ARUN-TESTS-20CLASS-29"></a>

### [class](d25a) `40ants-ci/jobs/run-tests:run-tests` (lisp-job)

This job test runs tests for a given `ASDF` system.

<a id="x-2840ANTS-CI-2FJOBS-2FDOCS-3ABUILD-DOCS-20FUNCTION-29"></a>

### [function](31f7) `40ants-ci/jobs/docs:build-docs` &key asdf-system asdf-version (error-on-warnings t)

Creates a job of class [`build-docs`][1ddb].

<a id="x-2840ANTS-CI-2FJOBS-2FDOCS-3ABUILD-DOCS-20CLASS-29"></a>

### [class](b840) `40ants-ci/jobs/docs:build-docs` (lisp-job)

Builds documentation and uploads it to GitHub using ["40ants/build-docs" github action][613f].

<a id="x-2840ANTS-CI-2FJOBS-2FLINTER-3ALINTER-20FUNCTION-29"></a>

### [function](dd52) `40ants-ci/jobs/linter:linter` &key asdf-systems asdf-version check-imports

Creates a job which will run `SBL`int for given `ASDF` systems.

If no `ASD` files given, it will use all `ASD` files from
the current `ASDF` system.

<a id="x-2840ANTS-CI-2FJOBS-2FLINTER-3ALINTER-20FUNCTION-29"></a>

### [function](dd52) `40ants-ci/jobs/linter:linter` &key asdf-systems asdf-version check-imports

Creates a job which will run `SBL`int for given `ASDF` systems.

If no `ASD` files given, it will use all `ASD` files from
the current `ASDF` system.

<a id="x-2840ANTS-CI-2FJOBS-2FCRITIC-3ACRITIC-20CLASS-29"></a>

### [class](fb7b) `40ants-ci/jobs/critic:critic` (lisp-job)

<a id="x-2840ANTS-CI-2FJOBS-2FCRITIC-3ACRITIC-20FUNCTION-29"></a>

### [function](0544) `40ants-ci/jobs/critic:critic` &key asdf-systems asdf-version ignore-critiques

Creates a job which will run Lisp Critic for given `ASDF` systems.

If argument `ASDF-SYSTEMS` is `NIL`, it will use `ASDF` system
to which current lisp file is belong.

You may also provide `ASDF-VERSION` argument. It should be
a string. By default, the latest `ASDF` version will be used.


[2100]: https://40ants.com/40ants-critic
[b882]: https://40ants.com/build-doc
[613f]: https://40ants.com/build-docs/
[3f72]: https://40ants.com/ci/
[b171]: https://40ants.com/ci/#x-28-23A-28-289-29-20BASE-CHAR-20-2E-20-2240ants-ci-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29
[484a]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FCRITIC-3ACRITIC-20FUNCTION-29
[1ddb]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FDOCS-3ABUILD-DOCS-20CLASS-29
[13b8]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FDOCS-3ABUILD-DOCS-20FUNCTION-29
[6cb7]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FRUN-TESTS-3ARUN-TESTS-20CLASS-29
[e35d]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FRUN-TESTS-3ARUN-TESTS-20FUNCTION-29
[f2be]: https://40ants.com/docs-builder/
[8469]: https://40ants.com/run-tests
[59d7]: https://40ants.com/run-tests/
[8de1]: https://40ants.com/setup-lisp/
[b60c]: https://coveralls.io/
[e681]: https://github.com/40ants/ci
[de0b]: https://github.com/40ants/ci/actions
[e732]: https://github.com/40ants/ci/blob/2f79ef8ec38a63bfd4ed8423b9cc96d87c6f5748/src/core.lisp#L526
[fb7b]: https://github.com/40ants/ci/blob/2f79ef8ec38a63bfd4ed8423b9cc96d87c6f5748/src/jobs/critic.lisp#L13
[0544]: https://github.com/40ants/ci/blob/2f79ef8ec38a63bfd4ed8423b9cc96d87c6f5748/src/jobs/critic.lisp#L23
[b840]: https://github.com/40ants/ci/blob/2f79ef8ec38a63bfd4ed8423b9cc96d87c6f5748/src/jobs/docs.lisp#L13
[31f7]: https://github.com/40ants/ci/blob/2f79ef8ec38a63bfd4ed8423b9cc96d87c6f5748/src/jobs/docs.lisp#L20
[33a6]: https://github.com/40ants/ci/blob/2f79ef8ec38a63bfd4ed8423b9cc96d87c6f5748/src/jobs/linter.lisp#L13
[dd52]: https://github.com/40ants/ci/blob/2f79ef8ec38a63bfd4ed8423b9cc96d87c6f5748/src/jobs/linter.lisp#L32
[d25a]: https://github.com/40ants/ci/blob/2f79ef8ec38a63bfd4ed8423b9cc96d87c6f5748/src/jobs/run-tests.lisp#L19
[b652]: https://github.com/40ants/ci/blob/2f79ef8ec38a63bfd4ed8423b9cc96d87c6f5748/src/jobs/run-tests.lisp#L29
[2f94]: https://github.com/cxxxr/sblint

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
