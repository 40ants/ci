<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

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

* Description: A tool simplify continuous deployment for Common Lisp projects.
* Licence: `BSD`
* Author: Alexander Artemenko
* Homepage: [https://40ants.com/ci/][3f72]
* Source control: [GIT][e681]
* Depends on: [alexandria][8236], [serapeum][c41d], [str][ef7f], [yason][aba2]

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-40REASONS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Reasons to Use

* This system hides all entrails related to caching.
* Includes a few ready to use job types.
* Custom job types can be defined and distributed as separate `ASDF` systems.
* You don't have to write `YAML` anymore!

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-40QUICKSTART-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Quickstart

This system allows you to define workflows in the lisp code. The best way is to make these
definitions a part of your `ASDF` system. This way `40ants-ci` ([`1`][900b] [`2`][b171]) will be able to
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
<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-40JOB-TYPES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Job Types

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-40AUTOTAG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Autotag

This job is automates git tag placement on the commit where you have changed the ChangeLog.md.

This can be a useful to automate package deployment and releases. You update the changelog,
a job pushes a new git tag and the next action triggers on this tag and build a release.

Or you if you publish your library at Quicklisp distribution, then you can change
it's source type to the `latest-github-tag` to provide more stable releases to your
users. This way you commits into master will be ignored until you change the changelog and
git tag will be pushed. Here is an [example][1cec] how to setup this kind of quicklisp project source.

```lisp
(defworkflow release
  :on-push-to "master"
  :jobs ((40ants-ci/jobs/autotag:autotag)))
```
<a id="x-2840ANTS-CI-2FJOBS-2FAUTOTAG-3AAUTOTAG-20FUNCTION-29"></a>

##### [function](7d37) `40ants-ci/jobs/autotag:autotag` &key (filename \*default-filename\*) (regex \*default-regex\*) (tag-prefix \*default-tag-prefix\*) (token-pattern \*default-token-pattern\*) env

Creates a job which will run autotagger to create a new git tag for release.

<a id="x-2840ANTS-CI-2FJOBS-2FAUTOTAG-3AAUTOTAG-20CLASS-29"></a>

##### [class](7d95) `40ants-ci/jobs/autotag:autotag` (job)

This type of the job created a git tag when finds a new tag in specified file.

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-40LINTER-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

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
          "uses": "actions/checkout@v4"
        },
        {
          "name": "Setup Common Lisp Environment",
          "uses": "40ants/setup-lisp@v4",
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

##### [class](380d) `40ants-ci/jobs/linter:linter` (lisp-job)

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-40CRITIC-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

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

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-40RUN-TESTS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Running Tests

Another interesting job type is `40ants-ci/jobs/run-tests:run-tests` ([`1`][e35d] [`2`][6cb7]).

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
          "uses": "actions/checkout@v4"
        },
        {
          "name": "Setup Common Lisp Environment",
          "uses": "40ants/setup-lisp@v4",
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

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-40MATRIX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

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
<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-40MULTIPLE-JOBS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

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

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-40BUILD-DOCS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Building Docs

Third predefined job type is `40ants-ci/jobs/docs:build-docs` ([`1`][13b8] [`2`][1ddb]).
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
          "uses": "actions/checkout@v4"
        },
        {
          "name": "Setup Common Lisp Environment",
          "uses": "40ants/setup-lisp@v4",
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
<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-40CACHING-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

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
           "uses": "actions/checkout@v4"
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
+          "uses": "actions/cache@v3",
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
           "uses": "40ants/setup-lisp@v4",
           "with": {
             "asdf-system": "40ants-ci",
             "qlfile-template": ""
-          }
+          },
+          "if": "steps.cache.outputs.cache-hit != 'true'"
         },
         {
```
<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-40DETAILS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Details

`TODO`: I have to write a few chapters with details on additional job's parameters
and a way how to create new job types.

But for now, I want to show a small example, how to define a workflow with a
job which takes care about lisp installation and then calls a custom step:

```lisp
(defworkflow ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((40ants-ci/jobs/lisp-job:lisp-job :name "check-ros-config"
                                           :lisp "ccl-bin"
                                           :steps ((40ants-ci/steps/sh:sh "Show Roswell Config"
                                                                          "ros config")))))
```
Here we are using the class [`40ants-ci/jobs/lisp-job:lisp-job`][2f4c] which is base for most classes in this `ASDF` system
and pass a custom `40ants-ci/steps/sh:sh` ([`1`][4d70] [`2`][0f59]) step to it. This step will be called after the repostory checkout and `CCL-BIN` lisp installation.
so, thus when this step will run `ros config` command, it will output something like that:

```
asdf.version=3.3.5.3
ccl-bin.version=1.12.2
setup.time=3918000017
sbcl-bin.version=2.4.1
default.lisp=ccl-bin

Possible subcommands:
set
show
```
Pay attention to the `NAME` argument of [`40ants-ci/jobs/lisp-job:lisp-job`][2f4c] class. If you omit it, then default "lisp-job" name will be used.

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## API

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### 40ANTS-CI

<a id="x-28-23A-28-289-29-20BASE-CHAR-20-2E-20-2240ANTS-CI-22-29-20PACKAGE-29"></a>

#### [package](aa87) `40ants-ci`

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-2840ANTS-CI-3AGENERATE-20FUNCTION-29"></a>

##### [function](254f) `40ants-ci:generate` system &key path

Generates GitHub workflow for given `ASDF` system.

This function searches workflow definitions in all packages
of the given `ASDF` system.

If `PATH` argument is not given, workflow files will be written
to .github/workflow/ relarive to the `SYSTEM`.

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FGITHUB-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### 40ANTS-CI/GITHUB

<a id="x-28-23A-28-2816-29-20BASE-CHAR-20-2E-20-2240ANTS-CI-2FGITHUB-22-29-20PACKAGE-29"></a>

#### [package](94d8) `40ants-ci/github`

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FGITHUB-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-2840ANTS-CI-2FGITHUB-3AGENERATE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](d86e) `40ants-ci/github:generate` obj path

<a id="x-2840ANTS-CI-2FGITHUB-3APREPARE-DATA-20GENERIC-FUNCTION-29"></a>

##### [generic-function](e563) `40ants-ci/github:prepare-data` obj

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FGITHUB-3FVariables-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Variables

<a id="x-2840ANTS-CI-2FVARS-3A-2ACURRENT-SYSTEM-2A-20-28VARIABLE-29-29"></a>

##### [variable](1739) `40ants-ci/vars:*current-system*` -unbound-

When workflow is generated for `ASDF` system, this variable will contain a primary `ASDF` system.

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FJOBS-2FAUTOTAG-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### 40ANTS-CI/JOBS/AUTOTAG

<a id="x-28-23A-28-2822-29-20BASE-CHAR-20-2E-20-2240ANTS-CI-2FJOBS-2FAUTOTAG-22-29-20PACKAGE-29"></a>

#### [package](fbd4) `40ants-ci/jobs/autotag`

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FJOBS-2FAUTOTAG-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FJOBS-2FAUTOTAG-24AUTOTAG-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### AUTOTAG

<a id="x-2840ANTS-CI-2FJOBS-2FAUTOTAG-3AAUTOTAG-20CLASS-29"></a>

###### [class](7d95) `40ants-ci/jobs/autotag:autotag` (job)

This type of the job created a git tag when finds a new tag in specified file.

**Readers**

<a id="x-2840ANTS-CI-2FJOBS-2FAUTOTAG-3AFILENAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FAUTOTAG-3AAUTOTAG-29-29"></a>

###### [reader](e1df) `40ants-ci/jobs/autotag:filename` (autotag) (:filename = \*default-filename\*)

File where to search for version numbers.

<a id="x-2840ANTS-CI-2FJOBS-2FAUTOTAG-3AREGEX-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FAUTOTAG-3AAUTOTAG-29-29"></a>

###### [reader](ca60) `40ants-ci/jobs/autotag:regex` (autotag) (:regex = \*default-regex\*)

Regexp used to extract version numbers.

<a id="x-2840ANTS-CI-2FJOBS-2FAUTOTAG-3ATAG-PREFIX-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FAUTOTAG-3AAUTOTAG-29-29"></a>

###### [reader](a28a) `40ants-ci/jobs/autotag:tag-prefix` (autotag) (:tag-prefix = \*default-tag-prefix\*)

Tag prefix.

<a id="x-2840ANTS-CI-2FJOBS-2FAUTOTAG-3ATOKEN-PATTERN-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FAUTOTAG-3AAUTOTAG-29-29"></a>

###### [reader](ecc5) `40ants-ci/jobs/autotag:token-pattern` (autotag) (:token-pattern = \*default-token-pattern\*)

Auth token pattern.

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FJOBS-2FAUTOTAG-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-2840ANTS-CI-2FJOBS-2FAUTOTAG-3AAUTOTAG-20FUNCTION-29"></a>

##### [function](7d37) `40ants-ci/jobs/autotag:autotag` &key (filename \*default-filename\*) (regex \*default-regex\*) (tag-prefix \*default-tag-prefix\*) (token-pattern \*default-token-pattern\*) env

Creates a job which will run autotagger to create a new git tag for release.

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FJOBS-2FCRITIC-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### 40ANTS-CI/JOBS/CRITIC

<a id="x-28-23A-28-2821-29-20BASE-CHAR-20-2E-20-2240ANTS-CI-2FJOBS-2FCRITIC-22-29-20PACKAGE-29"></a>

#### [package](3dd9) `40ants-ci/jobs/critic`

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FJOBS-2FCRITIC-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FJOBS-2FCRITIC-24CRITIC-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### CRITIC

<a id="x-2840ANTS-CI-2FJOBS-2FCRITIC-3ACRITIC-20CLASS-29"></a>

###### [class](0e22) `40ants-ci/jobs/critic:critic` (lisp-job)

**Readers**

<a id="x-2840ANTS-CI-2FJOBS-2FCRITIC-3AASDF-SYSTEMS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FCRITIC-3ACRITIC-29-29"></a>

###### [reader](4ce6) `40ants-ci/jobs/critic:asdf-systems` (critic) (:asdf-systems)

Critic can validate more than one system, but for the base class we need provide only one.

<a id="x-2840ANTS-CI-2FJOBS-2FCRITIC-3AIGNORE-CRITIQUES-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FCRITIC-3ACRITIC-29-29"></a>

###### [reader](c3f7) `40ants-ci/jobs/critic:ignore-critiques` (critic) (:ignore-critiques)

A list strigns with names of critiques to ignore.

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FJOBS-2FCRITIC-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-2840ANTS-CI-2FJOBS-2FCRITIC-3ACRITIC-20FUNCTION-29"></a>

##### [function](b475) `40ants-ci/jobs/critic:critic` &key asdf-systems asdf-version ignore-critiques env

Creates a job which will run Lisp Critic for given `ASDF` systems.

If argument `ASDF-SYSTEMS` is `NIL`, it will use `ASDF` system
to which current lisp file is belong.

You may also provide `ASDF-VERSION` argument. It should be
a string. By default, the latest `ASDF` version will be used.

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FJOBS-2FDOCS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### 40ANTS-CI/JOBS/DOCS

<a id="x-28-23A-28-2819-29-20BASE-CHAR-20-2E-20-2240ANTS-CI-2FJOBS-2FDOCS-22-29-20PACKAGE-29"></a>

#### [package](ef10) `40ants-ci/jobs/docs`

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FJOBS-2FDOCS-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FJOBS-2FDOCS-24BUILD-DOCS-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### BUILD-DOCS

<a id="x-2840ANTS-CI-2FJOBS-2FDOCS-3ABUILD-DOCS-20CLASS-29"></a>

###### [class](eec2) `40ants-ci/jobs/docs:build-docs` (lisp-job)

Builds documentation and uploads it to GitHub using ["40ants/build-docs" github action][613f].

**Readers**

<a id="x-2840ANTS-CI-2FJOBS-2FDOCS-3AERROR-ON-WARNINGS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FDOCS-3ABUILD-DOCS-29-29"></a>

###### [reader](b39f) `40ants-ci/jobs/docs:error-on-warnings` (build-docs) (:error-on-warnings = t)

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FJOBS-2FDOCS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-2840ANTS-CI-2FJOBS-2FDOCS-3ABUILD-DOCS-20FUNCTION-29"></a>

##### [function](cd06) `40ants-ci/jobs/docs:build-docs` &key asdf-system asdf-version (error-on-warnings t) env

Creates a job of class [`build-docs`][1ddb].

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FJOBS-2FJOB-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### 40ANTS-CI/JOBS/JOB

<a id="x-28-23A-28-2818-29-20BASE-CHAR-20-2E-20-2240ANTS-CI-2FJOBS-2FJOB-22-29-20PACKAGE-29"></a>

#### [package](57aa) `40ants-ci/jobs/job`

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FJOBS-2FJOB-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FJOBS-2FJOB-24JOB-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### JOB

<a id="x-2840ANTS-CI-2FJOBS-2FJOB-3AJOB-20CLASS-29"></a>

###### [class](cd27) `40ants-ci/jobs/job:job` ()

**Readers**

<a id="x-2840ANTS-CI-2FJOBS-2FJOB-3AEXCLUDE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FJOB-3AJOB-29-29"></a>

###### [reader](4013) `40ants-ci/jobs/job:exclude` (job) (:exclude = nil)

A list of plists denoting matrix combinations to be excluded.

<a id="x-2840ANTS-CI-2FJOBS-2FJOB-3AEXPLICIT-STEPS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FJOB-3AJOB-29-29"></a>

###### [reader](cc97) `40ants-ci/jobs/job:explicit-steps` (job) (:steps = nil)

This slot holds steps given as a `STEPS` argument to a job constructor. Depending on a job class, it might add additional steps around these explicit steps.

<a id="x-2840ANTS-CI-2FJOBS-2FJOB-3AJOB-ENV-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FJOB-3AJOB-29-29"></a>

###### [reader](aa59) `40ants-ci/jobs/job:job-env` (job) (:env = nil)

An alist of environment variables and their values to be added on job level. Values are evaluated in runtime.

<a id="x-2840ANTS-CI-2FJOBS-2FJOB-3ANAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FJOB-3AJOB-29-29"></a>

###### [reader](d21e) `40ants-ci/jobs/job:name` (job) (:name)

If this name was not given in constructor, then name will be lowercased name of the job class.

<a id="x-2840ANTS-CI-2FJOBS-2FJOB-3AOS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FJOB-3AJOB-29-29"></a>

###### [reader](6ec5) `40ants-ci/jobs/job:os` (job) (:OS = "ubuntu-latest")

<a id="x-2840ANTS-CI-2FJOBS-2FJOB-3APERMISSIONS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FJOB-3AJOB-29-29"></a>

###### [reader](cc2a) `40ants-ci/jobs/job:permissions` (job) (:permissions = nil)

A plist of permissions need for running the job.

These permissions will be bound to `secrets.GITHUB_TOKEN` variable.
Use default-initargs to override permissions in subclasses:

```lisp
(:default-initargs
 :permissions '(:content "write"))
```
<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FJOBS-2FJOB-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-2840ANTS-CI-2FJOBS-2FJOB-3AMAKE-ENV-20GENERIC-FUNCTION-29"></a>

##### [generic-function](e72a) `40ants-ci/jobs/job:make-env` job

<a id="x-2840ANTS-CI-2FJOBS-2FJOB-3AMAKE-MATRIX-20GENERIC-FUNCTION-29"></a>

##### [generic-function](0e36) `40ants-ci/jobs/job:make-matrix` job

<a id="x-2840ANTS-CI-2FJOBS-2FJOB-3AMAKE-PERMISSIONS-20GENERIC-FUNCTION-29"></a>

##### [generic-function](284c) `40ants-ci/jobs/job:make-permissions` job

Should return an alist with mapping from string to string where keys are scopes and values are permission names. Default method generates this alist from the plist of job's "permissions" slot.

<a id="x-2840ANTS-CI-2FJOBS-2FJOB-3ASTEPS-20GENERIC-FUNCTION-29"></a>

##### [generic-function](8e1a) `40ants-ci/jobs/job:steps` job

<a id="x-2840ANTS-CI-2FJOBS-2FJOB-3AUSE-MATRIX-P-20GENERIC-FUNCTION-29"></a>

##### [generic-function](a757) `40ants-ci/jobs/job:use-matrix-p` job

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FJOBS-2FLINTER-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### 40ANTS-CI/JOBS/LINTER

<a id="x-28-23A-28-2821-29-20BASE-CHAR-20-2E-20-2240ANTS-CI-2FJOBS-2FLINTER-22-29-20PACKAGE-29"></a>

#### [package](ea25) `40ants-ci/jobs/linter`

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FJOBS-2FLINTER-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FJOBS-2FLINTER-24LINTER-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### LINTER

<a id="x-2840ANTS-CI-2FJOBS-2FLINTER-3ALINTER-20CLASS-29"></a>

###### [class](380d) `40ants-ci/jobs/linter:linter` (lisp-job)

**Readers**

<a id="x-2840ANTS-CI-2FJOBS-2FLINTER-3AASDF-SYSTEMS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FLINTER-3ALINTER-29-29"></a>

###### [reader](b278) `40ants-ci/jobs/linter:asdf-systems` (linter) (:asdf-systems = nil)

Linter can validate more than one system, but for the base class we need provide only one.

<a id="x-2840ANTS-CI-2FJOBS-2FLINTER-3ACHECK-IMPORTS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FLINTER-3ALINTER-29-29"></a>

###### [reader](f1c0) `40ants-ci/jobs/linter:check-imports` (linter) (:check-imports = nil)

Linter will check for missing or unused imports of package-inferred systems.

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FJOBS-2FLINTER-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-2840ANTS-CI-2FJOBS-2FLINTER-3ALINTER-20FUNCTION-29"></a>

##### [function](a775) `40ants-ci/jobs/linter:linter` &key asdf-systems asdf-version check-imports env

Creates a job which will run `SBL`int for given `ASDF` systems.

If no `ASD` files given, it will use all `ASD` files from
the current `ASDF` system.

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FJOBS-2FLISP-JOB-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### 40ANTS-CI/JOBS/LISP-JOB

<a id="x-28-23A-28-2823-29-20BASE-CHAR-20-2E-20-2240ANTS-CI-2FJOBS-2FLISP-JOB-22-29-20PACKAGE-29"></a>

#### [package](e27e) `40ants-ci/jobs/lisp-job`

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FJOBS-2FLISP-JOB-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FJOBS-2FLISP-JOB-24LISP-JOB-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### LISP-JOB

<a id="x-2840ANTS-CI-2FJOBS-2FLISP-JOB-3ALISP-JOB-20CLASS-29"></a>

###### [class](d06c) `40ants-ci/jobs/lisp-job:lisp-job` (job)

This job checkouts the sources, installs Roswell and Qlot. Also, it caches results between runs.

**Readers**

<a id="x-2840ANTS-CI-2FJOBS-2FLISP-JOB-3AASDF-SYSTEM-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FLISP-JOB-3ALISP-JOB-29-29"></a>

###### [reader](19dc) `40ants-ci/jobs/lisp-job:asdf-system` (lisp-job) (:asdf-system = nil)

<a id="x-2840ANTS-CI-2FJOBS-2FLISP-JOB-3AASDF-VERSION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FLISP-JOB-3ALISP-JOB-29-29"></a>

###### [reader](612f) `40ants-ci/jobs/lisp-job:asdf-version` (lisp-job) (:asdf-version = nil)

`ASDF` version to use when setting up Lisp environment. If `NIL`, then the latest will be used.

<a id="x-2840ANTS-CI-2FJOBS-2FLISP-JOB-3ALISP-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FLISP-JOB-3ALISP-JOB-29-29"></a>

###### [reader](9ec6) `40ants-ci/jobs/lisp-job:lisp` (lisp-job) (:LISP = "sbcl-bin")

<a id="x-2840ANTS-CI-2FJOBS-2FLISP-JOB-3AQLFILE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FLISP-JOB-3ALISP-JOB-29-29"></a>

###### [reader](fcc7) `40ants-ci/jobs/lisp-job:qlfile` (lisp-job) (:qlfile = nil)

<a id="x-2840ANTS-CI-2FJOBS-2FLISP-JOB-3AQLOT-VERSION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FLISP-JOB-3ALISP-JOB-29-29"></a>

###### [reader](4314) `40ants-ci/jobs/lisp-job:qlot-version` (lisp-job) (:qlot-version = nil)

Qlot version to use when setting up Lisp environment. If `NIL`, then will be used version, pinned in `setup-lisp` github action.

<a id="x-2840ANTS-CI-2FJOBS-2FLISP-JOB-3AQUICKLISP-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FLISP-JOB-3ALISP-JOB-29-29"></a>

###### [reader](1876) `40ants-ci/jobs/lisp-job:quicklisp` (lisp-job) (:QUICKLISP = "quicklisp")

<a id="x-2840ANTS-CI-2FJOBS-2FLISP-JOB-3AROSWELL-VERSION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FLISP-JOB-3ALISP-JOB-29-29"></a>

###### [reader](39ea) `40ants-ci/jobs/lisp-job:roswell-version` (lisp-job) (:roswell-version = nil)

Roswell version to use when setting up Lisp environment. If `NIL`, then will be used version, pinned in `setup-lisp` github action.

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FJOBS-2FRUN-TESTS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### 40ANTS-CI/JOBS/RUN-TESTS

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-2240ANTS-CI-2FJOBS-2FRUN-TESTS-22-29-20PACKAGE-29"></a>

#### [package](4827) `40ants-ci/jobs/run-tests`

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FJOBS-2FRUN-TESTS-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FJOBS-2FRUN-TESTS-24RUN-TESTS-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### RUN-TESTS

<a id="x-2840ANTS-CI-2FJOBS-2FRUN-TESTS-3ARUN-TESTS-20CLASS-29"></a>

###### [class](2040) `40ants-ci/jobs/run-tests:run-tests` (lisp-job)

This job test runs tests for a given `ASDF` system.

**Readers**

<a id="x-2840ANTS-CI-2FJOBS-2FRUN-TESTS-3ACOVERAGE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FRUN-TESTS-3ARUN-TESTS-29-29"></a>

###### [reader](48d6) `40ants-ci/jobs/run-tests:coverage` (run-tests) (:coverage = nil)

<a id="x-2840ANTS-CI-2FJOBS-2FRUN-TESTS-3ACUSTOM-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FJOBS-2FRUN-TESTS-3ARUN-TESTS-29-29"></a>

###### [reader](3992) `40ants-ci/jobs/run-tests:custom` (run-tests) (:custom = nil)

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FJOBS-2FRUN-TESTS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-2840ANTS-CI-2FJOBS-2FRUN-TESTS-3ARUN-TESTS-20FUNCTION-29"></a>

##### [function](852c) `40ants-ci/jobs/run-tests:run-tests` &rest rest &key coverage qlfile asdf-system asdf-version os quicklisp lisp exclude custom env

Creates a job step of class [`run-tests`][6cb7].

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FSTEPS-2FACTION-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### 40ANTS-CI/STEPS/ACTION

<a id="x-28-23A-28-2822-29-20BASE-CHAR-20-2E-20-2240ANTS-CI-2FSTEPS-2FACTION-22-29-20PACKAGE-29"></a>

#### [package](482b) `40ants-ci/steps/action`

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FSTEPS-2FACTION-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FSTEPS-2FACTION-24ACTION-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### ACTION

<a id="x-2840ANTS-CI-2FSTEPS-2FACTION-3AACTION-20CLASS-29"></a>

###### [class](0c11) `40ants-ci/steps/action:action` (step)

**Readers**

<a id="x-2840ANTS-CI-2FSTEPS-2FACTION-3AACTION-ARGS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FSTEPS-2FACTION-3AACTION-29-29"></a>

###### [reader](f5ef) `40ants-ci/steps/action:action-args` (action) (:args)

A plist to be passed as "with" dictionary to the action.

<a id="x-2840ANTS-CI-2FSTEPS-2FACTION-3AUSES-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FSTEPS-2FACTION-3AACTION-29-29"></a>

###### [reader](5740) `40ants-ci/steps/action:uses` (action) (:uses)

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FSTEPS-2FACTION-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-2840ANTS-CI-2FSTEPS-2FACTION-3AACTION-20FUNCTION-29"></a>

##### [function](111f) `40ants-ci/steps/action:action` name uses &rest args &key id if env &allow-other-keys

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FSTEPS-2FSH-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### 40ANTS-CI/STEPS/SH

<a id="x-28-23A-28-2818-29-20BASE-CHAR-20-2E-20-2240ANTS-CI-2FSTEPS-2FSH-22-29-20PACKAGE-29"></a>

#### [package](76db) `40ants-ci/steps/sh`

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FSTEPS-2FSH-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FSTEPS-2FSH-24SH-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### SH

<a id="x-2840ANTS-CI-2FSTEPS-2FSH-3ASH-20CLASS-29"></a>

###### [class](d7bc) `40ants-ci/steps/sh:sh` (step)

**Readers**

<a id="x-2840ANTS-CI-2FSTEPS-2FSH-3ACOMMAND-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FSTEPS-2FSH-3ASH-29-29"></a>

###### [reader](9adf) `40ants-ci/steps/sh:command` (sh) (:command)

<a id="x-2840ANTS-CI-2FSTEPS-2FSH-3ASHELL-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FSTEPS-2FSH-3ASH-29-29"></a>

###### [reader](7b8b) `40ants-ci/steps/sh:shell` (sh) (:shell = \*default-shell\*)

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FSTEPS-2FSH-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-2840ANTS-CI-2FSTEPS-2FSH-3ASH-20FUNCTION-29"></a>

##### [function](a546) `40ants-ci/steps/sh:sh` name command &key id if (shell \*default-shell\*) env

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FSTEPS-2FSH-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Macros

<a id="x-2840ANTS-CI-2FSTEPS-2FSH-3ASECTIONS-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

##### [macro](d4e5) `40ants-ci/steps/sh:sections` &body body

Returns a string with a bash script where some parts are grouped.

In this example we have 3 sections:

```lisp
(sections
      ("Help Argument"
       "qlot exec cl-info --help")
      ("Version Argument"
       "qlot exec cl-info --version")
      ("Lisp Systems Info"
       "qlot exec cl-info"
       "qlot exec cl-info cl-info defmain"))
```
It will be compiled into:

```bash
echo ::group::Help Argument
qlot exec cl-info --help
echo ::endgroup::
echo ::group::Version Argument
qlot exec cl-info --version
echo ::endgroup::
echo ::group::Lisp Systems Info
qlot exec cl-info
qlot exec cl-info cl-info defmain
echo ::endgroup::
```
<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FSTEPS-2FSTEP-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### 40ANTS-CI/STEPS/STEP

<a id="x-28-23A-28-2820-29-20BASE-CHAR-20-2E-20-2240ANTS-CI-2FSTEPS-2FSTEP-22-29-20PACKAGE-29"></a>

#### [package](8655) `40ants-ci/steps/step`

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FSTEPS-2FSTEP-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FSTEPS-2FSTEP-24STEP-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### STEP

<a id="x-2840ANTS-CI-2FSTEPS-2FSTEP-3ASTEP-20CLASS-29"></a>

###### [class](850b) `40ants-ci/steps/step:step` ()

**Readers**

<a id="x-2840ANTS-CI-2FSTEPS-2FSTEP-3AENV-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FSTEPS-2FSTEP-3ASTEP-29-29"></a>

###### [reader](52b2) `40ants-ci/steps/step:env` (step) (:env = nil)

An alist of environment variables.

<a id="x-2840ANTS-CI-2FSTEPS-2FSTEP-3ASTEP-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FSTEPS-2FSTEP-3ASTEP-29-29"></a>

###### [reader](b2d4) `40ants-ci/steps/step:step-id` (step) (:id = nil)

<a id="x-2840ANTS-CI-2FSTEPS-2FSTEP-3ASTEP-IF-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FSTEPS-2FSTEP-3ASTEP-29-29"></a>

###### [reader](8f70) `40ants-ci/steps/step:step-if` (step) (:if = nil)

<a id="x-2840ANTS-CI-2FSTEPS-2FSTEP-3ASTEP-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-CI-2FSTEPS-2FSTEP-3ASTEP-29-29"></a>

###### [reader](55ca) `40ants-ci/steps/step:step-name` (step) (:name = nil)

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FUTILS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### 40ANTS-CI/UTILS

<a id="x-28-23A-28-2815-29-20BASE-CHAR-20-2E-20-2240ANTS-CI-2FUTILS-22-29-20PACKAGE-29"></a>

#### [package](9685) `40ants-ci/utils`

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FUTILS-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-2840ANTS-CI-2FUTILS-3ASYSTEM-PACKAGES-20GENERIC-FUNCTION-29"></a>

##### [generic-function](3d06) `40ants-ci/utils:system-packages` system

Returns a list of packages created by `ASDF` system.

Default implementation returns a package having the same name as a system
and all packages matched to package-inferred subsystems:

```
CL-USER> (docs-builder/utils:system-packages :docs-builder)
(#<PACKAGE "DOCS-BUILDER">
 #<PACKAGE "DOCS-BUILDER/UTILS">
 #<PACKAGE "DOCS-BUILDER/GUESSER">
 #<PACKAGE "DOCS-BUILDER/BUILDERS/GENEVA/GUESSER">
 #<PACKAGE "DOCS-BUILDER/BUILDER">
 #<PACKAGE "DOCS-BUILDER/BUILDERS/MGL-PAX/GUESSER">
 #<PACKAGE "DOCS-BUILDER/DOCS">
 #<PACKAGE "DOCS-BUILDER/BUILDERS/MGL-PAX/BUILDER">)
```
<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FUTILS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-2840ANTS-CI-2FUTILS-3AALISTP-20FUNCTION-29"></a>

##### [function](c29a) `40ants-ci/utils:alistp` list

Test wheather `LIST` argument is a properly formed alist.

In this library, alist has always a string as a key.
Because we need them to have this form to serialize
to `JSON` propertly.

(alistp '(("cron" . "0 10 * * 1"))) -> T
(alistp '((("cron" . "0 10 * * 1")))) -> `NIL`

<a id="x-2840ANTS-CI-2FUTILS-3ACURRENT-SYSTEM-NAME-20FUNCTION-29"></a>

##### [function](b910) `40ants-ci/utils:current-system-name`

<a id="x-2840ANTS-CI-2FUTILS-3ADEDENT-20FUNCTION-29"></a>

##### [function](29fd) `40ants-ci/utils:dedent` text

Removes common leading whitespace from each string.

A few examples:

```
(dedent "Hello
          World
          and all Lispers!")

"Hello
World
and all Lispers!"
```
```
(dedent "
    Hello
    World
    and all Lispers!")

"Hello
World
and all Lispers!"
```
```
(dedent "This is a code:

              (symbol-name :hello-world)

          it will output HELLO-WORLD.")

"This is a code:

    (symbol-name :hello-world)

it will output HELLO-WORLD."
```
<a id="x-2840ANTS-CI-2FUTILS-3AENSURE-LIST-OF-PLISTS-20FUNCTION-29"></a>

##### [function](caeb) `40ants-ci/utils:ensure-list-of-plists` data

<a id="x-2840ANTS-CI-2FUTILS-3AENSURE-PRIMARY-SYSTEM-20FUNCTION-29"></a>

##### [function](2620) `40ants-ci/utils:ensure-primary-system` system

<a id="x-2840ANTS-CI-2FUTILS-3AMAKE-GITHUB-WORKFLOWS-PATH-20FUNCTION-29"></a>

##### [function](1d7e) `40ants-ci/utils:make-github-workflows-path` system

<a id="x-2840ANTS-CI-2FUTILS-3APLIST-TO-ALIST-20FUNCTION-29"></a>

##### [function](fedb) `40ants-ci/utils:plist-to-alist` plist &key (string-keys t) (lowercase t)

Make an alist from a plist `PLIST`.

By default, transforms keys to lowercased strings

<a id="x-2840ANTS-CI-2FUTILS-3APLISTP-20FUNCTION-29"></a>

##### [function](a325) `40ants-ci/utils:plistp` list

Test wheather `LIST` is a properly formed plist.

<a id="x-2840ANTS-CI-2FUTILS-3ASINGLE-20FUNCTION-29"></a>

##### [function](e2e0) `40ants-ci/utils:single` list

Test wheather `LIST` contains exactly 1 element.

<a id="x-2840ANTS-CI-2FUTILS-3ATO-JSON-20FUNCTION-29"></a>

##### [function](9187) `40ants-ci/utils:to-json` data

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-4040ANTS-CI-2FVARS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### 40ANTS-CI/VARS

<a id="x-28-23A-28-2814-29-20BASE-CHAR-20-2E-20-2240ANTS-CI-2FVARS-22-29-20PACKAGE-29"></a>

#### [package](5b8b) `40ants-ci/vars`

<a id="x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-7C-4040ANTS-CI-2FVARS-3FVariables-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Variables

<a id="x-2840ANTS-CI-2FVARS-3A-2ACURRENT-SYSTEM-2A-20-28VARIABLE-29-29"></a>

##### [variable](1739) `40ants-ci/vars:*current-system*` -unbound-

When workflow is generated for `ASDF` system, this variable will contain a primary `ASDF` system.

<a id="x-2840ANTS-CI-2FVARS-3A-2AUSE-CACHE-2A-20-28VARIABLE-29-29"></a>

##### [variable](c1cd) `40ants-ci/vars:*use-cache*` nil

Workflow will set this variable when preparing the data or `YAML` generation.


[2100]: https://40ants.com/40ants-critic
[b882]: https://40ants.com/build-doc
[613f]: https://40ants.com/build-docs/
[3f72]: https://40ants.com/ci/
[900b]: https://40ants.com/ci/#x-28-23A-28-289-29-20BASE-CHAR-20-2E-20-2240ANTS-CI-22-29-20PACKAGE-29
[b171]: https://40ants.com/ci/#x-28-23A-28-289-29-20BASE-CHAR-20-2E-20-2240ants-ci-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29
[484a]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FCRITIC-3ACRITIC-20FUNCTION-29
[1ddb]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FDOCS-3ABUILD-DOCS-20CLASS-29
[13b8]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FDOCS-3ABUILD-DOCS-20FUNCTION-29
[2f4c]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FLISP-JOB-3ALISP-JOB-20CLASS-29
[6cb7]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FRUN-TESTS-3ARUN-TESTS-20CLASS-29
[e35d]: https://40ants.com/ci/#x-2840ANTS-CI-2FJOBS-2FRUN-TESTS-3ARUN-TESTS-20FUNCTION-29
[0f59]: https://40ants.com/ci/#x-2840ANTS-CI-2FSTEPS-2FSH-3ASH-20CLASS-29
[4d70]: https://40ants.com/ci/#x-2840ANTS-CI-2FSTEPS-2FSH-3ASH-20FUNCTION-29
[f2be]: https://40ants.com/docs-builder/
[8469]: https://40ants.com/run-tests
[59d7]: https://40ants.com/run-tests/
[8de1]: https://40ants.com/setup-lisp/
[b60c]: https://coveralls.io/
[e681]: https://github.com/40ants/ci
[de0b]: https://github.com/40ants/ci/actions
[aa87]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/core.lisp#L1
[254f]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/core.lisp#L9
[94d8]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/github.lisp#L1
[d86e]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/github.lisp#L16
[e563]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/github.lisp#L36
[fbd4]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/autotag.lisp#L1
[7d95]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/autotag.lisp#L23
[e1df]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/autotag.lisp#L24
[ca60]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/autotag.lisp#L29
[a28a]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/autotag.lisp#L34
[ecc5]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/autotag.lisp#L39
[7d37]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/autotag.lisp#L49
[3dd9]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/critic.lisp#L1
[0e22]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/critic.lisp#L14
[4ce6]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/critic.lisp#L16
[c3f7]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/critic.lisp#L19
[b475]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/critic.lisp#L24
[ef10]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/docs.lisp#L1
[eec2]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/docs.lisp#L15
[b39f]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/docs.lisp#L16
[cd06]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/docs.lisp#L22
[57aa]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/job.lisp#L1
[a757]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/job.lisp#L104
[0e36]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/job.lisp#L109
[e72a]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/job.lisp#L120
[284c]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/job.lisp#L149
[cd27]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/job.lisp#L29
[d21e]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/job.lisp#L30
[6ec5]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/job.lisp#L33
[4013]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/job.lisp#L36
[aa59]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/job.lisp#L40
[cc97]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/job.lisp#L45
[cc2a]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/job.lisp#L49
[8e1a]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/job.lisp#L90
[ea25]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/linter.lisp#L1
[380d]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/linter.lisp#L16
[b278]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/linter.lisp#L17
[f1c0]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/linter.lisp#L22
[a775]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/linter.lisp#L35
[e27e]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/lisp-job.lisp#L1
[d06c]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/lisp-job.lisp#L28
[1876]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/lisp-job.lisp#L29
[9ec6]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/lisp-job.lisp#L32
[fcc7]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/lisp-job.lisp#L35
[19dc]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/lisp-job.lisp#L38
[612f]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/lisp-job.lisp#L42
[39ea]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/lisp-job.lisp#L47
[4314]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/lisp-job.lisp#L52
[4827]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/run-tests.lisp#L1
[2040]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/run-tests.lisp#L19
[48d6]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/run-tests.lisp#L20
[3992]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/run-tests.lisp#L23
[852c]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/jobs/run-tests.lisp#L29
[482b]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/steps/action.lisp#L1
[0c11]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/steps/action.lisp#L13
[5740]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/steps/action.lisp#L14
[f5ef]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/steps/action.lisp#L16
[111f]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/steps/action.lisp#L22
[76db]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/steps/sh.lisp#L1
[d7bc]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/steps/sh.lisp#L17
[9adf]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/steps/sh.lisp#L18
[7b8b]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/steps/sh.lisp#L20
[a546]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/steps/sh.lisp#L26
[d4e5]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/steps/sh.lisp#L42
[8655]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/steps/step.lisp#L1
[850b]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/steps/step.lisp#L19
[b2d4]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/steps/step.lisp#L20
[55ca]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/steps/step.lisp#L23
[52b2]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/steps/step.lisp#L26
[8f70]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/steps/step.lisp#L31
[9685]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/utils.lisp#L1
[e2e0]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/utils.lisp#L154
[a325]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/utils.lisp#L159
[c29a]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/utils.lisp#L169
[caeb]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/utils.lisp#L185
[fedb]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/utils.lisp#L197
[1d7e]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/utils.lisp#L215
[9187]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/utils.lisp#L232
[2620]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/utils.lisp#L25
[3d06]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/utils.lisp#L31
[b910]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/utils.lisp#L70
[29fd]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/utils.lisp#L85
[5b8b]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/vars.lisp#L1
[c1cd]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/vars.lisp#L14
[1739]: https://github.com/40ants/ci/blob/b675cee75ee9791f1ba3378bbd793f48588d8e31/src/vars.lisp#L9
[2f94]: https://github.com/cxxxr/sblint
[1cec]: https://github.com/quicklisp/quicklisp-projects/blob/ee133271c81caf5d8bbf8cef3054544ff47b64c6/projects/alexa/source.txt
[8236]: https://quickdocs.org/alexandria
[c41d]: https://quickdocs.org/serapeum
[ef7f]: https://quickdocs.org/str
[aba2]: https://quickdocs.org/yason

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
