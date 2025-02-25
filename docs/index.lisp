(uiop:define-package #:40ants-ci-docs/index
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection
                #:reader
                #:section
                #:defsection-copy)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:import-from #:40ants-ci-docs/changelog
                #:@changelog)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:40ants-ci-docs/index)


(defmethod docs-config ((system (eql (asdf:find-system "40ants-ci-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  (uiop:symbol-call :ql :quickload :40ants-doc-theme-40ants)
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(defsection @index (:title "40Ants-CI - Github Workflow Generator"
                    :ignore-words ("YAML"
                                   "GIT"
                                   "CL"
                                   "CI"
                                   "JSON"
                                   "OS"
                                   "SBL"
                                   "SBCL"
                                   "BSD"
                                   "TODO"
                                   "ASD"
                                   "ASDF"
                                   "EXAMPLE"))
  "
[![](https://github-actions.40ants.com/40ants/ci/matrix.svg)](https://github.com/40ants/ci/actions)

![Quicklisp](http://quickdocs.org/badge/ci.svg)

This is a small utility, which can generate GitHub workflows for Common Lisp
projects.

It generates workflow for running tests and building docs. These workflows
use [40ants/run-tests](https://40ants.com/run-tests) and [40ants/build-docs](https://40ants.com/build-doc)
actions and [`SBLint`](https://github.com/cxxxr/sblint) to check code for compilation errors.
"
  (40ants-ci system)
  (@reasons section)
  (@quickstart section)
  (@details section)
  (@api section))


(defsection-copy @readme @index)


(defsection @reasons (:title "Reasons to Use")
  "
- This system hides all entrails related to caching.
- Includes a few ready to use job types.
- Custom job types can be defined and distributed as separate ASDF systems.
- You don't have to write YAML anymore!
")


(defsection @quickstart (:title "Quickstart")
  "
This system allows you to define workflows in the lisp code. The best way is to make these
definitions a part of your ASDF system. This way 40ANTS-CI will be able to
automatically understand for which system it builds a workflow.

Each workflow consists of jobs and each job is a number of steps.

There are three predefine types of jobs and you can create your own. Predefined jobs
allows to reuse steps in multiple CL libraries.

In next examples, I'll presume you are writing code in a file which is the part
of the package inferred ASDF system `EXAMPLE/CI`. A file should have the following header:

```lisp
(defpackage #:example/ci
  (:use #:cl)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/run-tests)
  (:import-from #:40ants-ci/jobs/docs))
```
"
  (@job-types section)
  (@caching section)
  (@env section)
  (@running-custom-steps section))


(defsection @job-types (:title "Job Types")
  (@autotag section)
  (@linter section)
  (@critic section)
  (@run-tests section)
  (@build-docs section))


(defsection @linter (:title "Linter")
  "
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
  \"name\": \"LINTER\",
  \"on\": {
    \"pull_request\": null
  },
  \"jobs\": {
    \"linter\": {
      \"runs-on\": \"ubuntu-latest\",
      \"env\": {
        \"OS\": \"ubuntu-latest\",
        \"QUICKLISP_DIST\": \"quicklisp\",
        \"LISP\": \"sbcl-bin\"
      },
      \"steps\": [
        {
          \"name\": \"Checkout Code\",
          \"uses\": \"actions/checkout@v4\"
        },
        {
          \"name\": \"Setup Common Lisp Environment\",
          \"uses\": \"40ants/setup-lisp@v4\",
          \"with\": {
            \"asdf-system\": \"example\"
          }
        },
        {
          \"name\": \"Install SBLint\",
          \"run\": \"qlot exec ros install cxxxr/sblint\",
          \"shell\": \"bash\"
        },
        {
          \"name\": \"Run Linter\",
          \"run\": \"qlot exec sblint example.asd\",
          \"shell\": \"bash\"
        }
      ]
    }
  }
}
```

Here you can see, a few steps in the job:

1. Checkout the code.
2. Install Roswell & Qlot using [40ants/setup-lisp](https://40ants.com/setup-lisp/) action.
3. Install [`SBLint`](https://github.com/cxxxr/sblint).
4. Run linter for `example.asd`.

Another interesting thing is that this workflow automatically uses `ubuntu-latest` OS,
`Quicklisp` and `sbcl-bin` Lisp implementation. Later I'll show you how to redefine these settings.
"
  (40ants-ci/jobs/linter:linter class))


(defsection @critic (:title "Critic")
  "
This job is similar to linter, but instead of SBLint it runs
[Lisp Critic](https://40ants.com/40ants-critic).

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

and they will be executed in parallel. See docs on 40ANTS-CI/JOBS/CRITIC:CRITIC function
to learn about supported arguments.")


(defsection @autotag (:title "Autotag")
  "
This job is automates git tag placement on the commit where you have changed the ChangeLog.md.

This can be a useful to automate package deployment and releases. You update the changelog,
a job pushes a new git tag and the next action triggers on this tag and build a release.

Or you if you publish your library at Quicklisp distribution, then you can change
it's source type to the `latest-github-tag` to provide more stable releases to your
users. This way you commits into master will be ignored until you change the changelog and
git tag will be pushed. Here is an [example](https://github.com/quicklisp/quicklisp-projects/blob/ee133271c81caf5d8bbf8cef3054544ff47b64c6/projects/alexa/source.txt) how to setup this kind of quicklisp project source.

```lisp
(defworkflow release
  :on-push-to \"master\"
  :jobs ((40ants-ci/jobs/autotag:autotag)))
```
"
  (40ants-ci/jobs/autotag:autotag function)
  (40ants-ci/jobs/autotag:autotag class))


(defsection @run-tests (:title "Running Tests"
                        :ignore-words ("ASDF:TEST-SYSTEM"))
  "
Another interesting job type is 40ANTS-CI/JOBS/RUN-TESTS:RUN-TESTS.

When using this job type, make sure, your system
runs tests on `(ASDF:TEST-SYSTEM :system-name)` call
and signals error if something went wrong.

```lisp

(defworkflow ci
  :on-push-to \"master\"
  :by-cron \"0 10 * * 1\"
  :on-pull-request t
  :jobs ((40ants-ci/jobs/run-tests:run-tests
          :coverage t)))

```

Here I've added a few options to the workflow:

- `by-cron` - sets a schedule.
- `on-push-to` - defines a branch or branches to track.

It will generate `.github/workflows/ci.yml` with following content:

```json
{
  \"name\": \"CI\",
  \"on\": {
    \"push\": {
      \"branches\": [
        \"master\"
      ]
    },
    \"pull_request\": null,
    \"schedule\": [
      {
        \"cron\": \"0 10 * * 1\"
      }
    ]
  },
  \"jobs\": {

    \"run-tests\": {
      \"runs-on\": \"ubuntu-latest\",
      \"env\": {
        \"OS\": \"ubuntu-latest\",
        \"QUICKLISP_DIST\": \"quicklisp\",
        \"LISP\": \"sbcl-bin\"
      },
      \"steps\": [
        {
          \"name\": \"Checkout Code\",
          \"uses\": \"actions/checkout@v4\"
        },
        {
          \"name\": \"Setup Common Lisp Environment\",
          \"uses\": \"40ants/setup-lisp@v4\",
          \"with\": {
            \"asdf-system\": \"example\"
          }
        },
        {
          \"name\": \"Run Tests\",
          \"uses\": \"40ants/run-tests@v2\",
          \"with\": {
            \"asdf-system\": \"example\",
            \"coveralls-token\": \"${{ secrets.github_token }}\"
          }
        }
      ]
    }
  }
}

```

The result is similar to the workflow generated for Linter,
but uses [40ants/setup-lisp](https://40ants.com/run-tests/) action
at the final step.

Also, I've passed an option `:coverage t` to the job. Thus coverage
report will be uploaded to [Coveralls.io](https://coveralls.io/) automatically.
"
  (@matrix section)
  (@multiple-jobs section))


(defsection @matrix (:title "Defining a test Matrix")
  "
Lisp has many implementations and can be used on multiple platforms. Thus
it is a good idea to test our software on many combinations of OS and lisp
implementations. Workflow generator makes this very easy.

Here is an example of workflow definition with three dimentional matrix.
It not only tests a library under different lisps and OS, but also checks
if it works with the latest Quicklisp and Ultralisp distributions:

```lisp
(defworkflow ci
  :on-pull-request t
  :jobs ((run-tests
          :os (\"ubuntu-latest\"
               \"macos-latest\")
          :quicklisp (\"quicklisp\"
                      \"ultralisp\")
          :lisp (\"sbcl-bin\"
                 \"ccl-bin\"
                 \"allegro\"
                 \"clisp\"
                 \"cmucl\")
          :exclude (;; Seems allegro is does not support 64bit OSX.
                    ;; Unable to install it using Roswell:
                    ;; alisp is not executable. Missing 32bit glibc?
                    (:os \"macos-latest\" :lisp \"allegro\")))))
```
")


(defsection @multiple-jobs (:title "Multiple jobs")
  "
Besides a build matrix, you might specify a multiple jobs of the same type,
but with different parameters:

```lisp
(defworkflow ci
  :on-push-to \"master\"
  :on-pull-request t
  :jobs ((run-tests
          :lisp \"sbcl-bin\")
         (run-tests
          :lisp \"ccl-bin\")
         (run-tests
          :lisp \"allegro\")))
```

This will generate a workflow with three jobs: \"run-tests\", \"run-tests-2\" and \"run-tests-3\".

Meaningful names might be specified as well:

```lisp
(defworkflow ci
  :on-push-to \"master\"
  :on-pull-request t
  :jobs ((run-tests
          :name \"test-on-sbcl\"
          :lisp \"sbcl-bin\")
         (run-tests
          :name \"test-on-ccl\"
          :lisp \"ccl-bin\")
         (run-tests
          :name \"test-on-allegro\"
          :lisp \"allegro\")))
```

Here is how these jobs will look like in the GitHub interface:

![](https://user-images.githubusercontent.com/24827/151619261-2d49e2a6-bc5c-42db-aec5-674d9229a1b0.png)

")


(defsection @build-docs (:title "Building Docs")
  "
Third predefined job type is 40ANTS-CI/JOBS/DOCS:BUILD-DOCS.
It uses [40ants/build-docs](https://40ants.com/build-docs/)
action and will work only if your ASDF system uses a documentation builder supported by
[40ants/docs-builder](https://40ants.com/docs-builder/).

To build docs on every push to master, just use this code:

```lisp

(defworkflow docs
  :on-push-to \"master\"
  :jobs ((40ants-ci/jobs/docs:build-docs)))

```

It will generate `.github/workflows/docs.yml` with following content:

```json

{
  \"name\": \"DOCS\",
  \"on\": {
    \"push\": {
      \"branches\": [
        \"master\"
      ]
    }
  },
  \"jobs\": {
    \"build-docs\": {
      \"runs-on\": \"ubuntu-latest\",
      \"env\": {
        \"OS\": \"ubuntu-latest\",
        \"QUICKLISP_DIST\": \"quicklisp\",
        \"LISP\": \"sbcl-bin\"
      },
      \"steps\": [
        {
          \"name\": \"Checkout Code\",
          \"uses\": \"actions/checkout@v4\"
        },
        {
          \"name\": \"Setup Common Lisp Environment\",
          \"uses\": \"40ants/setup-lisp@v4\",
          \"with\": {
            \"asdf-system\": \"example\",
            \"qlfile-template\": \"\"
          }
        },
        {
          \"name\": \"Build Docs\",
          \"uses\": \"40ants/build-docs@v1\",
          \"with\": {
            \"asdf-system\": \"example\"
          }
        }
      ]
    }
  }
}

```

")


(defsection @caching (:title "Caching")
  "
To significantly speed up our tests, we can cache installed Roswell,
Qlot and Common Lisp fasl files.

To accomplish this task, you don't need to dig into GitHub's docs anymore!
Just add one line `:cache t` to your workflow definition:

```lisp
(defworkflow docs
  :on-push-to \"master\"
  :cache t
  :jobs ((40ants-ci/jobs/docs:build-docs)))
```

Here is the diff of the generated workflow file. It shows steps, added automatically:


```diff
modified   .github/workflows/docs.yml
@@ -20,13 +20,40 @@
           \"name\": \"Checkout Code\",
           \"uses\": \"actions/checkout@v4\"
         },
+        {
+          \"name\": \"Grant All Perms to Make Cache Restoring Possible\",
+          \"run\": \"sudo mkdir -p /usr/local/etc/roswell\\n                 sudo chown \\\"${USER}\\\" /usr/local/etc/roswell\\n                 # Here the ros binary will be restored:\\n                 sudo chown \\\"${USER}\\\" /usr/local/bin\",
+          \"shell\": \"bash\"
+        },
+        {
+          \"name\": \"Get Current Month\",
+          \"id\": \"current-month\",
+          \"run\": \"echo \\\"::set-output name=value::$(date -u \\\"+%Y-%m\\\")\\\"\",
+          \"shell\": \"bash\"
+        },
+        {
+          \"name\": \"Cache Roswell Setup\",
+          \"id\": \"cache\",
+          \"uses\": \"actions/cache@v3\",
+          \"with\": {
+            \"path\": \"qlfile\\n                           qlfile.lock\\n                           /usr/local/bin/ros\\n                           ~/.cache/common-lisp/\\n                           ~/.roswell\\n                           /usr/local/etc/roswell\\n                           .qlot\",
+            \"key\": \"${{ steps.current-month.outputs.value }}-${{ env.cache-name }}-ubuntu-latest-quicklisp-sbcl-bin-${{ hashFiles('qlfile.lock') }}\"
+          }
+        },
+        {
+          \"name\": \"Restore Path To Cached Files\",
+          \"run\": \"echo $HOME/.roswell/bin >> $GITHUB_PATH\\n                 echo .qlot/bin >> $GITHUB_PATH\",
+          \"shell\": \"bash\",
+          \"if\": \"steps.cache.outputs.cache-hit == 'true'\"
+        },
         {
           \"name\": \"Setup Common Lisp Environment\",
           \"uses\": \"40ants/setup-lisp@v4\",
           \"with\": {
             \"asdf-system\": \"40ants-ci\",
             \"qlfile-template\": \"\"
-          }
+          },
+          \"if\": \"steps.cache.outputs.cache-hit != 'true'\"
         },
         {
```

")


(defsection @details (:title "Details"
                      :ignore-words ("ASDF"
                                     "CCL-BIN"))
  "
TODO: I have to write a few chapters with details on additional job's parameters
and a way how to create new job types.

But for now, I want to show a small example, how to define a workflow with a
job which takes care about lisp installation and then calls a custom step:


```lisp
(defworkflow ci
  :on-push-to \"master\"
  :by-cron \"0 10 * * 1\"
  :on-pull-request t
  :cache t
  :jobs ((40ants-ci/jobs/lisp-job:lisp-job :name \"check-ros-config\"
                                           :lisp \"ccl-bin\"
                                           :steps ((40ants-ci/steps/sh:sh \"Show Roswell Config\"
                                                                          \"ros config\")))))
```

Here we are using the class 40ANTS-CI/JOBS/LISP-JOB:LISP-JOB which is base for most classes in this ASDF system
and pass a custom 40ANTS-CI/STEPS/SH:SH step to it. This step will be called after the repostory checkout and CCL-BIN lisp installation.
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

Pay attention to the NAME argument of 40ANTS-CI/JOBS/LISP-JOB:LISP-JOB class. If you omit it, then default \"lisp-job\" name will be used.
")


(defsection @env (:title "Adding env variables")
  "
You can specify additional environment variables on any level of the GitHub workflow: for workflow itself, for a job or for a step.

To specify env for workflow or a job, just add an ENV argument with alist or plist value like this:

```lisp
(defworkflow release
  :on-push-to \"master\"
  :env (:github-token \"${{ secrets.autotag_token }}\")
  :jobs ((40ants-ci/jobs/autotag:autotag)))
```

or as alist:

```lisp
(defworkflow release
  :on-push-to \"master\"
  :env ((\"github_token\" . \"${{ secrets.autotag_token }}\"))
  :jobs ((40ants-ci/jobs/autotag:autotag)))
```

or for the job itself:

```lisp
(defworkflow release
  :on-push-to \"master\"
  :jobs ((40ants-ci/jobs/autotag:autotag
           :env (:github-token \"${{ secrets.autotag_token }}\"))))
```

the same way it can be specified on a custom step:

```lisp
(40ants-ci/steps/sh:sh \"Custom env-var example\"
                       \"echo $CUSTOM_VAR\"
                       :env (:custom-var \"Hello world!\"))
```

Note - environment variable names are always transformed to uppercase and dashes are replaced with underscores.

")


(defsection @running-custom-steps (:title "Running custom steps")
  "
Sometimes you might need to install custom system packages or do something before the job will finish. To accomplish
these task you can provide custom steps using BEFORE-STEPS argument or AFTER-STEPS argument.

Here is an example where we are installing system package libunaq1-dev before running the testsuite:


```lisp
(defparameter *required-steps*
  (list (sh \"Install libunac\"
            \"sudo apt-get install -y libunac1-dev\")))

(defworkflow ci
  :on-pull-request t
  :cache t
  :jobs ((run-tests
          :steps-before *required-steps*
          :asdf-system \"my-asdf-system\")))
```
")


(defautodoc @api (:system "40ants-ci"))
