(uiop:define-package #:40ants-ci
  (:nicknames #:40ants-ci/core)
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection
                #:reader
                #:section)
  (:import-from #:40ants-ci/github)
  (:export #:generate))
(in-package 40ants-ci)


(defsection @index (:title "40Ants-CI - Github Workflow Generator"
                    :ignore-words ("YAML"
                                   "CL"))
  "
[![](https://github-actions.40ants.com/40ants/ci/matrix.svg)](https://github.com/40ants/ci/actions)

This is a small utility, which can generate GitHub workflows for Common Lisp
projects.

It generates workflow for running tests and building docs. These workflows
use [40ants/run-tests](https://40ants.com/run-tests) and [40ants/build-docs](https://40ants.com/build-doc)
actions and [SBLint](https://github.com/cxxxr/sblint) to check code for compilation errors.
"
  (@reasons section)
  (@quickstart section)
  (@details section))



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
  (@caching section))


(defsection @job-types (:title "Job Types")
  (@linter section)
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
          \"uses\": \"actions/checkout@v1\"
        },
        {
          \"name\": \"Setup Common Lisp Environment\",
          \"uses\": \"40ants/setup-lisp@v1\",
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
3. Install [SBLint](https://github.com/cxxxr/sblint).
4. Run linter for `example.asd`.

Another interesting thing is that this workflow automatically uses `ubuntu-latest` OS,
`Quicklisp` and `sbcl-bin` Lisp implementation. Later I'll show you how to redefine these settings.
")


(defsection @run-tests (:title "Running Tests")
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
          \"uses\": \"actions/checkout@v1\"
        },
        {
          \"name\": \"Setup Common Lisp Environment\",
          \"uses\": \"40ants/setup-lisp@v1\",
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
          \"uses\": \"actions/checkout@v1\"
        },
        {
          \"name\": \"Setup Common Lisp Environment\",
          \"uses\": \"40ants/setup-lisp@v1\",
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
           \"uses\": \"actions/checkout@v1\"
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
+          \"uses\": \"actions/cache@v2\",
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
           \"uses\": \"40ants/setup-lisp@v1\",
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


(defsection @details (:title "Details")
  "
TODO: I have to write a few chapters with details on additional job's parameters
and a way how to create new job types.
")


(defun generate (system &key path)
  (40ants-ci/github:generate system path))
