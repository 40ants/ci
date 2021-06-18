===========
 ChangeLog
===========

0.2.2 (2021-06-18)
==================

* Fixed an occasional failure on ``qlot update`` inside linter workflow.
  Usually it happed when quicklisp distribution was updated and ``qlfile.lock``
  changed.

0.2.1 (2021-04-22)
==================

* Linter step was fixed to use default
  ASDF system if it wasn't specified explicitly.

0.2.0 (2021-04-15)
==================

* Supported option :ERROR-ON-WARNINGS for documentation builder.
* Option :ASD-SYSTEM was renamed to :ASDF-SYSTEM.
* Moved this project's documentation to 40ANTS-DOC system.

0.1.1 (2021-03-08)
==================

Fixed the cache key to use *.asd files.

0.1.0 (2021-02-26)
==================

Initial version.
