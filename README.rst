|build-status| |melpa-dev|

  A ball of mud approach to purging MELPA's original sin.

This package aims to Do the Right Thing by advising ``package-installed-p``
to disregard the MELPA-imposed version in ``PACKAGE-pkg.el``, and
instead refer directly to the Version header in package source files.

To ensure the user has acknowledged the risks of `Advising Functions`_, he
must explicitly insert into his ``.emacs``:

::

(patch-melpa-requires-activate)

Rationale: ``package-install`` generally will not update a bumped package dependency
because MELPA's timestamp versioning is incompatible with the semantic version numbers
in ``Package-Requires`` clauses (the Schism).

The particulars of the Schism are expatiated in uninteresting detail in `Issue 2944`_.

.. |build-status|
   image:: https://github.com/dickmao/patch-melpa-requires/workflows/CI/badge.svg?branch=dev
   :target: https://github.com/dickmao/patch-melpa-requires/actions
   :alt: Build Status
.. |melpa-dev|
   image:: https://melpa.org/packages/patch-melpa-requires-badge.svg
   :target: http://melpa.org/#/patch-melpa-requires
   :alt: MELPA current version

Install
=======
As described in `Getting started`_, ensure melpa's whereabouts in ``init.el`` or ``.emacs``::

   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

Then

::

   M-x package-refresh-contents RET
   M-x package-install RET patch-melpa-requires RET

Or, directly clone this repo and ``make install``.

.. _Getting started: http://melpa.org/#/getting-started
.. _Issue 2944: https://github.com/melpa/melpa/issues/2944
.. _Advising Functions: https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html
