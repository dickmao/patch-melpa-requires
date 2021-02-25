|build-status| |melpa-dev|

  A ball of mud approach to purging MELPA's original sin.

.. COMMENTARY (see Makefile)

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

Alternatively, copy ``patch-melpa-requires.el`` to a directory among ``C-h v RET load-path``.

Or, directly clone this repo and ``make install``.

Finally, add ``(require 'patch-melpa-requires)`` to ``.emacs``.

.. _Getting started: http://melpa.org/#/getting-started
.. _Issue 2944: https://github.com/melpa/melpa/issues/2944
