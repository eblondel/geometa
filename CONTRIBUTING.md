# Contributing to geometa

---------------
Contributing guidelines - 2023-03-07 - version 1 (subject to revision/consolidation)

---------------

The [geometa](https://github.com/eblondel/geometa) project openly welcomes contributions (bug reports, bug fixes, code enhancements/features, etc.).  This document will outline some guidelines on contributing to `geometa`.  As well, the [geometa discussions](https://github.com/eblondel/geometa/discussions) is a great place to get an idea of how to connect and participate in geometa community, and it should be used primarily for questions regarding the use of geometa.

geometa has the following modes of contribution:

- GitHub Commit Access
- GitHub Pull Requests

Code of Conduct
---------------

Contributors to this project are expected to act respectfully toward others in accordance with the geometa [code of conduct](https://github.com/eblondel/geometa/blob/master/CODE_OF_CONDUCT.md).

Contributions and Licensing
---------------------------

Contributors are asked to confirm that they comply with the geometa project [MIT license](https://github.com/eblondel/geometa/blob/master/LICENSE) and underlying guidelines.

### GitHub Commit Access


- proposals to provide developers with GitHub commit access shall be discussed first by contacting the geometa author/maintainer.  Proposals shall be approved by the geometa author and associated geometa developers responsible of specific geometa areas of development. Committers shall be added by the project admin.
- Direct commit access shall be granted only after significant amount of code / pull requests have been contributed by the developer, and when the latter has proven his/her knowledge of the geometa source code, capacity to manage it and guarantee its integrity.
- removal of commit access shall be handled in the same manner
- each committer must send an email to the geometa author/maintainer list agreeing to the license guidelines (see below `Contributions and Licensing Agreement Template <#contributions-and-licensing-agreement-template>`_).  **This is only required once**
- each committer shall be listed as contributor in the geometa package [description](https://github.com/eblondel/geometa/blob/master/DESCRIPTION)

### GitHub Pull Requests

- development proposals shall be discussed first on the [geometa discussions](https://github.com/eblondel/geometa/discussions) and then described through a Github issue. 
- pull requests can provide agreement to license guidelines as text in the pull request or via email to the geometa author/maintainer_  (see `Contributions and Licensing Agreement Template <#contributions-and-licensing-agreement-template>`_).  **This is only required for a contributor's first pull request.  Subsequent pull requests do not require this step**
- pull requests may include copyright in the source code header by the contributor if the contribution is significant (in general targeting a new core feature or a major enhancement of the geometa core engine) and the contributor would like to claim copyright on their contribution. The copyright shall be handled under the current geometa license (MIT).
- all contributors shall be listed at https://github.com/eblondel/geometa/graphs/contributors
- unclaimed copyright, by default, is assigned to the main copyright holders as specified in https://github.com/eblondel/geometa/blob/master/LICENSE

### Contributions and Licensing Agreement Template

``Hi all, I'd like to contribute <feature X|bugfix Y|docs|something else> to geometa.
I confirm that my contributions to geometa will be compatible with the geometa license guidelines at the time of contribution.``

GitHub
------

Code, tests, documentation, wiki and issue tracking are all managed on GitHub.
Make sure you have a `GitHub account <https://github.com/signup/free>`_.

Code Overview
-------------

- the geometa `wiki <https://github.com/eblondel/geometa/wiki>`_ documents an overview of the package

Documentation
-------------

- documentation is primarily managed in as standard R documentation associated with R classes & methods.
- additional documentation / user guides may be provided, such as vignettes

Bugs
----

geometa's `issue tracker <https://github.com/eblondel/geometa/issues>`_ is the place to report bugs or request enhancements. To submit a bug be sure to specify the geometa version you are using, the appropriate component, a description of how to reproduce the bug, as well as what version of R and platform.

Forking geometa
-------------

Contributions are most easily managed via GitHub pull requests.  `Fork <https://github.com/eblondel/geometa/fork>`_
geometa into your own GitHub repository to be able to commit your work and submit pull requests.

Development
-----------

### GitHub Commit Guidelines

- enhancements and bug fixes should be identified with a GitHub issue
- commits should be granular enough for other developers to understand the nature / implications of the change(s)
- non-trivial Git commits shall be associated with a GitHub issue. As documentation can always be improved, no need to open tickets for improving the docs
- Git commits shall include a description of changes
- Git commits shall include the GitHub issue number (i.e. ``#1234``) in the Git commit log message
- all enhancements or bug fixes must successfully pass all unit/integrations tests before they are committed
- when missing, especially for new features/enhancements, unit/integration tests should be part of the pull request

### Coding Guidelines

- geometa (all lowercase) instead of Geometa, GeoMeta, geoMeta, GeoMETA
- always code with good R coding practices. The [Hadley Wickam's Advanced R Style guide](http://adv-r.had.co.nz/Style.html) can be used as reference.

### Submitting a Pull Request

This section will guide you through steps of working on geometa.  This section assumes you have forked geometa into your own GitHub repository.
* create a branch with a meaningful name, and referencing the issue number
* commit your changes (including unit/integration tests if needed) into this branch
* push the branch/commits to your Github fork

Your changes are now visible on your geometa repository on GitHub.  You are now ready to create a pull request. A member of the geometa team will review the pull request and provide feedback / suggestions if required.  If changes are required, make them against the same branch and push as per above (all changes to the branch in the pull request apply).

If ready for integration, the pull request will then be merged by the geometa team. You can then delete your local branch (on GitHub), and then update
your own repository to ensure your geometa repository is up to date with geometa master.
