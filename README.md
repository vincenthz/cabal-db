A simple tool for cabal database query
======================================

A misc set of tools to operate misc queries on the local cabal database and the associated packages

Command
-------

* diff: run the diff command between two different versions of a package.
* revdeps: print all reverse dependencies of a package.
* info: print all available versions of a package and some misc information.
* search-author: search all the database for match in the author field.
* search-maintainer: search all the database for match in the maintainer field.
* graph: generate a dot format graph of the dependency
* license: list all licenses used by packages and their dependencies
* bumpable: list all the upper version bounds that could be bumped for a list of packages
* check-policy: list the contraints type (policy) of dependency of packages
* check-revdeps-policy: list the contraints type (policy) of a specific package as used by all packages

Check the original blog post for more information:

* [Cabal-db announcement](http://tab.snarc.org/posts/haskell/2013-03-13-cabal-db.html)
* [Cabal-db license](http://tab.snarc.org/posts/haskell/2014-03-29-cabal-db-license.html)
