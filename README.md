449-w18-T2-Haskell

use compiler version 8.2.1

modules for branch and bound:
https://hackage.haskell.org/package/ListTree-0.2.3/docs/Data-List-Tree.html#t:Tree
https://hackage.haskell.org/package/scyther-proof-0.3.0/docs/Control-Monad-BoundedDFS.html


To build and run this system:

1) make sure Stack is installed along with all dependencies (see https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md)
2) on command line, go into the main project directory and type the following:
    ```
    stack build
    stack exec haskell-proj-exe
    ```

3) If this is the first time running this project and the above doesn;t work, try running <stack init>
4) To run the test suite as specified in the package.yaml file, run <stack test>

To add a package as a dependency:

1) Add the package name and version to the extra-deps list in stack.yaml
    (This will taek care of the package's dependencies during when you run "stack build")
2) Add desired module(s) from the package under "dependencies" in package.yaml
3) run <stack build>