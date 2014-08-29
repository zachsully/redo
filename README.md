# **redo**
written in Haskell;
implementation by Zach Sullivan

## Discription

redo is a system for building binaries up to date. redo builds it's 
targets atomically. The build scripts for each target it kept in it's
own target.do file.

## redo-ifchange

Dependencies of a target are declared by **redo-ifchange** in a build script.
Calling **redo-ifchange** on a dependency will bring the dependency up to
date. If **redo-ifchange** finds that it had to bring any targets up to date
of it's arguements (which are the targets dependencies) than it will continue
on with the build script.

### Types of Dependencies

**redo-ifchange** handles different types of dependencies in different ways.

* *Source* -
  **redo-ifchange** will check to see if a source file has changed with a
  checksum. This works as a base-case for the **redo**'s recursion.

* *Abstract Targets* -
  Since abstract targets do not build an actual target file, **redo-ifchange**
  will just recurse by calling the abstract target's build script. 

* *Concrete Targets* -
  **redo-ifchange** will recurse calling the build script of a concrete target.
  After the script has completed, **redo-ifchange** will use a checksum to see
  the if the target has changed

## Motivation

I've created my own implementation of redo for a couple of reasons. One,
I am just learning to program in haskell myself and writing this has given me
my first introduction to using haskell libraries to find what I need, using
various haskell developement tools, and even concurrent programming. I also
find recursive algorithms incredibly facinating and working on this satisfies
my curiousity in a way.

## Credits

The original idea for this program was concieved by djb. I took inspiration
from other redo implementations on github, especially jekor's which is also
written in haskell.