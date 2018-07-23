# threefx/relalg

A relational algebra tester for the DMDB course.

This project originates from 11th grade, where I wrote the basic version of
this interpreter for testing purposes.

It doesn't depend on external Haskell libraries, run it as 'ghci relalg.hs'

### Supported Operations

#### Basics

- Projection (`project`)
- Renaming (`rho`)
  - expects a list of tuples `[(oldname, newname)]`
- Selection (`select`)
  - currently has a _very_ complicated interface, I'll fix this

#### Algebra

- Union (`\/`)
- Intersection (`/\`)
- Difference (`-`)

#### Joins

- Natural Join (`|><|`)
  - joins two tables on _all_ identical named columns
- Equijoin
  - joins two tables on the listed columns on equality
- Left Outer Join (`=|><|`)
- Right Outer Join (`|><|=`)
- Full Outer Join (`=|><|=`)


### Data Structures

    data Table = Table [String] [[Obj]]

    data Obj = S String
             | I Int
             | D Double
             | Null

Currently, there is no support for reading data from files - it can only be
entered as Haskell data type. See the end of the code file for example tables.

Maybe I'll implement this in the future.