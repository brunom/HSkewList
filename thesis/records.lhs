\chapter{Records}
\label{chapt.records}

%\renewcommand{\tt}{\normalfont \ttfamily}

Grouping is essential to programming.
For example, a payroll program has departments grouping employees
grouping name and salary.
A department is a homogeneous group,
a collection, since all its employees have the same type.
But an employee is heterogeneous, a record:
name is a text and salary an amount.

The elements of a record are called fields.
A field has a label and a value.
When labels are {0, 1, 2, ..., n},
a record is called a tuple.
Many programming languages make tuples more convenient than records,
allowing for instance a function to return a tuple
without defining a tuple type in advance.
But tuples become unwieldy past triples or so.
Remembering what each position of a large tuple holds
wastes programmer memory.
Removing a field may shift the position of subsequent fields
and require modification of many parts of the program.
In a way, abusing tuples is like programming in old dialects of Basic
that number program lines.

Statically typed programming languages check before running a program
that all fields read or written really exist
in the corresponding record or tuple.
Compilation erases field names.
Records and tuples essentially become the same thing at runtime.
On the other hand, dynamic programming languages
don't track the type of variables
and somewhat erase the distinction between tuples and lists,
and records and dictionaries.
Accessing a field entails a search in a hash table.
In Python, the underlying dictionary of an object is exposed as |__dict__|:

\begin{verbatim}
>>> class Employee(object):
...   def __init__(self):
...     self.name = 'John'
...     self.salary = 20000
...
>>> e = Employee()
>>> e.__dict__
{'name': 'John', 'salary': 20000}
\end{verbatim}

\noindent while tuples are immutable sequences.

Records are so mainstream
that some programmers may have trouble
imagining working without them.
Placing fields accessed together
close by in memory improves performance,
because accessing a field also brings
nearby fields closer to the processor.
The same is true when working with a collection of record instances.
If only a subset of fields is needed, it’s best to partition data into
several collections of the fields needed for each computation.
In the extreme, a collection of records gets converted
into a record of collections of atomic data:
from array of structs (AoS) into struct of arrays (SoA).
Columnar storage \cite{copeland1985decomposition} treads this themes.
For example, a street racing game needs to update pedestrians each frame.
A pedestrian has a position and a velocity per frame.
Each frame adds the velocity to the position.
Storing the velocity and the position in separate arrays
allows SIMD instructions to update all pedestrians at top speed.
Libraries \cite{SDLT} and languages \cite{pharr2012ispc} can transparently convert to SoA style.

%within a record instance and across
%To improve performance
%fields accessed together
%array of structures be pivoted to structure of arrays.
%
%
%a serious programming language
%not three separate collections.
%When an employee finds another job, her record is removed from the collection
%
%simonm lib
%padding
%java struct
%databases
%columnar db
%typos
%dynamic lang
%products
%K
%sort es un caso donde no sirve serruchar fields

