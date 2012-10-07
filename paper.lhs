\documentclass[natbib,preprint]{sigplanconf}
%\usepackage{pdfsync}
\usepackage{color}
\usepackage{graphicx}
 
%\usepackage{amsmath}
\usepackage{tikz}
%\usepackage{pgflibraryarrows}
%\usetikzlibrary{arrows}
%\newcommand{\todo}[1]{}
\newcommand{\todo}[1]{%\error                uncomment to make sure there are no todos left
 \textcolor{blue}{\mbox{$^\ast$}}\marginpar{\raggedright
 \hspace{0pt}\sffamily\tiny{\sc \textcolor{blue}{todo:}}\\ \textcolor{blue}{#1}}}
\newcommand{\bruno}[1]{\textcolor{red}{\textbf{Bruno:}#1}}
\newcommand{\alberto}[1]{\textcolor{red}{\textbf{Alberto: }#1}}
\newcommand{\btext}[1]{\textcolor{blue}{#1}}
\newcommand{\marcos}[1]{\textcolor{red}{\textbf{Marcos:}#1}}
%\renewcommand{\bruno}[1]{}
%\renewcommand{\alberto}[1]{}
%\renewcommand{\marcos}[1]{}
%let paper = True

%include lhs2TeX.fmt
%include polycode.fmt

%if style==poly

%format forall = "\forall"
%format exists = "\exists"
%format ^ = "\;"

%format ~         = "\mathbin{\;\sim\!}"
%format .*.       = "\mathbin{.\!\!*\!\!.}"
%format .=.       = "\mathbin{.\!\!=\!\!.}"
%format .+.       = "\mathbf{\;\oplus\;}"

%format epsilon    = "\epsilon"
%format alpha      = "\alpha"
%format beta       = "\beta"
%format alph_1     = "\alpha_{1}"
%format alph_n     = "\alpha_{n}"

%endif

%if style==newcode
\begin{code}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Paper where

import Data.Array
import GHC.Exts
import Unsafe.Coerce

\end{code}
%endif

%\setlength{\parindent}{0in}

\begin{document}

\conferenceinfo{Partial Evaluation and Program Manipulation,} {January 20-21, 2013, Rome, Italy}
\CopyrightYear{2013}
%\copyrightdata{978-1-60558-332-7/09/08}

%\titlebanner{submitted to Haskell Symposium 2011}         % These are ignored unless
\preprintfooter{version of Jul 21, 2012}     % 'preprint' option specified.

%\title{Fast Extensible Records In Haskell}
\title{Just Do It While Compiling!}
\subtitle{Fast Extensible Records In Haskell}


\authorinfo{Bruno Martinez Aguerre}
           {Instituto de Computaci\'{o}n \\ Universidad de la  Rep\'{u}blica\\ Montevideo, Uruguay}
           {brunom@@fing.edu.uy}
\authorinfo{Marcos Viera}
           {Instituto de Computaci\'{o}n \\ Universidad de la  Rep\'{u}blica\\ Montevideo, Uruguay}
           {mviera@@fing.edu.uy}
\authorinfo{Alberto Pardo}
           {Instituto de Computaci\'{o}n \\ Universidad de la  Rep\'{u}blica\\ Montevideo, Uruguay}
           {pardo@@fing.edu.uy}
\maketitle

\begin{abstract}
\bruno{acordarse de sacar estos comentarios antes de mandar}
The library for strongly typed heterogeneous collections HList
provides an implementation of extensible records in Haskell that needs
only a few common extensions of the language. In HList, records are
represented as linked lists of label-value pairs with a lookup
operation that is linear-time in the number of fields. In this paper,
we use type-level programming techniques to develop a more efficient
representation of extensible records for HList. We propose two
internal encodings for extensible records that improve lookup at
runtime without needing a total order on the labels. One of the
encodings performs lookup in constant time but at a cost of linear
time insertion. The other one performs lookup in logarithmic time
while preserving the fast insertion of simple linked lists. Through
staged compilation, the required slow search for a field is moved to
compile time in both cases.
\end{abstract}

\category{D.3.3}{Programming languages}{Language Constructs and Features}
\category{D.1.1}{Programming techniques}{Applicative (Functional) Programming}
\terms Design, Languages, Performance

\keywords
 Extensible Records, Type-level programming, Staged Computation, Haskell, HList, Balanced Trees 

\section{Introduction} \label{sec:intro}

\marcos{la intro quedo medio rara ahora que tenemos dos implementaciones y una de ellas usa array}
Although there have been many different proposals for Extensible Records in Haskell 
\cite{Gaster96apolymorphic, Jones99lightweightextensible, LabeledFunctions, Leijen:fclabels, Leijen:scopedlabels},
it is still an open problem to find an implementation that manipulates records with satisfactory efficiency.
Imperative dynamic languages use hash tables for objects,
achieving constant time insertion and lookup.\marcos{esto es algo que sabemos o sospechamos? hay referencias para dar?}
\bruno{sabemos pero no encuentro mejor referencia que http://en.wikipedia.org/wiki/Hashtable}
Inserting a field changes the table in place,
destructing the old version of the object,
not allowing for persistency as required in functional languages.
Copying the underlying array of the hash table
to preserve the old version makes insertion slower.

Clojure \cite{Hickey:2008:CPL:1408681.1408682} implements vectors with trees of small contiguous arrays,
so insertion is logarithmic due to structural sharing. 
Clojure's hash map, built on top of vectors,
then achieves logarithmic time insertion and lookup.

The usual strategies for record insertion in functional languages are
copying all existing fields along with the new one to a brand new tuple,
or using a linked list. \marcos{esto es algo que sabemos o sospechamos? hay referencias para dar?}
\bruno{sospechamos}
The tuple strategy offers the fastest possible lookup, but insertion is linear time.
The linked list sits in opposite in the tradeoff curve,
with constant time insertion but linear time lookup.

Since a record is essentially a dictionary,
the obvious strategy to bridge this gap is a search tree.
While lookup is much improved to logarithmic time,
insertion is also hit and rendered logarithmic.

Hash maps and ordered trees need hashing and compare functions.
This ends up being the biggest turnoff for these techniques in our setting.
Types, standing as field labels, don't have natural, readily accessible implementations
for these functions.

This paper aims to contribute a solution in that direction. 
Our starting point is the library for strongly typed heterogeneous collections HList \cite{KLS04}
which provides an example implementation of extensible records. 
A drawback of HList is that lookup, the most used operation on records,
is linear time.
We propose two alternative implementations for extensible records, using the same techniques as HList.
|ArrayRecord| uses an array to hold the fields, achieving constant time lookup but linear time insertion.
|SkewRecord| on the other hand maintains constant time insertions but lowers lookup to logarithmic time.\marcos{no se habla de la version Array} \bruno{ahora si}

Another contribution of this paper is the trick we use to reduce the run time work.
We have observed that, when looking-up an element in a HList,
the element is first searched at compile time in order to determine whether it
belongs to the list and raise an error when it does not.
This search generates the path the program follows at run time to obtain the element.
In Figure~\ref{fig:search-hlist} we represent with a dashed arrow the compile time search, 
and with a solid arrow the generated path followed at run time. 
Since the structure is linear, the search and the path have the same length.

\begin{figure}[tp]
\begin{center}
\includegraphics[scale=0.5]{search-hlist.pdf}
\end{center}
\caption{Search |l7| in HList} \label{fig:search-hlist}
\end{figure}

Thus, the key idea is very simple. Instead of a linear structure as used by HList, 
we propose the use of an alternative structure for the representation of heterogeneous collections which 
is based on balanced trees.
Such a structure better profits 
from the information given by the compile time search, leading to logarithmic length paths in the run time traversal
(see Figure~\ref{fig:search-skew}). \marcos{no se habla de la version Array}

Although this paper is focused on showing a more efficient implementation of extensible records,
our aim is mainly to show how harnessing type level programming techniques it is possible
to improve the run time performance of some operations by moving certain computations to compile time.
Type level programming is commonly used to increase the expressivity and type safety of programs,
but in this paper we show it can also be helpful for efficiency matters. 


\begin{figure}[htp]
\begin{center}
\includegraphics[scale=0.5]{search-skew.pdf}
\end{center}
\caption{Search |l7| in balanced tree} \label{fig:search-skew}
\end{figure}


In the rest of this paper we review the type-level techniques used to implement extensible records by HList (Section~\ref{sec:hlist}) and how we use these techniques to provide an alternative implementation, based on so called skew lists \cite{Mye83,OkaThesis},
which turns out to be faster at run time (Section~\ref{sec:faster}).\marcos{no se habla de la version Array} 
In Section~\ref{sec:efficiency} we show some results about the efficiency of our approach compared to HList.
Finally, in Section~\ref{sec:conclusions} we present some conclusions and possible directions for future work.


\section{HList}\label{sec:hlist}

HList is a Haskell library that implements typeful heterogeneous collections, 
such as heterogeneous lists or records, using extensions of Haskell for multi-parameter 
classes \cite{PJM97} and functional dependencies \cite{Jon00}.
HList strongly relies on \emph{type-level programming} techniques by means of which
types are used to represent type-level values, and classes are used to represent 
type-level functions.

We illustrate the use of type-level programming by means of two simple examples that
will be used later in the paper. We start with a type-level representation of booleans values. 
Since we are only interested in type-level computations, we define empty 
types |HTrue| and |HFalse| corresponding to each boolean value.

\begin{code}
data   HTrue   ; hTrue   = undefined :: HTrue
data   HFalse  ; hFalse  = undefined :: HFalse
\end{code}

The inhabitants |hTrue| and |hFalse| of those types are defined solely
to be used in value-level expressions to construct type-level values by 
referring to the types of such expressions.

Type-level functions can be described using multi-parameter classes with functional dependencies. 
For example, we can encode type-level negation by defining the following class:
\begin{code}
class HNot t t' | t -> t' where
  hNot :: t -> t' 
\end{code}
The functional dependency |t -> t'| expresses that the parameter |t|
uniquely determines the parameter |t'|. 
Therefore, once |t| is instantiated,
the instance of |t'| must be uniquely inferable by the type-system.
In other words, the relation between |t| and |t'| is actually a function.
Whereas the class definition describes the type signature of the type-level function, 
the function itself is defined by the following instance declarations:

\begin{code}
instance HNot  HFalse  HTrue   where hNot _ = hTrue
instance HNot  HTrue   HFalse  where hNot _ = hFalse
\end{code}
If we write the expression |(hNot hFalse)|, then we know that |t| is |HFalse|. 
So, the first instance of |hNot| is selected and thus |t'| is inferred to be |HTrue|.
Observe that the computation is completely at the type-level; 
no interesting value-level computation takes place.

Another example is the type-level representation of the maybe type. 
In this case we are interested in manipulating a value-level value associated with each type constructor.

\begin{code}
data HNothing  = HNothing
data HJust e   = HJust e deriving Show
\end{code}

We aim to construct a type-level value of the maybe type from a boolean. For this purpose
we define the following multi-parameter class. The parameter |v| specifies the type 
of the values to be contained by a |HJust|.

\begin{code}
class HMakeMaybe b v m | b v -> m where
    hMakeMaybe :: b -> v -> m
instance HMakeMaybe HFalse v HNothing where
    hMakeMaybe b v = HNothing
instance HMakeMaybe HTrue v (HJust v) where
    hMakeMaybe b v = HJust v
\end{code}

Another operation that will be of interest on this type is the one that combines 
two values of type maybe. 

\begin{code}
class HPlus a b c | a b -> c where
    hPlus :: a -> b -> c
instance HPlus (HJust a) b (HJust a) where
    hPlus a b = a
instance HPlus HNothing b b where
    hPlus a b = b
\end{code}

\subsection{Heterogeneous Lists}

Heterogeneous lists can be represented with the data types |HNil| and |HCons|,
which model the structure of lists both at the value and type level:

\begin{code}
data HNil       = HNil
data HCons e l  = HCons e l
infixr 2 `HCons`
\end{code}

For example, the value |HCons True (HCons 'a' HNil)| is a heterogeneous list of type |HCons Bool (HCons Char HNil)|.

\subsection{Extensible Records}
\label{sec:extensiblerecords}

Records are mappings from labels to values.
They are modeled by an |HList| containing a heterogeneous list of fields.
A field with label |l| and value of type |v| is represented by the type:

\begin{code}
newtype Field l v   =   Field { value :: v }
(.=.)               ::  l -> v -> Field l v
_  .=.  v           =   Field v
\end{code}

\noindent 
Notice that the label is a phantom type \cite{Hin03}. 
We can retrieve the label value by using the function |label|, which exposes 
the phantom type parameter:

\begin{code}
label  ::  Field l v -> l
label  =   undefined
\end{code}

We define separate types and constructors for labels.

\begin{code}
data L1 = L1
data L2 = L2
data L3 = L3
data L4 = L4
data L5 = L5
data L6 = L6
data L7 = L7
\end{code}

Thus, the following defines a record (|rList|) with seven fields:

\begin{code}
rList =
  (L1  .=.  True     )  `HCons` 
  (L2  .=.  9        )  `HCons` 
  (L3  .=.  "bla"    )  `HCons`
  (L4  .=.  'c'      )  `HCons`
  (L5  .=.  Nothing  )  `HCons` 
  (L6  .=.  [4,5]    )  `HCons`
  (L7  .=.  "last"   )  `HCons`
  HNil
\end{code}

The class |HListGet| retrieves from a record the value part
corresponding to a specific label:

\begin{code}
class HListGet r l v | r l -> v where
    hListGet :: r -> l -> v
\end{code}

\noindent 
At the type-level it is statically checked that the record |r| indeed has
a field with label |l| associated with a value of the type |v|.
At value-level |(hListGet)| returns the value of type |v|.
For example, the following expression returns the string |"last"|:

\begin{code}
lastList = hListGet rList L7
\end{code}

Instead of polluting the definitions of type-level functions
with the overlapping instance extension when comparing two types to be equal (e.g. labels),
HList encapsulates type comparison in |HEq|.
The type equality predicate |HEq| results in |HTrue| in case the compared types are equal and |HFalse| otherwise.
Thus, when comparing two types in other type-level functions (like |HListGet| below), 
these two cases can be discriminated without using overlapping instances.
\begin{code}
class HEq x y b | x y -> b
hEq :: HEq x y b => x -> y -> b
hEq = undefined
\end{code}
%
We will not delve into the different possible definitions for |HEq|.
For completeness, here is one that suffices for our purposes.
For a more complete discussion about type equality in Haskell
we refer to \cite{type-eq}.
\begin{code}
instance                HEq x x HTrue
instance b ~ HFalse =>  HEq x y b
\end{code}
%if False
\begin{code}
class TypeCast x y | x -> y, y -> x
instance TypeCast x x
instance TypeCast b HFalse => HEq x y b
instance TypeCast b HTrue => HEq x x b
\end{code}
%endif
At this point we can see that the use of overlapping instances is unavoidable. This explains why 
the implementation of HList is based on type classes and functional dependencies instead of \emph{type families} \cite{Chak05,1086397,Schrijvers2008} (which do not support overlapping instances). 

|HListGet| uses |HEq| to discriminate the two possible cases.
Either the label of the current field matches |l|,
or the search must continue to the next node.

\begin{code}
instance
    (  HEq l l' b
    ,  HListGet' b v' r' l v) =>
       HListGet (HCons (Field l' v') r') l v where
    hListGet (HCons f'@(Field v') r') l =
        hListGet' (hEq l (label f')) v' r' l
\end{code}
 
|HListGet'| has two instances, for the cases |HTrue| and |HFalse|.

\begin{code}
class HListGet' b v' r' l v | b v' r' l -> v where
    hListGet':: b -> v' -> r' -> l -> v

instance
    HListGet' HTrue v r' l v
    where
    hListGet' _ v _ _ = v

instance
    HListGet r' l v =>
    HListGet' HFalse v' r' l v where
    hListGet' _ _ r' l = hListGet r' l
\end{code}

\noindent
If the labels match, the corresponding value is returned, both at the value and type levels.
Otherwise, |HHasFieldList'| calls back to |HHasFieldList| to continue the search.  
The two type-functions are mutually recursive.  
There is no case for the empty list; lookup fails.

At the value level, the functions |hListGet| and |hListGet'| are trivial,
devoid of logic and conditions.
For this reason,
GHC is smart enough to elide the dictionary objects and indirect jumps for |hListGet|.
The code is inlined to a case cascade, but the program must traverse the linked list.
For example, this is the GHC core of the example:

\begin{code}
lastListCore = case rList of
  HCons _ rs1 -> case rs1 of
    HCons _  rs2 -> case rs2 of
      HCons _  rs3 -> case rs3 of
        HCons _ rs4 -> case rs4 of
          HCons _ rs5 -> case rs5 of
            HCons _ rs6 -> case rs6 of
              HCons e _ -> e
\end{code}

\section{Faster Extensible Records}\label{sec:faster}

Extensible records can double as
``static type-safe" dictionaries, that is,
collections that guarantee at compile time
that all labels searched for are available.
For example, \cite{FlyFirstClass}, a library for first-class attribute
grammars, uses extensible records to encode the collection of
attributes associated to each non-terminal. If we wanted to use it to
implement a system with a big number of attributes (e.g. a compiler)
an efficient structure would be needed.
Increasing the size of GHC's context reduction stack
makes the program compile
but at run time the linear time lookup algorithm
hurts performance.\marcos{queda medio raro esto}
The usual replacement when lookup in a linked list is slow
is a search tree.
In that case we would need to define a |HOrd| type-function
analogue to HList's magic |HEq|
and port some staple balanced tree to compile time,
tricky rotations and all.
As unappealing as this already is,
the real roadblock is |HOrd|.
Without help from the compiler,
defining such type function for
unstructured labels is beyond (our) reach. 

The key insight is that sub-linear behavior is only needed at run time.
We do not worry to keep the work done at compile time superlinear
if it helps us to speed up our programs at run time.
|HListGet| already looks for our label at compile time
to fail compilation if we require a field for a record
without such label.
So our idea is to maintain the fields stored unordered, but
%we just store our field unordered 
in a structure that allows fast random access and depends on the compiler to
hardcode the path to our fields.

We will present two variants of faster records.
The first follows the conventional approach of
storing the record as a tuple.
However, because Haskell does not offer
genericity over the length of tuples as in \cite{Tullsen00thezip}, i.e. efficient access to the $i$-th element of an arbitrary length tuple,
%if False
\alberto{aca lo que queres decir es que no tenes tuplas arbitrarias de largo |n| con sus correspondientes proycciones, no? si armaramos con tuplas un estructura telescopica con pares anidados, acceder un field nos quedaria un camino |fst . snd . ...| y eso es orden |n|. Este problema de las tuplas de largo |n| es una de las motivaciones de staged programming, y en particular de Template Haskell (TH). No se podra combinar lo de type-level programming con TH para en lugar de generar un array, generar una tupla de largo |n| y acceder al i-esimo elemento? tiene pinta de ser equivalente a generar el array, pero es otra alternativa.}
%endif
we will use an array instead, converting field values to a common type.
%|Any| via |unsafeCoerce|,
%since array elements must be of the same type.
%Apart from this breach of type safety,
This implementation supports linear time insertions
and constant time lookups.

The second variant is tree-like, being
based on Skew Binary Random-Access Lists~\cite{OkaThesis}, a structure that guarantees constant time  insertions and logarithmic time access to any element. 
Other, perhaps simpler, data structures
such as Braun trees \cite{brauntrees} could have been chosen, since
the key property
of searching at compile time while retrieving at run time
works unchanged in any balanced tree structure.
However, those structures do not offer constant time insertion
and are not drop-in replacements for simple linear lists.
A structure with logarithmic insertion slows down
applications heavy on record modification.

\subsection{Array Records}\label{sec:array}

An Array Record has two components:
an array containing the values of the fields, and an heterogeneous list used to find a field's ordinal for lookup in the array.
To allow the storage of elements of different types in the array, we use the type |Any|\footnote{A special type that can be used as a safe placeholder for any value.}. 
Items are then |unsafeCoerce|d on the way in and out based on the type information we keep in the heterogeneous list.
%A proper implementation would hide the data constructor
%in a separate module to ensure type safety.
%\alberto{type safety o type abstraction?}
%
%
\begin{code}
data ArrayRecord r =
  ArrayRecord r (Array Int Any)
\end{code}

Lookup is done as a two step operation.
First the ordinal of a certain label in the record is found with |HFind|.
Second the index obtained is used
to retrieve the correct element from the array. 
Figure~\ref{fig:search-array} shows a graphical representation of this process. 
Dashed arrow represents the compile time search of the field in the heterogeneous list which results in the index of the element in the array. Using this index the element is retrieved from the array in constant time at run time (solid arrow).

\begin{figure}[htp]
\begin{center}
\includegraphics[scale=0.5]{search-array.pdf}
\end{center}
\caption{Search |l7| in Array} \label{fig:search-array}
\end{figure}

|HFind| follows the same pattern as |HListGet| shown earlier, 
using |HEq| to discriminate the cases of 
the label of the current field, which may match or not the searched one. 
%
\begin{code}
class HFind r l v | r l -> v where
  hFind :: r -> l -> Int
instance
    (  HEq l l' b
    ,  HFind' b v' r' l v) =>
       HFind (HCons (Field l' v') r') l v where
    hFind ~(HCons f'@(Field v') r') l =
        hFind' (hEq l (label f')) v' r' l
\end{code}
%
If the label is found, then the index 0 is returned.
Otherwise, we increase the index by one and continue searching.
%
\begin{code}
class HFind' b v' r l v | b v' r l -> v where
    hFind':: b -> v' -> r -> l -> Int
instance 
    HFind' HTrue v r l v 
    where
      hFind' _ _ _ _ = 0
instance
    HFind r l v => HFind' HFalse v' r l v 
    where
      hFind' _ _ r l = 1 + hFind r l
\end{code}
%
The function |hFind| returns both the type of the field value (at type-level)
and the index of the field in the record (at value-level).
Note that the input is not examined at the value level.
GHC reduces each invocation of hFind to a simple integer constant via inlining and constant folding,
as any competent compiler is expected to do.
For this to work, the |HCons| pattern must be lazy, or code needs to be generated to test the data for undefined values.

In |hArrayGet|, we use the index to obtain the element from the array and the
type (|v|) to coerce the element to its correct type.
%
\begin{code}
hArrayGet :: HFind r l v =>
  ArrayRecord r -> l -> v
hArrayGet (ArrayRecord r a) l = 
  unsafeCoerce (a ! hFind r l)
\end{code}

An empty |ArrayRecord| consists of an empty heterogeneous list and an empty array.
%
\begin{code}
emptyArrayRecord =
  ArrayRecord HNil (array (0, -1) [])
\end{code}
%
Function |hArrayExtend| adds a field to an array record.
%
\begin{code}
hArrayExtend f (ArrayRecord r _) =
  let  r'  = HCons f r 
       fs  = hMapAny r' 
  in   ArrayRecord r' (listArray (0, length fs - 1) fs)
\end{code}
%
%if False
> infixr 2 `hArrayExtend`
%endif
%
The new field (which includes the type information of the element) is added to the heterogeneous list of the old record. The extended heterogeneous list
is then converted to a plain Haskell list with |hMapAny|
and turned into the array of the new record with |listArray|.
Note that the array of the old record is not used.
In this way, if several fields are added to a record
but lookup is not done on the intermediate records,
the intermediate arrays are not ever created by virtue of Haskell's laziness.
Adding $n$ fields is then a linear time operation instead of quadratic.
This optimization is the reason why an |ArrayRecord| contains the actual corresponding |HList|
instead of just the field value type relation as a phantom parameter (i.e. only at the type-level).
The function |hMapAny| iterates over the heterogeneous list \emph{coercing} its elements to values
of type |Any|.
%
\begin{code}
class HMapAny r where
  hMapAny :: r -> [Any]
instance HMapAny HNil where
  hMapAny _ = []
instance
  HMapAny r =>
  HMapAny (HCons (Field l v) r)
  where
  hMapAny (HCons (Field v) r) =
    unsafeCoerce v : hMapAny r
\end{code}



\subsection{Skew Binary Random-Access List}\label{sec:skew}

We start with a description of Skew Binary Random-Access List \cite{Mye83} in a less principled
but easier and more direct fashion than \cite{OkaThesis}, which is founded on numerical representations.
A skew list is a linked list spine of complete binary trees.

The invariant of skew lists is that the height of trees
get strictly larger along the linked list,
except that the first two trees may be of equal size.
Because of the size restriction,
the spine is bounded by the logarithm of the element count,
as is each tree.
Hence, we can get to any element in logarithmic effort.
This is a fundamental property of skew lists we will take advantage of. 

\begin{figure}[thp]
\begin{center}
\includegraphics[scale=0.5]{insert.pdf}
\end{center}
\caption{Insertion in a Skew} \label{fig:insert}
\end{figure}

Insertion maintaining the invariant is constant time
and considers two cases: 
(1) when the spine has at least two trees
and the first two trees are of equal size,
we remove them and insert a new node built
of the new element and the two trees removed; and
(2) we just insert a new leaf.
In Figure~\ref{fig:insert} we show a graphic representation of
the successive skew lists that arise in the process of construction of a skew list with the elements of |rList| from section \ref{sec:extensiblerecords}.
Nodes connected by arrows represent linked-lists and nodes connected by lines represent trees.
The first two steps (adding elements with label |l7| and |l6|) are in case (2), 
thus two leaves are inserted into the spine. 
On the other hand, the third step (adding an element with label |l5|) is in case (1), so a node has to be built with the new element as root and the two previous trees as subtrees.

Skew lists are not optimal for merging records.  In the view of tree
instances as numbers, merging is equivalent to number addition.  Some
priority queue structures do support fast merging (or melding), but
usually the resulting trees are very deep and do not support efficient
access to some elements.

\subsection{SkewRecord}

In this subsection we present our implementation of extensible records
using (heterogeneous) skew lists. 
First, we introduce some types to model heterogeneous binary trees:

\begin{code}
data  HEmpty           =  HEmpty
data  HNode  e  t  t'  =  HNode  e  t  t'
type  HLeaf  e         =  HNode e HEmpty HEmpty
\end{code}
and a smart constructor for leaves:
\begin{code}
hLeaf        e         =  HNode e HEmpty HEmpty
\end{code}

\noindent
The element precedes the subtrees in |HNode|
so all elements in expressions read in order left to right.
The common leaf case warrants the helper type |HLeaf|
and the smart constructor |hLeaf|. 

A (heterogeneous) skew list is then defined as a heterogeneous list of (heterogeneous) binary trees. 
The following declarations define a skew list with the elements of the fourth step of Figure~\ref{fig:insert}:

\begin{code}
four =
    HCons  (hLeaf  (L4  .=.  'c')) $
    HCons  (HNode  (L5  .=.  Nothing) 
                   (hLeaf (L6  .=.  [4,5])) 
                   (hLeaf (L7  .=.  "last"))) $
    HNil
\end{code}

%% $ fix emacs color highlighting
We define a smart constructor |emptySkewRecord| for empty skew lists, i.e. an empty list of trees.

\begin{code}
emptySkewRecord = HNil
\end{code}

%\noindent
|HHeight| returns the height of a tree, where |HZero| and |HSucc| implement naturals at type-level.
We will use it to detect the case of two leading equal height trees in the spine.
%
\begin{code}
data HZero
data HSucc n

class HHeight t h | t -> h
instance  HHeight HEmpty HZero
instance  HHeight t h =>
          HHeight (HNode e t t') (HSucc h)
\end{code}

\noindent
|HSkewCarry| finds out if a skew list |l| is in case (1) or (2). 
This will be used for insertion to decide whether we need to take the two leading existing trees
and put them below a new |HNode| (case 1),
or just insert a new |HLeaf| (case 2).
In the numerical representation of data structures,
adding an item is incrementing the number.
If each top level tree is a digit,
building a new taller tree is a form of carry,
so |HSkewCarry| returns |HTrue|.
%
\begin{code}
class HSkewCarry l b | l -> b

hSkewCarry :: HSkewCarry l b => l -> b
hSkewCarry = undefined
\end{code}
%
\alberto{no quedaria mejor definir |hSkewCarry| como metodo de |HSkewCarry|?}
\marcos{creo que esta bien como esta ahora, dado que todo se hace a type-level}
If the spine has none or one single tree we return |HFalse|.
\begin{code}
instance HSkewCarry HNil HFalse
instance HSkewCarry (HCons t HNil) HFalse
\end{code}
%
In case the spine has more than one tree, 
we return |HTrue| if the first two trees are of equal size and
|HFalse| otherwise.
%
\begin{code}
instance
    (  HHeight t h
    ,  HHeight t' h'
    ,  HEq h h' b) =>
       HSkewCarry (HCons t (HCons t' ts)) b
\end{code}

All these pieces allow us to define |HSkewExtend|,
which resembles the |HCons| constructor.
\begin{code}
class HSkewExtend f r r' | f r -> r'
    where hSkewExtend :: f -> r -> r'
infixr 2 `hSkewExtend`
\end{code}
|HSkewExtend| looks like |HListGet| shown earlier.
|HSkewCarry| is now responsible for discriminating
the current case,
while |HListGet| used |HEq| on the two labels.
A smart test type-function saves on repetition.
\alberto{quien es esa \emph{smart test type-function}? es |hSkewExtend'|? decir explicitamente a cual se refiere.} 

\begin{code}
instance
    (  HSkewCarry r b
    ,  HSkewExtend' b  f r r') =>
       HSkewExtend     f r r' where
    hSkewExtend f r =
        hSkewExtend' (hSkewCarry r) f r

class HSkewExtend' b f r r' | b f r -> r' where
    hSkewExtend' :: b -> f -> r -> r'
\end{code}
\noindent
Here |HFalse| means that we should not add up the first two trees of the spine.
Either the size of the two leading trees are different, or the spine is empty or a singleton.
We just use |HLeaf| to insert a new tree at the beginning of the spine.
\begin{code}
instance
    HSkewExtend'
        HFalse
        f
        r
        (HCons (HLeaf f) r) where
    hSkewExtend' _ f r = HCons (hLeaf f) r
\end{code}
%
When |HSkewCarry| returns |HTrue|, however, we build a new tree reusing the two trees that were at the start of the spine.
The length of the spine is reduced in one, since we take two elements but only add one.
%
\begin{code}
instance
    HSkewExtend'
        HTrue
        f
        (HCons t (HCons t' r))
        (HCons (HNode f t t') r) where
    hSkewExtend' _ f (HCons t (HCons t' r)) =
        (HCons (HNode f t t') r)
\end{code}

%The missing piece is
Now, we turn to the introduction of |HSkewGet|,
which explores all paths at compile time
but follows only the right one at run time.

\begin{code}
class HSkewGet r l v | r l -> v where
    hSkewGet :: r -> l -> v
\end{code}
Deciding on the path to the desired field
is now more involved.
The cases that both the test function and the work function must consider
are more numerous and long.
\alberto{no me queda claro quienes serian la test function y la work function. ademas, no seria worker function?}
Thus, we merge both functions.
|HSkewGet| returns a type level and value level Maybe,
that is,
|HNothing| when no field with the label is found,
and |HJust| of the field's type/value otherwise.
For branching constructors |HCons| and |HNode|,
|HPlus| (presented in subsection~\ref{sec:hlist}) chooses the correct path for us.

We will run |HSkewGet| on both the spine and each tree, so we have two base cases.
|HNil| is encountered at the end of the spine, and |HEmpty| at the bottom of trees.
In both cases, the field was not found, so we return |HNothing|.
\begin{code}
instance HSkewGet HNil l HNothing where
    hSkewGet _ _ = HNothing
instance HSkewGet HEmpty l HNothing where
    hSkewGet _ _ = HNothing
\end{code}
The |HCons| case must consider that the field may be found on the current tree or further down the spine.
A recursive call is made for each sub-case, and the results are combined with |HPlus|.
If the field is found in the current tree,
|HPlus| returns it, otherwise, it returns what the search down the spine did.
%
\begin{code}
instance
    (  HSkewGet r   l vr
    ,  HSkewGet r'  l vr'
    ,  HPlus vr vr' v) =>
       HSkewGet (HCons r r') l v where
    hSkewGet (HCons r r') l =
        hSkewGet r l `hPlus` hSkewGet r' l
\end{code}
%
The |HNode| case is a bigger version of the |HCons| case.
Here three recursive calls are made,
for the current field, the left tree, and the right tree.
Thus two |HPlus| calls are needed to combine the result.
%
\begin{code}
instance
    (  HSkewGet f   l vf
    ,  HSkewGet r   l vr
    ,  HSkewGet r'  l vr'
    ,  HPlus vf   vr     vfr
    ,  HPlus vfr  vr'  v) =>
       HSkewGet (HNode f r r') l v where
    hSkewGet (HNode f r r') l =
        hSkewGet f l 
            `hPlus` hSkewGet r l 
               `hPlus` hSkewGet r' l
\end{code}
%
Finally, the |Field| case, when a field is found, is the case 
%\alberto{cual, la que sigue o la anterior? no queda bien arrancar la orcion con And.....} 
that may actually build a |HJust| result.
As in |HHasFieldList| for linked lists, |HEq| compares both labels.
We call |HMakeMaybe| with the result of the comparison,
and |HNothing| or |HJust| is returned as appropriate.
%
\begin{code}
instance
    (  HEq l l' b
    ,  HMakeMaybe b v m) =>
       HSkewGet (Field l' v) l m where
    hSkewGet f l =
        hMakeMaybe
            (hEq l (label f))
            (value f)
\end{code}

When we repeat the experiment at the end of subsection \ref{sec:extensiblerecords}, 
but constructing a |SkewRecord| instead of an |HList|:
% using |emptySkewRecord| to construct a |SkewRecord|: \alberto{quien es |emptySkewRecord|?} 

%\alberto{yo capaz definiria un smart constructor que se llamara |hSkewEmpty| o por el estilo y lo pondria en lugar de HNil en la expresion de |rSkew|.}
%
\begin{code}
rSkew =
  (L1  .=.  True     )  `hSkewExtend` 
  (L2  .=.  9        )  `hSkewExtend` 
  (L3  .=.  "bla"    )  `hSkewExtend` 
  (L4  .=.  'c'      )  `hSkewExtend` 
  (L5  .=.  Nothing  )  `hSkewExtend` 
  (L6  .=.  [4,5]    )  `hSkewExtend` 
  (L7  .=.  "last"   )  `hSkewExtend` 
  emptySkewRecord

lastSkew = hSkewGet rSkew L7
\end{code}
the resulting core code is:

\begin{code}
lastSkewCore = case rSkew of
  HCons t1 _ -> case t1 of
    HNode _ _ t12 -> case t12 of
      HNode _ _ t121 ->case t121 of
        HNode e _ _ -> e
\end{code}
Thus, getting to |l7| at run time only traverses a (logarithmic length) fraction of the elements,
as we have seen in Figure~\ref{fig:search-skew}.
Later we will examine runtime benchmarks.

\marcos{me parece que aqui habria que insistir que esto se debe al mecanimso de resolucion de instancias y no a algo especifico de GHC}

\section{Efficiency}\label{sec:efficiency}

\marcos{Actualizar graficas, agregar caso ArrayRecord. Agregar una mini-intro, incluyendo computadora usada y version de GHC.}
We time accessing the last of an increasing number of fields.
The program constructs the list once
and runs a 10.000.000 iteration lookup loop.
We compile and run the program in a 4 core 2.2 Ghz Intel i7 MacBook Pro Notebook with 8 GB of RAM.
We use GHC version 7.6.1 64 bits under OS X 10.8 Mountain Lion.
The code is like this:

\begin{spec}
main = go (9999999::Int) where
    go i = if i == 0
    then return()
    else go (i - hListGet (make i) L2)

{-# NOINLINE make #-}
make i = list

list =
 HCons (L1 .=. (0::Int)) $
 HCons (L1 .=. (0::Int)) $
 HCons (L1 .=. (0::Int)) $
 HCons (L1 .=. (0::Int)) $
 HCons (L2 .=. (1::Int)) HNil
\end{spec}

A certain convolution is needed to ensure that Haskell's lazy nature
does not optimize away the benchmark.

Run time comparisons are shown in Figure~\ref{run_time}.

\begin{figure}[h]
\begin{center}
\begin{tikzpicture}[x=0.03cm,y=0.15cm]

  \def\xmin{0}
  \def\xmax{200}
  \def\ymin{0}
  \def\ymax{25}

  % grid
  \draw[style=help lines, xstep=12.5, ystep=2.5] (\xmin,\ymin) grid
  (\xmax,\ymax);

  % axes
  \draw[->] (\xmin,\ymin) -- (\xmax,\ymin) node[right] {field count};
  \draw[->] (\xmin,\ymin) -- (\xmin,\ymax) node[above] {time (s)};

  % xticks and yticks
  \foreach \x in {25,50,...,\xmax}
  \node at (\x, \ymin) [below] {\x};
  \foreach \y in {5,10,...,\ymax}
  \node at (\xmin,\y) [left] {\y};

  \draw[red] plot coordinates {
    (0,  0.8)
    (10, 1.5)
    (20, 2.4)
    (30, 3.4)
    (40, 4.3)
    (50, 5.2)
    (60, 6.5)
    (70, 7.8)
    (100,11.3)
    (150,18.3)
    (200,24.9)
  };
  \node[right,red] at (150, 25) {Record};


  \draw[green] plot coordinates {
    (0,  1.9)
    (10, 1.8)
    (20, 1.7)
    (30, 1.8)
    (40, 1.8)
    (50, 1.8)
    (60, 1.8)
    (70, 1.8)
    (100,1.8)
    (150,1.8)
    (200,1.8)
  };
  \node[right,green] at (150, 10) {ArrayRecord};

  \draw[blue] plot coordinates {
    (0,  0.7)
    (10, 1.1)
    (20, 1.2)
    (30, 1.1)
    (40, 1.2)
    (50, 1.5)
    (60, 1.2)
    (70, 1.4)
    (100,1.4)
    (150,1.6)
    (200,1.5)
  };
  \node[right,blue] at (150, 5) {SkewRecord};

\end{tikzpicture}
\end{center}
\caption{Running time}
\label{run_time}
\end{figure}

Note how in practice |ArrayRecord| and |SkewRecord| take the same time no matter the length of the record.
Actually, sometimes larger records run faster than smaller for |SkewRecord|.
For example, a 31 size skew list contains a single tree,
so elements are at most 5 hops away.
But a 28 size skew lists contains trees sized
1, 1, 3, 7 and 15,
and getting to the last takes 8 hops.

Up to ten elements, simple linked lists are faster than skew lists.
By fusing the spine list and the tree nodes,
skew lists can be tweaked to improve the performance with few elements.
This results in a single node type,
with an element and three child node references,
one to the next node,
one to the right subtree,
and one to the node of the next tree.
We chose the unfused exposition for clarity.
Another option is to use linked list for small records and switch to
skew list when over 10 fields.
Since the test is done at compile time, the adaptive structure has no
run time overhead
above having to copy the 10 fields from the linked list to the tree
when the limit is surpassed.

Figure~\ref{compile_time} shows how compile time for the three implementations grows.
|SkewRecord| is twice as slow as |HList| records, and |ArrayRecord| falls in between.
In previous versions of this paper that run the benchmarks with GHC version 7.4,
|SkewRecord| was comparatively much slower and we had to advise against it for
debugging and development, which require rapid turn around.
When insertion is rare, we prefer |ArrayRecord| because of the compile time speed.
Otherwise, |SkewRecord| is the best choice

To improve performance, the code can be rewritten with type families. \cite{PerfLeaks} suggest constraint reordering and striving for tail calls to improve
performance.
It did not work for us and it made the presentation less clear, so we went with the straightforward version.

\begin{figure}[h]
\begin{center}
\begin{tikzpicture}[x=0.015cm,y=0.15cm]

  \def\xmin{0}
  \def\xmax{400}
  \def\ymin{0}
  \def\ymax{35}

  % grid
  \draw[style=help lines, xstep=25, ystep=2.5] (\xmin,\ymin) grid
  (\xmax,\ymax);

  % axes
  \draw[->] (\xmin,\ymin) -- (\xmax,\ymin) node[right] {field count};
  \draw[->] (\xmin,\ymin) -- (\xmin,\ymax) node[above] {time (s)};

  % xticks and yticks
  \foreach \x in {50,100,...,\xmax}
  \node at (\x, \ymin) [below] {\x};
  \foreach \y in {5,10,...,\ymax}
  \node at (\xmin,\y) [left] {\y};

  \draw[red] plot coordinates {
    (0  ,0.35)
    (25 ,0.47)
    (50 ,0.63)
    (75 ,0.89)
    (100,1.15)
    (125,1.46)
    (150,1.87)
    (175,2.32)
    (200,2.88)
    (225,3.42)
    (250,4.11)
    (275,4.85)
    (300,6.12)
    (325,6.26)
    (350,7.53)
    (375,8.61)
    (400,9.64)
   };
  \node[right,red] at (400, 10) {Record};

  \draw[green] plot coordinates {
    (0  ,0.54)
    (25 ,0.55)
    (50 ,0.84)
    (75 ,1.27)
    (100,1.83)
    (125,2.54)
    (150,3.53)
    (175,4.86)
    (200,6.17)
    (225,7.81)
    (250,9.72)
    (275,13.20)
    (300,15.80)
    (325,19.07)
    (350,21.94)
    (375,27.50)
    (400,31.29)
  };
  \node[right,green] at (400,30) {SkewRecord};

  \draw[blue] plot coordinates {
    (0  ,0.36)
    (25 ,0.56)
    (50 ,0.86)
    (75 ,1.12)
    (100,1.52)
    (125,1.97)
    (150,2.56)
    (175,3.30)
    (200,4.11)
    (225,4.84)
    (250,5.93)
    (275,6.76)
    (300,8.31)
    (325,9.57)
    (350,11.56)
    (375,13.18)
    (400,15.58)
  };
  \node[right,blue] at (400,15) {ArrayRecord};
\end{tikzpicture}
\end{center}
\caption{Compile time}
\label{compile_time}
\end{figure}


\section{Conclusions and Future Work}\label{sec:conclusions}

Using type level programming techniques we have developed 
an implementation of extensible records for Haskell that, during run-time, 
takes logarithmic time for searching and removing elements and constant time for
inserting elements. This run time performance is achieved by moving 
most of the effort to compile time. 

\marcos{se podria decir en que casos es mejor usar Skew y en cuales Array}
\bruno{hecho}

This approach can be used to improve the performance of systems
that make extensive use of extensible records. 
Some examples of such systems are the first-class attribute grammars library AspectAG \cite{FlyFirstClass}, 
the OOHaskell \cite{OOHaskell} library for object-oriented functional programming,
or libraries for relational databases such as CoddFish \cite{SV06} and HaskellDB \cite{haskelldb}.

Interesting future work is to find a way to reduce compilation time.
Experiments demonstrate that GHC memoizes class instances,
but some particularity of our instances seem to confuse the mechanism.
\marcos{tambien se podria decir que este es un buen ejemplo de type-level programming y que entre los trabajos futuros podría estar el probar otras tecnicas, como type families o incluso lenguages de tipos dependientes, como Agda.}

\alberto{el siguiente texto se podria usar en las conclusiones. refiriendo a overlapping 
\emph{This is the main reason why we base our development on functional dependencies. 
Case further investigation on type families solve this problem we would be able to rephrase our implementation in terms of type families with a trivial translation, achieving a more functional style implementation.}}

\bibliographystyle{plainnat}

\begin{flushleft}
\bibliography{biblio}
\end{flushleft}

% \appendix

\end{document}

%%% Local Variables: **
%%% mode: latex **
%%% TeX-command-default: "LiterateHaskell" **
%%% TeX-master: t **
%%% TeX-default-extension: "lhs" **
%%% TeX-region: "_region_" **
%%% End: **

