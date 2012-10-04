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

data HZero
data HSucc n

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
represented as linked lists of label-value pairs with a look-up
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

Although there have been many different proposals for Extensible Records in Haskell 
\cite{Gaster96apolymorphic, Jones99lightweightextensible, LabeledFunctions, Leijen:fclabels, Leijen:scopedlabels},
it is still an open problem to find an implementation that manipulates records with satisfactory efficiency.
Imperative dynamic languages use hash tables for objects,
achieving constant time insertion and lookup.
Inserting a field changes the table in place,
destructing the old version of the object,
not allowing for persistency as required in functional languages.
Copying the underlying array of the hash table
to preserve the old version makes insertion slower.

Clojure \cite{Hickey:2008:CPL:1408681.1408682} implements vectors with trees of small contiguous arrays,
so insertion is logarithmic due to structural sharing. 
Clojure's hash map, built on top of vectors,
then achieves logarithmic time insertion and look-up.

The usual strategies for record insertion in functional languages are
copying all existing fields along with the new one to a brand new tuple,
or using a linked list.
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
We propose an alternative implementation for extensible records, using the same techniques as HList,
with a lookup operation that runs in logarithmic time. \marcos{no se habla de la version Array} 

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
\includegraphics[scale=0.5]{search-array.pdf}
\end{center}
\caption{Search |l7| in Array} \label{fig:search-array}
\end{figure}


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
with the overlapping instance extension,
HList encapsulates type comparison in |HEq|.

\begin{code}
class HEq x y b | x y -> b
hEq :: HEq x y b => x -> y -> b
hEq = undefined
\end{code}

We will not delve into the different possible definitions for |HEq|.
For completeness, here is one that suffices for our purposes.

\begin{code}
class TypeCast x y | x -> y, y -> x
instance TypeCast x x
instance TypeCast b HFalse => HEq x y b
instance TypeCast b HTrue => HEq x x b
\end{code}

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
We are willing \alberto{yo mejor diria} \btext{We do not worry} to keep the work done at compile time superlinear
if it helps us to speed up our programs at run time.
|HListGet| already looks for our label at compile time
to fail compilation if we require a field for a record
without such label.
So \btext{our idea is to maintain the fields stored unordered, but} 
%we just store our field unordered 
in a structure that allows fast random access and depends on the compiler to
hardcode the path to our fields.

We will present two variants of faster records.
The first follows the conventional approach of
storing the record as a tuple.
But because GHC does not offer
genericity over the length of tuples as in \cite{Tullsen00thezip},
\alberto{aca lo que queres decir es que no tenes tuplas arbitrarias de largo |n| con sus correspondientes proycciones, no? si armaramos con tuplas un estructura telescopica con pares anidados, acceder un field nos quedaria un camino |fst . snd . ...| y eso es orden |n|. Este problema de las tuplas de largo |n| es una de las motivaciones de staged programming, y en particular de Template Haskell (TH). No se podra combinar lo de type-level programming con TH para en lugar de generar un array, generar una tupla de largo |n| y acceder al i-esimo elemento? tiene pinta de ser equivalente a generar el array, pero es otra alternativa.}
we will use an array instead,
converting field values to |Any| via |unsafeCoerce|,
since array elements must be of the same type.
Apart from this breach of type safety,
the implementation supports linear time insertions
and constant time look-ups.

The second variant is tree-like, being
based on Skew Binary Random-Access Lists~\cite{OkaThesis}.
Other, perhaps simpler, data structures
such as Braun trees \cite{brauntrees}
do not offer constant time insertion
and are not drop-in replacements for simple linear lists.
A structure with logarithmic insertion slows down
applications heavy on record modification.
That aside, the key property
of searching at compile time while retrieving at run time
works unchanged in other balanced tree structures.
\alberto{no entendi bien que quisiste decir en esta ultima frase}

\subsection{Array Records}\label{sec:array}

An Array Record has two components:
an array containing the values of the fields, and an heterogeneous list used to find a field's ordinal for look-up in the array.
To allow the storage of elements of different types in the array, we use |Any|\footnote{A special type that can be used as a safe placeholder for any value.} as the array. 
Items are then |unsafeCoerce|d on the way in and out.
A proper implementation would hide the data constructor
in a separate module to ensure type safety.
\alberto{type safety o type abstraction?}

\begin{code}
data ArrayRecord r =
  ArrayRecord r (Array Int Any)
arrayEmptyRecord =
  ArrayRecord HNil (listArray (0, 0) [])
\end{code}
\alberto{no me gusta mucho el uso del Any, pero parece medio inevitable. una alternativa es usar un tipo existencial, pero se termina en algo similar.}

\alberto{el caso para HNil deberia ser |array (1,0) []| que define un array vacio. |listArray (0, 0) []| define un array de un elemento valiendo bottom.}

\alberto{le llamaria |emptyArrayRecord|}

Function |hArrayExtend| adds a field to an array record.
%
\begin{code}
hArrayExtend f (ArrayRecord r _) =
  let  r'  = HCons f r 
       fs  = hMapAny r' 
  in   ArrayRecord r' (listArray (0, length fs) fs)
infixr 2 `hArrayExtend`
\end{code}
\bruno{hArrayExtend tb quiere ser infix}

\alberto{yo sacaria la declaracion de infixr, no es relevante.}

\alberto{se esta asignado un lugar de mas al array. deberia ser |listArray (1, length list) list| o |listArray (0, length list - 1) list|, dependiendo de como se obtiene el indice en |arrayRank|, me parece que es el ultimo.}

The new field is added to the heterogeneous list of the old record,
which is then converted to a plain Haskell list with |hMapAny|
and turned into the array of the new record with |listArray|.
Note that the array of the old record is not used.
In this way, if several fields are added to a record
but look-up is not done on the intermediate records,
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

Finally, look-up is done as a two step operation.
First the ordinal of a certain label in the record is found with |ArrayRank|.
Second the index obtained is used
to retrieve the correct element from the array.
|ArrayRank| follows the same pattern as |HListGet| shown earlier, 
using |HEq| to discriminate the cases of 
the label of the current field, which may match or not the searched one. 
%
\begin{code}
class ArrayRank r l v | r l -> v where
  arrayRank :: r -> l -> Int
instance
    (  HEq l l' b
    ,  ArrayRank' b v' r' l v) =>
       ArrayRank (HCons (Field l' v') r') l v where
    arrayRank (HCons f'@(Field v') r') l =
        arrayRank' (hEq l (label f')) v' r' l
\end{code}
%
If the label is found, then the index 0 is returned.
Otherwise, we increase the index by one and continue searching.
%
\begin{code}
class ArrayRank' b v' r l v | b v' r l -> v where
    arrayRank':: b -> v' -> r -> l -> Int
instance 
    ArrayRank' HTrue v r l v 
    where
      arrayRank' _ _ _ _ = 0
instance
    ArrayRank r l v => ArrayRank' HFalse v' r l v 
    where
      arrayRank' _ _ r l = 1 + arrayRank r l
\end{code}
%
The function |arrayRank| returns both the type of the field value (at type-level)
and the index of the field in the record (at value-level).
In |hArrayGet|, we use the index to obtain the element from the array and the
type (|v|) to coerce the element to its correct type.
%
\begin{code}
hArrayGet :: ArrayRank r l v => ArrayRecord r -> l -> v
hArrayGet (ArrayRecord r a) l = 
  unsafeCoerce (a ! arrayRank r l)
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

\noindent
|HHeight| returns the height of a tree.
We will use it to detect the case of two leading equal height trees in the spine.
%
\begin{code}
class HHeight t h | t -> h
instance HHeight HEmpty HZero
instance
    (  HHeight t h) =>
       HHeight (HNode e t t') (HSucc h)
\end{code}
\noindent

\alberto{cuidado que |HZero| y |HSucc| no estan definidos en el paper. habria que agregarlos en la seccion de |HList|.}

|HSkewCarry| finds out if a skew list |l| is in case (1) or (2). This will be used for insertion to decide whether we need to take the two leading existing trees
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
And this is the case \alberto{cual, la que sigue o la anterior? no queda bien arrancar la orcion con And.....} that may actually build a |HJust| result.
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
but using |emptySkewRecord| to construct a |SkewRecord|: \alberto{quien es |emptySkewRecord|?} 

\alberto{yo capaz definiria un smart constructor que se llamara |hSkewEmpty| o por el estilo y lo pondria en lugar de HNil en la expresion de |rSkew|.}
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
  HNil

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


\section{Efficiency}\label{sec:efficiency}

\marcos{Actualizar graficas, agregar caso ArrayRecord. Agregar una mini-intro, incluyendo computadora usada y version de GHC.}
We time accessing the last of an increasing number of fields.
The program constructs the list once
and runs a 100.000.000 iteration |(#)| loop.
Run time comparisons are shown in Figure~\ref{run_time}.

\begin{figure}[h]
\begin{center}
\begin{tikzpicture}[x=0.03cm,y=0.12cm]

  \def\xmin{0}
  \def\xmax{150}
  \def\ymin{0}
  \def\ymax{35}

  % grid
  \draw[style=help lines, xstep=10, ystep=2.5] (\xmin,\ymin) grid
  (\xmax,\ymax);

  % axes
  \draw[->] (\xmin,\ymin) -- (\xmax,\ymin) node[right] {field count};
  \draw[->] (\xmin,\ymin) -- (\xmin,\ymax) node[above] {time (s)};

  % xticks and yticks
  \foreach \x in {20,40,...,\xmax}
  \node at (\x, \ymin) [below] {\x};
  \foreach \y in {5,10,...,\ymax}
  \node at (\xmin,\y) [left] {\y};

  \draw[blue] plot coordinates {
    (0,  1.2)
    (10, 3.7)
    (20, 5.8)
    (30, 8.0)
    (40,  11)
    (50,  13)
    (60,  15)
    (70,  17)
    (100, 24)
    (150, 35)
  };
  \node[right,blue] at (150, 35) {Record};


  \draw[red] plot coordinates {
    (0,  1.6)
    (10, 2.5)
    (20, 2.7)
    (30, 2.6)
    (40, 2.9)
    (50, 3.4)
    (60, 2.9)
    (70, 3.3)
    (100,3.2)
    (150,3.7)
  };
  \node[right,red] at (150, 3.7) {SkewRecord};
\end{tikzpicture}
\end{center}
\caption{Running time}
\label{run_time}
\end{figure}

Note how the |SkewRecord| version barely increases the run time at a logarithm rate.
Actually, sometimes larger records run faster.
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

Figure~\ref{compile_time} shows that compile time for both implementations grows super linearly and rapidly accelerating.
Already at two hundred fields |SkewRecord| is unwieldy,
while plain |Record| is still usable.
Were we to stop the comparison there,
it would give the impression that the structures behave qualitatively different.
Further increase in the number of fields uncovers however
that the difference is most likely a matter of constants.
Others have also found performance regressions in newer ghc versions \cite{PerfLeaks} 
and suggest constraint reordering and striving for tail calls.
It did not work for us and it made the presentation less clear.
As always, remember that a compiled program is run many times,
so long compile times are amortized.
For development, when rapid turn around is key,
|Record| can be used, as the interface is the same.

\begin{figure}[h]
\begin{center}
\begin{tikzpicture}[x=0.012cm,y=0.030cm]

  \def\xmin{0}
  \def\xmax{425}
  \def\ymin{0}
  \def\ymax{140}

  % grid
  \draw[style=help lines, xstep=25, ystep=10] (\xmin,\ymin) grid
  (\xmax,\ymax);

  % axes
  \draw[->] (\xmin,\ymin) -- (\xmax,\ymin) node[right] {field count};
  \draw[->] (\xmin,\ymin) -- (\xmin,\ymax) node[above] {time (s)};

  % xticks and yticks
  \foreach \x in {50,100,...,\xmax}
  \node at (\x, \ymin) [below] {\x};
  \foreach \y in {20,40,...,\ymax}
  \node at (\xmin,\y) [left] {\y};

  \draw[blue] plot coordinates {
    (0, 0)
    (30, 1.02)
    (40, 1.18)
    (50, 1.42)
    (60, 1.68)
    (70, 2.04)
    (100, 3.30)
    (150, 8.48)
    (200, 16.9)
    (250, 27.2)
    (300, 45.9)
    (350, 79.5)
    (400, 103)
    (425, 140)
   };
  \node[right,blue] at (150, 24) {Record};

  \draw[red] plot coordinates {
    (0, 0)
    (30, 1.31)
    (40, 1.90)
    (50, 2.64)
    (60, 3.59)
    (70, 5.07)
    (100, 10.7)
    (150, 31.9)
    (200, 70.7)
    (250, 140)
  };
  \node[right,red] at (150,110) {SkewRecord};
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

This approach can be used to improve the performance of systems
that make extensive use of extensible records, 
like the first-class attribute grammars library AspectAG \cite{FlyFirstClass}, 
the OOHaskell \cite{OOHaskell} library for object-oriented functional programming,
or the library for relational databases CoddFish \cite{SV06}.

Interesting future work is to find a way to reduce compilation time.
Experiments demonstrate that GHC memoizes class instances,
but some particularity of our instances seem to confuse the mechanism.
\marcos{tambien se podria decir que este es un buen ejemplo de type-level programming y que entre los trabajos futuros podría estar el probar otras tecnicas, como type families o incluso lenguages de tipos dependientes, como Agda.}

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

