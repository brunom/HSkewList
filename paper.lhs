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
\newcommand{\alberto}[1]{\textcolor{red}{\textbf{Alerto:}#1}}
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.HList.FakePrelude(HEq, hEq, HTrue, HFalse, HOr, hOr, Proxy, HSucc, HZero, HCond, hCond)
import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

infixr 2 .*.
infixr 4 .=.
infixr 9 #

main = go (99999999::Int) where
    go i = if i == 0 then return() else go (i - (hSum (make i)))
--    go i = if i == 0 then return() else go (i - (hSum (hUpdate l2 (l2 .=. (1::Int)) (make i))))

instance
    HUpdate l e (Record HNil) (Record HNil) where
    hUpdate _ _ = id

instance
    (  HSkewUpdate l e t t'
    ,  HUpdate l e (Record ts) (Record ts')) =>
       HUpdate l e (Record (HCons t ts)) (Record (HCons t' ts')) where
    hUpdate l e (Record (HCons t ts)) =
        Record (HCons
            (hSkewUpdate l e t)
            (case (hUpdate l e (Record ts)) of Record ts -> ts))

class HSum r where
  hSum :: r -> Int
instance HSum (SkewRecord HNil) where
  hSum r = 0
instance HSum HEmpty where
  hSum r = 0
instance (HSum x, HSum (SkewRecord xs)) => HSum (SkewRecord (HCons x xs)) where
  hSum (SkewRecord (HCons x xs)) = hSum x + hSum (SkewRecord xs)
instance (HSum t, HSum t') => HSum (HNode (LVPair l Int) t t') where
  hSum (HNode e t t') = valueLVPair e + hSum t + hSum t'

{-# NOINLINE make #-}
make i = list





list =
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l2 .=. (1::Int) .*.
--    emptyRecord
    emptySkewRecord
\end{code}
%endif

%% update&sum
%% largo:0   0.57 0.67
%% largo:10  2.78 2.83
%% largo:20  4.82 4.94
%% largo:30  6.82 6.9
%% largo:40  9.45 9.6
%% largo:50  12.1 12.5
%% largo:70  17.1 16.8
%% largo:100 23.8 23.9
%% largo:150 35.7 35.6

%% update&lookup
%% largo:0   skew:0.55 list:0.27
%% largo:10  skew:0.87 list:1.21
%% largo:20  skew:1.00 list:2.09
%% largo:40  skew:1.10 list:14.39
%% largo:100 skew:1.18 list:63.4



%% lookup
%% largo:10  skew:     list:0.783
%% largo:20  skew:0.961 list:0.872
%% largo:30  skew:1.31 list:1.02
%% largo:40  skew:1.90 list:1.18
%% largo:50  skew:2.64 list:1.42
%% largo:60  skew:3.59 list:1.68
%% largo:70  skew:5.07 list:2.04
%% largo:100 skew:10.7 list:3.30
%% largo:150 skew:31.9 list:8.48
%% largo:200 skew:70.7 list:16.9
%% largo:250 skew:136  list:27.2
%% largo:300 skew:     list:45.9
%% largo:350 skew:     list:79.5
%% largo:400 skew:     list:103
%% largo:450 skew:     list:165


%\setlength{\parindent}{0in}

\begin{document}

\conferenceinfo{Workshop on Partial Evaluation and Program Manipulation,} {Mon-Tue, January 23-24, 2012, Philadelphia, Pennsylvania, USA }
\CopyrightYear{2011}
%\copyrightdata{978-1-60558-332-7/09/08}

%\titlebanner{submitted to Haskell Symposium 2011}         % These are ignored unless
\preprintfooter{version of Oct 6, 2011}     % 'preprint' option specified.

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
The library for strongly typed heterogeneous collections HList
provides an implementation of extensible records in Haskell
that needs only a few common extensions of the language.
In HList, records are represented as linked lists of label-value pairs
with a look-up operation that is linear-time in the number of fields.
In this paper, we use type-level programming techniques
to develop a more efficient representation of extensible records for HList.
We propose an internal encoding for extensible records
that does lookup in logarithmic time
without needing a total order on the labels,
while preserving the fast insertion of simple linked lists.
Through staged compilation,
the required slow search for a field is moved to compile time. \marcos{movi esto para el final, porque me parece que tenia mas sentido asi}
\end{abstract}

%\category{D.3.3}{Programming languages}{Language Constructs and Features}
%\category{D.1.1}{Programming techniques}{Applicative (Functional) Programming}
%\terms Design, Languages, Performance, Standardization

\keywords
 Extensible Records, Type-level programming, Staged Computation, Haskell, HList, Balanced Trees 

\section{Introduction} \label{sec:intro}

Although there have been many different proposals for Extensible Records in Haskell 
\cite{Gaster96apolymorphic, Jones99lightweightextensible, LabeledFunctions, Leijen:fclabels, Leijen:scopedlabels},
it is still an open problem to find an implementation that manipulates records with satisfactory efficiency.
Imperative dynamic languages use hash tables for objects,
achieving constant time insertion and lookup,
but this implementation does not allow for persistency as required for functional languages.
The usual strategies for record insertion in functional languages are
copying all existing fields along with the new one to a brand new tuple,
or using a linked list.
The tuple strategy \marcos{primera vez que se usa ese nombre.} \bruno{eh? la oracion anterior, de las estrategias, dice que se copia a una nueva tupla} offers the fastest possible lookup, but insertion is linear time.
\marcos{tenemos alguna referencia para darle fuerza a estos argumentos?}
The linked list sits in opposite in the tradeoff curve,
with constant time insertion but linear time lookup.
Since a record is essentially a dictionary,
the obvious strategy to bridge this gap is a search tree.
While lookup is much improved to logarithmic time,
insertion is also hit and rendered logarithmic.
A bigger turnoff is that we would need to devise an order for the field labels.

This paper aims to contribute a solution in that direction. 
Our starting point is the library for strongly typed heterogeneous collections HList \cite{KLS04}
which provides an example implementation of extensible records. 
A drawback of HList is that lookup, the most used operation on records,
is linear time.
We propose an alternative implementation for extensible records, using the same techniques as HList,
with a lookup operation that runs in logarithmic time.  

Another contribution of this paper is the trick we use to reduce the run time work.
We have observed that, when looking-up an element in a HList,
the element is first searched at compile time in order to determine whether it
belongs to the list and raise an error when it does not.
This search generates the path the program follows at run time to obtain the element.
In Figure~\ref{fig:search-hlist} we represent with a dashed arrow the compile time search, 
and with a continued arrow the generated path followed at run time. 
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
(see Figure~\ref{fig:search-skew}).

Although this paper is focused on showing a more efficient implementation of extensible records,
our aim is mainly to show how harnessing type level programming techniques it is possible
to improve the run time performance of some operations by moving certain computations to compile time.
Type level programming is commonly used to increase the expressivity and type safety of programs,
but in this paper we show it can also be helpful for efficiency matters. \marcos{otro movimiento, me parece que queda mejor aca}

\begin{figure}[htp]
\begin{center}
\includegraphics[scale=0.5]{search-skew.pdf}
\end{center}
\caption{Search |l7| in balanced tree} \label{fig:search-skew}
\end{figure}


In the rest of this paper we review the type-level techniques used to implement extensible records by HList (Section~\ref{sec:hlist}) and how we use these techniques to provide an alternative implementation, based on so called skew lists \cite{Mye83,OkaThesis},
which turns out to be faster at run time (Section~\ref{sec:faster}). 
In Section~\ref{sec:efficiency} we show some results about the efficiency of our approach compared to HList.
Finally, in Section~\ref{sec:conclusions} we present some conclusions and possible directions for future work.


\section{HList}\label{sec:hlist}

HList is a Haskell library that implements typeful heterogeneous collections, 
such as heterogeneous lists or records, using extensions of Haskell for multi-parameter 
classes \cite{PJM97} and functional dependencies \cite{Jon00}.
HList strongly relies on \emph{type-level programming} techniques by means of which
types are used to represent type-level values, and classes are used to represent 
type-level types and functions.

We illustrate the use of type-level programming by means of two simple examples that
will be used later in the paper. We start with a type-level representation of booleans values. 
Since we are only interested in type-level computations, we define empty 
types |HTrue| and |HFalse| corresponding to each boolean value.

\begin{spec}
data   HTrue   ; hTrue   = undefined :: HTrue
data   HFalse  ; hFalse  = undefined :: HFalse
\end{spec}

The inhabitants |hTrue| and |hFalse| of those types are defined solely
to be used in value-level expressions to construct type-level values by 
referring to the types of such expressions.

Type-level functions can be described using multi-parameter classes with functional dependencies. 
For example, we can encode type-level negation by defining the following class:
\begin{spec}
class HNot t t' | t -> t' where
  hNot :: t -> t' 
\end{spec}
The functional dependency |t -> t'| expresses that the parameter |t|
uniquely determines the parameter |t'|. 
Therefore, once |t| is instantiated,
the instance of |t'| must be uniquely inferable by the type-system.
In other words, the relation between |t| and |t'| is actually a function.
Whereas the class definition describes the type signature of the type-level function, 
the function itself is defined by the following instance declarations:

\begin{spec}
instance HNot  HFalse  HTrue   where hNot _ = hTrue
instance HNot  HTrue   HFalse  where hNot _ = hFalse
\end{spec}
If we write the expression |(hNot hFalse)|, then we know that |t| is |HFalse|. 
So, the first instance of |hNot| is selected and thus |t'| is inferred to be |HTrue|.
Observe that the computation is completely at the type-level; 
no interesting value-level computation takes place.

Another example is the type-level representation of the maybe type. 
In this case we are interested in manipulating a value-level value associated with each type constructor.

\begin{code}
data HNothing  = HNothing
data HJust e   = HJust e
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
\end{code}

For example, the value |HCons True (HCons 'a' HNil)| is a heterogeneous list of type |HCons Bool (HCons Char HNil)|.

To prevent the formation of incorrect heterogeneous lists, such as |HCons True False|, 
it is possible to introduce a class |HList| whose instances specify the type of 
well-formed heterogeneous lists. 

\begin{spec}
class     HList l
instance  HList HNil
instance  HList l => HList (HCons e l)
\end{spec}

To improve readability, we will not include this well-formedness condition for 
heterogeneous lists or other type-level types, like naturals or booleans.

The following class describes the extension of heterogeneous collections. 

\begin{code}
class HExtend e l l' | e l -> l'
    where (.*.) :: e -> l -> l'
\end{code}

The functional dependency |e l -> l'| makes |HExtend| a type-level function.
It specifies the extension of a collection of type |l| with an element of type |e|,
resulting in a collection of type |l'|.
Function |(.*.)| performs the same computation at the level of values.

%We removed |l' -> e l|, an additional dependency present in the original HList formulation.
%The compiler refuses our instances implementing Skew lists because it cannot prove that the instances satisfy the %dependency. \marcos{no queda muy bien esa frase}


\subsection{Extensible Records}
\label{sec:extensiblerecords}

Records are mappings from labels to values.
They are modeled by an |HList| containing a heterogeneous list of fields.

\begin{code}
newtype Record r = Record r
\end{code}

\noindent 
An empty record is a |Record| containing an empty heterogeneous list:

\begin{code}
emptyRecord :: Record HNil
emptyRecord = Record HNil
\end{code}

A field with label |l| and value of type |v| is represented by the type:

\begin{code}
newtype LVPair l v  =   LVPair { valueLVPair :: v }
(.=.)               ::  l -> v -> LVPair l v
_  .=.  v           =   LVPair v
\end{code}

\noindent 
Notice that the label is a phantom type \cite{Hin03}. 
We can retrieve the label value by using the function |labelLVPair|, which exposes 
the phantom type parameter:

\begin{code}
labelLVPair :: LVPair l v -> l
labelLVPair = undefined
\end{code}

\noindent

\noindent 
Labels are represented by |HList|'s |Proxy| type constructor.
|HList| defines the key typeclass |HEq| so that
a proxy only equals itself.
By choosing a different phantom type for each label to be represented we can distinguish them.

The instance of |HExtend| for records extends the list with a new field:

\begin{code}
instance HExtend e (Record l) (Record (HCons e l))
    where e .*. Record l = Record (HCons e l)
\end{code}

\noindent 
Thus, the following defines a record (|myR|) with seven fields:

\begin{code}
data L1; l1 = undefined :: Proxy L1
data L2; l2 = undefined :: Proxy L2
data L3; l3 = undefined :: Proxy L3
data L4; l4 = undefined :: Proxy L4
data L5; l5 = undefined :: Proxy L5
data L6; l6 = undefined :: Proxy L6
data L7; l7 = undefined :: Proxy L7

myR =  l1  .=.  True     .*. 
       l2  .=.  9        .*. 
       l3  .=.  "bla"    .*. 
       l4  .=.  'c'      .*. 
       l5  .=.  Nothing  .*. 
       l6  .=.  [4,5]    .*.
       l7  .=.  "last"   .*. 
       emptyRecord
\end{code}

%% $

%% \noindent Since our lists will represent collections of attributes
%% we want to express statically that we do not have more than a single definition for each attribute occurrence
%% and so the labels in a record should be all different.
%% This constraint is represented by requiring an instance of the class |HRLabelSet| to be available when defining extendibility for records:

%% \begin{code}
%% instance HRLabelSet (HCons (LVPair l v) r)
%%     => HExtend  (LVPair l v) (Record r)
%%                 (Record (HCons (LVPair l v) r))
%%   where hExtend f (Record r) = Record (HCons f r)
%% \end{code}

The class |HHasField| is used to retrieve from a record the value part
corresponding to a specific label:

\begin{code}
class HHasField l r v | l r -> v where
    (#) :: r -> l -> v
\end{code}

\noindent 
At the type-level it is statically checked that the record |r| indeed has
a field with label |l| associated with a value of the type |v|.
At value-level the operator |(#)| returns the value of type |v|.
For example, the following expression returns the string |"last"|:

\begin{code}
last = myR # l7
\end{code}

\noindent
The |HHasField| instance for |Record| just unpacks the list
and delegates the job to |HHasFieldList|.

\begin{code}
instance
    HHasFieldList l r v =>
    HHasField l (Record r) v where
    Record r # l = hListGet l r

class HHasFieldList l r v | l r -> v where
    hListGet :: l -> r -> v
\end{code}

\noindent
We use a different class to avoid a clash with
the |HHasField| instance for |SkewRecord| to be shown later,
which also uses |HCons| and |HNil| internally.

\begin{code}
instance
    (  HEq l l' b
    ,  HHasFieldList' b l (HCons (LVPair l' v') r) v) =>
       HHasFieldList l (HCons (LVPair l' v') r) v where
    hListGet l r@(HCons f' _) =
        hListGet' (hEq l (labelLVPair f')) l r
\end{code}

\noindent
HList provides |HEq| to avoid overlapping instances everywhere.
Here |HHasFieldList| uses |HEq| to discriminate the two possible cases.
Either the label of the current field matches |l|,
or the search must continue to the next node.

\begin{code}
class HHasFieldList' b l r v | b l r -> v where
    hListGet':: b -> l -> r -> v
\end{code}

\noindent
|HHasFieldList'| receives the |b| from |HEq|, with two cases.

\begin{code}
instance
    HHasFieldList' HTrue l (HCons (LVPair l v) r) v
    where
    hListGet' _ _ (HCons (LVPair v) _) = v
\end{code}

\noindent
If the labels match, the corresponding value is returned, both at the value and type levels.

\begin{code}
instance
    HHasFieldList l r v =>
    HHasFieldList' HFalse l (HCons fld r) v where
    hListGet' _ l (HCons _ r) = hListGet l r
\end{code}

\noindent
Otherwise, |HHasFieldList'| calls back to |HHasFieldList| to continue the search.  
The two type-functions are mutually recursive.  
There's no case for the empty list; lookup fails.

At the value level, the functions |hListGet| and |hListGet'| are trivial,
devoid of logic and conditions.
For this reason,
GHC is smart enough to elide the dictionary objects and indirect jumps for |(#)|.
The code is inlined to a case cascade, but the program must traverse the linked list.
For example, this is the GHC core of the example:

\begin{spec}
last =
  case myR  of 
  HCons  _  rs1  ->  case rs1  of 
  HCons  _  rs2  ->  case rs2  of 
  HCons  _  rs3  ->  case rs3  of 
  HCons  _  rs4  ->  case rs4  of 
  HCons  _  rs5  ->  case rs5  of 
  HCons  _  rs6  ->  case rs6  of 
  HCons  e  _    -> e
\end{spec}


\section{Faster Extensible Records}\label{sec:faster}

Extensible records can double as
"static type-safe" dictionaries, that is,
collections that guarantee at compile time
that no label is duplicated
and that all labels searched for are available.
For example, \cite{FlyFirstClass}, a library for first-class attribute
grammars, uses extensible records to encode the collection of
attributes associated to each non-terminal. If we wanted to use it to
implement a system with a big number of attributes (i.e. a compiler)
an efficient structure would be needed.
Increasing the size of GHC's context reduction stack
makes the program compile
but at run time the linear time lookup algorithm
hurts performance.\marcos{queda un poco rara esta frase aca}
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
unstructured labels is beyond (our) reach. \marcos{movi tod el parrafo para aca para darle mas ``visibilidad"}

The key insight is that sub-linear behavior is only needed at run time.
We are willing to keep the work done at compile time superlinear
if it helps us to speed up our programs at run time.
|HHasField| already looks for our label at compile time
to fail compilation if we require a field for a record
without such label.
So we just store our field unordered in a structure
that allows fast random access and depend on the compiler to
hardcode the path to our fields.

Following \cite{OkaThesis} we leaned on Skew Binary Random-Access Lists.
Other, perhaps simpler, data structures
such as Braun trees \cite{brauntrees}
do not offer constant time insertion
and are not drop-in replacements for simple linear lists.
A structure with logarithmic insertion slows down
applications heavy on record modification.
That aside, the key property
of searching at compile time while retrieving at run time
works unchanged in other balanced tree structures.
\subsection{Skew Binary Random-Access List}\label{sec:skew}

We will describe Skew Binary Random-Access List \cite{Mye83} in a less principled
but easier and more direct fashion
than \cite{OkaThesis}, which is founded on numerical representations.
A skew list is a linked list spine of complete binary trees.

The invariant of skew lists is that the height of trees
get strictly larger along the linked list,
except that the first two trees may be of equal size.
Because of the size restriction,
the spine is bounded by the logarithm of the element count,
as is each tree.
Hence, we can get to any element in logarithmic effort.

Insertion maintaining the invariant is constant time
and considers two cases.
When the spine has at least two trees
and the first two trees are of equal size (\emph{case 1}),
we remove them and insert a new node built
of the new element and the two trees removed.
Else (\emph{case 2}), we just insert a new leaf.
In Figure~\ref{fig:insert} we show a graphic representation of
the construction of a skew list with the elements of |myR| from section \ref{sec:extensiblerecords}.
Nodes connected by arrows represent linked-lists and nodes connected by lines represent trees.
The first two steps (adding elements with label |l6| and |l5|) are in case 2, 
thus two leaves are inserted into the spine. 
On the other hand, the third step is in case 1, so a node has to be made with the new element and the 
two previous sub-trees.


\begin{figure}[htp]
\begin{center}
\includegraphics[scale=0.5]{insert.pdf}
\end{center}
\caption{Insertion in a Skew} \label{fig:insert}
\end{figure}

Skew list is not optimal for merging records.  In the view of tree
instances as numbers, merging is equivalent to number addition.  Some
priority queue structures do support fast merging (or melding), but
usually the resulting trees are very deep and do not support efficient
access to some elements.

\subsection{SkewRecord}

In this subsection we present our implementation of extensible records
using skew lists. First, we introduce some types to model the structure:

\begin{code}
data  HEmpty           =  HEmpty
data  HNode  e  t  t'  =  HNode  e  t  t'
type  HLeaf  e         =  HNode e HEmpty HEmpty
hLeaf        e         =  HNode e HEmpty HEmpty
\end{code}

\noindent
The element precedes the subtrees in |HNode|
so all elements in expressions read in order left to right.
The common leaf case warrants helper type |HLeaf|
and smart constructor |hLeaf|.
The following declarations define a list with the elements of the fourth step of Figure~\ref{fig:insert} :

\begin{code}
four =
    HCons  (hLeaf  (l4  .=.  'c')) $
    HCons  (HNode  (l5  .=.  Nothing) 
                   (hLeaf (l6  .=.  [4,5])) 
                   (hLeaf (l7  .=.  "last"))) $
    HNil
\end{code}

%% $ fix emacs color highlighting


We define a new tag |SkewRecord|
and the corresponding |HExtend| instance
to be able to use |(.*.)|,
as we did for |Record|.

\begin{code}
newtype SkewRecord r = SkewRecord r
emptySkewRecord :: SkewRecord HNil
emptySkewRecord = SkewRecord HNil

instance
    (  HSkewExtend (LVPair l v) ts ts'
    ,  HSkewHasField l ts HNothing) =>
       HExtend
         (LVPair l v)
         (SkewRecord ts)
         (SkewRecord ts') where
    e .*. SkewRecord ts =
        SkewRecord (hSkewExtend e ts)
\end{code}
The class |HSkewExtend| does the actual work.
With the constraint |(HSkewHasField l ts HNothing)| 
we restrict the extending record |(SkewRecord ts)| to not
include a field with label |l|.

\noindent
|HComplete| below checks that all root to leaf paths have the same length
and returns it.  We will use it to detect the case of two leading equal height trees in the spine.
\begin{code}
class HComplete t h | t -> h
instance HComplete HEmpty HZero
instance
    (  HComplete t h
    ,  HComplete t' h) =>
       HComplete (HNode e t t') (HSucc h)
\end{code}
For a node |(HNode e t t')|, if |t| and |t'| have different lengths
a compile-time error will be produced since there is no 
instance defined for this case.

\noindent
|HSkewCarry| finds out if we need to take two existing trees
and put them below a new |HNode|,
or just insert a new |HLeaf|.
The name evokes the carry possible when adding two numbers.
If each top level tree is a digit,
building a new taller tree is a form of carry,
so |HSkewCarry| returns |HTrue|.
\begin{code}
class HSkewCarry l b | l -> b
instance HSkewCarry HNil HFalse
instance HSkewCarry (HCons t HNil) HFalse
instance
    (  HComplete t h
    ,  HComplete t' h'
    ,  HEq h h' b) =>
       HSkewCarry (HCons t (HCons t' ts)) b
hSkewCarry :: HSkewCarry l b => l -> b
hSkewCarry = undefined
\end{code}

\noindent
|HSkewExtend| looks like |HHasFieldList| earlier.
In this case, |HSkewCarry| is responsible for discriminating
the current case,
while |HHasFieldList| used |HEq| on the two labels.
Because pattern matching in type classes is more limited than
run time value pattern matching
and as-patterns are missing,
a smart test type-function saves on repetition.
\begin{code}
class HSkewExtend e l l' | e l -> l'
    where hSkewExtend :: e -> l -> l'
instance
    (  HSkewCarry l b
    ,  HSkewExtend' b e l l') =>
       HSkewExtend e l l' where
    hSkewExtend e l =
        hSkewExtend' (hSkewCarry l) e l

class HSkewExtend' b e l l' | b e l -> l' where
    hSkewExtend' :: b -> e -> l -> l'
\end{code}

\noindent
Here |HFalse| means that we should not add up the first two trees of the spine.
Either the size of the trees are different, or the spine is empty or a singleton.
We just use |HLeaf| to insert a new tree at the beginning of the spine.
\begin{code}
instance
    HSkewExtend'
        HFalse
        e
        l
        (HCons (HLeaf e) l) where
    hSkewExtend' _ e l = HCons (hLeaf e) l
\end{code}

\noindent
When |HSkewCarry| returns |HTrue|, however, we build a new tree reusing the two old trees at the start of the spine.
The length of the spine is reduced in one, since we take two elements but only add one.
\begin{code}
instance
    HSkewExtend'
        HTrue
        e
        (HCons t (HCons t' l))
        (HCons (HNode e t t') l) where
    hSkewExtend' _ e (HCons t (HCons t' l)) =
        (HCons (HNode e t t') l)
\end{code}

The missing piece is |HHasField| for |SkewRecord|.
As already mentioned,
we explore all paths at compile time
but follow only the right one at run time.

Mirroring the definitions for linked list records,
we need a |HHasField| instance for |SkewRecord|
and an auxiliary type class |HSkewHasField|.
However, deciding on the path to the desired field
is now much more involved.
The cases that both the test function and the work function must consider
are more numerous and long.
Thus, we merge both functions.
|HSkewHasField| returns a type level and value level Maybe,
that is,
|HNothing| when no field with the label is found,
and |HJust| of the field's type/value otherwise.
For branching constructors |HCons| and |HNode|,
|HPlus| chooses the correct path for us.

\begin{code}
class HSkewHasField l ts v | l ts -> v where
    hSkewGet :: l -> ts -> v
instance HSkewHasField l HNil HNothing where
    hSkewGet _ _ = HNothing
instance HSkewHasField l HEmpty HNothing where
    hSkewGet _ _ = HNothing
\end{code}
\noindent
We will run |HSkewHasField| on both the spine and each tree, so we have two base cases.
|HNil| is encountered at the end of the spine, and |HEmpty| at the bottom of trees.
In both cases, the field was not found, so we return |HNothing|.

\begin{code}
instance
    (  HSkewHasField l t vt
    ,  HSkewHasField l ts vts
    ,  HPlus vt vts v) =>
       HSkewHasField l (HCons t ts) v where
    hSkewGet l (HCons t ts) =
        hSkewGet l t `hPlus` hSkewGet l ts
\end{code}
\noindent
The |HCons| case must consider that the field may be found on the current tree or further down the spine.
A recursive call is made for each sub case, and the results combined with |HPlus|, implemented earlier.  If the field is found in the current tree, |HPlus| returns it, otherwise, it returns what the search down the spine did.
That |HPlus| gives priority to the current tree has no consequence.  Records do not contain duplicate fields, as enforces by |HExtend|.

\begin{code}
instance
    (  HSkewHasField l e et
    ,  HSkewHasField l t vt
    ,  HSkewHasField l t' vt'
    ,  HPlus et vt evt
    ,  HPlus evt vt' v) =>
       HSkewHasField l (HNode e t t') v where
    hSkewGet l (HNode e t t') =
        hSkewGet l e 
            `hPlus` hSkewGet l t 
               `hPlus` hSkewGet l t'
\end{code}
\noindent
The |HNode| case is a bigger version of the |HCons| case.
Here three recursive calls are made,
for the current field, the left tree, and the right tree.
Thus two |HPlus| calls are needed to combine the result.

\begin{code}
instance
    (  HEq l l' b
    ,  HMakeMaybe b v m) =>
       HSkewHasField l (LVPair l' v) m where
    hSkewGet l f =
        hMakeMaybe
            (hEq l (labelLVPair f))
            (valueLVPair f)
\end{code}
\noindent
And this is the case that may actually build a |HJust| result.
As in |HHasFieldList| for linked lists, |HEq| compares both labels.
We call |HMakeMaybe| with the result of the comparison,
and |HNothing| or |HJust| is returned as appropriate.

Finally, |HHasField| requires
top level |HSkewHasField| to return |HJust|,
so compilation fails when looking for a non existent field,
At run time, |(#)| unwraps the input |SkewRecord|
and the intermediate |HJust| from |HSkewHasField|.
\begin{code}
instance
    HSkewHasField l ts (HJust v) =>
    HHasField l (SkewRecord ts) v where
    SkewRecord ts # l =
        case hSkewGet l ts of HJust e -> e
\end{code}

When we repeat the experiment at the end of subsection \ref{sec:extensiblerecords}, 
but using |emptySkewRecord| to construct a |SkewRecord|:
\begin{code}
myR' =  l1  .=.  True     .*. 
        l2  .=.  9        .*. 
        l3  .=.  "bla"    .*. 
        l4  .=.  'c'      .*. 
        l5  .=.  Nothing  .*. 
        l6  .=.  [4,5]    .*.
        l7  .=.  "last"   .*. 
        emptySkewRecord

last' = myR' # l7
\end{code}
the resulting core code is:\marcos{explicar un poco esto}

\begin{spec}
last' =
  case myR'   of HCons  t1   _        ->
  case t1     of HNode  _   _   t12   ->
  case t12    of HNode  _   _   t121  ->
  case t121   of HNode  e   _   _     ->
  e
\end{spec}
Thus, getting to |l7| at run time only traverses a (logarithmic length) fraction of the elements,
as we have seen in Figure~\ref{fig:search-skew}.
Later we'll examine runtime benchmarks.


\subsection{Update}\label{sec:update}

We can define a type-function |HUpdate| to change
a field of some label
with a new field with possibly new label and value.
Analogously to |HHasField| and |HExtend|,
|HUpdate| unpacks and repacks the |SkewRecord|,
doing |HSkewUpdate| all the real work.
|HSkewHasField| checks that the record
does contain a field with our label. \marcos{hay que meter m\'as texto entre medio del c\'odigo de update}

\begin{code}
class HUpdate l e r r' | l e r -> r' where
    hUpdate :: l -> e -> r -> r'
instance
    (  HSkewHasField l r (HJust v)
    ,  HSkewUpdate l e r r') =>
       HUpdate l e (SkewRecord r) (SkewRecord r') where
    hUpdate l e (SkewRecord r) =
        SkewRecord (hSkewUpdate l e r)

class HSkewUpdate l e r r' | l e r -> r' where
    hSkewUpdate :: l -> e -> r -> r'
\end{code}

\noindent
The base cases just return their argument using |id|.
With no field, there's nothing to change.
\begin{code}
instance HSkewUpdate l e HNil HNil where
    hSkewUpdate _ _ = id
instance HSkewUpdate l e HEmpty HEmpty where
    hSkewUpdate _ _ = id
\end{code}

\noindent
|HSkewUpdate| is simpler than |HHasField|.
We do not have to decide which subtree has the field to change.
Instead, we just call |hSkewUpdate| recursively for all parts.
\begin{code}
instance
    (  HSkewUpdate l e t t'
    ,  HSkewUpdate l e ts ts') =>
       HSkewUpdate l e (HCons t ts) (HCons t' ts') where
    hSkewUpdate l e (HCons t ts) =
        HCons
            (hSkewUpdate l e t)
            (hSkewUpdate l e ts)
\end{code}
In the |HNode| case, |hSkewUpdate| is recursively called to
the left and right sub-trees, and also to the element of the node.
\begin{code}
instance
    (  HSkewUpdate l e e' e''
    ,  HSkewUpdate l e tl tl'
    ,  HSkewUpdate l e tr tr') =>
       HSkewUpdate l e (HNode e' tl tr) (HNode e'' tl' tr')
       where
    hSkewUpdate l e (HNode e' tl tr) =
        HNode
            (hSkewUpdate l e e')
            (hSkewUpdate l e tl)
            (hSkewUpdate l e tr)
\end{code}

\noindent
The bottom case |LVPair| uses |HCond|
from the |HList| type-function collection to only return
an updated field if the labels match. 
In other case the original element is returned.
\begin{code}
instance
    (  HEq l l' b
    ,  HCond b e (LVPair l' v') p) =>
       HSkewUpdate l e (LVPair l' v') p where
    hSkewUpdate l e e' =
        hCond
            (hEq l (labelLVPair e'))
            e
            e'
\end{code}

Of course, we want |HUpdate| to run as fast as possible.
Rebuilding only the path to the field suffices,
keeping all other subtrees intact,
so the operation runs in logarithm time to the size of the record.
But we do not make any effort to reuse untouched parts of our original structure.
In particular, the |HNode| case calls |hSkewUpdate| for the children
and reassembles a new |HNode| with the result,
even when no matching field exists below the current node.
Executed literally, this program touches all nodes
and runs in linear time,
not what we want.
However, GHC with optimizations enabled is smart enough
to recognize that we are constructing values already available
and changes our naive program to the smart, logarithm-time, version.

%if style==newcode
\begin{code}
{-# NOINLINE myR' #-}
updR = hUpdate l1 (l3 .=. "hi") myR'
\end{code}
%endif

\subsection{Remove}

Removing a field is easy based on updating.
We overwrite the field we want gone with the first node,
and then we remove the first node.

\noindent
First we need a helper to remove the first element of a skew list.
\begin{code}
class HSkewTail ts ts' | ts -> ts' where
    hSkewTail :: ts -> ts'
\end{code}
 \begin{figure}[htp]
\begin{center}
\includegraphics[scale=0.5]{tail.pdf}
\end{center}
\caption{Tail in a Skew} \label{fig:tail}
\end{figure}

\noindent
In Figure~\ref{fig:tail} we show an example of the possible cases 
we can find.
The easy case is when the spine begins with a leaf.
We just return the tail of the spine list.
\begin{code}
instance HSkewTail (HCons (HLeaf e) ts) ts where
    hSkewTail (HCons _ ts) = ts
\end{code}

\noindent
The other case is when the spine begins with a tree of three or more elements.
Since |HLeaf| is a synonym of |HNode HEmpty HEmpty|,
we need to assert that the subtrees of the root |HNode|
are |HNode|s themselves.
By construction, both subtrees have the same shape
and pattern matching only the first suffices
to make sure this case does not overlap with the previous one.
In this case we grow the spine with the subtrees, throwing away the root.
\begin{code}
instance
    HSkewTail
        (HCons (HNode e t (HNode e' t' t'')) ts)
        (HCons t ((HCons (HNode e' t' t'')) ts)) where
    hSkewTail (HCons (HNode _ t t') ts) =
        HCons t (HCons t' ts)
\end{code}


Finally |HRemove| is done in a single instance.
We take the first node and call |HSkewUpdate|
to duplicate it where the label we want gone was.
Then |HSkewTail| removes the original occurrence,
at the start of the list.
\begin{code}
class HRemove l r r' | l r -> r' where
    hRemove :: l -> r -> r'
instance
    (  HSkewUpdate l e (HCons (HNode e t t') ts) r'
    ,  HSkewTail r' r'') =>
       HRemove
        l
        (SkewRecord (HCons (HNode e t t') ts))
        (SkewRecord r'') where
    hRemove l (SkewRecord (HCons (HNode e t t') ts)) =
        SkewRecord $
        hSkewTail $
        hSkewUpdate l e (HCons (HNode e t t') ts)
\end{code}


\section{Efficiency}\label{sec:efficiency}

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
We chose the unfusioned exposition for clarity.
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
\marcos{hay que darle un poco m\'as a este p\'arrafo. falta tambi\'en decir que ambos son exponenciales} \bruno{a ver ahi?}

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
\end{figure}\marcos{por qu\'e la gr\'afica empieza en 30?? otra cosa, yo seguir\'ia la gr\'afica de SkewRecord y/o recortar\'ia la de Record para que se corten en el mismo lado (ej 140 s).} \bruno{retoque eso}
 


\section{Conclusions and Future Work}\label{sec:conclusions}

Using type level programming techniques we have developed 
an implementation of extensible records for Haskell that at run time 
is logarithmic time at searching and removing elements and constant time at
inserting elements. This run time performance is achieved by moving 
most of the effort to compile time. 

This approach can be used to improve the performance of systems
that make extensive use of extensible records, 
like the first-class attribute grammars library AspectAG \cite{FlyFirstClass}, 
or the OOHaskell \cite{OOHaskell} library for object-oriented functional programming.

Interesting future work is to find a way to reduce compilation time.
Experiments demonstrate that GHC memoizes class instances,
but some particularity of our instances seem to confuse the mechanism.


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

