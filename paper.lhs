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
%% \renewcommand{\bruno}[1]{}
%% \renewcommand{\alberto}[1]{}
%% \renewcommand{\marcos}[1]{}
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

import Data.HList.FakePrelude(HEq, hEq, HTrue, HFalse, HOr, hOr, Proxy, proxy, HSucc, HZero)
import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

infixr 2 .*.
infixr 4 .=.
infixr 9 #

main = go (99999999::Int) where
    go i = if i == 0 then return() else go (i - make i # l2)

{-# NOINLINE make #-}
make i = list

list =
    l2 .=. 1 .*.
--    emptyRecord
    emptySkewRecord
\end{code}
%endif

%\setlength{\parindent}{0in}

\begin{document}

\conferenceinfo{ICFP'11,} {September 19--21, 2011, Tokyo, Japan}
\CopyrightYear{2011}
%\copyrightdata{978-1-60558-332-7/09/08}

\titlebanner{submitted to ICFP 2011}         % These are ignored unless
\preprintfooter{version of Mar 18, 2011}     % 'preprint' option specified.

\title{Scalable extensible records}
%\subtitle{How to do Aspect Oriented Programming in Haskell}

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
In this pearl, we use type-level programming techniques
to develop a more efficient representation of extensible records,
which moves most of the effort concerning the manipulation
of the record representation to compile-time
and implements a look-up operation that runs in logarithmic-time.
\end{abstract}

%\category{D.3.3}{Programming languages}{Language Constructs and Features}
%\category{D.1.1}{Programming techniques}{Applicative (Functional) Programming}
%\terms Design, Languages, Performance, Standardization

%\keywords
%Attribute Grammars, Class system, Lazy evaluation, Type-level programming, Haskell, HList


\section{Introduction} \label{sec:intro}


\begin{figure}[htp]
\begin{center}
\includegraphics[scale=0.5]{search-hlist.pdf}
\end{center}
\caption{Search |l5| in HList} \label{fig:search-hlist}
\end{figure}

\begin{figure}[htp]
\begin{center}
\includegraphics[scale=0.5]{search-skew.pdf}
\end{center}
\caption{Search |l5| in Skew} \label{fig:search-skew}
\end{figure}


\section{HList}\label{sec:hlist}

HList  \cite{KLS04} is a Haskell library that implements typeful heterogeneous collections, 
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

The inhabitants |hTrue| and |hFalse| of those types are defined for the only purpose 
to be used in value-level expressions to construct type-level values by 
referring to the types of such expressions.

Type-level functions can be described using multi-parameter classes with functional dependencies. 
For example, we can encode type-level negation by defining the following class:
\begin{spec}
class HNot t t' | t -> t' where
  hNot :: t -> t' 
\end{spec}

\noindent
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

\noindent 
If we write the expression |(hNot hFalse)|, then we know that |t| is |HFalse|. 
So, the first instance of |hNot| is selected and thus |t'| is inferred to be |HTrue|.
Observe that the computation is completely at the type-level; 
no interesting value-level computation takes place.

Another example is the type-level representation of the maybe type. 
In this case we are interesting in manipulating a value-level value associated with each type constructor.

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
instance HPlus HNothing (HJust b) (HJust b) where
    hPlus a b = b
instance HPlus HNothing HNothing HNothing where
    hPlus a b = HNothing
\end{code}

\subsection{Heterogeneous Lists}

Heterogeneous lists can be represented with the data types |HNil| and |HCons|,
which model the structure of lists both at the value and type level:

\begin{code}
data HNil       = HNil
data HCons e l  = HCons e l
\end{code}

For example, the value |HCons True (HCons (HJust 'a') HNil)| is a heterogeneous list of type |HCons Bool (HCons (HJust Char) HNil)|.

To prevent the formation of incorrect heterogeneous lists, such as |HCons True False|, 
it is possible to introduce a class |HList| whose instances specify the type of 
well-formed heterogeneous lists. 

\begin{spec}
class     HList l
instance  HList HNil
instance  HList l => HList (HCons e l)
\end{spec}

For space reasons, we we will avoid the inclusion of this well-formedness condition for 
heterogeneous lists as well as for other type-level types, like naturals or booleans,
in order to make codes shorter. 

\subsection{Extensible Records}

%% In our code we will make heavy use of non-homogeneous collections:
%% grammars are a collection of productions,
%% and nodes have a collection of attributes and a collection of children nodes.
%% Such collections, which can be extended and shrunk,
Records
map typed labels to values
and are modeled by an |HList| containing a heterogeneous list of fields,
marked with the data type |Record|.
We will refer to them as records from now on:

\begin{code}
newtype Record r = Record r
\end{code}

\noindent An empty record is a |Record| containing an empty heterogeneous list:

\begin{code}
emptyRecord :: Record HNil
emptyRecord = Record HNil
\end{code}

\noindent
A field with label |l| (a phantom type \cite{Hin03}) and value of type |v| is represented by the type:

\begin{code}
newtype LVPair l v  =   LVPair { valueLVPair :: v }
(.=.)               ::  l -> v -> LVPair l v
_  .=.  v           =   LVPair v
\end{code}

\noindent Labels are now almost first-class objects, and can be used as type-level values.
We can retrieve the label value using the function |labelLVPair|, which exposes the phantom type parameter:

\begin{code}
labelLVPair :: LVPair l v -> l
labelLVPair = undefined
\end{code}

\noindent Since we need to represent many labels, we introduce a polymorphic type |Proxy| to represent them;
by choosing a different phantom type for each label to be represented we can distinguish them:

\begin{spec}
data Proxy e ; proxy = undefined :: Proxy e
\end{spec}



To mix and match both kinds of records we introduce a multi-parameter class |HExtend|.

\begin{code}
class HExtend e l l' | e l -> l'
    where (.*.) :: e -> l -> l'
\end{code}

The functional dependency |e l -> l'| makes |HExtend| a type-level function, instead of a relation:
once |e| and |l| are fixed |l'| is uniquely determined.
It fixes the type |l'| of a collection,
resulting from extending a collection of type |l| with an element of type |e|.
The member |hExtend| performs the same computation at the level of values.

We remove |l' -> e l|, an additional dependency present in the original HList formulation.
The compiler refuses our instances implementing Skew lists because it can't prove that the instances satisfy the dependency.

\begin{code}
instance HExtend e (Record l) (Record (HCons e l))
    where e .*. Record l = Record (HCons e l)
\end{code}

\noindent Thus, the following declarations define a record (|myR|) with two elements, labelled by |Label1| and |Label2|:

\begin{code}
data Label1; label1 = proxy :: Proxy Label1
data Label2; label2 = proxy :: Proxy Label2

field1  = label1  .=.  True
field2  = label2  .=.  "bla"

myR = field1 .*. field2 .*. emptyRecord
\end{code}

%% $

%% \noindent Since our lists will represent collections of attributes
%% we want to express statically that we do not have more than a single definition for each attribute occurrence
%% and so the labels in a record should be all different.
%% This constraint is represented by requiring an instance of the class |HRLabelSet| to be available when defining extendability for records:

%% \begin{code}
%% instance HRLabelSet (HCons (LVPair l v) r)
%%     => HExtend  (LVPair l v) (Record r)
%%                 (Record (HCons (LVPair l v) r))
%%   where hExtend f (Record r) = Record (HCons f r)
%% \end{code}

The class |HHasField| is used to retrieve the value part
corresponding to a specific label from a record:

\begin{code}
class HHasField l r v | l r -> v where
    (#) :: r -> l -> v
\end{code}

\noindent At the type-level it is statically checked that the record |r| indeed has
a field with label |l| associated with a value of the type |v|.
At value-level the operator |(#)| returns the value of type |v|.
So, the following expression returns the string |"bla"|:

< myR # label2

The |HHasField| instance for |Record| just unpacks the list
and delegate the job to |HHasFieldList|.
We use a different class to avoid a clash with
the |HHasField| instance for |SkewRecord| below,
which also uses |HCons| and |HNil| internally.

\begin{code}
instance
    (HHasFieldList l r v)
    => HHasField l (Record r) v where
    Record r # l =
        hListGet l r

class HHasFieldList l r v | l r -> v where
    hListGet :: l -> r -> v

instance
    (  HEq l l' b
    ,  HHasFieldList' b l (HCons (LVPair l' v') r) v)
    => HHasFieldList l (HCons (LVPair l' v') r) v where
    hListGet l r@(HCons f' _) =
        hListGet' (hEq l (labelLVPair f')) l r

class HHasFieldList' b l r v | b l r -> v where
    hListGet':: b -> l -> r -> v

instance
    HHasFieldList' HTrue l (HCons (LVPair l v) r) v where
    hListGet' _ _ (HCons (LVPair v) _) =
        v

instance
    HHasFieldList l r v =>
    HHasFieldList' HFalse l (HCons fld r) v where
    hListGet' _ l (HCons _ r) =
        hListGet l r
\end{code}

\noindent
While only |HCons| may have the field we are looking for,
we need to consider two cases.
The head of the list may be the correct field,
or the field may be present further along the list.
We need to assert |HEq l l' b| and delegate to another type-function (|HHasFieldList'|)
so that the two cases are disambiguated in an instance head.
Haskell won't disambiguate two instances based on the instance context.
|HEq l l' b| reifies which case it is into |b|, which we then feed to |HHasFieldList'|.
This pervasive HList trick limits the most dangerous type class extensions
to the implementation of |HEq|.
The two |HHasFieldList'| instances differ in the first type-bool argument.
|HTrue| signals that the label from the field in the list head
is the right one, so we just return the value of the field.
Otherwise, in the |HFalse| case, we recursively call |HHasFieldList|.

At the value level, the functions |hListGet| and |hListGet'| are trivial,
devoid of logic and conditions.
For this reason,
GHC is smart enough to elide the dictionary objects and indirect jumps for |(#)|.
The code is inlined to a case cascade, but the program must traverse the linked list.
This is a sample program and its GHC core.

%if style==newcode
\begin{code}
data L1; l1 = undefined :: Proxy L1
data L2; l2 = undefined :: Proxy L2
data L3; l3 = undefined :: Proxy L3
data L4; l4 = undefined :: Proxy L4
\end{code}
%endif

%{-# NOINLINE squares #-}
\begin{code}
squares =
    l1  .=. 1   .*.
    l2  .=. 4   .*.
    l3  .=. 9   .*.
    l4  .=. 16  .*.
    emptyRecord
sq3 = squares # l3
\end{code}

\begin{spec}
sq3 =
  case squares  of HCons  _  l1  ->
  case l1       of HCons  _  l2  ->
  case l2       of HCons  e  _   ->
  e
\end{spec}

When the number of fields increases,
as in EDSLs that use extensible records internally \cite{FlyFirstClass},
we just bump GHC's context reduction stack and the program compiles.
At runtime, however, we may hurt from the linear time lookup algorithm.
The natural replacement when lookup in a linked list is slow
is usually a search tree.
We would need to define a |HOrd| type-function
analogue to HList's magic |HEq|
and port some staple balanced tree to compile-time,
tricky rotations and all.
As unappealing as this is,
the real roadblock is |HOrd|.
Without help from the compiler,
defining such type function for
unstructured labels is beyond (our) reach.

\section{Faster Extensible Records}\label{sec:hlist}

The key insight is that sub-linear behavior is only needed at runtime.
We are willing to keep the work done at compile-time superlinear
if it helps us to speed up our programs at runtime.
|HHasField| already looks for our label at compile-time
to fail compilation if we require a field for a record
wihout such label.
So we just store our field unordered in a structure
that allows fast random access and depend on the compiler to
hardcode the path to our fields.
Following, \cite{OkaThesis} we leaned on Skew Binary Random-Access Lists.

\subsection{Skew Binary Random-Access List}\label{sec:hlist}

We'll describe Skew Binary Random-Access List \cite{Mye83} in a less principled
but easier and more direct fashion
than \cite{OkaThesis}, which is founded on numerical representations.
A skew list is a linked list spine of complete binary trees.


\begin{figure}[htp]
\begin{center}
\includegraphics[scale=0.5]{insert.pdf}
\end{center}
\caption{Insertion in a Skew} \label{fig:insert}
\end{figure}


\subsection{SkewRecord}

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
The following declarations define a list with elements 1..5:

\begin{code}
onefive =
    HCons (hLeaf 1) $
    HCons (hLeaf 2) $
    HCons (HNode 3 (hLeaf 4) (hLeaf 5)) $
    HNil
\end{code}
\marcos{No me gusta eso de introducir un nuevo ejemplo para cada cosa. Yo usar\'ia |squares|, o mejor, har\'ia que |myR| de 2.2 sea m\'as grande y lo usar\'ia en lugar de |squares| y |onefive|.}
\bruno{La gracia de este ejemplo es mostrar la estructura real de una skew list.  Usar .*. no lo muestra.  Igual este ejemplo vuela y lo sustituye una figura, no?}

%% $ fix emacs color highlighting

The invariant of skew lists is that the height of trees
get strictly larger along the linked list,
except that the first two trees may be of equal size.
Because of the size restriction,
the spine is bounded by the logarithm of the element count,
as is each tree.
Hence, we can get to any element in logarithm effort.

Insertion maintaining the invariant is constant time
and considers two cases.
When the spine has at least two trees
and the first two trees are of equal size,
we remove them and insert a new |HNode| built
of the new element and the two trees removed.
Else, we just insert a new |HLeaf|.

We define a new tag |SkewRecord|
and the corresponding |HExtend| instance
to be able to use |(.*.)|.
|HSkewRecord| does the actual work.
\marcos{Para seguir el estilo de HList tendr\'iamos que tener un |HSkew| que sea el |SkewRecord| descripto ahora y un |HSkewRecord| con un smart constructor |mkHSkewRecord| que imponga la constraint de que el HSkew sea un ``LabelSet"}
\begin{code}
newtype SkewRecord r = SkewRecord r
emptySkewRecord :: SkewRecord HNil
emptySkewRecord = SkewRecord HNil

instance
    (HSkewExtend (LVPair l v) ts ts',
    HHasFieldSkew l ts HNothing) =>
    HExtend (LVPair l v) (SkewRecord ts) (SkewRecord ts') where
    e .*. SkewRecord ts =
        SkewRecord (hSkewExtend e ts)
\end{code}

|HComplete| below checks that all root to leaf paths have the same length
and returns it.
\begin{code}
class HComplete t h | t -> h
instance HComplete HEmpty HZero
instance
    (HComplete t h, HComplete t' h) =>
    HComplete (HNode e t t') (HSucc h)
\end{code}

|HSkewCarry| finds out if we need take two existing trees
and put them below a new |HNode|,
or just insert a new |HLeaf|.
The name evokes the carry possible when adding two numbers.
If each top level tree is a digit,
building a new, taller, is a form of carry,
so |HSkewCarry| return |HTrue|.
\begin{code}
class HSkewCarry l b | l -> b
instance HSkewCarry HNil HFalse
instance HSkewCarry (HCons t HNil) HFalse
instance
    (HComplete t h
    ,HComplete t' h'
    ,HEq h h' b)
    => HSkewCarry (HCons t (HCons t' ts)) b
hSkewCarry :: HSkewCarry l b => l -> b
hSkewCarry = undefined
\end{code}

|HSkewExtend| looks like |HHasFieldList| earlier.
In this case, |HSkewCarry| is responsible for discriminating
the current case.
Because pattern matching in type classes is more limited than
runtime value pattern matching
and as-patterns are missing,
a smart test type-function saves on repetition.
\begin{code}
class HSkewExtend e l l' | e l -> l'
    where hSkewExtend :: e -> l -> l'
instance
    (HSkewCarry l b
    ,HSkewExtend' b e l l') =>
    HSkewExtend e l l' where
    hSkewExtend e l = hSkewExtend' (hSkewCarry l) e l

class HSkewExtend' b e l l' | b e l -> l' where
    hSkewExtend' :: b -> e -> l -> l'
instance
    HSkewExtend'
        HFalse
        e
        l
        (HCons (HLeaf e) l) where
    hSkewExtend' _ e l = HCons (hLeaf e) l
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
we explore all paths at compile-time
but follow only the right one at runtime.

\begin{code}
data HNothing  = HNothing
data HJust e   = HJust e

class HMakeMaybe b v m | b v -> m where
    hMakeMaybe :: b -> v -> m
instance HMakeMaybe HFalse v HNothing where
    hMakeMaybe b v = HNothing
instance HMakeMaybe HTrue v (HJust v) where
    hMakeMaybe b v = HJust v

class HPlus a b c | a b -> c where
    hPlus :: a -> b -> c
instance HPlus (HJust a) b (HJust a) where
    hPlus a b = a
instance HPlus HNothing (HJust b) (HJust b) where
    hPlus a b = b
instance HPlus HNothing HNothing HNothing where
    hPlus a b = HNothing
\end{code}

Mirroring the definitions for linked list records,
we need a |HHasField| instance for |SkewRecord|
and an auxiliary type class |HHasFieldSkew|.
However, deciding on the path to the desired field
is now much more involved.
The cases that both the test function and the work function must consider
are more numerous and involved.
Thus, we merge both functions.
|HHasFieldSkew| 
\begin{code}
instance
    (HHasFieldSkew l ts (HJust v)) =>
    HHasField l (SkewRecord ts) v where
    SkewRecord ts # l =
        case hSkewGet l ts of HJust e -> e

class HHasFieldSkew l ts v | l ts -> v where
    hSkewGet :: l -> ts -> v
instance HHasFieldSkew l HNil HNothing where
    hSkewGet _ _ = HNothing
instance HHasFieldSkew l HEmpty HNothing where
    hSkewGet _ _ = HNothing
instance
    (HHasFieldSkew l t vt
    ,HHasFieldSkew l ts vts
    ,HPlus vt vts v) =>
    HHasFieldSkew l (HCons t ts) v where
    hSkewGet l (HCons t ts) =
        hSkewGet l t `hPlus` hSkewGet l ts
instance
    (HHasFieldSkew l e et
    ,HHasFieldSkew l t vt
    ,HHasFieldSkew l t' vt'
    ,HPlus et vt evt
    ,HPlus evt vt' v) =>
    HHasFieldSkew l (HNode e t t') v where
    hSkewGet l (HNode e t t') =
        hSkewGet l e `hPlus` hSkewGet l t `hPlus` hSkewGet l t'
instance
    (HEq l l' b
    ,HMakeMaybe b v m) =>
    HHasFieldSkew l (LVPair l' v) m where
    hSkewGet l f = hMakeMaybe (hEq l (labelLVPair f)) (valueLVPair f)
\end{code}

\section{Efficiency}

\todo{incluir gr\'aficas}

\begin{tikzpicture}[x=0.04cm,y=0.16cm]

  \def\xmin{0}
  \def\xmax{150}
  \def\ymin{0}
  \def\ymax{35}

  % grid
  \draw[style=help lines, ystep=2.5, xstep=10] (\xmin,\ymin) grid
  (\xmax,\ymax);

  % axes
  \draw[->] (\xmin,\ymin) -- (\xmax,\ymin) node[right] {list length};
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

\begin{tikzpicture}[x=0.04cm,y=0.04cm]

  \def\xmin{0}
  \def\xmax{150}
  \def\ymin{0}
  \def\ymax{110}

  % grid
  \draw[style=help lines, ystep=10, xstep=10] (\xmin,\ymin) grid
  (\xmax,\ymax);

  % axes
  \draw[->] (\xmin,\ymin) -- (\xmax,\ymin) node[right] {list length};
  \draw[->] (\xmin,\ymin) -- (\xmin,\ymax) node[above] {time (s)};

  % xticks and yticks
  \foreach \x in {20,40,...,\xmax}
  \node at (\x, \ymin) [below] {\x};
  \foreach \y in {20,40,...,\ymax}
  \node at (\xmin,\y) [left] {\y};

  \draw[blue] plot coordinates {
    (0,  2.3)
    (10, 2.5)
    (20, 2.8)
    (30, 3.2)
    (40, 3.7)
    (50, 4.3)
    (60, 5.1)
    (70, 6.1)
    (100, 11)
    (150, 24)
  };
  \node[right,blue] at (150, 24) {Record};

  \draw[red] plot coordinates {
    (0,  2.5)
    (10, 2.7)
    (20, 3.3)
    (30, 4.5)
    (40, 6.3)
    (50, 8.7)
    (60,  12)
    (70,  16)
    (100, 37)
    (150,110)
  };
  \node[right,red] at (150,110) {SkewRecord};
\end{tikzpicture}

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
