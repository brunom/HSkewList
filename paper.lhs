\documentclass[natbib,preprint]{sigplanconf}
%\usepackage{pdfsync}
\usepackage{color}
%\usepackage{amsmath}
%\usepackage{tikz}
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

module Paper where

import Data.HList.FakePrelude(HEq, hEq, HTrue, HFalse, HOr, hOr, Proxy, proxy, HSucc, HZero)
import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

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

\section{HList}\label{sec:hlist}

The library HList  \cite{KLS04} implements typeful heterogeneous collections (lists, records, ...),
using techniques for dependently typed programming in Haskell \cite{Hall01,McB02}
which in turn use Haskell 98 extensions for multi-parameter classes \cite{PJM97} and functional dependencies \cite{Jon00}.
\emph{Type-level programming} uses types to represent type-level values, and classes to represent type-level types and functions.

In order to be self-contained we start out with a small introduction.
To represent Boolean values at the type level we define a new type for each of the Boolean values.
%% The class |HBool| represents the type-level type of Booleans.
%% We may read the instance definitions as ``the type-level values |HTrue| and |HFalse| have the type-level type |HBool|'':
%% class  HBool x
\begin{spec}
data   HTrue   ; hTrue   = undefined :: HTrue
data   HFalse  ; hFalse  = undefined :: HFalse
\end{spec}

%% instance HBool HTrue
%% instance HBool HFalse

Since we are only interested in type-level computation,
we defined |HTrue| and |HFalse| as empty types.
By defining an inhabitant for each value we can,
by writing expressions at the value level,
construct values at the type-level
by referring to the types of such expressions.

Multi-parameter classes can be used to describe type-level \emph{relations},
whereas functional dependencies restrict such relations to functions.
As an example we define the class |HOr| for type-level disjunction:

%% class (HBool t, HBool t', HBool t'')
%%     => HOr t t' t'' | t t' -> t''

\begin{spec}
class HOr t t' t'' | t t' -> t''
hOr :: HOr t t' t'' => t -> t' -> t''
hOr = undefined
\end{spec}

\noindent
The functional dependency |t t' -> t''| expresses that the parameters |t| and |t'|
uniquely determine the parameter |t''|.
This implies that once |t| and |t'| are instantiated,
the instance of |t''| must be uniquely inferable by the type-system,
and that thus we are defining a type-level function from |t| and |t'| to |t''|.
The type-level function itself is defined by the following non-overlapping instance declarations:

\begin{spec}
instance HOr  HFalse  HFalse  HFalse
instance HOr  HTrue   HFalse  HTrue
instance HOr  HFalse  HTrue   HTrue
instance HOr  HTrue   HTrue   HTrue
\end{spec}

\noindent If we write |(hOr hTrue hFalse)|,
we know that |t| and |t'| are |HTrue| and |HFalse|, respectively.
So, the second instance is chosen to select |hOr| from and thus |t''| is inferred to be |HTrue|.

%% Despite the fact that is looks like a computation at the value level,
%% its actual purpose is to express a computation at the type-level;
%% no interesting value level computation is taking place at all.

%% If we had defined |HTrue| and |HFalse| in the following way:

%% < data HTrue   = HTrue   ; hTrue  = HTrue   :: Htrue
%% < data HFalse  = HFalse  ; hFalse = HFalse  :: HFalse

%% \noindent then the same computation would also be performed at the value level,
%% resulting in the value  |HTrue| of type |HTrue|.



\subsection{Heterogeneous Lists}
Heterogeneous lists are represented with the data types |HNil| and |HCons|,
which model the structure of a normal list both at the value and type level:

\begin{code}
data HNil       = HNil
data HCons e l  = HCons e l
\end{code}

The sequence |HCons True (HCons "bla" HNil)| is a correct heterogeneous list
with type |HCons Bool (HCons String HNil)|.
%% Since we want to prevent that an expression |HCons True False| represents a correct heterogeneous list
%% (the second |HCons| argument is not a type-level list)
%% we introduce the classes |HList| and its instances,
%% and express express this constraint by adding a context condition to the |HCons ...| instance:

%% \begin{code}
%% class     HList l
%% instance  HList HNil
%% instance  HList l => HList (HCons e l)
%% \end{code}

%% The main reason for introducing the class |HExtend| is to make it possible
%% to encode constraints on the things which can be |HCons|-ed;
%% here we have expressed that the second parameter should be a list again.
%% In the next subsection we will see how to make use of this facility.

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

%if style==newcode
\begin{code}
infixr 2 .*.
infixr 4 .=.
\end{code}
%endif

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

The class |HasField| is used to retrieve the value part
corresponding to a specific label from a record:

\begin{code}
class HasField l r v | l r -> v where
    hLookupByLabel :: l -> r -> v
\end{code}

\noindent At the type-level it is statically checked that the record |r| indeed has
a field with label |l| associated with a value of the type |v|.
At value-level the member |hLookupByLabel| returns the value of type |v|.
So, the following expression returns the string |"bla"|:

< hLookupByLabel label2 myR

The instances for |HasField| are

\begin{code}
instance
    (HasFieldList l r v)
    => HasField l (Record r) v where
    hLookupByLabel l (Record r) =
        hListLookupByLabel l r

class HasFieldList l r v | l r -> v where
    hListLookupByLabel :: l -> r -> v

instance
    (  HEq l l' b
    ,  HasFieldList' b l (HCons (LVPair l' v') r) v)
    => HasFieldList l (HCons (LVPair l' v') r) v where
    hListLookupByLabel l r@(HCons f' _) =
        hListLookupByLabel' (hEq l (labelLVPair f')) l r

class HasFieldList' b l r v | b l r -> v where
    hListLookupByLabel':: b -> l -> r -> v

instance
    HasFieldList' HTrue l (HCons (LVPair l v) r) v where
    hListLookupByLabel' _ _ (HCons (LVPair v) _) =
        v

instance
    HasFieldList l r v =>
    HasFieldList' HFalse l (HCons fld r) v where
    hListLookupByLabel' _ l (HCons _ r) =
        hListLookupByLabel l r
\end{code}

\noindent
While only |HCons| may have the field we are looking for,
we need to consider two cases.
The head of the list may be the correct field,
or the field may be present further along the list.
We need to assert |HEq l l' b| and delegate to another type-function (|HasFieldList'|)
so that the two cases are disambiguated in an instance head.
Haskell won't disambiguate two instances based on the instance context.

GHC is smart enough to elide the dictionary objects and indirect jumps for |hLookupByLabel|.
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

\begin{code}
{-# NOINLINE squares #-}
squares =
    Record             $
    HCons (l1 .=. 1)   $
    HCons (l2 .=. 4)   $
    HCons (l3 .=. 9)   $
    HCons (l4 .=. 16)  $
    HNil
sq3 = hLookupByLabel l3 squares
\end{code}
\marcos{Sacar\'ia el pragma ... y lo escribir\'ia |squares = l1 .=. 1 .*. l2 .=. 4 .*. l3 .=. 9 .*. l4 .=. 16 .*. emptyRecord|}
%% $

\begin{spec}
Paper.sq3 =
  case Paper.squares of _ { Paper.HCons ds_d15d r_aYq ->
  case r_aYq of _ { Paper.HCons f'_aYD ds1_d15l ->
  case ds1_d15l of _ { Paper.HCons f'1_X10B ds2_X17k ->
  f'1_X10B
\end{spec}
\marcos{Cambiar\'ia los nombres para que quede m\'as legible el c\'odigo. Ej: |case squares of _ { HCons x1 x1s -> case x1s of ... |}

%% The possibility to update an element in a record at a given label position is provided by:

%% < class HUpdateAtLabel l v r r' | l v r -> r' where
%% <   hUpdateAtLabel :: l -> v -> r -> r'

%% In order to keep our programs readable we introduce infix operators for some of the previous functions:

%% \begin{code}
%% infixr 2 .*.
%% infixr 4 .=.
%% infixr 9 #
%% (.*.) ::  HExtend e l l' => e -> l -> l'
%% (.*.) =   hExtend
%% r  #    l  =  hLookupByLabel l r
%% \end{code}

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
|HasField| already looks for our label at compile-time
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
A skew list is a linked list spine of complete binary trees
with elements in both
leaves and internal nodes.


\subsection{SkewRecord}

\begin{code}
newtype  HLeaf  e         =  HLeaf  e
data     HNode  e  t  t'  =  HNode  e  t  t'
\end{code}

\noindent
The element precedes the subtrees in |HNode|
so all elements in expressions read in order left to right.
The following declarations define a list with elements 1..5:

\begin{code}
onefive =
    HCons (HLeaf 1) $
    HCons (HLeaf 2) $
    HCons (HNode 3 (HLeaf 4) (HLeaf 5)) $
    HNil
\end{code}

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
Else, we just insert a |HLeaf| of the new element.

We define a new tag |SkewRecord|
and the corresponding |HExtend| instance
to be able to use |(.*.)|.
|HSkewRecord| does the actual work.
\begin{code}
newtype SkewRecord r = SkewRecord r
emptySkewRecord :: SkewRecord HNil
emptySkewRecord = SkewRecord HNil

instance
    (HSkewExtend e l l') =>
    HExtend e (SkewRecord l) (SkewRecord l') where
    e .*. SkewRecord l =
        SkewRecord (hSkewExtend e l)
\end{code}

|HComplete| below checks that all root to leaf paths have the same length
and returns it.
\begin{code}
class HComplete t h | t -> h
instance HComplete (HLeaf e) HZero
instance
    (HComplete t h, HComplete t' h) =>
    HComplete (HNode e t t') (HSucc h)
\end{code}

|HSkewCarry|, named as
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

class HSkewExtend e l l' | e l -> l'
    where hSkewExtend :: e -> l -> l'
instance
    (HSkewCarry l b
    ,HSkewExtend' b e l l') =>
    HSkewExtend e l l' where
    hSkewExtend e l = hSkewExtend' (hSkewCarry l) e l


class HSkewExtend' b e l l' where
    hSkewExtend' :: b -> e -> l -> l'
instance
    HSkewExtend'
        HFalse
        e
        l
        (HCons (HLeaf e) l) where
    hSkewExtend' _ e l = HCons (HLeaf e) l

instance
    HSkewExtend'
        HTrue
        e
        (HCons t (HCons t' l))
        (HCons (HNode e t t') l) where
    hSkewExtend' _ e (HCons t (HCons t' l)) =
        (HCons (HNode e t t') l)
\end{code}

\noindent
The |HBalanced| type-function returns the height of the given tree.
While we are at it, we check that the tree is complete.
The three cases for |HSkewExtend| handle
the empty, singleton, and length 2+ lists, respectively.
For the latter, only when the first two lists are the same size,
as indicated by |HBalanced|,
we insert a new |HNode|.
In all other cases, we use |HLeaf|.

The missing piece is |HasField| for |SkewRecord|.
As already mentioned,
we explore all paths at compile-time
but follow only the right one at runtime.

\begin{code}
instance
    HasFieldSkew l ts v =>
    HasField l (SkewRecord ts) v where
    hLookupByLabel l (SkewRecord ts) =
        hSkewLookupByLabel l ts

class HasFieldSkew l ts v | l ts -> v where
    hSkewLookupByLabel :: l -> ts -> v
instance
    (HasFieldB l t bt
    ,HasFieldB l ts bts
    ,HasFieldCons bt bts l t ts v) =>
    HasFieldSkew l (HCons t ts) v where
    hSkewLookupByLabel l (HCons t ts) =
        hConsLookupByLabel (hasField l t) (hasField l ts) l t ts
instance
    HasFieldSkew l (HLeaf (LVPair l v)) v where
    hSkewLookupByLabel l (HLeaf (LVPair v)) = v
instance
    (HEq l l' bl'
    ,HasFieldB l t bt
    ,HasFieldB l t' bt'
    ,HasFieldNode bl' bt bt' l v' t t' v)
    => HasFieldSkew l (HNode (LVPair l' v') t t') v where
    hSkewLookupByLabel l (HNode f@(LVPair v') t t') =
        hNodeLookupByLabel
        (hEq l (labelLVPair f))
        (hasField l t)
        (hasField l t')
        l
        v'
        t
        t'

class HasFieldB l r b | l r -> b where
instance HasFieldB l HNil HFalse
instance (HasFieldB l t bt, HasFieldB l ts bts, HOr bt bts b)
    => HasFieldB l (HCons t ts) b
instance HEq l l' b => HasFieldB l (HLeaf (LVPair l' v)) b
instance
    (HEq l l' bl
    ,HasFieldB l l1 b1
    ,HasFieldB l l2 b2
    ,HOr bl b1 bl1
    ,HOr bl1 b2 bl12)
    => HasFieldB l (HNode (LVPair l' v) l1 l2) bl12
hasField :: HasFieldB l r b => l -> r -> b
hasField = undefined


class HasFieldCons bt bts l t ts v where
    hConsLookupByLabel :: bt -> bts -> l -> t -> ts -> v
instance
    HasFieldSkew l t v =>
    HasFieldCons HTrue bts l t ts v where
    hConsLookupByLabel _ _ l t ts = hSkewLookupByLabel l t
instance
    HasFieldSkew l ts v =>
    HasFieldCons HFalse HTrue l t ts v where
    hConsLookupByLabel _ _ l t ts =
        hSkewLookupByLabel l ts

class HasFieldNode be bt bt' l e t t' v where
    hNodeLookupByLabel :: be -> bt -> bt' -> l -> e -> t -> t' -> v
instance
    HasFieldNode HTrue bt bt' l (LVPair l v) t t' v where
    hNodeLookupByLabel _ _ _ l e t t' = valueLVPair e
instance
    HasFieldSkew l t v =>
    HasFieldNode HFalse HTrue bt' l e t t' v where
    hNodeLookupByLabel _ _ _ l e t t' =
        hSkewLookupByLabel l t
instance
    HasFieldSkew l t' v =>
    HasFieldNode HFalse HFalse HTrue l e t t' v where
    hNodeLookupByLabel _ _ _ l e t t' =
        hSkewLookupByLabel l t'
\end{code}

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
