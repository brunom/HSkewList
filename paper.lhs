\documentclass[natbib]{sigplanconf}
%\usepackage{pdfsync}
\usepackage{color}
\usepackage{amsmath}
\usepackage{tikz}
%\usepackage{pgflibraryarrows}
\usetikzlibrary{arrows}
%\newcommand{\todo}[1]{}
\newcommand{\todo}[1]{%\error                uncomment to make sure there are no todos left
 \textcolor{blue}{\mbox{$^\ast$}}\marginpar{\raggedright
 \hspace{0pt}\sffamily\tiny{\sc \textcolor{blue}{todo:}}\\ \textcolor{blue}{#1}}}
\newcommand{\wouter}[1]{\textcolor{red}{\textbf{Wouter:}#1}}
\newcommand{\doaitse}[1]{\textcolor{red}{\textbf{Doaitse:}#1}}
\newcommand{\marcos}[1]{\textcolor{red}{\textbf{Marcos:}#1}}
\renewcommand{\wouter}[1]{}
\renewcommand{\doaitse}[1]{}
\renewcommand{\marcos}[1]{}
%let paper = True

%include lhs2TeX.fmt
%include polycode.fmt

%format forall = "\forall"
%format exists = "\exists"

%format ^         = " "
%format ^^        = "\;"
%format DATA      = "\mathbf{DATA}"
%format ATTR      = "\mathbf{ATTR}"
%format INH       = "\mathbf{INH}"
%format SYN       = "\mathbf{SYN}"
%format SEM       = "\mathbf{SEM}"
%format lhs       = "\mathbf{lhs}"
%format .         = "."
%format (A(n)(f)) = @ n . f

%format ~         = "\mathbin{\;\sim\!}"
%format .*.       = "\mathbin{.\!\!*\!\!.}"
%format .=.       = "\mathbin{.\!\!=\!\!.}"
%format .+.       = "\mathbf{\;\oplus\;}"

%format bl_        = "\{"
%format el_        = "\}"

%format br_        = "\{\!\{"
%format er_        = "\}\!\}"
 

%format inherit_      = "\mathbf{inherit}"
%format synthesize_   = "\mathbf{synthesize}"
%format copy_         = "\mathbf{copy}"
%format define_       = "\mathbf{define}"
%format use_          = "\mathbf{use}"
%format compute      = "\mathbf{compute}"
%format from         = "\mathbf{from}"
%format select       = "\mathbf{select}"
%format at_           = "\mathbf{\;at\;}"

%format epsilon    = "\epsilon"
%format alpha      = "\alpha"
%format beta       = "\beta"
%format alph_1     = "\alpha_{1}" 
%format alph_n     = "\alpha_{n}" 

%format sem_Root_     = sem_Root
%format sem_Tree_     = sem_Tree
%format asp_smin_     = asp_smin
%format asp_ival_     = asp_ival
%format asp_sres_     = asp_sres

%% Template Haskell quotation
%format TH(a)      = $ ( a )
%format (THQ (a))  = "`" a
%format (THQQ (a)) = "``" a


%if style==code

\begin{code}

{-# OPTIONS -XMultiParamTypeClasses -XFunctionalDependencies 
            -XFlexibleContexts -XFlexibleInstances 
            -XUndecidableInstances 
            -XExistentialQuantification 
            -XEmptyDataDecls 
            -XNoMonomorphismRestriction#-}

module FCAG where

import Data.HList hiding ((.+.))

import Data.HList.FakePrelude
import Data.HList.TypeEqGeneric1


instance TypeCast HFalse HFalse where typeCast = id
instance TypeCast HTrue  HTrue  where typeCast = id
instance TypeEq x y b => HEq (Proxy x) (Proxy y) b

\end{code}

%endif



%\setlength{\parindent}{0in}

\begin{document}

\conferenceinfo{ICFP'09,} {August 31--September 2, 2009, Edinburgh, Scotland, UK.}
\CopyrightYear{2009}
\copyrightdata{978-1-60558-332-7/09/08}

\titlebanner{submitted to ICFP 2009}         % These are ignored unless
\preprintfooter{version of Mar 2, 2009}     % 'preprint' option specified.

\title{Attribute Grammars Fly First-Class}
\subtitle{How to do Aspect Oriented Programming in Haskell}

\authorinfo{Marcos Viera}
           {Instituto de Computaci\'{o}n \\ Universidad de la  Rep\'{u}blica\\ Montevideo, Uruguay}
           {mviera@@fing.edu.uy}
\authorinfo{S. Doaitse Swierstra}
           {Department of Computer Science\\ Utrecht University\\ Utrecht, The Netherlands}
           {doaitse@@cs.uu.nl}
\authorinfo{Wouter Swierstra}
           {Chalmers University of Technology\\G\"oteborg, Sweden}
           {wouter@@chalmers.se}
\maketitle

\begin{abstract}
Attribute Grammars (AGs),  a general-purpose
formalism for describing recursive computations over data types,  avoid the trade-off which arises when building software incrementally: should it be easy to add new data types and data type  alternatives or to add new operations on existing data types? 
However, AGs are usually implemented as a pre-processor, leaving e.g. type checking to later processing phases and making interactive development, proper error reporting and 
debugging difficult. Embedding AG into Haskell as a combinator library
solves these problems. 

Previous attempts at embedding AGs as a 
domain-specific language were based on extensible records and thus exploiting Haskell's type system to check the well-formedness of the AG, but fell short in compactness and the possibility to abstract over oft occurring AG patterns. Other attempts used a very generic mapping for which the AG well-formedness could not be statically checked.

We present a typed embedding of AG in Haskell satisfying all
these requirements. The key lies in using HList-like typed
heterogeneous collections (extensible polymorphic records) and
expressing AG well-formedness conditions as type-level predicates
(i.e., type-class constraints). By further type-level programming we can also express common programming patterns, corresponding to the typical use cases of monads such as |Reader|, |Writer| and |State|. The paper presents a realistic
example of type-class-based type-level programming in Haskell.

% Attribute grammar programs are factored along two axes: the function axis and the data axis. As a consequence they can be easily adapted and extended. The questions arises whether the attribute grammar approach to programming can be made available as an embedded domain specific language (EDSL) in Haskell. In this paper we show how this can be achieved by making extensive use of the type class mechanism. The paper identifies the characteristic properties of attribute grammars, shows how to emulate them, and shows how to overcome some problems associated with earlier attempts to do so. Finally we show how common patterns found in attribute gammar programming can be made explicit, leading to extensions of our EDSL.
\end{abstract}

\category{D.3.3}{Programming languages}{Language Constructs and Features}
\category{D.1.1}{Programming techniques}{Applicative (Functional) Programming}
\terms Design, Languages, Performance, Standardization 

\keywords
Attribute Grammars, Class system, Lazy evaluation, Type-level programming, Haskell, HList


\section{Introduction} \label{sec:intro}
Functional programs can be easily extended by defining extra functions. If however a data type is extended with a new alternative, each parameter position and each case expression where a value of this type is matched has to be inspected and modified accordingly. In object oriented programing the situation is reversed: if we implement the alternatives of a data type by sub-classing, it is easy to add a new alternative by defining a new subclass in which we define a method for each part of desired global functionality. If however we want to define a new function for a data type, we have to inspect all the existing subclasses and add a method describing the local contribution to the global computation over this data type. This problem was first noted by Reynolds \cite{ReynoldsExpression} and later referred to as  ``the expression problem'' by Wadler \cite{WadlerExpressionProblem}. We start out by showing how the use of AGs overcomes this problem.

% Thus far one of the solutions to this problem has been the use of attribute grammars; whereas early attribute grammar systems favoured a more functional view, the newer systems really allow one to choose along which axis to structure one's program and even switching structuring axis within a single program poses no problems. The question to be answered in this paper is whether, and if so how and with which limitations, we can bring this way of structuring your program text to the Haskell world.


%\end{figure}
 
As running example we use the classic |repmin| function \cite{BirdRepMin}; it takes a tree argument, and returns a tree of similar shape, in which the leaf values are replaced by the minimal value of the leaves in the original tree (see  Figure \ref{fig:tree}). 
The program was originally introduced to describe so-called circular programs, i.e. programs in which part of a result of a function is again used as one of its arguments. We will use this example to show that the computation is composed of three so-called \emph{aspects}: the computation of the minimal value as the first component of the result of |sem_Tree| (|asp_smin|), passing down the globally minimal value from the root to the leaves as the parameter |ival| (|asp_ival|), and the construction of the resulting tree as the second component of the result (|asp_sres|). 

\begin{figure}
\begin{code}
data Root  =  Root Tree
data Tree  =  Node Tree Tree
           |  Leaf Int
 
repmin = sem_Root 

sem_Root_ ( Root tree )
     =             let  ( smin,  sres )  = (sem_Tree_ tree  )  smin
                   in   ( sres  )

sem_Tree_ ( Node l r )
     =  \ ival ->  let  ( lmin,  lres )  = (sem_Tree_ l     )  ival
                        ( rmin,  rres )  = (sem_Tree_ r     )  ival
                   in   ( lmin `min` rmin  , Node lres rres   )
sem_Tree_ (Leaf i) 
     =  \ ival ->       (i, Leaf ival)

\end{code}
\begin{center}
\begin{tabular}{c c c}
\begin{tikzpicture}[level distance=6mm]
\tikzstyle{every node}=[circle,inner sep=1pt,minimum size=4mm,draw]
\tikzstyle{level 2}=[sibling distance=15mm]
\tikzstyle{level 3}=[sibling distance=10mm]
\tikzstyle{level 4}=[sibling distance=5mm,
set style={{every node}+=[rectangle]}]
\node {}
child {node {}
child {node {}
child {node {}
child {node {5}}
child {node {1}}
}
child {node {}
child {node {2}}
child {node {8}}
}
}
child {node {}
child {node[rectangle] {4}}
child {node {}
child {node {3}}
child {node {6}}
}
}
};
\end{tikzpicture}


&

\raisebox{10ex}{$\Rightarrow$}

&

\begin{tikzpicture}[level distance=6mm]
\tikzstyle{every node}=[circle,inner sep=1pt,minimum size=4mm,draw]
\tikzstyle{level 2}=[sibling distance=15mm]
\tikzstyle{level 3}=[sibling distance=10mm]
\tikzstyle{level 4}=[sibling distance=5mm,
set style={{every node}+=[rectangle]}]
\node {}
child {node {}
child {node {}
child {node {}
child {node {1}}
child {node {1}}
}
child {node {}
child {node {1}}
child {node {1}}
}
}
child {node {}
child {node[rectangle] {1}}
child {node {}
child {node {1}}
child {node {1}}
}
}
};
\end{tikzpicture}


\end{tabular}
\end{center}
\caption{|repmin| replaces leaf values by their minimal value\label{fig:tree}}
\end{figure}


% As we can see from the code this is the case at the |Root| level: the computed minimal value |smin| is passed to the function |sem_Tree tree| and passed down to its recursive calls, so it can be used in the construction of the |Leave|-s of the resulting tree, which is returned in |sres|.

Now suppose we want to change the function |repmin| into a function |repavg| which replaces the leaves by the average value of the leaves. Unfortunately we have to change almost every line of the program, because instead of computing the minimal value we have to compute both the sum of the leaf values and the total number of leaves. At the root level we can then divide the total sum by the total number of leaves to compute the average leaf value. However, the traversal of the tree, the passing of the value to be used in constructing the new leafs and the construction of the new tree all remain unchanged. 
What we are now looking for is a way to define the function |repmin| as:

\begin{code}
repmin = sem_Root (asp_smin .+. asp_ival .+. asp_sres)
\end{code}
so we can easily replace the aspect |asp_smin| by |asp_savg|:
\begin{code}
repavg = sem_Root  (asp_savg .+. asp_ival .+. asp_sres)
\end{code}

In Figure \ref{fig:AG} we have expressed the solution of the |repmin| problem in terms of a domain specific language, i.e., as an attribute grammar \cite{SwieAzSar98Braga}. Attributes are values associated with tree nodes. We will refer to a collection of (one or more) related attributes, with their defining rules, as an aspect. After defining the underlying data types by a few |DATA| definitions, we define the different aspects: for the two ``result'' aspects we introduce synthesized attributes (|SYN smin| and |SYN sres|), and for the ``parameter'' aspect we introduce an inherited attribute (|INH ival|).

Note that attributes are introduced separately, and that for each attribute/alternative pair we have a separate piece of code describing what to compute in a |SEM| rule; the defining expressions at the right hand side of the |=|-signs are all written in Haskell, using minimal syntactic extensions to refer to attribute values (the identifiers starting with a |@|). These expressions are copied directly into the generated program: only the attribute references are replaced by references to values defined in the generated program. The attribute grammar system only checks whether for all attributes a definition has been given. Type checking of the defining expressions is left to the Haskell compiler when compiling the generated program (given in Figure \ref{fig:tree}). 

\begin{figure}[t]
< DATA Root  | Root tree
< DATA Tree  | Node l, r : Tree
<            | Leaf i : {Int}
< 
< 
< SYN  Tree  [ smin : Int ]
< SEM  Tree  
<      | Leaf  lhs   .  smin  = @i
<      | Node  lhs   .  smin  = A l smin  `min`   A r smin
< 
< 
< INH  Tree [ ival : Int ]
< SEM  Root 
<      | Root  tree  .  ival  = A tree smin
< SEM  Tree 
<      | Node  l     .  ival  = A lhs ival
<              r     .  ival  = A lhs ival
< 
< 
< SYN  Root Tree  [ sres : Tree ] 
< SEM  Root
<      | Root  lhs   .  sres  = A tree sres
< SEM  Tree 
<      | Leaf  lhs   .  sres  = Leaf  (A lhs ival)
<      | Node  lhs   .  sres  = Node (A l sres)  (A r sres)
\caption{AG specification of repmin} \label{fig:AG}
\end{figure}


As a consequence type errors are reported in terms of the generated program.
Although this works reasonably well in practice, the question arises whether we we can define a set of combinators which enables us to embed the  AG formalism directly in Haskell, thus making the separate generation step uncalled for and immediately profiting from Haskell's type checker and getting error messages referring to the original source code.

A first approach to such an embedded attribute grammar notation was made by de Moor et al. \cite{MPW00}. Unfortunately this approach, which is based on extensible records \cite{Gaster96apolymorphic}, necessitates the introduction of a large set of combinators, which encode positions of children-trees explicitly. Furthermore combinators are indexed by a number which indicates the number of children a node has where the combinator is to be applied. The \emph{first contribution} of this paper is that we show how to overcome this shortcoming by making use of the Haskell class system. 


The \emph{second contribution} is that we show how to express the previous solution in terms of heterogeneous collections, thus avoiding the use of Hugs-style extensible records are not supported by the main Haskell compilers.


Attribute grammars exhibit typical design patterns; an example of such a pattern is the inherited attribute |ival|, which is distributed to all the children of a node, and so on recursively. Other examples are attributes which thread a value through the tree, or collect information from all the children which have a specific attribute and combine this into a synthesized attribute of the father node. In normal Haskell programming this would be done by introducing a collection of monads (|Reader|, |State| and |Writer| monad respectively), and by using monad transformers to combine these in to a single monadic computation. Unfortunately this approach breaks down once too many attributes have to be dealt with, when the data flows backwards, and especially if we have a non-uniform grammar, i.e., a grammar which has several different non-terminals each with a different collection of attributes. In the latter case a single monad will no longer be sufficient.

One way of making such computational patterns first-class is by going to a universal representation for all the attributes, and packing and unpacking them whenever we need to perform a computation.  In this way all attributes have the same type at the attribute grammar level, and non-terminals can now be seen as functions which map dictionaries to dictionaries, where such dictionaries are tables mapping |String|s representing attribute names to universal attribute values  \cite{MBS00}. Although this provides us with a powerful mechanism for describing attribute flows by Haskell functions, this comes at a huge price; all attributes have to be unpacked before the contents can be accesses, and to be repacked before they can be passed on. Worse still, the check that verifies that all attributes are completely defined, is no longer a static check, but rather something which is implicitly done at run-time by the evaluator, as a side-effect of looking up attributes in the dictionaries. The \emph{third contribution} of this paper is that we show how patterns corresponding to the mentioned monadic constructs can be described, again using the Haskell class mechanism. 

The \emph{fourth contribution} of this paper is that it presents yet another large example of how to do type-level programming in Haskell, and what can be achieved with it. In our conclusions we will come back to this. 

Before going into the technical details we want to give an impression of what our embedded Domain Specific Language (DSL) looks like. In  Figure \ref{fig:aspects} we give our definition of the |repmin| problem in a lightly sugared notation.

\begin{figure}
< data Root  =  Root { tree :: Tree}
<            deriving Show
< data Tree  =  Node {l::Tree, r::Tree}
<            |  Leaf {i::Int}
<            deriving Show

< $(deriveAG ''Root)
< $(attLabels ["smin","ival","sres"])

< asp_smin_ =  synthesize_   smin          at_  {Tree} 
<              use_          min   0       at_  {Node}
<              define_  at_  Leaf = i
<
< asp_ival_ =  inherit_      ival          at_  {Tree}
<              copy_    at_  {Node}
<              define_  at_  Root.tree = tree.smin 
< 
< asp_sres_ =  synthesize_   sres            at_  {Root, Tree}
<              use_          Node  (Leaf 0)  at_  {Node}
<              define_  at_  Root = tree.sres
<                            Leaf = Leaf lhs.ival

< asp_repmin =  asp_smin .+. asp_sres .+. asp_ival
< repmin t = select sres from compute asp_repmin t

\caption{|repmin| in our embedded DSL\label{fig:aspects}}
\end{figure}

To completely implement the |repmin| function the user of our library\footnote{Available as \emph{AspectAG} in Hackage.} needs to undertake the following steps (Figure \ref{fig:aspects}):

\begin{itemize}
\item define the Haskell data types involved;
\item optionally, generate some boiler-plate code using calls to Template Haskell;
\item define the aspects, by specifying whether the attribute is inherited or synthesized, with which non-terminals it is associated, how to compute its value if no explicit definition is given (i.e., which computational pattern it follows), and providing definitions for the attribute at the various data type constructors (productions in grammar terms) for which it needs to be defined, resulting in |asp_repmin|;
\item composing the aspects into a single large aspect |asp_repmin| 
\item define the function |repmin| that takes a |tree|, executes the semantic function
for the tree and the aspect |asp_repmin|, and selects the synthesized
attribute |sres| from the result.
\end{itemize}



Together these rules define for each of the productions a so-called Data Dependency Graph (DDG). A DDG is basically a data-flow graph (Figure \ref{fig:DDG}), with as incoming values has the inherited attributes of the father node and the synthesized attributes of the children nodes (indicated by closed arrows), and as outputs the inherited attributes of the children nodes and the synthesized attributes of the father node (open arrows). The semantics of our DSL is defined as the data-flow graph which results from composing all the DDGs corresponding to the individual nodes of the abstract syntax tree. Note that the semantics of a tree is thus represented by a function which maps the inherited attributes of the root node onto its synthesized attributes.

\begin{figure}[h]
\begin{center}
\begin{tikzpicture}[level distance=9mm]

\begin{scope}
\tikzstyle{every node}=[circle,inner sep=1pt,minimum size=4mm,draw]
\tikzstyle{level 1}=[sibling distance=28mm]
\node[] {p}
child {node {l}}
child {node {r}};
\end{scope}


\begin{scope}[>=open triangle 45, thick, font=\footnotesize]
\draw[->] (0.5, -0.2) -- (0.5,  0.2);
\draw (0.5, 0.15)  node[right]  {|lmin `min` rmin|};
\draw (0.5,-0.15)  node[right]  {|Node lres rres|};

\draw[->] (-1.9,-0.7) -- (-1.9,-1.1);
\draw (-1.9,-0.8)  node[left]   {|ival|};

\draw[->] (0.9, -0.7) -- (0.9, -1.1);
\draw (0.9, -0.8)  node[left]   {|ival|};
\end{scope}


\begin{scope}[>=triangle 45, thick, font=\footnotesize]
\draw[<-] (-0.5,-0.2) -- (-0.5, 0.2);
\draw (-0.5,0.15)  node[left]  {|ival|};

\draw[<-] (-0.9,-0.7) -- (-0.9,-1.1);
\draw (-0.9,-0.7)  node[right] {|lmin|};
\draw (-0.9,-1.0)  node[right] {|lres|};

\draw[<-] (1.9, -0.7) -- (1.9,-1.1);
\draw (1.9, -0.7)  node[right] {|rmin|};
\draw (1.9, -1.0)  node[right] {|rres|};

\end{scope}

\begin{scope}[fill=white, minimum size=5mm, minimum width=10mm, rectangle, font=\footnotesize]
\draw node[xshift=-14mm, yshift=-14mm, draw]{|sem_Tree|};
\draw node[xshift= 14mm, yshift=-14mm, draw]{|sem_Tree|};
\end{scope}

\end{tikzpicture}
\end{center}
\caption{The DDG for |Node|}\label{fig:DDG}
\end{figure}

The main result of this paper is a combinator based implementation of attribute grammars in Haskell; it has statically type checked semantic functions, it is statically checked for correctness at the attribute grammar level, and high-level attribute evaluation patterns can be described.

In Section \ref{sec:hlist} we introduce the heterogeneous collections, which are used to combine a collection of inherited or synthesised attributes into a single value. In Section \ref{sec:rules} we show how individual attribute grammar rules are represented. In Section \ref{sec:aspects} we introduce the aforementioned |.+.| operator which combines the aspects. In Section \ref{sec:semfunc} we introduce a function |knit| which takes the DDG associated with the production used at the root of a tree and  the mappings (|sem_...| functions) from inherited to synthesised attributes for its children (i.e. the data flow over the children trees) and out of this constructs a data flow computation over the combined tree. In Section \ref{sec:patterns} we show how the common patterns can be encoded in our library, and in Section \ref{sec:defaspects} we show how default aspects can be defined. In Section \ref{sec:related} we discuss related work, and in Section \ref{sec:conclusions} we conclude.



\wouter{Hasn't related work already been discussed?}

\doaitse{maybe we should say something about the OO world? to make Phil Wadler happy?}

\section{HList}\label{sec:hlist}

The library HList  \cite{KLS04} implements typeful heterogeneous collections (lists, records, ...),
using techniques for dependently typed programming in Haskell \cite{Hall01,McB02}
which in turn make use of Haskell 98 extensions for multi-parameter classes \cite{PJM97} and functional dependencies \cite{Jon00}.
The idea of \emph{type-level programming} is based on the use of types to represent type-level values, and classes to represent type-level types and functions. 

In order to be self-contained we start out with a small introduction.
To represent Boolean values at the type level we define a new type for each of the Boolean values. The class |HBool| represents the type-level type of Booleans. We may read the instance definitions as ``the type-level values |HTrue| and |HFalse| have the type-level type |HBool|'': 
\begin{code}
class     HBool  x 

data HTrue    ; hTrue   = undefined :: HTrue
data HFalse   ; hFalse  = undefined :: HFalse

instance  HBool  HTrue
instance  HBool  HFalse
\end{code}

Since we are only interested in type-level computation, 
we defined |HTrue| and |HFalse| as empty types. 
By defining an inhabitant for each value we can, by writing expressions at the value level, construct values at the type-level by referring to the types of such expressions.

Multi-parameter classes can be used to describe type-level \emph{relations}, whereas functional dependencies restrict such relations to functions.
As an example we define the class |HOr| for type-level disjunction:

\begin{code}
class (HBool t, HBool t', HBool t'') 
    => HOr t t' t'' | t t' -> t''
  where hOr :: t -> t' -> t''
\end{code}

\noindent The context |(HBool t, HBool t', HBool t'')| expresses that the types
|t|, |t'| and |t''| have to be type-level values of the type-level type |HBool|.
The functional dependency |t t' -> t''| expresses that the parameters |t| and |t'| 
uniquely determine the parameter |t''|. This implies that once |t| and |t'| are instantiated, the instance of |t''| must be uniquely inferable by the type-system, and that thus we are defining a type-level function from |t| and |t'| to |t''|. 
The type-level function itself is defined by the following non-overlapping instance declarations:

\begin{code}
instance HOr  HFalse  HFalse  HFalse
  where hOr _ _ = hFalse

instance HOr  HTrue   HFalse  HTrue
  where hOr _ _ = hTrue

instance HOr  HFalse  HTrue   HTrue
  where hOr _ _ = hTrue

instance HOr  HTrue   HTrue   HTrue
  where hOr _ _ = hTrue
\end{code}

\noindent If we write |(hOr hTrue hFalse)|, 
we know that |t| and |t'| are |HTrue| and |HFalse|, respectively. 
So, the second instance is chosen to select |hOr| from and thus |t''| is inferred to be |HTrue|. 

Despite the fact that is looks like a computation at the value level, its actual purpose is to express a computation at the type-level; no interesting value level computation is taking place at all. 
If we had defined |HTrue| and |HFalse| in the following way:

< data HTrue   = HTrue   ; hTrue  = HTrue   :: Htrue
< data HFalse  = HFalse  ; hFalse = HFalse  :: HFalse
 
\noindent then the same computation would also be performed at the value level,
resulting in the value  |HTrue| of type |HTrue|.



\subsection{Heterogeneous Lists}
Heterogeneous lists are represented with the data types |HNil| and |HCons|,
which model the structure of a normal list both at the value and type level:

< data HNil = HNil
< data HCons e l = HCons e l

The sequence |HCons True (HCons "bla" HNil)| is a correct heterogeneous list 
with type |HCons Bool (HCons String HNil)|.
Since we want to prevent that an expression |HCons True False| represents a correct heterogeneous list (the second |HCons| argument is not a type-level list) we introduce the classes |HList| and its instances,and express express this constraint by adding a context condition to the |HCons ...| instance:

< class HList l
< instance HList HNil
< instance HList l => HList (HCons e l)

The library includes a multi-parameter class |HExtend| to model the extension of heterogeneous collections.

< class HExtend e l l' | e l -> l', l' -> e l
<  where hExtend :: e -> l -> l'

The functional dependency |e l -> l'| makes that |HExtend| is a type-level function, instead of a relation: once |e| and |l| are fixed |l'| is uniquely determined.
It fixes the type |l'| of a collection, resulting from extending a collection of type |l| with an element of type |e|.
The member |hExtend| performs the same computation at the level of values.
The instance of |HExtend| for heterogeneous lists includes the well-formedness condition:

< instance HList l => HExtend e l (HCons e l)
<  where hExtend = HCons

The main reason for introducing the class |HExtend| is to make it possible to encode constraints on the things which can be |HCons|-ed; here we have expressed that the second parameter should be a list again. In the next subsection we will see how to make use of this facility.


\subsection{Extensible Records}

In our code we will make heavy use of non-homogeneous collections: grammars are a collection of productions, and nodes have a collection of attributes and a collection of children nodes. Such collections, which can be extended and shrunk, map typed labels to values and are modeled by an |HList| containing a heterogeneous list of fields, marked with the data type |Record|. We will refer to them as records from now on:

< newtype Record r = Record r

\noindent An empty record is a |Record| containing an empty heterogeneous list:

< emptyRecord :: Record HNil
< emptyRecord = Record HNil 
 
\noindent A field with label |l| (a phantom type \cite{Hin03}) and value of type |v| is represented by the type:

< newtype LVPair l v = LVPair { valueLVPair :: v }

\noindent Labels are now almost first-class objects, and can be used as type-level values. 
We can retrieve the label value using the function |labelLVPair|, which exposes the phantom type parameter:

< labelLVPair :: LVPair l v -> l
< labelLVPair = undefined

\noindent Since we need to represent many labels, we introduce a polymorphic type |Proxy| to represent them; by choosing a different phantom type for each label to be represented we can distinguish them: 

< data Proxy e ; proxy = undefined :: Proxy e

\noindent Thus, the following declarations define a record (|myR|) with two elements, labelled by |Label1| and |Label2|:

< data Label1; label1 = proxy :: Proxy Label1
< data Label2; label2 = proxy :: Proxy Label2
<
< field1 = LVPair True   :: LVPair (Proxy Label1) Bool
< field2 = LVPair "bla"  :: LVPair (Proxy Label2) [Char]
<
< myR = Record  (HCons field1 (HCons field2 HNil )

\noindent Since our lists will represent collections of attributes we want to express statically that we do not have more than a single definition for each attribute occurrence, and so the labels in a record should be all different.  
This constraint is represented by requiring an instance of the class |HRLabelSet| to be available when defining extendability for records:

< instance HRLabelSet (HCons (LVPair l v) r)
<     => HExtend  (LVPair l v) (Record r) 
<                 (Record (HCons (LVPair l v) r))
<   where hExtend f (Record r) = Record (HCons f r)


The class |HasField| is used to retrieve the value part corresponding to a specific label from a record:

< class HasField l r v | l r -> v where
<   hLookupByLabel :: l -> r -> v

\noindent At the type-level it is statically checked that the record |r| indeed has
a field with label |l| associated with a value of the type |v|. 
At value-level the member |hLookupByLabel| returns the value of type |v|.
So, the following expression returns the string |"bla"|:

< hLookupByLabel label2 myR


The possibility to update an element in a record at a given label position is provided by:

< class HUpdateAtLabel l v r r' | l v r -> r' where
<   hUpdateAtLabel :: l -> v -> r -> r'



In order to keep our programs readable we introduce infix operators for some of the previous functions:

< (.*.)      =  hExtend
< _  .=.  v  =  LVPair v
< r  #    l  =  hLookupByLabel l r

Furthermore we will use the following syntactic sugar to denote lists and records in the rest of the paper:
\begin{itemize}
\item |bl_ v1, ..., vn el_| for |(v1 .*. ... .*. vn .*. HNil)| 
\item |br_ v1, ..., vn er_| for |(v1 .*. ... .*. vn .*. emptyRecord)| 
\end{itemize}

\noindent So, for example the definition of  |myR| can now be written as:
 
< myR = br_ label1 .=. True, label2 .=. "bla" er_

% In the rest of this paper records will be used to store a collection of inherited or synthesized attributes. In the next section we will see how we can build rule descriptions on top of such records.

\section{Rules\label{sec:rules}}

In this subsection we show how attributes and their defining rules are represented.
An \emph{attribution} is a finite mapping from attribute names to attribute values, represented as a |Record|, in which each field represents the name and value of an attribute.

\begin{code}

type Att att val = LVPair att val 
\end{code}

\noindent The labels\footnote{These and all needed labels can be generated automatically by Template Haskell functions available in the library} 
(attribute names) for the attributes of the example are:

\begin{code}
data Att_smin;   smin    = proxy::Proxy Att_smin
data Att_ival;   ival    = proxy::Proxy Att_ival
data Att_sres;   sres    = proxy::Proxy Att_sres
\end{code}

When inspecting what happens at a production we see that information flows 
from the inherited attribute of the parent and the synthesized attributes 
of the children (henceforth called in the |input| family) to the synthesized attributes 
of the parent and the inherited attributes of the children (together called the |output| family from now on). 
Both the input and the output attribute family is represented by an instance of:

\begin{code}
data Fam c p = Fam c p
\end{code}

\noindent A |Fam| contains a single attribution for the parent and a collection of attributions  for the children.
Thus the type |p| will always be a |Record| with fields of type |Att|, 
and the type |c| a |Record| with fields of the type:

\begin{code}

type Chi ch atts = LVPair ch atts
\end{code}

\noindent where |ch| is a label that represents the name of that child 
and |atts| is again a |Record| with the fields of type |Att| associated with this particular child.
In our example the |Root| production has a single child |Ch_tree| of type |Tree|, the |Node| production has two children labelled by |Ch_l| and |Ch_r| of type |Tree|, and the |Leaf| production has a single child called |Ch_i| of type |Int|. Thus we generate, using template Haskell:

\begin{code}

data Ch_tree;   ch_tree    = proxy::Proxy (Ch_tree,Tree)
data Ch_r;      ch_r       = proxy::Proxy (Ch_r,Tree)
data Ch_l;      ch_l       = proxy::Proxy (Ch_l,Tree)
data Ch_i;      ch_i       = proxy::Proxy (Ch_i,Int)
\end{code}

\noindent Note that we encode both the name and the type of the child in
the type representing the label.

\begin{figure}[htp]
\begin{center}
\begin{tabular}{c c}

\begin{tikzpicture}[level distance=7mm]

\begin{scope}
\tikzstyle{every node}=[circle,inner sep=1pt,minimum size=4mm,draw]
\tikzstyle{level 1}=[sibling distance=18mm]

\node[] {p}
child {node {l}}
child {node {r}};
\end{scope}

\begin{scope}[>=triangle 45, thick, font=\footnotesize]
\draw[->] (-0.4,0.2) -- (-0.4,-0.2);
\draw (-0.4,0) node[left] {ival};

\draw[->] (-0.5,-0.9) -- (-0.5,-0.5);
\draw (-0.5,-0.6) node[right] {smin};
\draw (-0.5,-0.9) node[right] {sres};

\draw[->] (1.3,-0.9) -- (1.3,-0.5);
\draw (1.3,-0.6) node[right] {smin};
\draw (1.3,-0.9) node[right] {sres};
\end{scope}

\end{tikzpicture}

&

\begin{tikzpicture}[level distance=7mm]

\begin{scope}
\tikzstyle{every node}=[circle,inner sep=1pt,minimum size=4mm,draw]
\tikzstyle{level 1}=[sibling distance=18mm]

\node[] {p}
child {node {l}}
child {node {r}};
\end{scope}

\begin{scope}[>=open triangle 45, thick, font=\footnotesize]
\draw[->] (0.4, -0.2) -- (0.4,  0.2);
\draw (0.4,  0.2) node[right] {smin};
\draw (0.4, -0.1) node[right] {sres};

\draw[->] (-1.3,-0.5) -- (-1.3,-0.9);
\draw (-1.3,-0.7) node[left]  {ival};

\draw[->] (0.5, -0.5) -- (0.5, -0.9);
\draw (0.5, -0.7) node[left]  {ival};
\end{scope}

\end{tikzpicture}

\end{tabular}
\end{center}

\caption{Repmin's input and output families for Node} \label{fig:inout}
\end{figure}


\noindent Families are used to model the input and output attributes of attribute computations. For example, 
Figure \ref{fig:inout} shows the input (black arrows) and output (white arrows) attribute
families of the repmin problem for the production Node. We now give the attributions associated with the output family of the |Node| production, which are the synthesized attributes of the parent (|SP|) and the inherited attributions for the left and right child (|IL| and |IR|): 

< type SP  = Record (  HCons  (Att  (Proxy Att_smin)  Int)
<                      HCons  (Att  (Proxy Att_sres)  Tree)
<                      HNil)
<
< type IL  = Record (  HCons  (Att  (Proxy Att_ival)  Int)
<                      HNil)
<
< type IR  = Record (  HCons  (Att  (Proxy Att_ival)  Int)
<                      HNil)
<

The next type collects the last two children attributions into a single record: 
< type IC  = Record (  HCons  (Chi  (Proxy (Ch_l, Tree)  IL)
<                      HCons  (Chi  (Proxy (Ch_r, Tree)  IR)
<                      HNil)
<

We now have all the ingredients to define the output family for |Node|-s.

< type Output_Node = Fam IC SP

Attribute computations are defined in terms of \emph{rules}. As defined by \cite{MBS00}, a
rule is a mapping from an input family to an output family. In order to make rules composable
we define a rule as a mapping from input attributes to a function which extends a family of output
attributes with the new elements defined by this rule:

\begin{code}
type Rule sc ip ic sp ic' sp' 
         = Fam sc ip -> Fam ic sp -> Fam ic' sp'
\end{code}

Thus, the type |Rule| states that a rule takes as input the synthesized attributes 
of the children |sc| and the inherited attributes of the parent |ip| and returns
a function from the output constructed thus far (inherited attributes of the children |ic| 
and synthesized attributes of the parent |sp|) to the extended output.   

The composition of two rules is the composition of the two functions after  applying each of them to the input family first:
\begin{code}
ext :: Rule sc ip ic' sp' ic'' sp'' -> Rule sc ip ic sp ic' sp'
    -> Rule sc ip ic sp ic'' sp''  
(f `ext` g) input = f input . g input
\end{code}

\subsection{Rule Definition}

We now introduce the functions |syndef| and |inhdef|, which are used  to define 
primitive rules which define a synthesized or an inherited attribute respectively. Figure \ref{fig:rulesrepmin} lists
all the rule definitions for our running example. The naming convention is such that a rule with name |prod_att| defines the
attribute |att| for the production |prod|. 
Without trying to completely understand the definitions we suggest the reader to
compare them with their respective |SEM| specifications in Figure \ref{fig:AG}. 


\begin{figure}[htp]
\begin{code}

leaf_smin  (Fam chi par)  
      = syndef  smin  (chi  # ch_i )
node_smin  (Fam chi par)  
      = syndef  smin  (  ((chi  # ch_l)   #  smin) 
                              `min`
                         ((chi  # ch_r)   #  smin) ) 

root_ival  (Fam chi par)  
      = inhdef  ival  bl_ nt_Tree el_ 
                      br_  ch_tree 
                           .=.  (chi # ch_tree) # smin er_ 
node_ival  (Fam chi par)  
      = inhdef  ival  bl_ nt_Tree el_ 
                      br_  ch_l .=.  par  #  ival
                      ,    ch_r .=.  par  #  ival er_
 
root_sres  (Fam chi par)  
     =  syndef  sres  ((chi # ch_tree) # sres)
leaf_sres  (Fam chi par)  
     =  syndef  sres  (Leaf  (par # ival))
node_sres  (Fam chi par)  
     =  syndef  sres  (Node  ((chi # ch_l)  #  sres)
                             ((chi # ch_r)  #  sres) )

\end{code}
\caption{Rule definitions for repmin} \label{fig:rulesrepmin}
\end{figure}

The function |syndef| adds the definition of a synthesized attribute. 
It takes a label |att| representing the name of the new attribute, 
a value |val| to be assigned to this attribute, and it builds a function which 
updates the output constructed thus far. 

\begin{code}

syndef  ::  HExtend (Att att val) sp sp'
        =>  att -> val -> (Fam ic sp -> Fam ic sp')
syndef  att val (Fam ic sp) = Fam  ic (att .=. val .*. sp)
\end{code}

\noindent The record |sp| with the synthesized attributes of the parent
is extended with a field with name |att| and value |val|, 
as shown in Figure \ref{fig:syndef}. 
If we look at the type of the function, the check that we have not already defined this attribute is done by the constraint |HExtend (Att att val) sp sp'|, meaning that |sp'| 
is the result of adding the field |(Att att val)| to |sp|, 
which cannot have any field with name |att|. 
Thus we are statically preventing duplicated attribute definitions.
  

\begin{figure}[htp]

\begin{tabular} {c c c}
\begin{tikzpicture}[level distance=7mm]

\begin{scope}
\tikzstyle{every node}=[circle,inner sep=1pt,minimum size=4mm,draw]
\tikzstyle{level 1}=[sibling distance=18mm]

\node[] {p}
child {node {c$_1$}}
child {node {c$_n$}};
\end{scope}

\begin{scope}[>=open triangle 45, thick, font=\footnotesize]
\draw[->] (0.4, -0.2) -- (0.4,  0.2);
\draw (0.4,  0.1) node[right] {sp};

\draw[->] (-1.3,-0.5) -- (-1.3,-0.9);
\draw (-1.3,-0.7) node[left]  {ic$_1$};

\draw[->] (0.5, -0.5) -- (0.5, -0.9);
\draw (0.5, -0.7) node[left]  {ic$_n$};
\end{scope}

\draw (-0.4, -0.7) node {\textbf{. . .}};
\end{tikzpicture}

&

\raisebox{4ex}{\begin{tikzpicture}
\draw[->,line width=2pt] (-0.2,0) -- (0.2,-0);
\end{tikzpicture}}

&

\begin{tikzpicture}[level distance=7mm]

\begin{scope}
\tikzstyle{every node}=[circle,inner sep=1pt,minimum size=4mm,draw]
\tikzstyle{level 1}=[sibling distance=18mm]

\node[] {p}
child {node {c$_1$}}
child {node {c$_n$}};
\end{scope}

\begin{scope}[>=open triangle 45, thick, font=\footnotesize]
\draw[->] (0.4, -0.2) -- (0.4,  0.2);
\draw (0.4,  0.1) node[right] {sp,};
\draw (0.4, -0.1) node[right] {att = val};

\draw[->] (-1.3,-0.5) -- (-1.3,-0.9);
\draw (-1.3,-0.7) node[left]  {ic$_1$};

\draw[->] (0.5, -0.5) -- (0.5, -0.9);
\draw (0.5, -0.7) node[left]  {ic$_n$};
\end{scope}

\draw (-0.4, -0.7) node {\textbf{. . .}};
\end{tikzpicture}
\end{tabular}

\caption{Synthesized attribute definition} \label{fig:syndef}
\end{figure}

\noindent Let us take a look at the rule definition |node_smin| of the attribute |smin| 
for the production |Node| in Figure \ref{fig:rulesrepmin}.
The children |ch_l| and |ch_r| are retrieved from the input family so we can subsequently retrieve the attribute |smin| from these attributions,
and construct the computation of the synthesized attribute |smin|. This process is demonstrated in Figure \ref{fig:node_smin}.
The attribute |smin| is required (underlined) in the children |l| and |r| of the input, and the parent of the output
is extended with |smin|.


\begin{figure}[htp]

\begin{center}
\begin{tabular} {c c}

\begin{tikzpicture}[level distance=7mm]

\begin{scope}
\tikzstyle{every node}=[circle,inner sep=1pt,minimum size=4mm,draw]
\tikzstyle{level 1}=[sibling distance=18mm]

\node[] {p}
child {node {l}}
child {node {r}};
\end{scope}

\begin{scope}[>=triangle 45, thick, font=\footnotesize]
\draw[->] (-0.4,0.2) -- (-0.4,-0.2);
\draw (-0.4,0) node[left] {ival};

\draw[->] (-0.5,-0.9) -- (-0.5,-0.5);
\draw (-0.5,-0.6) node[right] {sres};
\draw (-0.5,-0.9) node[right] {\underline{smin}};

\draw[->] (1.3,-0.9) -- (1.3,-0.5);
\draw (1.3,-0.6) node[right] {sres};
\draw (1.3,-0.9) node[right] {\underline{smin}};
\end{scope}

\end{tikzpicture}

&

\raisebox{4ex}{\begin{tikzpicture}
\draw[->,line width=2pt] (-0.2,0) -- (0.2,-0);
\end{tikzpicture}}

\end{tabular}



\begin{tabular} {c c c}
\begin{tikzpicture}[level distance=7mm]

\begin{scope}
\tikzstyle{every node}=[circle,inner sep=1pt,minimum size=4mm,draw]
\tikzstyle{level 1}=[sibling distance=18mm]

\node[] {p}
child {node {l}}
child {node {r}};
\end{scope}

\begin{scope}[>=open triangle 45, thick, font=\footnotesize]
\draw[->] (0.4, -0.2) -- (0.4,  0.2);
\draw (0.4,  0.1) node[right] {...};

\draw[->] (-1.3,-0.5) -- (-1.3,-0.9);
\draw (-1.3,-0.7) node[left]  {...};

\draw[->] (0.5, -0.5) -- (0.5, -0.9);
\draw (0.5, -0.7) node[left]  {...};
\end{scope}

\end{tikzpicture}

&

\raisebox{4ex}{\begin{tikzpicture}
\draw[->,line width=2pt] (-0.2,0) -- (0.2,-0);
\end{tikzpicture}}

&

\begin{tikzpicture}[level distance=7mm]

\begin{scope}
\tikzstyle{every node}=[circle,inner sep=1pt,minimum size=4mm,draw]
\tikzstyle{level 1}=[sibling distance=18mm]

\node[] {p}
child {node {l}}
child {node {r}};
\end{scope}

\begin{scope}[>=open triangle 45, thick, font=\footnotesize]
\draw[->] (0.4, -0.2) -- (0.4,  0.2);
\draw (0.4,  0.1) node[right] {...,};
\draw (0.4, -0.1) node[right] {smin};

\draw[->] (-1.3,-0.5) -- (-1.3,-0.9);
\draw (-1.3,-0.7) node[left]  {...};

\draw[->] (0.5, -0.5) -- (0.5, -0.9);
\draw (0.5, -0.7) node[left]  {...};
\end{scope}

\end{tikzpicture}
\end{tabular}
\end{center}

\caption{Rule |node_sres|} \label{fig:node_smin}
\end{figure}


\noindent If we take a look at the type which is inferred for |node_sres| we find back all the constraints which are normally checked by an off-line attribute grammar system, i.e., an attribute |smin| is made available by each child and an attribute |smin| can be safely added to the current synthesized attribution of the parent:
\footnote{In order to keep the explanation simple we will suppose that |min| is not overloaded, and takes |Int|'s as parameter.}

< node_sres   ::  (  HasField (Proxy (Ch_l, Tree)) sc scl 
<                 ,  HasField (Proxy Att_smin) scl Int
<                 ,  HasField (Proxy (Ch_r, Tree)) sc scr
<                 ,  HasField (Proxy Att_smin) scr Int
<                 ,  HExtend  (Att (Proxy Att_smin) Int) 
<                             sp sp') 
<             =>  Rule sc ip ic sp ic sp'

% \noindent It is statically checked that:
% \begin{itemize}
% \item the input includes the children (|Ch_l|,|Tree|) and (|Ch_r|,|Tree|) \wouter{you may want to explain what you mean with this point}
% \item both children have the attribute |Att_smin| with type |Int| defined into their attributions (|scl| and |scr|) 
% \item the attribute |Att_smin| was not defined before in the output
% \end{itemize}

The function |inhdef| introduces a new inherited attribute for a collection of non-terminals. It takes the following parameters:
\begin{description}
\item[|att|] the attribute which is being defined;
\item[|nts|] the non-terminals with which this attribute is being associated;
\item[|vals|] a record labelled with child names and containing values, describing how to compute the attribute being defined at each of the applicable child  positions.
\end{description}


\noindent The parameter |nts| takes over the role of the |INH| declaration in Figure \ref{fig:AG}. Here this extra parameter seems to be superfluous, since its value can be inferred, but adds an additional restriction to be checked (yielding to better errors) and it will be used in the introduction of default rules later.
The names for the non-terminals of our example are:

> nt_Root = proxy::Proxy Root
> nt_Tree = proxy::Proxy Tree


The result of |inhdef| again is a function which updates the output constructed thus far. 

\begin{code}

inhdef  ::  Defs att nts vals ic ic' 
        =>  att -> nts -> vals -> (Fam ic sp -> Fam ic' sp)
inhdef att nts vals (Fam ic sp) = 
        Fam (defs att nts vals ic) sp
\end{code}

\noindent The class |Def| is defined by induction over the record |vals| containing the new definitions. The function |defs| inserts each definition into the attribution of the corresponding child. 

\begin{code}

class Defs att nts vals ic ic'  | vals ic -> ic' where
  defs :: att -> nts -> vals -> ic -> ic'
\end{code}

\noindent We start out with the base case, where we have no more definitions to add. In this case 
the inherited attributes of the children are returned unchanged.

\begin{code}

instance Defs att nts (Record HNil) ic ic where
  defs _ _ _ ic = ic
\end{code}

\noindent The instance for |HCons| given below first recursively processes the rest of the definitions
by updating the collection of collections of inherited attributes of the children |ic| into |ic'|. 
A helper type level function |SingleDef| (and its corresponding value level function |singledef|) 
is used to incorporate the single definition (|pch|) into |ic'|, resulting in a new set |ic''|. \noindent The type level functions |HasLabel| and |HMember| are used to statically check 
whether the child being defined (|lch|) exists in |ic'| and if its type (|t|) belongs to the
non-terminals |nts|, respectively. The result of both functions are |HBool|s 
(either |HTrue| or |HFalse|) which are passed as parameters to |SingleDef|. We are now ready to give the definition for the non-empty case:
  
\begin{code}

instance  ( Defs att nts (Record vs) ic ic'
          , HasLabel (Proxy (lch,t)) ic' mch
          , HMember (Proxy t) nts mnts
          , SingleDef  mch mnts att 
                  (Chi (Proxy (lch,t)) vch) 
                  ic' ic'' ) 
      => Defs  att nts 
               (Record (HCons  (Chi (Proxy (lch,t)) vch) vs)) 
               ic ic'' 
      where 
  defs att nts ~(Record (HCons pch vs)) ic = 
         singledef mch mnts att pch ic'  
         where  ic'     = defs att nts (Record vs) ic
                lch     = labelLVPair pch
                mch     = hasLabel lch ic'
                mnts    = hMember (sndProxy lch) nts
                
\end{code}

The class |Haslabel| can be encoded straightforwardly, together with a function which retrieves part of a phantom type:

\begin{code}

class HBool b => HasLabel l r b | l r -> b
instance HasLabel l r b => HasLabel l (Record r) b
instance (HEq l lp b, HasLabel l r b', HOr b b' b'') 
   => HasLabel l (HCons (LVPair lp vp) r) b''
instance HasLabel l HNil HFalse

hasLabel :: HasLabel l r b => l -> r -> b
hasLabel = undefined

sndProxy :: Proxy (a,b) -> Proxy b
sndProxy _ = undefined
\end{code}


We only show the instance with both |mch| and |mnts| equal to |HTrue|, 
which is the case we expect to apply in a correct attribute grammar definition: we do not refer to children which do not exist, and this child has the type we expect.\footnote{The instances for error cases could just be left undefined,
yielding to ``undefined instance'' type errors. 
In our library we use a class |Fail| (as defined in \cite{KLS04}, section 6) in order to get more instructive type error messages.}


\begin{code}

class  SingleDef mch mnts att pv ic ic' 
       | mch mnts pv ic -> ic' 
  where singledef :: mch -> mnts -> att -> pv -> ic -> ic'

instance  ( HasField lch ic och
          , HExtend (Att att vch) och och'
          , HUpdateAtLabel lch och' ic ic') 
      => SingleDef  HTrue HTrue att (Chi lch vch) ic ic' 
  where singledef _ _ att pch ic = 
           hUpdateAtLabel lch (att .=. vch .*. och) ic  
           where  lch  = labelLVPair  pch
                  vch  = valueLVPair  pch
                  och  = hLookupByLabel lch ic 
\end{code}


\noindent We will guarantee that the collection of attributions |ic| (inherited attributes of the children) contains an attribution |och| for the child |lch|, and so we can use |hUpdateAtlabel| to extend the attribution for this child with a field |(Att att vch)|, thus binding attribute |att| to value |vch|.  The type system checks, thanks to the presence of |HExtend|,  that the attribute |att| was not defined before in |och|.


\section{Aspects\label{sec:aspects}}

We represent aspects as records which contain for each production a rule field. 

\begin{code}

type Prd prd rule = LVPair prd rule
\end{code}
For our example we thus introduce fresh labels to refer to repmin's productions:

\begin{code}

data P_Root;   p_Root    = proxy::Proxy P_Root
data P_Node;   p_Node    = proxy::Proxy P_Node
data P_Leaf;   p_Leaf    = proxy::Proxy P_Leaf
\end{code}

\noindent We now can define the aspects of repmin as records with the rules of Figure \ref{fig:rulesrepmin}.\footnote{We assume that the monomorphism restriction has been switched off.}

\begin{code}

asp_smin_     =  br_  p_Leaf  .=.  leaf_smin 
                 ,    p_Node  .=.  node_smin er_

asp_ival_     =  br_  p_Root  .=.  root_ival 
                 ,    p_Node  .=.  node_ival er_

asp_sres_     =  br_  p_Root  .=.  root_sres 
                 ,    p_Node  .=.  node_sres
                 ,    p_Leaf  .=.  leaf_sres er_
\end{code}

\subsection{Aspects Combination}

We define the class |Com| which will provide the instances we need for combining aspects:

\begin{code}

class  Com r r' r'' | r r' -> r''
  where (.+.) :: r -> r' -> r''

\end{code}


\noindent With this operator we can now combine the three aspects which together make up the repmin problem: 

\begin{code}

asp_repmin = asp_smin .+. asp_ival .+. asp_sres 

\end{code}


\noindent Combination of aspects is a sort of union of records 
where, in case of fields with the same label (i.e., for rules for the same production),
the rule combination (|ext|) is applied to the values. 
To perform the union we iterate over the second record, inserting 
the next element into the first one if it is new and combining it with an existing entry if it exists:

\begin{code}

instance Com r (Record HNil) r
  where   r .+. _ = r

instance  (  HasLabel lprd r b
          ,  ComSingle b (Prd lprd rprd) r r'''
          ,  Com r''' (Record r') r'')
        => Com  r (Record (HCons (Prd lprd rprd) r')) r''
  where
   r .+. (Record (HCons prd r')) = r''
    where  b       = hasLabel (labelLVPair prd) r
           r'''    = comsingle b prd r
           r''     = r''' .+. (Record r')
\end{code}

\noindent We use the class |ComSingle| to insert a single element into
the first record.  The type-level Boolean parameter |b| is used to distinguish those cases where the left hand operand already contains a field for the rule to be added and the case where it is new. \footnote{This parameter
  can be avoided by allowing overlapping instances, but we prefer to
  minimize the number of Haskell extensions we use.} 

\begin{code}

class  ComSingle b f r r' | b f r -> r'
  where comsingle :: b -> f -> r -> r'
\end{code}

\noindent If the first record has a field with the same label |lprd|, 
we update its value by composing the rules.

\begin{code}

instance  (  HasField  lprd  r (Rule sc ip ic' sp' ic'' sp'')
          ,  HUpdateAtLabel  lprd (Rule  sc    ip 
                                         ic    sp 
                                         ic''  sp'') 
                             r r')
         => ComSingle   HTrue (Prd lprd (Rule sc ip ic sp ic' sp')) 
                        r r'
   where 
    comsingle _ f r  = hUpdateAtLabel n ((r # n) `ext` v) r
     where  n  = labelLVPair f
            v  = valueLVPair f
\end{code}

\noindent In case the first record does not have a field with the label, 
we just insert the element in the record.

\begin{code}

instance ComSingle  HFalse f (Record r) 
                    (Record (HCons f r))
   where comsingle _ f (Record r) = Record (HCons f r)
\end{code}


\section{Semantic Functions\label{sec:semfunc}}


Our overall goal is to construct a |Tree|-algebra and a |Root|-algebra. 
For the domain associated with each non-terminal we take the function 
mapping its inherited to its synthesized attributes. The hard work is done by the function |knit|, the purpose of which is to combine the data flow defined by the DDG --which was constructed by combining all the rules for this production-- with the semantic functions 
of the children (describing the flow of data from their inherited to their synthesized attributes) into the semantic function for the parent.

With the attribute computations as first-class entities, 
we can now pass them as an argument to functions of the form |sem_<nt>|. The following code follows the definitions of the data types at hand: it contains recursive calls for all children of an alternative, each of which results in a mapping from inherited to synthesized attributes for that child followed by a call to knit, which stitches everything together:  

\begin{code}

sem_Root asp (Root t  )  
        = knit (asp # p_Root)  br_  ch_tree .=. sem_Tree asp t er_
sem_Tree asp (Node l r)   
        = knit (asp # p_Node)  br_  ch_l .=. sem_Tree asp l 
                               ,    ch_r .=. sem_Tree asp r er_
sem_Tree asp (Leaf i  )   
        = knit (asp # p_Leaf)  br_  ch_i .=. sem_Lit i er_
\end{code}

\begin{code}

sem_Lit e (Record HNil) = e

\end{code}

\noindent Since this code is completely generic we provide a 
Template Haskell function |deriveAG| which
automatically generates the functions such as |sem_Root| and |sem_Tree|, together with the labels for the non-terminals and labels for referring to children.
Thus, to completely implement the |repmin| function we need to undertake the following steps:

\begin{itemize}
\item Generate the semantic functions and the corresponding labels by using:  
< TH(deriveAG (THQQ(Root)))
\item Define and compose the aspects as shown in the previous sections, resulting in |asp_repmin|.
\item Define the function |repmin| that takes a |tree|, executes the semantic function
for the tree and the aspect |asp_repmin|, and selects the synthesized
attribute |sres| from the result.

\begin{code}

repmin tree 
  = sem_Root asp_repmin (Root tree) () # sres

\end{code}

\end{itemize}



\subsection{The Knit Function}

As said before the function |knit| takes the combined rules for a node and the semantic functions of the children, and builds a
function from the inherited attributes of the parent to its
synthesized attributes. We start out by constructing an empty output family, containing an empty attribution for each child and one for the parent. To each of these attributions we apply the corresponding part of the rules, which will construct the inherited attributes of the children and the synthesized attributes of the parent (together forming the output family). Rules however contain references to the input family, which is composed of the inherited attributes
of the parent |ip| and the synthesized attributes of the children |sc|.

\begin{code}

knit  ::  (  Empties fc ec, Kn fc ic sc ) 
      =>  Rule sc ip ec (Record HNil) ic sp
          -> fc -> ip -> sp
knit  rule fc ip =  
  let  ec            = empties fc 
       (Fam ic sp)   = rule  (Fam sc  ip) 
                             (Fam ec  emptyRecord)
       sc            = kn fc ic
  in   sp

\end{code}

The function |kn|, which takes the semantic functions of the children (|fc|) 
and their inputs (|ic|), computes the results for the children (|sc|). 
The functional dependency |fc -> ic sc| indicates that |fc| determines
|ic| and |sc|, so the shape of the record with the semantic functions
determines the shape of the other records: 

\begin{code}

class Kn fc ic sc  | fc  -> ic sc where  
  kn :: fc -> ic -> sc
\end{code}

\noindent We declare a helper instance of |Kn| to remove the |Record| tags of the parameters, 
in order to be able to iterate over their lists without having to tag and untag at 
each step:   

\begin{code}

instance Kn fc ic sc 
 => Kn (Record fc) (Record ic) (Record sc) where
  kn (Record fc) (Record ic) = Record $ kn fc ic
\end{code}

\noindent When the list of children is empty, we just return an empty list
of results.

\begin{code}

instance Kn HNil HNil HNil where
  kn _ _ = hNil
\end{code}

\noindent The function |kn| is a type level |zipWith ($)|, 
which applies the functions contained in the first argument list to the corresponding element in the second argument list. 

%$
\begin{code}

instance Kn fcr icr scr
         => Kn    (HCons (Chi lch (ich->sch))     fcr) 
                  (HCons (Chi lch ich)            icr) 
                  (HCons (Chi lch sch)            scr) 
         where
  kn  ~(HCons pfch fcr) ~(HCons pich icr) = 
    let  scr        = kn fcr icr
         lch        = labelLVPair  pfch
         fch        = valueLVPair  pfch
         ich        = valueLVPair  pich
    in   HCons  (newLVPair lch (fch ich)) scr

\end{code}

The class |Empties| is used to construct the record, with an empty attribution for each child, 
which we have used to initialize the computation of the input attributes with.

\begin{code}
class Empties fc ec | fc -> ec where
  empties :: fc -> ec
\end{code}

\noindent In the same way that |fc| determines the shape of |ic| and |sc| in |Kn|, it also
tells us how many empty attributions |ec| to produce and in which order:

\begin{code}
instance Empties fc ec 
    => Empties (Record fc) (Record ec) where 
  empties (Record fc) = Record $ empties fc

instance Empties fcr ecr
    => Empties  (HCons (Chi lch fch)            fcr) 
                (HCons (Chi lch (Record HNil))  ecr) 
     where
  empties  ~(HCons pch fcr)  = 
    let  ecr     = empties fcr
         lch     = labelLVPair  pch
    in   HCons  (newLVPair lch emptyRecord) ecr 

instance Empties HNil HNil where
  empties _ = hNil
\end{code}
%$
\section{Common Patterns\label{sec:patterns}}

At this point all the basic functionality of attribute grammars has been implemented. In practice however we want more. If we look at the code in Figure \ref{fig:AG} we see that the rules for |ival| at the production |Node| are ``free of semantics'', since the value is copied unmodified to its children. If we were dealing with a tree with three children instead of two the extra line would look quite similar. When programming attribute grammars such patterns are quite common and most attribute grammar systems contain implicit rules which automatically insert such ``trivial'' rules. As a result descriptions can decrease in size dramatically. The question now arises whether we can extend our embedded language to incorporate such more high level data flow patterns.   

\subsection{Copy Rule}

The most common pattern is the copying of  an inherited attribute 
from the parent to all its children.  
We capture this pattern with the an operator |copy|, 
which takes the name of an attribute |att| and 
an heterogeneous list of non-terminals |nts| for which the attribute has to be defined,
and generates a copy rule for this. This corresponds closely to the introduction of a |Reader| monad.

\begin{code}

copy  ::  (Copy att nts vp ic ic', HasField att ip vp) 
      =>  att -> nts -> Rule sc ip ic sp ic' sp
\end{code}

\noindent Thus, for example, the rule |node_ival| of Figure \ref{fig:rulesrepmin} 
can now be written as:  

< node_ival  input  = copy ival bl_ nt_Tree el_ input

The function |copy| uses a function |defcp| to define the attribute |att|
as an inherited attribute of its children. 
This function is similar in some sense to |inhdef|, 
but instead of taking a record containing the new definitions it gets
the value |vp| of the attribute which is to be copied to the children:


\begin{code}

copy att nts (Fam _ ip) = defcp att nts (ip # att)

defcp  ::  Copy att nts vp ic ic' 
       =>  att -> nts -> vp -> (Fam ic sp -> Fam ic' sp)
defcp att nts vp (Fam ic sp)  = 
        Fam (cpychi att nts vp ic) sp
\end{code}

\noindent The class |Copy| iterates over the record |ic| 
containing the output attribution of the children, and inserts the
attribute |att| with value |vp| if the type of the child is
included in the list |nts| of non-terminals and the attribute is not
already defined for this child.

\begin{code}

class  Copy att nts vp ic ic' | ic -> ic' where
  cpychi  ::  att -> nts -> vp -> ic -> ic'
\end{code}


\begin{code}

instance Copy att nts vp (Record HNil) (Record HNil) 
  where cpychi  _ _ _ _ = emptyRecord

instance  ( Copy att nts vp (Record ics) ics'
          , HMember (Proxy t) nts mnts
          , HasLabel att vch mvch 
          , Copy'  mnts mvch att vp 
                   (Chi (Proxy (lch, t)) vch)  
                   pch
          , HExtend pch ics' ic) 
      => Copy  att nts vp 
               (Record (HCons (Chi (Proxy (lch, t)) vch) ics))
               ic
      where 
  cpychi att nts vp (Record (HCons pch ics)) = 
            cpychi' mnts mvch att vp pch .*. ics'
           where  ics'  = cpychi att nts vp (Record ics) 
                  lch   = sndProxy (labelLVPair pch)
                  vch   = valueLVPair pch
                  mnts  = hMember lch nts
                  mvch  = hasLabel att vch
\end{code}

\noindent The function |cpychi'| updates the field |pch|
by adding the new attribute: 

\begin{code}

class  Copy' mnts mvch att vp pch pch'  
       | mnts mvch pch -> pch' 
  where
   cpychi'  ::  mnts -> mvch -> att -> vp -> pch -> pch'
\end{code}

\noindent When the type of the child 
doesn't belong to the non-terminals for which the attribute is defined we define an instance which leaves the field |pch| unchanged.

\begin{code}

instance Copy' HFalse mvch att vp pch pch where 
  cpychi' _ _ _ _ pch = pch
\end{code}

\noindent We also leave |pch| unchanged if the attribute is already defined for this child.

\begin{code}

instance Copy' HTrue HTrue att vp pch pch where 
  cpychi' _ _ _ _ pch = pch
\end{code}
 
\noindent In other case 
the attribution |vch| is extended with the attribute |(Att att vp)|.

\begin{code}

instance HExtend (Att att vp) vch vch' 
    => Copy' HTrue HFalse att vp  (Chi lch vch) 
                           (Chi lch vch') where
  cpychi' _ _ att vp pch = lch .=. (att .=. vp .*. vch) 
            where  lch  = labelLVPair pch
                   vch  = valueLVPair pch
\end{code}


\subsection{Other Rules}
In this section we introduce two more constructs of our DSL, without giving their implementation. Besides the |Reader| monad, there is also the |Writer| monad. Often we want to collect information provided by some of the children into an attribute of the parent. This can be used to e.g. collect all identifiers contained in an expression. Such a 
synthesized attribute can be declared using the |use| rule,
which combines the attribute values of the children in similar way
as Haskell's |foldr1|. 
The |use| rule takes the following arguments: the attribute to be defined, 
the list of non-terminals for which the attribute is defined,
a monoidal operator which combines the attribute values, 
and a unit value to be used in those cases where none of the children has such an attribute. 

\begin{code}

use  ::  (Use att nts a sc, HExtend (Att att a) sp sp') 
     =>  att -> nts -> (a -> a -> a) -> a 
         -> Rule sc ip ic sp ic sp'
\end{code}

\noindent Using this new combinator the rule |node_smin| of Figure \ref{fig:rulesrepmin} 
becomes:

< node_smin = use smin bl_ nt_Tree el_ min 0  


A third common pattern corresponds to the use of the |State| monad. A value is threaded in a depth-first way through the tree, being updated every now and then. For this we have chained attributes (both inherited and synthesized). If a definition for a synthesized attribute of the parent with this name is missing we look for the right-most child with a synthesized attribute of this name. If we are missing a definition for one of the children, we look for the right-most of its left siblings which can provide such a value, and if we cannot find it there, we look at the inherited attributes of the father.  


\begin{code}

chain  ::  (  Chain att nts val sc ic sp ic' sp' 
           ,  HasField att ip val )
      => att -> nts -> Rule sc ip ic sp ic' sp'
\end{code}



\section{Defining Aspects\label{sec:defaspects}}
Now we have both implicit rules to define attributes, and explicit rules which contain explicit definitions, we may want to combine these into a single \emph{attribute aspect} which contains all the definitions for single attribute. We now refer to 
Figure \ref{fig:asp_repmin} which is a desugared version of the notation presented in the introduction.

\begin{figure*}[htp] 
\begin{code}

asp_smin      = synAspect  smin  bl_ nt_Tree el_                                                                       -- synthesize at 
                                 min 0 bl_ p_Node el_                                                                  -- use at
                                 br_  p_Leaf .=. (\(Fam chi _) -> chi # ch_i) er_                                      -- define at

asp_ival      = inhAspect  ival  bl_ nt_Tree el_                                                                       -- inherit
                                 bl_ p_Node el_                                                                        -- copy at
                                 br_  p_Root .=. (\(Fam chi _) -> br_  ch_tree .=. (chi # ch_tree) # smin er_ ) er_    -- define at
                                                              
asp_sres      = synAspect  sres  bl_ nt_Root, nt_Tree el_                                                              -- synthesize at
                                 Node (Leaf 0) bl_ p_Node el_                                                          -- use at
                                 br_  p_Root .=. (\(Fam  chi  _    )  -> (chi # ch_tree) # sres)                       -- define at
                                 ,    p_Leaf .=. (\(Fam  _    par  )  -> Leaf (par # ival)) er_
\end{code}
\caption{Aspects definition for repmin}\label{fig:asp_repmin}
\end{figure*}

An inherited attribute aspect, like |asp_ival| in Figure \ref{fig:asp_repmin}, 
can be defined using the function |inhAspect|. 
It takes as arguments: the name of the attribute |att|, 
the list |nts| of non-terminals where the attribute is defined,
the list |cpys| of productions where the copy rule has to be applied, 
and a record |defs| containing the explicit definitions for some productions:  

\begin{code}

inhAspect att nts cpys defs 
   =     (defAspect  (FnCpy att nts)  cpys)
   .+.   (attAspect  (FnInh att nts)  defs) 
\end{code}

\noindent The function |attAspect| generates an attribute aspect given the explicit definitions, 
whereas |defAspect| constructs an attribute aspect based in a common pattern's rule.
Thus, an inherited attribute aspect is defined as a composition of two attribute aspects:
one with the explicit definitions and other with the application of the copy rule. 
In the following sections we will see how |attAspect| and |defAspect| are implemented.
%In this case they perform some kind of |map| of the functions 
%|((.) (inhdef att nts))| and |(copy att nts)| into the values of |defs| and |cpys| respectively.
 

A synthesized attribute aspect, like |asp_smin| and |asp_sres| in Figure \ref{fig:asp_repmin}, can be defined using |synAspect|. 
Here the rule applied is the use rule, 
which takes |op| as the monoidal operator and |unit| as the unit value. 

\begin{code}

synAspect att nts op unit uses defs 
   =     (defAspect  (FnUse att nts op unit)    uses)
   .+.   (attAspect  (FnSyn att)                defs)
\end{code}


A chained attribute definition introduces both an inherited and a synthesized attribute. In this case the pattern to be applied is the chain rule. 

\begin{code}

chnAspect att nts chns inhdefs syndefs 
   =     (defAspect  (FnChn att nts)   chns)
   .+.   (attAspect  (FnInh att nts)   inhdefs)
   .+.   (attAspect  (FnSyn att)       syndefs)
\end{code}


\subsection{Attribute Aspects}

Consider the explicit definitions of the aspect |asp_sres|.
The idea is that, when declaring the explicit definitions, 
instead of completely writing the rules, like:

< br_  p_Root  .=. (\input -> 
<                   syndef  sres  ((chi input # ch_tree) # sres))
< ,    p_Leaf  .=. (\input -> 
<                   syndef  sres  (Leaf  (par input # ival)))   er_ 

\noindent we just define a record with the functions from the input
to the attribute value:

< br_  p_Root  .=. (\input -> (chi input # ch_tree) # sres)
< ,    p_Leaf  .=. (\input -> Leaf (par input # ival)) er_

\noindent By mapping the function |((.) (syndef sres))| over such records,
we get back our previous record containing rules.
The function |attAspect| updates all the values of a record by 
applying a function to them:

\begin{code}

class AttAspect rdef defs rules | rdef defs -> rules 
   where attAspect :: rdef -> defs -> rules

instance  (  AttAspect rdef (Record defs) rules
          ,  Apply rdef def rule  
          ,  HExtend (Prd lprd rule) rules rules' )  
         => AttAspect  rdef 
                       (Record (HCons  (Prd lprd def) 
                                       defs)) 
                       rules' 
  where
   attAspect rdef (Record (HCons def defs)) = 
         let  lprd = (labelLVPair def)
         in   lprd .=. apply rdef (valueLVPair def) 
              .*.  attAspect rdef (Record defs)   

instance AttAspect rdef (Record HNil) (Record HNil) 
  where attAspect _ _ = emptyRecord
\end{code}

\noindent The class |Apply| (from the HList library) models the function application, 
and it is used to add specific constraints on the types:

\begin{code}
class Apply f a r | f a -> r where
  apply :: f -> a -> r
\end{code}

\noindent In the case of synthesized attributes we apply |((.) (syndef att))|
to values of type |(Fam sc ip -> val)| in order to construct a rule of type |(Rule sc ip ic sp ic sp')|.
The constraint |HExtend (LVPair att val) sp sp'| is introduced by the use of |syndef|.
The data type |FnSyn| is used to determine which instance of |Apply| has to be chosen.

\begin{code}

data FnSyn att = FnSyn att

instance  HExtend (LVPair att val) sp sp'
         => Apply  (FnSyn att) (Fam sc ip -> val) 
                   (Rule sc ip ic sp ic sp') where 
  apply (FnSyn att) f =  syndef att . f 

\end{code}

\noindent In the case of inherited attributes the function applied to define the rule is |((.) (inhdef att nts))|.

\begin{code}

data FnInh att nt = FnInh att nt

instance  Defs att nts vals ic ic' 
         => Apply  (FnInh att nts) (Fam sc ip -> vals) 
                   (Rule sc ip ic sp ic' sp) where 
  apply (FnInh att nts) f = inhdef att nts . f 
\end{code}




\subsection{Default Aspects}

The function |defAspect| is used to construct an aspect
given a rule and a list of production labels. 

\begin{code}
class DefAspect deff prds rules | deff prds -> rules 
  where defAspect :: deff -> prds -> rules
\end{code}

\noindent It iterates over the list of labels |prds|, constructing a record with these labels 
and a rule determined by the parameter |deff| as value.
For inherited attributes we apply the copy rule |copy att nts|,
for synthesized attributes |use att nt op unit| and for chained attributes |chain att nts|.
The following types are used, in a similar way than in |attAspect|, to determine the rule to be applied: 

\begin{code}
data FnCpy att nts = FnCpy att nts
data FnUse att nt op unit = FnUse att nt op unit
data FnChn att nt = FnChn att nt
\end{code}

\noindent Thus, for example in the case of the aspect |asp_ival|, the application:

< defAspect (FnCpy ival bl_ nt_Tree el_ ) bl_ p_Node el_ 

\noindent generates the default aspect:

< br_ p_Node .=. copy ival bl_ nt_Tree el_  er_


%if False

\noindent As in |attAspect| we use the class |Apply| to introduce new constraints,
but since there is no application in this case, we pass a dummy type |None| as argument.
 
\begin{code}
instance DefAspect deff HNil (Record HNil) where
  defAspect _ _ = emptyRecord

instance  (  Poly deff deff'
          ,  DefAspect deff prds rules
          ,  HExtend (Prd prd deff') rules rules' )  
         => DefAspect deff (HCons prd prds) rules' where
  defAspect deff (HCons prd prds)  =
              prd .=. poly deff  .*.  defAspect deff prds   

class Poly a b where
  poly :: a -> b
\end{code}


\begin{code}

data FnCpy att nts = FnCpy att nts

instance  (  Copy att nts vp ic ic'
          ,  HasField att ip vp
          ,  TypeCast (Rule sc ip ic sp ic' sp) r) 
         => Poly  (FnCpy att nts) r  where 
  poly (FnCpy att nts)  = typeCast $ copy att nts 
\end{code}


\begin{code}
data FnUse att nt op unit = FnUse att nt op unit

instance  (  Use att nts a sc
          ,  HExtend (LVPair att a) sp sp'
          ,  TypeCast (Rule sc ip ic sp ic sp') r) 
         => Poly  (FnUse att nts (a -> a -> a) a) r where 
  poly (FnUse att nts op unit)  = 
          typeCast $ use att nts op unit 
\end{code}


\begin{code}
data FnChn att nt = FnChn att nt

instance  (  Chain att nts val sc ic sp ic' sp' 
          ,  HasField att ip val 
          ,  TypeCast (Rule sc ip ic sp ic' sp') r) 
      => Poly   (FnChn att nts) r where 
  poly (FnChn att nts) = typeCast $ chain att nts
\end{code}

%endif


%$
\section{Related Work}
\label{sec:related}

There have been several previous attempts at incorporating
first-class attribute grammars in lazy functional languages. To the best of our knowledge all these
attempts exploit some form of extensible records to collect
attribute definitions. They however do not exploit the Haskell class system as we do. \citet{MPW00} introduce a whole collection of functions, and a result it is no longer possible to define copy, use and chain rules. Other approaches fail to provide some of the
static guarantees that we have enforced~\cite{MBS00}.

The exploration of the limitations of type-level programming in
Haskell is still a topic of active research. For example, there has
been recent work on modelling relational data bases using techniques  similar
to those applied in this paper~\cite{SV06}. 

As to be expected the type-level programming performed here in Haskell can also be
done in dependently typed languages such as
Agda~\cite{norell:afp,oury-swierstra:power-of-pi2}. By doing so, we
use Boolean values in type level-functions, thereby avoiding the
need for a separate definition of the type-level Booleans. This
would certainly simplify certain parts of our development. On the
other hand, because Agda only permits the definition of total
functions, we would need to maintain even more information in our
types to make it evident that all our functions are indeed
total.

An open question is how easy it will be to extend the approach taken to more global strategies of accessing attributes definitions; some attribute grammars systems allow references to more remote attributes \cite{512645, Boyland:2005p52}. Although we are convinced that we can in principle encode such systems too, the question remains how much work this turns out to be.

Another thing we could have done is to make use of associated types \cite{1086397} in those cases where our relations are actually functions; since this feature is still experimental and has only recently become available we have refrained from doing so for the moment.

\section{Conclusions}\label{sec:conclusions}
In the first place we remark that we have achieved all four goals stated in the introduction:
\begin{enumerate}
\item removing the need for a whole collection of indexed combinators as used in \cite{MPW00}
\item replacing extensible records completely by heterogeneous collections
\item the description of common attribute grammar patterns in order to reduce code size, and making them almost first class objects
\item give a nice demonstration of type level programming
\end{enumerate}

We have extensive experience with attribute grammars in the construction of the Utrecht Haskell compiler \cite{uhc:architecture}. The code of this compiler is completely factored out along the two axes mentioned in the introduction \cite{dijkstra04thag,abstrIntUsingAG2008, dijkstra07ehcstruct}, using the notation used in Figure \ref{fig:AG}. In doing so we have found the possibility to factor the code into separate pieces of text indispensable.

We also have come to the conclusion that the so-called monadic approach, although it may seem attractive at first sight, in the end brings considerable complications when programs start to grow \cite{jones99thih}. Since monad transformers are usually type based we already run into problems if we extend a state twice with a value of the same type without taking explicit measures to avoid confusion. Another complication is that the interfaces of non-terminals are in general not uniform, thus necessitating all kind of tricks to change the monad at the right places, keeping information to be reused later, etc. In our generated Haskell compiler \cite{uhc:architecture} we have non-terminals with more than 10 different attributes, and glueing all these together or selectively leaving some out turns out to be impossible to do by hand.

In our attribute grammar system (|uuagc| on Hackage), we perform a global flow analysis, which makes it possible to schedule the computations explicitly \cite{kastens80order-attrgam}. Once we know the evaluation order we do not have to rely on lazy evaluation, and all parameter positions can be made strict.
When combined with a uniqueness analysis we can, by reusing space occupied by unreachable attributes, get an even further increase in speed. This leads to a considerable, despite constant,  speed improvement. Unfortunately we do not see how we can perform such analyses with the approach described in this paper: the semantic functions defining the values of the attributes in principle access the whole input family, and we cannot find out which functions only access part of such a family, and if so which part.

Of course a  straightforward implementation of extensible records will be quite expensive, since basically we use nested pairs to represent attributions. We think however that a not too complicated program analysis will reveal enough information to be able to transform the program into a much more efficient form by flattening such nested pairs. Note that thanks to our type-level functions, which are completely evaluated by the compiler, we do not have to perform any run-time checks as in \cite{MBS00}: once the program type-checks there is nothing which will prevent it to run to completion, apart form logical errors in the definitions of the attributes.

Concluding we think that the library described here is quite useful and relatively easy to experiment with. We notice furthermore that a conventional attribute grammar restriction, stating that no attribute should depend on itself, does not apply since we build on top of a lazily evaluated language. An example of this can be found in online pretty printing \cite{PPTr2004, CambridgeJournals:2837460}. Once we go for speed it may become preferable to use more conventional off-line generators. Ideally we should like to have a mixed approach in which we can use the same definitions as input for both systems.

  
\section{Acknowledgments}
We want to thank Oege de Moor for always lending an ear in discussing the merits of attribute grammars, and how to further profit from them.  Marcos Viera wants to thank the EU project Lernet for funding his stay in Utrecht. Finally, we would like to thank the anonymous referees for their helpful reviews.


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
