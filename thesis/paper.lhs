\chapter{Fast records for Haskell}
\label{chapt.first-class-grammaqqqmr}

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
%format !!! = "''"
%format !!!: = "'':"
%else
%format !!! = " ~''"
%format !!!: = "'':"
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
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wunticked-promoted-constructors #-}

import Data.Array
import GHC.Exts
import Unsafe.Coerce
import Data.Kind
import Data.Singletons
import Data.Singletons.TypeLits
import Data.Singletons.Prelude
import Data.Singletons.Prelude.Maybe
import Data.Singletons.Prelude.List
import Data.Singletons.TH
import GHC.TypeLits

\end{code}

%format ^ =

%endif

%\setlength{\parindent}{0in}

\section{Introduction} \label{sec:intro}

Many proposals add Extensible Records to Haskell
\cite{Gaster96apolymorphic, Jones99lightweightextensible, LabeledFunctions, Leijen:fclabels, Leijen:scopedlabels, Jel10},
but they focus on features and the type system, not on runtime performance.
Most implementations baked into the language store fields adjacent in memory,
like a tuple or array.
Library implementations use linked lists.

Imperative dynamic languages like JavaScript and Python implement objects as hash tables,
achieving constant time insertion and lookup.
%\marcos{esto es algo que sabemos o sospechamos? hay referencias para dar?}
%\bruno{sabemos pero no encuentro mejor referencia que http://en.wikipedia.org/wiki/Hashtable}
%\marcos{hash tables heterogeneas?}
%\bruno{heterogeneas, si, porque son lenguajes dinamicos}
Inserting or changing a field mutates the object.
The old version of the object disappears.
Functional languages require data persistence.
New versions must not disturb old versions.
The easy workaround to achieve persistence, cloning the hash table,
makes insertion linear time and much slower.

Clojure \cite{Hickey:2008:CPL:1408681.1408682} made popular
and Scala \cite{odersky2008programming} picked up
implementing immutable arrays as trees of small contiguous arrays,
so insertion is logarithmic due to structural sharing.
Clojure's hash map, built on top of vectors,
then achieves logarithmic time insertion and lookup.

The common strategies for record insertion in functional languages are
copying all existing fields along with the new one to a brand new tuple,
or using a linked list \cite{Gaster96apolymorphic}.
The tuple implementation offers the fastest possible lookup, but insertion is linear time.
Linked lists make the reverse trade-off,
with constant time insertion but linear time lookup.

Since a record is essentially a dictionary from labels to field values,
search trees seem to bridge the gap between lists and arrays
by offering both insertion and lookup in logarithmic time.
But just as unordered tuples offered constant time lookup
because the position of each field is statically known,
trees don't need to be ordered to fulfill requests in logarithmic time.
Balance suffices.

Our core trick is the observation that when looking up a field
by its compile time label in an unordered branching data structure,
paths that don't get to the field are pruned by the compiler and are not traversed at runtime.
The compiler traverses all fields but the program itself only has to traverse a small subset.
Figure~\ref{fig:search-skew} demonstrates that even when the wanted field is the last,
it can be reached visiting only one node of each tree level.

This thesis modernizes our presentation in \cite{martinez2013just}.
Harnessing modern Haskell features somewhat distances our solution from the original HList \cite{KLS04}.
In particular we take advantage of singletons \cite{eisenberg2013dependently}
to cleanly delimit the work at compile time later consumed at runtime.
We first present a linked list solution less featured than production implementations such as \cite{vinyl}
but with a more modern implementation
that lays the groundwork for later tree and array implementations.
We show experimental results that confirm this behaviour.

\begin{figure}[tp]
\begin{center}
\includegraphics[scale=0.5]{search-skew.pdf}
\end{center}
\caption{Search |l7| in balanced tree} \label{fig:search-skew}
\end{figure}

TODO thesis organization

\section{Singletons and classes}

Since \cite{yorgey2012giving} standard data types are also available as kinds.
One data type may be indexed with another data type that has been promoted.
The killer application of promoted kinds is using a promoted natural number
\begin{spec}
data Nat
    =  Zero
    |  Succ Nat
\end{spec}
to signal the length of a vector
\begin{spec}
data Vec :: * -> Nat -> *  where
    VNil   :: Vec a !!!Zero
    VCons  :: a->Vec a n->Vec a (!!!Succ n)
\end{spec}
We decorate promoted uses of constructors with ' to aid understanding
although it's only rarely needed to disambiguate.

Singletons are specially crafted types
that derive from another promoted type
and have only one value,
so there's a one to one relationship between a singleton value and it's type.
The singleton for the natural numbers is
\begin{spec}
data SNat :: Nat -> * where
    SZero  :: SNat !!!Zero
    SSucc  :: forall (n::Nat). SNat n -> SNat (!!!Succ n) 
\end{spec}
Only |SZero| (and undefined) has type |SNat !!!Zero| and so on.
The singletons library can generate singletons via Template Haskell \cite{SPJ02}
observing the naming convention illustrated above.

We'll also use some 'partial' singletons.
For the standard |Maybe| data type defined as
\begin{spec}
data Maybe a
  =  Nothing
  |  Just a
\end{spec}
which turns into the singleton
\begin{spec}
data SMaybe a where
  SNothing :: SMaybe  !!!Nothing
  SMaybe :: Sing e -> SMaybe (!!!Just e)
\end{spec}
we'll define the GADT \cite{peyton2006simple}
\begin{code}
data HMaybe a where
    HNothing :: HMaybe !!!Nothing
    HJust :: e -> HMaybe (!!!Just e)
\end{code}
with the H prefix honoring HList.
Notice that |HJust| wraps a naked |e|,
not a |Sing e| like |SJust|.
Both |SMaybe| and |HMaybe| can force term and type level
copmutations to run in-sync.

%if style==newcode
\begin{code}
instance Show (HMaybe !!!Nothing) where
    show HNothing = "HNothing"
instance Show m => Show (HMaybe (!!!Just m)) where
    show (HJust a) = "(HJust " ++ show a ++ ")"
\end{code}
%endif

Both type classes, by dispatching on types,
and normal functions, by doing case analysis on constructors,
can implement |HMaybe| operations.
For example, the function
\begin{code}
$(singletons [d|
    plus :: Maybe a -> Maybe a -> Maybe a
    plus Nothing b = b
    plus (Just a) b = Just a
    |])
\end{code}
can be lifted to |HMaybe| simply as
\begin{spec}
hPlusSing :: HMaybe a -> HMaybe b -> HMaybe (Plus a b)
hPlusSing HNothing b = b
hPlusSing (HJust a) _ = HJust a
\end{spec}
or more elaborately as
\begin{spec}
class HPlusClass a where
    hPlusClass :: HMaybe a -> HMaybe b -> HMaybe (Plus a b)
instance HPlusClass !!!Nothing where
    hPlusClass _  b = b
instance HPlusClass (!!!Just a) where
    hPlusClass a  _ = a
\end{spec}

|hPlusSing| inspects the first argument at runtime
to take one path or the other,
while |hPlusClass| just needs the types.
If the first argument of |hPlusSing| is |undefined|
|hPlusSing| returns |undefined|.
But |hPlusClass| works when the first argument is |undefined :: HNothing|.

We'll use |HMaybe| as the return type of our lookup functions.
When a record doesn't contain a field of a given label,
lookup returns |HNothing|, so we statically know at compile time
that the operation failed.
When lookup succeeds, |HJust| signals that fact at compile time
and offers the value of the field at runtime.

\subsection{List record}

Records can be implemented with a list defined by
\begin{code}
data ListRecord (fs :: [(l, Type)]) where
   ListNil  :: ListRecord !!![]
   ListCons :: v -> ListRecord fs -> ListRecord ( !!!(l,v) !!!: fs)
\end{code}
We leave open the kind of labels.
We can use types as in the original HList
or symbols now that Haskell supports compile time string literals.
|ListRecord| is indexed by a list of pairs of labels and values.
List of pairs is the native data type of Prelude's |lookup| function.
The label is purely a phantom type.
It doesn't appear in the left hand side of the |ListCons| equation.

We define a smart constructor |hListExtend|
to avoid specifying the type of constructed records.
We won't always care so much for ergonomics,
specially in internal functions.
|hListExtend| also aligns |ListRecord| with our other implementations of records,
which need nontrivial constructors.
|.=.| receives a label wrapped in a |proxy| to support labels not from kind |Type|,
particularly |Symbol|, the promotion of text.
Since |proxy| is a variable, we support any wrapper, |Proxy| and |Sing| for example.
\begin{code}
newtype Field l v   =   Field v
(.=.)               ::  proxy l -> v -> Field l v
_  .=.  v           =   Field v

hListExtend :: Field l v -> ListRecord fs -> ListRecord ('(l, v) ': fs)
hListExtend (Field v) vs = v `ListCons` vs
infixr 2 `hListExtend`

hListEmpty :: ListRecord !!![]
hListEmpty = ListNil
\end{code}

We define a bunch of labels of kind |Symbol|
\begin{code}
l1 = undefined :: Proxy "L1"
l2 = undefined :: Proxy "L2"
l3 = undefined :: Proxy "L3"
l4 = undefined :: Proxy "L4"
l5 = undefined :: Proxy "L5"
l6 = undefined :: Proxy "L6"
l7 = undefined :: Proxy "L7"
\end{code}
so that we can define our running sample record with seven fields:

%if style==newcode
\begin{code}
{-# NOINLINE rList #-}
\end{code}
%endif  

\begin{code}
rList =
  (l1  .=.  True     )  `hListExtend`
  (l2  .=.  9        )  `hListExtend`
  (l3  .=.  "bla"    )  `hListExtend`
  (l4  .=.  'c'      )  `hListExtend`
  (l5  .=.  Nothing  )  `hListExtend`
  (l6  .=.  [4,5]    )  `hListExtend`
  (l7  .=.  "last"   )  `hListExtend`
  hListEmpty
\end{code}

The implementation of lookup for |ListRecords|
is more elaborate than needed to set the stage for the other record variants.
|PathList| is the series of steps to reach a field.
It's actually just a unary number.
At each node the path leads to the head or the tail of the list.
\begin{code}
$(singletons [d|
    data PathList = PathListTail PathList | PathListHead
    |])
\end{code}

|makePathList| searches for the field in the pair list.
We'll only need it at compile time,
but the runtime definition allows easier testing.
The singletons package generates the promoted type family for us.
At the term-level we can write the function a little messy
without type families limited expressibility
forcing us to chunk it into a lot of auxiliary definitions.
\begin{code}
$(singletons [d|
    makePathList :: Eq l => l -> [(l,v)] -> Maybe PathList 
    makePathList l [] = Nothing
    makePathList l ((l2, v) : fs) = 
        if l == l2 
        then Just PathListHead
        else case makePathList l fs of
            Nothing -> Nothing
            Just p -> Just $ PathListTail p
\end{code}

Finally, |walkList| traverses the |PathList| and the pair list
simultaneously to retrieve the field value. 
\begin{code}
    walkList :: [(l, v)] -> PathList -> v
    walkList ((_, v) : fs) PathListHead = v
    walkList (_ : fs) (PathListTail p) = walkList fs p
    |])
\end{code}

To consume |PathList|,
we'll define helper function |maybeMap|, which didn't make the cut
to be included among the built in functions that come with the singletons library.
It's a specialization of |fmap| for |Maybe| only.
The type itself suffices to determine the function.
\begin{code}    
$(singletons [d|
    maybeMap :: (a -> b) -> Maybe a -> Maybe b
    maybeMap f Nothing = Nothing
    maybeMap f (Just a) = Just (f a)
    |])
\end{code}

The runtime implementation of list record lookup
depends on the compiler to compute the promoted |MakePathList|
as part of type checking.
\begin{code}
hListGetSing ::
    forall proxy l fs.
    SingI (MakePathList l fs) =>
    proxy l ->
    ListRecord fs ->
    HMaybe (MaybeMap (WalkListSym1 fs) (MakePathList l fs))
hListGetSing l fs = hWalkListSing fs (sing :: Sing (MakePathList l fs))
\end{code}

\noindent The |SingI| constraint forces the compiler to compute the path to the field.
|sing| converts the constraint into a singleton object we can inspect.
This worker/wrapper pattern is very common with singletons.
|SingI| is convenient for callers because the compiler fills it automatically,
but the implementation needs to destruct an object itself.
Since |SingI| only has method |sing|,
the method dictionary for |SingI| is |sing| itself.
After desugaring, a function that takes a constraint
and one that takes a singleton object are indistinguishable.

The rest is just a mechanical transliteration of |walkList|.
\begin{code}
hWalkListSing :: ListRecord fs -> Sing p -> HMaybe (MaybeMap (WalkListSym1 fs) p)
hWalkListSing fs SNothing = HNothing
hWalkListSing fs (SJust p) = HJust $ hWalkList'Sing fs p

hWalkList'Sing :: ListRecord fs -> Sing p -> WalkList fs p
hWalkList'Sing (v `ListCons` _) SPathListHead = v
hWalkList'Sing (_ `ListCons` fs) (SPathListTail m) = hWalkList'Sing fs m    
\end{code}

The implementation with type classes is analogous.
Funtional dependencies are unnecessary.
|Proxy| substitutes |Sing| and the path is never consumed at runtime,
gaining a little efficiency.
\begin{code}

hListGetClass :: forall proxy l fs p. (p ~ MakePathList l fs, HWalkListClass p) => proxy l -> ListRecord fs -> HMaybe (MaybeMap (WalkListSym1 fs) p)
hListGetClass l fs = hWalkListClass fs (undefined :: Proxy p)

class HWalkListClass p where
    hWalkListClass :: ListRecord fs -> proxy p -> HMaybe (MaybeMap (WalkListSym1 fs) p)
instance HWalkListClass !!!Nothing where
    hWalkListClass _ _ = HNothing
instance HWalkList'Class p => HWalkListClass (!!!Just p) where
    hWalkListClass fs _ = HJust $ hWalkList'Class fs (undefined :: Proxy p)

class HWalkList'Class p where
    hWalkList'Class :: ListRecord fs -> proxy p -> WalkList  fs p
instance HWalkList'Class !!!PathListHead where
    hWalkList'Class (v `ListCons` _) _ = v
instance HWalkList'Class p => HWalkList'Class (!!!PathListTail p) where
    hWalkList'Class (_ `ListCons` fs) _ = hWalkList'Class fs (undefined :: Proxy p)
\end{code}

While |hListGetClass| defines its own type class,
|hListGetSing| uses predefined |SingI|.
Reuse is the spirit of functional programming.
If it weren't for |hListGetClass| slight performance advantage,
|hListGetSing| would win.
It would be nice if the compiler could generate the same code for both variants.
The difference in performance is larger in later functions.

For GHC, the type level machinery not only generates correct value level code,
but efficient code too.
At the value level, the functions |hWalkListClass| and |hWalkList'Class| are trivial,
devoid of logic and conditions.
For this reason,
GHC is smart enough to elide the dictionary objects and indirect jumps for |hWalkListClass|.
The code is inlined to a case cascade, but the program must traverse the linked list.
When GHC is nerfed to forbid it inlining |rList| and just producing |"last"|,
\begin{code}
lastListClass = hListGetClass l7 rList
\end{code}

%if style==newcode
\begin{code}
lastListSing = hListGetSing l7 rList
\end{code}
%endif  

\noindent generates the same core code as
\begin{code}
lastListClassCore = case rList of
  _ `ListCons` rs1 -> case rs1 of
    _ `ListCons` rs2 -> case rs2 of
     _ `ListCons` rs3 -> case rs3 of
       _ `ListCons` rs4 -> case rs4 of
          _ `ListCons` rs5 -> case rs5 of
            _ `ListCons` rs6 -> case rs6 of
              e `ListCons` _ -> HJust e
\end{code}
namely
%if style==poly
\begin{code}
lastListClassCore
  = case rList of { ListCons @ v @ fs @ l cobox ds fs1 ->
    case fs1 of {
      ListNil ipv -> $fHWalkList'ClassPathListTail3 `cast` <Co:68>;
      ListCons @ v1 @ fs2 @ l1 cobox1 ds1 fs3 ->
        case fs3 of {
          ListNil ipv -> $fHWalkList'ClassPathListTail3 `cast` <Co:133>;
          ListCons @ v2 @ fs4 @ l2 cobox2 ds2 fs5 ->
            case fs5 of {
              ListNil ipv -> $fHWalkList'ClassPathListTail3 `cast` <Co:194>;
              ListCons @ v3 @ fs6 @ l3 cobox3 ds3 fs7 ->
                case fs7 of {
                  ListNil ipv -> $fHWalkList'ClassPathListTail3 `cast` <Co:251>;
                  ListCons @ v4 @ fs8 @ l4 cobox4 ds4 fs9 ->
                    case fs9 of {
                      ListNil ipv -> $fHWalkList'ClassPathListTail3 `cast` <Co:304>;
                      ListCons @ v5 @ fs10 @ l5 cobox5 ds5 fs11 ->
                        case fs11 of {
                          ListNil ipv -> $fHWalkList'ClassPathListHead1 `cast` <Co:353>;
                          ListCons @ v6 @ fs12 @ l6 cobox6 v7 ds6 -> v7 `cast` <Co:371>
                        }
                    }
                }
            }
        }
    }
    }
\end{code}
%endif
It seems ghc makes core total by inserting explicit error calls for the |ListNil| case in |$fHWalkList'ClassPathListTail3|.
To get understandable core call it like ghc-8.2.1 thesis.hs -c -O3 -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes.

|lastListSing| generates less efficient code
because each step entails three pattern matches:
the list, the path and its equality constraint.
%if style==poly
\begin{code}
hWalkList'SingCore
  = \ (@ l)
      (@ (p :: PathList))
      (@ (fs :: [(l, *)]))
      (ds :: Sing p)
      (ds1 :: ListRecord fs) ->
      case ds `cast` <Co:2> of {
        SPathListTail @ n $d~ m ->
          case HEq_sc ($d~ `cast` <Co:5>) of cobox { __DEFAULT ->
          case ds1 of {
            ListNil ipv -> lvl35;
            ListCons @ v @ fs1 @ l1 cobox1 ds2 fs2 ->
              (hWalkList'Sing m fs2) `cast` <Co:48>
          }
          };
        SPathListHead $d~ ->
          case HEq_sc ($d~ `cast` <Co:4>) of cobox { __DEFAULT ->
          case ds1 of {
            ListNil ipv -> lvl35;
            ListCons @ v @ fs1 @ l1 cobox1 v1 ds2 -> v1 `cast` <Co:18>
          }
          }
      }
\end{code}
%endif  

\section{Faster Extensible Records}\label{sec:faster}

Extensible records can double as
``static type-safe" dictionaries, that is,
collections that guarantee at compile time
that all labels searched for are available.
For example, \cite{FlyFirstClass}, a library for first-class attribute
grammars, uses extensible records to encode the collection of
attributes associated to each non-terminal.
To use records to implement a system with a big number of attributes (e.g. a compiler),
we would need an efficient structure.
After increasing the size of GHC's context reduction stack,
the program compiles,
but it runs slowly due to the linear time lookup algorithm.
The usual replacement when lookup in a linked list is slow
is a search tree.
The need to compare labels at compile-time
rules out using plain types as labels.
Symbols do have ordering.
Balanced trees have complex implementations
with tricky rotations,
but the singletons library surely can promote them.
The problem is that we want to do the minimum of work at runtime.
With singletons the runtime definition mirrors the compile time definition,
and performance suffers.
The implementation of singleton comparison for Symbols actually compares each character.
\begin{spec}
instance SOrd Symbol where
  a `sCompare` b = case fromSing a `compare` fromSing b of
                     LT -> unsafeCoerce SLT
                     EQ -> unsafeCoerce SEQ
                     GT -> unsafeCoerce SGT
\end{spec}

The key insight is that sub-linear behavior is only needed at run time.
We do not worry if the work done at compile time is superlinear
as long as it helps us to speed up our programs at run time.
|MakePathList| can be slower than |walkList| without slowing programs.
So our idea is to maintain the fields stored unordered, but
in a structure that allows fast random access and depend on the compiler to
hardcode the path to our fields.

We will present two variants of faster records.
To make code listing shorter and easier to understand,
we implement each variant with independent interfaces.
However, it would be possible to provide a common class-based interface
for all variants.

The first variant follows the conventional approach of
storing the record as a tuple.
However, because Haskell does not offer
genericity over the length of tuples as in \cite{Tullsen00thezip}, i.e. efficient access to the $i$-th element of an arbitrary length tuple,
%if False
\alberto{aca lo que queres decir es que no tenes tuplas arbitrarias de largo |n| con sus correspondientes proycciones, no? si armaramos con tuplas un estructura telescopica con pares anidados, acceder un field nos quedaria un camino |fst . snd . ...| y eso es orden |n|. Este problema de las tuplas de largo |n| es una de las motivaciones de staged programming, y en particular de Template Haskell (TH). No se podra combinar lo de type-level programming con TH para en lugar de generar un array, generar una tupla de largo |n| y acceder al i-esimo elemento? tiene pinta de ser equivalente a generar el array, pero es otra alternativa.}
%endif
we will use an array instead, coercing field values to type |Any|.
%|Any| via |unsafeCoerce|,
%since array elements must be of the same type.
%Apart from this breach of type safety,
This implementation supports linear time insertions
and constant time lookups.

The second variant is tree-like,
based on Skew Binary Random-Access Lists~\cite{OkaThesis}, a structure with constant time insertion
and logarithmic time access to any element.
We could have chosen other, perhaps simpler, data structures
such as Braun trees \cite{brauntrees}, since
the key property
of searching at compile time while retrieving at run time
works unchanged in any balanced tree structure.
In every node of a Braun tree,
the left subtree has either the same number of nodes as the right subtree or one more,
guaranteeing the minimum distance from root to leaf.
Insertion proceeds level by level, left to right.
However, few structures offer constant time insertion
and are drop-in replacements for simple linear lists.
A structure with logarithmic insertion slows down
applications heavy on record modification.
Insertion in search trees is also logarithmic.
Otherwise it would be possible to sort a collection in linear time.
 
\subsection{Array Records}\label{sec:array}


The most popular implementation of records
is to place fields contiguous in memory.
This variant is the most pragmatic that we offer.
Contiguous memory uses cache best.
Since normal values in Haskell are boxed,
there's still one indirection left,
but it's still less indirections than the alternatives.

An Array Record has two components:
an runtime array containing the values of the fields, 
and a compile-time list used to find a field's type and its ordinal for lookup in the array.
To allow the storage of elements of different types in the array, 
we use element type |Any|, a placeholder type that documents our intention,
even though any boxed type would work.
Items are then |unsafeCoerce|d on the way in and out based on the type information we keep in the compile-time list.
An industrial implementation would hide the data constructor
in a separate module to ensure type safety
by only allowing white-listed functions to access the array directly.
\begin{code}
newtype ArrayRecord (fs :: [(l, Type)]) =
  ArrayRecord (Array Int Any)
\end{code}

\subsubsection{Construction}

An empty |ArrayRecord| consists of an empty array,
and an empty compile-time field list.

\begin{code}
hArrayEmpty :: ArrayRecord !!! []
hArrayEmpty =
  ArrayRecord (array (0, -1) [])
\end{code}

\noindent The -1 is an eye sore, but we keep the standard arrays for familiarity.

Function |hArrayExtend| adds a field to an array record.

\begin{code}
hArrayExtend :: Field l v -> ArrayRecord ls -> ArrayRecord ('(l, v) ': ls)
hArrayExtend (Field v) (ArrayRecord a) =
    ArrayRecord $ listArray (0, 1 + snd (bounds a)) (unsafeCoerce v : elems a)
infixr 2 `hArrayExtend`
\end{code}

\noindent The new field becomes the head of both the compile-time list and the runtime array.
Growing the array requires an intermediate list
and computing updated bounds.
|unsafeCoerce| explicitly converts the field value to |Any|,
because |Any| is not really a superclass, but a normal type.
Using |Any| to hide type information is purely a convention.
The type checker does not treat |Any| specially.
It's not part of the language.

The previous presentation of this material stored a singly linked list record alongside the array,
mainly because the typing side of field keeping was not as cleanly segregated from the runtime side as now.
But the linked list had the advantage of speeding up back to back insertions.
Since building a new record ignored array of the old record,
and the array of the new record only depended on the list of the new record,
the lazy language never built the array of a record that was only used to build bigger records.
Adding $n$ fields was then a linear time operation instead of quadratic,
but the list more than doubles the size of the record.
The optimization can be done manually when needed.

\subsubsection{Lookup}
Array lookup is not recursive,
so the extra overhead of singletons over classes is constant
and not proportional to the number of fields in the record.
The implementation also takes advantage of unique singleton features.
We won't bother writing an implementation with classes.
This is the case that makes singletons shine.
\begin{code}
hArrayGet :: 
    forall fs proxy l i.
    (i ~ ElemIndex l (Map FstSym0 fs)
    ,SingI i) => 
    proxy l -> 
    ArrayRecord fs -> 
    HMaybe (Maybe_
        !!!Nothing
        (JustSym0 :.$$$ SndSym0 :.$$$ (:!!$$) fs)
        i)
hArrayGet _ (ArrayRecord a) = case sing :: Sing i of
    SNothing -> HNothing
    SJust i' -> HJust $ unsafeCoerce (a ! (fromInteger $ fromSing i'))
\end{code}

\noindent
First we search the compile-time list of fields for the position of the label
with |ElemIndex l (Map FstSym0 fs)|.
|ElemIndex| is a standard prelude function
that the singletons library already promotes out of the box.
|FstSym0| is the version of compile time |fst| that can be used without arguments.
\footnote{Other versions end with broken dishes}
Three places need the position,
so it pays to abuse an equality constraint to avoid repeating the definition.

The return type uses the position of the field to get the field value.
At term-level, the core of the return type would be |maybe Nothing (Just . snd . (!!) fs) i|.
So in the case the label is present in the label list 
and |elemIndex| did return the position,
the value comes from indexing the pair list with the same position,
taking the value half of the pair,
and wrapping everything in |Just|.

At runtime, first |sing| turns the constraint into either |SJust| or |SNothing|.
Then |fromSing| computes the plain term-level index.
|unsafeCoerce| just restores the type the |Any| object actually has.
By virtue of inlining and constant folding, the compiler computes and hard-codes the position of the field.

Figure~\ref{fig:search-array} shows a graphical representation of this process.
Dashed arrow represents the compile time search of the field in the compile-time list
which results in the index of the element in the array.
Using this index the element is retrieved from the array in constant time at run time (solid arrow).

\begin{figure}[t]
\begin{center}
\includegraphics[scale=0.5]{search-array.pdf}
\end{center}
\caption{Search |l7| in Array} \label{fig:search-array}
\end{figure}

|HMaybe (Lookup l fs)| would be a nicer return type.
The problem is convincing the type checker that |lookup|
is equivalent to the longer composite expression with |elemIndex|, |!!| and |snd|.
Enlarging the scope of |unsafeCoerce| would work,
but that's heavier cheating than we already do.


%if style==newcode
\begin{code}
{-# NOINLINE rArray #-}
\end{code}
%endif  

%if style==newcode
\begin{code}
rArray =
  (l1  .=.  True     )  `hArrayExtend`
  (l2  .=.  9        )  `hArrayExtend`
  (l3  .=.  "bla"    )  `hArrayExtend`
  (l4  .=.  'c'      )  `hArrayExtend`
  (l5  .=.  Nothing  )  `hArrayExtend`
  (l6  .=.  [4,5]    )  `hArrayExtend`
  (l7  .=.  "last"   )  `hArrayExtend`
  hArrayEmpty
lastArray = hArrayGet l7 rArray
\end{code}
%endif  
%

\subsection{Skew Binary Random-Access List}\label{sec:skew}

We start with a description of Skew Binary Random-Access List \cite{Mye83} in a less principled
but easier and more direct fashion than \cite{OkaThesis}, which is founded on numerical representations.
A skew list is a linked list spine of complete binary trees.

The invariant of skew lists is that the height of trees
get strictly larger along the spine,
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
the successive skew lists that arise in the process of construction of a skew list.
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
First we need a tree type to tag our heterogeneous trees,
just as we tagged our list records with a list of pairs.
A Template Haskell splice generates type-level and singleton versions
of our term-level definition.
\begin{code}
$(singletons [d|
    data Tree a
        = Empty
        | Node a (Tree a) (Tree a)
    |])
\end{code}
    
For convenience, we define the |leaf| smart constructor. 
\begin{code}
leaf e = Node e Empty Empty
type Leaf e = !!!Node e !!!Empty !!!Empty
\end{code}
We have to manually define the type-level version
because the singletons library would define a type family
and not a type synonym.
Type families don't work in the header of a class instance
as we require later on.

The heterogeneous version of trees is a mechanical translation.
\begin{code}
data HTree t where
    HEmpty :: HTree !!!Empty
    HNode :: v -> HTree t1 -> HTree t2 -> HTree (!!!Node !!!(l, v) t1 t2) 
\end{code}
as is the heterogeneous version of |leaf|
\begin{code}
type  HLeaf e         =  HTree (Leaf e)
hLeaf :: v -> HLeaf !!!(l, v)
hLeaf  v         =  HNode v HEmpty HEmpty
\end{code}

We tie trees together in a spine
\begin{code}
data Spine ts where
    SpineNil :: Spine !!![]
    SpineCons :: HTree t -> Spine ts -> Spine (t !!!: ts)
infixr 2 `SpineCons`
\end{code}
that consists and is tagged by a list of trees.

The invariant of skew tree needs |height|.
\begin{code}
$(singletons [d|
    height :: Tree e -> Nat
    height Empty = 0
    height (Node _ _ t) = 1 + height t
    |])
\end{code}
\noindent Note that |height| returns |Nat|, not |Int| or |Integer|.
The change enables correct promotion
for the price of making the term-level version almost useless.

Each size determines a unique skew tree,
unlike search trees where the insertion order
determines slightly different trees even for balanced variants.
The shape of a skew tree is a shallow function from the previous size 
\begin{code}
$(promote [d|
    skew' f (a:b:ts) | height a == height b = Node f a b : ts
    skew' f ts = Node f Empty Empty : ts

    skew :: [e] -> [Tree e]
    skew [] = []
    skew (f : fs) = skew' f (skew fs)
    |])
\end{code}

\noindent We promote skew only to a type function and not also to a singleton function
to avoid having to rewrite the function without overlapping patterns.
We won't need the singleton version.

%if style==newcode
\begin{code}
$(promote [d|
    -- TODO find out why skew1 doesn't work like skew
    skew1 :: [e] -> [Tree e]
    skew1 ts = foldr skew' [] ts
    |])
\end{code}
%endif

|PathSpine| and |PathTree| mirror earlier |PathList| for linear lists,
and encode the steps to get to a field.
|PathTree| has three options.
The wanted field may be in the current node, to the left or to the right.
|PathSpine| is most similar to |PathList|,
but if we found the right spine element
we still need to continue the path down the current tree.
\begin{code}    
$(singletons [d|
    data PathSpine = PathSpineTail PathSpine | PathSpineHead PathTree
    data PathTree = PathTreeRoot | PathTreeLeft PathTree | PathTreeRight PathTree
    |])
\end{code}

With |maybeMap| and |plus| from earlier,
|makePathSpine| and |makePathTree| are simple.
|makePathTree| has three cases.
When the tree is empty, so is the path.
When the current node has the field, the path is |PathTreeRoot|.
Else the field may be left or right.
Since Haskell is lazy, we compute the path for both children
and have |plus| choose the non empty path.
|maybeMap| adds |PathTreeRight| or |PathTreeLeft| to the subpath
from the recursive call.
\begin{code}
$(singletons [d|
    makePathTree :: Eq l => l -> Tree (l, v) -> Maybe PathTree
    makePathTree l Empty = Nothing
    makePathTree l (Node (l2, v) t1 t2) = if l == l2 then Just PathTreeRoot else maybeMap PathTreeLeft (makePathTree l t1) `plus` maybeMap PathTreeRight (makePathTree l t2)
    |])
\end{code}

|makePathSpine| is similar, just taking into account the difference in the input structure.
\begin{code}
$(singletons [d|
    makePathSpine :: Eq l => l -> [Tree (l, v)] -> Maybe PathSpine
    makePathSpine l [] = Nothing
    makePathSpine l (t : ts) = maybeMap PathSpineHead (makePathTree l t) `plus` maybeMap PathSpineTail (makePathSpine l ts)
    |])
\end{code}

|walkSpine| and |walkTree| are simple.
\begin{code}    
$(singletons [d|
    walkSpine :: [Tree (l, v)] -> PathSpine -> v
    walkSpine (t : ts) (PathSpineHead p) = walkTree t p
    walkSpine (t : ts) (PathSpineTail p) = walkSpine ts p

    walkTree :: Tree (l, v) -> PathTree -> v
    walkTree (Node (l,v) t1 t2) PathTreeRoot = v
    walkTree (Node (l,v) t1 t2) (PathTreeLeft p) = walkTree t1 p
    walkTree (Node (l,v) t1 t2) (PathTreeRight p) = walkTree t2 p
    |])
\end{code}

We arrive at the nicest piece of code.
The |SkewRecord| type parameter is a list of fields, of label-value pairs.
But |Skew| computes the skew tree of the list,
which |SkewRecord| stores,
so it's well formed by construction.
\begin{code}    
newtype SkewRecord fs = SkewRecord (Spine (Skew fs))
\end{code}

%% $ fix emacs color highlighting

\subsubsection{Construction}

We define a smart constructor |hSkewEmpty| for empty skew lists, i.e. an empty spine of trees.
\begin{code}    
hSkewEmpty :: SkewRecord !!![]
hSkewEmpty = SkewRecord SpineNil
\end{code}

|hSkewExtendClass| or |hSkewExtendSing| can interchangeably add a field to a record.
Their type is the same except for the constraint.
|hSkewExtendSing| requires the compiler to compute the height of all trees in the spine,
but actually only uses the first two,
which is efficient thanks to the language laziness.
\begin{code}
hSkewExtendSing :: forall l v fs s. (s ~ (Map HeightSym0 (Skew fs)), SingI s) => Field l v -> SkewRecord fs -> SkewRecord (!!!(l, v) !!!: fs)
hSkewExtendSing (Field v) r = SkewRecord $ case r of
    (SkewRecord SpineNil) -> hLeaf v `SpineCons` SpineNil
    (SkewRecord (a `SpineCons` SpineNil)) -> hLeaf v `SpineCons` a `SpineCons` SpineNil
    (SkewRecord (ta `SpineCons` tb `SpineCons` ts)) -> case sing :: Sing s of
        (ha `SCons` (hb `SCons` _)) -> case ha %:== hb of
            STrue -> HNode v ta tb `SpineCons` ts
            SFalse -> hLeaf v `SpineCons` ta `SpineCons` tb `SpineCons` ts
infixr 2 `hSkewExtendSing`
\end{code}
We handle the base cases, when the input record is empty or a single field, directly.
When the record already has two or more fields,
we also inspect the heights.
Having the compiler compute the heights is necessary
to keep the runtime of the function constant.
If the height was computed at runtime, the function would run in logarithmic time.
But comparing heights at runtime is not optimal either.
It would be better to push that also to compile-time and have a simple |SBool| to steer the last case.
We don't do that because we need a total function from type list of trees to bool.
The cases with fewer than two elements need to be defined, which is ugly.

|hSkewExtendClass| just calls into custom class |HSkewExtend'|,
which mirrors |Skew'|.
\begin{code}
hSkewExtendClass :: HSkewExtend' (Skew fs) => Field l v -> SkewRecord fs -> SkewRecord (!!!(l, v) !!!: fs)
hSkewExtendClass f (SkewRecord ts) = SkewRecord $ hSkewExtend' f ts
infixr 2 `hSkewExtendClass`

class HSkewExtend' ts where
    hSkewExtend' :: Field l v -> Spine ts -> Spine (Skew' '(l, v) ts)
instance
    HSkewExtend' '[] where
    hSkewExtend' (Field v) SpineNil = hLeaf v `SpineCons` SpineNil
instance
    HSkewExtend' '[f] where
    hSkewExtend' (Field v) ts = hLeaf v `SpineCons` ts
instance
    ((Height ta :== Height tb) ~ b
    ,HSkewExtend'' b ta tb) =>
    HSkewExtend' (ta ': tb ': ts) where
    hSkewExtend' f ts = hSkewExtend'' (undefined :: Proxy b) f ts

class HSkewExtend'' (b::Bool) ta tb where
    hSkewExtend'' :: ts ~ (ta ': tb ': ts') => proxy b -> Field l v -> Spine ts -> Spine (Skew' !!!(l, v) ts)
instance (Height ta :== Height tb) ~ !!!True => HSkewExtend'' !!!True ta tb where
    hSkewExtend'' _ (Field v) (ta `SpineCons` tb `SpineCons` ts) = HNode v ta tb `SpineCons` ts
instance (Height ta :== Height tb) ~ !!!False => HSkewExtend'' !!!False ta tb where
    hSkewExtend'' _ (Field v) (ta `SpineCons` tb `SpineCons` ts) = hLeaf v `SpineCons` ta `SpineCons` tb `SpineCons` ts
\end{code}
\noindent
Here |HFalse| means that we should not add up the first two trees of the spine.
Either the size of the two leading trees are different, or the spine is empty or a singleton.
We just use |HLeaf| to insert a new tree at the beginning of the spine.

%
When |HSkewCarry| returns |HTrue|, however, we build a new tree reusing the two trees that were at the start of the spine.
The length of the spine is reduced in one, since we take two elements but only add one.
%

\subsubsection{Lookup}

%The missing piece is
Now, we turn to the introduction of |HSkewGet|,
which explores all paths at compile time
but follows only the right one at run time.

\begin{code}
hSkewGetSing ::
    forall p fs proxy l.
    (p ~ (MakePathSpine l (Skew fs))
    ,SingI p) =>
    proxy l ->
    SkewRecord fs ->
    HMaybe (MaybeMap (WalkSpineSym1 (Skew fs)) p)
hSkewGetSing l (SkewRecord ts) = hWalkSpineSing ts (sing :: Sing p)

hWalkSpineSing :: Spine ts -> Sing p -> HMaybe (MaybeMap (WalkSpineSym1 ts) p)
hWalkSpineSing ts SNothing = HNothing
hWalkSpineSing ts (SJust p) = HJust $ hWalkSpine'Sing ts p

hWalkSpine'Sing :: Spine ts -> Sing p -> WalkSpine ts p
hWalkSpine'Sing (t `SpineCons` ts) (SPathSpineHead p) = hWalkTreeSing t p
hWalkSpine'Sing (t `SpineCons` ts) (SPathSpineTail p) = hWalkSpine'Sing ts p

hWalkTreeSing :: HTree t -> Sing p -> WalkTree t p
hWalkTreeSing (HNode v t1 t2) SPathTreeRoot = v
hWalkTreeSing (HNode _ t1 t2) (SPathTreeLeft p) = hWalkTreeSing t1 p
hWalkTreeSing (HNode _ t1 t2) (SPathTreeRight p) = hWalkTreeSing t2 p


hSkewGetClass ::
    forall p fs proxy l.
    (p ~ (MakePathSpine l (Skew fs))
    ,HWalkSpineClass p) =>
    proxy l ->
    SkewRecord fs ->
    HMaybe (MaybeMap (WalkSpineSym1 (Skew fs)) p)
hSkewGetClass l (SkewRecord ts) = hWalkSpineClass (undefined :: Proxy p) ts

class HWalkSpineClass p where
    hWalkSpineClass :: proxy p -> Spine ts -> HMaybe (MaybeMap (WalkSpineSym1 ts) p)
instance HWalkSpineClass 'Nothing where
    hWalkSpineClass _ _ = HNothing
instance HWalkSpine'Class p => HWalkSpineClass ('Just p) where
    hWalkSpineClass _ ts = HJust $ hWalkSpine'Class (undefined :: Proxy p) ts

class HWalkSpine'Class p where
    hWalkSpine'Class :: proxy p -> Spine ts -> WalkSpine ts p
instance HWalkTreeClass p => HWalkSpine'Class ('PathSpineHead p) where
    hWalkSpine'Class _ (t `SpineCons` ts) = hWalkTreeClass (undefined :: Proxy p) t
instance HWalkSpine'Class p => HWalkSpine'Class ('PathSpineTail p) where
    hWalkSpine'Class _ (t `SpineCons` ts) = hWalkSpine'Class (undefined :: Proxy p) ts

class HWalkTreeClass p where
    hWalkTreeClass :: proxy p -> HTree t -> WalkTree t p
instance HWalkTreeClass !!!PathTreeRoot where
    hWalkTreeClass _ (HNode v t1 t2) = v
instance HWalkTreeClass p => HWalkTreeClass ('PathTreeLeft p) where
    hWalkTreeClass _ (HNode _ t1 t2) = hWalkTreeClass (undefined :: Proxy p) t1
instance HWalkTreeClass p => HWalkTreeClass ('PathTreeRight p) where
    hWalkTreeClass _ (HNode _ t1 t2) = hWalkTreeClass (undefined :: Proxy p) t2

\end{code}

Deciding on the path to the desired field
is now more involved.
The cases that both the test function and the worker function must consider
are more numerous and long.
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
The |HCons| case must consider that the field may be found on the current tree or further down the spine.
A recursive call is made for each sub-case, and the results are combined with |HPlus|.
If the field is found in the current tree,
|HPlus| returns it, otherwise, it returns what the search down the spine did.

Observe that when doing |hSkewGet r l `hPlus` hSkewGet r' l| if the label is not present in |r| then
the type system chooses the second instance of |HPlus|  (|HPlus HNothing b b|).
Thus, by lazy evaluation, the subexpression |hSkewGet r l| is not evaluated
since |hPlus| in that case simply returns its second argument.

\noindent The |HNode| case is a bigger version of the |HCons| case.
Here three recursive calls are made,
for the current field, the left tree, and the right tree.
Thus two |HPlus| calls are needed to combine the result.

Finally, the |Field| case, when a field is found, is the case
%\alberto{cual, la que sigue o la anterior? no queda bien arrancar la orcion con And.....}
that may actually build a |HJust| result.
As in |HListGet| for linked lists, |HEq| compares both labels.
We call |HMakeMaybe| with the result of the comparison,
and |HNothing| or |HJust| is returned as appropriate.
%

When we repeat the experiment at the end of subsection \ref{sec:extensiblerecords},
but constructing a |SkewRecord| instead of an |HList|:
% using |hSkewEmpty| to construct a |SkewRecord|: \alberto{quien es |hSkewEmpty|?}

\begin{code}
rSkew =
  (l1  .=.  True     )  `hSkewExtendClass`
  (l2  .=.  9        )  `hSkewExtendClass`
  (l3  .=.  "bla"    )  `hSkewExtendSing`
  (l4  .=.  'c'      )  `hSkewExtendClass`
  (l5  .=.  Nothing  )  `hSkewExtendClass`
  (l6  .=.  [4,5]    )  `hSkewExtendClass`
  (l7  .=.  "last"   )  `hSkewExtendSing`
  hSkewEmpty
lastSkewSing = hSkewGetSing l7 rSkew
lastSkewClass = hSkewGetClass l7 rSkew
\end{code}
the resulting core code is:

\begin{code}
lastSkewCore = case rSkew of
    SkewRecord ts -> case ts of
      t1 `SpineCons` _ -> case t1 of
        HNode _ _ t12 -> case t12 of
          HNode _ _ t121 -> case t121 of
            HNode v _ _ -> v
\end{code}
Thus, getting to |l7| at run time only traverses a (logarithmic length) fraction of the elements,
as we have seen in Figure~\ref{fig:search-skew}.
Later we will examine runtime benchmarks.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsubsection{Update}\label{sec:update}

%a type-function |HSkewUpdate|
We now define an update operation that makes it possible to change
a field of some label with a new field with possibly new label and value.
%
\begin{code}
class HSkewUpdate l e ts ts' | l e ts -> ts' where
    hSkewUpdate :: proxy l -> e -> Spine ts -> Spine ts'
class HSkewUpdateTree l e t t' | l e t -> t' where
    hSkewUpdateTree :: proxy l -> e -> HTree t -> HTree t'
class HSkewUpdateField l e l' v' l'' v'' | l e l' v' -> l'' v'' where
    hSkewUpdateField :: proxy l -> e -> Field l' v' -> Field l'' v''
\end{code}
%
We use the lookup operation |HSkewGet| to discriminate at type-level
whether the field with the searched label is present or not in the skew list.
%
%
In case the label is not present we have nothing to do than just returning the
structure unchanged.
%
%\begin{code}
%instance HSkewUpdate' 'Nothing l e r r  where
%    hSkewUpdate' _ l e r = r
%\end{code}
%
In the other cases (i.e. when lookup results in |HJust v|) we call |hSkewUpdate| recursively on all subparts in order to apply the update when necessary.
Because of the previous instance (when lookup returns |HNothing|), at run time recursion will not enter in those cases where the label is not present.
We start the process in the spine.
%
%\begin{code}
%instance
%    (  HSkewUpdateTree l e t t'
%    ,  HSkewUpdate l e ts ts') =>
%    HSkewUpdate' ('Just v) l e  (t ': ts)
%                                (t' ': ts')
%    where
%    hSkewUpdate' _ l e (t `SpineCons` ts) =
%        hSkewUpdateTree l e t `SpineCons`
%               hSkewUpdate l e ts
%\end{code}
%
On a |HNode|, |hSkewUpdate| is recursively called on
the left and right sub-trees as well as on the element of the node.
%\begin{code}
%instance
%    (  HSkewUpdateField l e l' v' l'' v''
%    ,  HSkewUpdateTree l e tl tl'
%    ,  HSkewUpdateTree l e tr tr') =>
%    HSkewUpdateTree' ('Just v) l e  ('Node '(l', v') tl tr)
%                                ('Node '(l'', v'') tl' tr')
%    where
%    hSkewUpdateTree' _ l e (HNode e' tl tr) =
%        HNode  (hSkewUpdateField l e e')
%               (hSkewUpdateTree l e tl)
%               (hSkewUpdateTree l e tr)
%\end{code}
%
Finally, when we arrive to a |Field| and we know the label is the one we
are searching for (because we are considering the case |HJust v|), we simply return the updated field.
%
%\begin{code}
%instance
%    HSkewUpdateField' ('Just v) l e l v l e
%     where
%       hSkewUpdateField' _ l e e' = Field e
%\end{code}

At run time, this implementation of |hSkewUpdate| only
rebuilds the path to the field to update,
keeping all other sub-trees intact.
% Due to lazy evaluation, the searches of the label are performed only at compile time.
Thus the operation runs in time logarithmic in the size of the record.

\subsubsection{Remove}

Removing a field is easy based on updating.
We overwrite the field we want to eliminate with the first field in the skew list,
and then we remove the first field from the list.
Thus, we remove elements in logarithmic time while keeping the tree balanced.

First, we need a helper to remove the first element of a skew list.
%
\begin{code}
class HSkewTail ts ts' | ts -> ts' where
    hSkewTail :: Spine ts -> Spine ts'
\end{code}

\noindent
In Figure~\ref{fig:tail} we show an example of the possible cases
we can find.

\begin{figure}[htp]
\begin{center}
\includegraphics[scale=0.5]{tail.pdf}
\end{center}
\caption{Tail in a Skew} \label{fig:tail}
\end{figure}

The easy case is when the spine begins with a leaf.
We just return the tail of the spine list.
\begin{code}
instance HSkewTail (Leaf e : ts) ts where
    hSkewTail (_ `SpineCons` ts) = ts
\end{code}

\noindent
The other case is when the spine begins with a tree of three or more elements.
Since |HLeaf| is a synonym of |HNode| with |HEmpty| as sub-trees,
we need to assert the case when the sub-trees of the root |HNode|
are nonempty (i.e. |HNode|s themselves).
By construction, both sub-trees have the same shape, but doing pattern matching on the first one only suffices to make sure this case does not overlap with the previous one.
In this case we grow the spine with the sub-trees, throwing away the root.
%
\begin{code}
instance
    HSkewTail
        ('Node e t ('Node e' t' t'') ': ts)
        (t ': 'Node e' t' t'' ': ts)
    where
    hSkewTail (HNode _ t t' `SpineCons` ts) =
        t `SpineCons` t' `SpineCons` ts
\end{code}


Last, |hSkewRemove| takes the first node and calls |hSkewUpdate|
to duplicate it where the label we want gone was.
Then |hSkewTail| removes the original occurrence,
at the start of the list.
%\begin{code}
%-- hSkewRemove :: (HSkewUpdate l e (ListRecord (HTree (Node e t t') ': ts)) (ListRecord ts'), HSkewTail ts' ts'') => Sing l -> ListRecord (HTree (Node e t t') ': ts) -> ListRecord ts''
%-- hSkewRemove l (H (HNode e t t') ts) =
%    -- hSkewTail $
%    -- hSkewUpdate l e (HNode e t t' `HCons` ts)
%\end{code}

%% $ fix emacs color highlighting

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Efficiency}\label{sec:efficiency}

In order to chose the best implementation in practice and as a sanity check,
we did some synthetic benchmarks of the code.
We compile and run the programs in a 4 core 2.2 Ghz second genertion (Sandy Bridge) Intel i7 MacBook Pro Notebook with 8 GB of RAM.
We use GHC version 7.6.1 64 bits under OS X 10.8 Mountain Lion.

We time accessing the last of an increasing number of fields.
The program constructs the list once
and runs a 10 million iteration lookup loop,
taking the necessary precautions to avoid the compiler
exploiting the language lazyness to optimize out all our code.
Run time comparisons are shown in Figure~\ref{run_time}.

\begin{figure}[b]
\begin{center}
\begin{tikzpicture}[x=0.027cm,y=0.16875cm]

  \def\xmin{0}
  \def\xmax{200}
  \def\ymin{0}
  \def\ymax{26}

  % grid
  \draw[style=help lines, xstep=12.5, ystep=2] (\xmin,\ymin) grid
  (\xmax,\ymax);

  % axes
  \draw[->] (\xmin,\ymin) -- (\xmax,\ymin) node[right] {field count};
  \draw[->] (\xmin,\ymin) -- (\xmin,\ymax) node[above] {time (s)};

  % xticks and yticks
  \foreach \x in {25,50,...,\xmax}
  \node at (\x, \ymin) [below] {\x};
  \foreach \y in {2,4,...,\ymax}
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
  \node[right,red] at (200, 25) {HListRecord};


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
  \node[right,green] at (200, 4) {ArrayRecord};

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
  \node[right,blue] at (200, 2) {SkewRecord};

\end{tikzpicture}
\end{center}
\caption{Lookup: run time}
\label{run_time}
\end{figure}

Note how in practice |ArrayRecord| and |SkewRecord| take the same time no matter the length of the record.
Actually, sometimes larger records run faster than smaller records for |SkewRecord|.
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

\begin{figure}[t]
\begin{center}
\begin{tikzpicture}[x=0.0135cm,y=0.3375cm]

  \def\xmin{0}
  \def\xmax{400}
  \def\ymin{0}
  \def\ymax{15}

  % grid
  \draw[style=help lines, xstep=25, ystep=1] (\xmin,\ymin) grid
  (\xmax,\ymax);

  % axes
  \draw[->] (\xmin,\ymin) -- (\xmax,\ymin) node[right] {field count};
  \draw[->] (\xmin,\ymin) -- (\xmin,\ymax) node[above] {time (s)};

  % xticks and yticks
  \foreach \x in {50,100,...,\xmax}
  \node at (\x, \ymin) [below] {\x};
  \foreach \y in {2,4,...,\ymax}
  \node at (\xmin,\y) [left] {\y};

  \draw[green] plot coordinates {
    (0,   0.08)
    (25,  0.53)
    (50,  1.01)
    (75,  1.53)
    (100, 2.04)
    (125, 2.73)
    (150, 3.39)
    (175, 4.28)
    (200, 5.40)
    (225, 5.94)
    (250, 6.67)
    (275, 7.81)
    (300, 8.88)
    (325, 9.83)
    (350,10.75)
    (375,11.60)
    (400,12.71)
  };
  \node[right,green] at (400, 13) {ArrayRecord};

\end{tikzpicture}
\end{center}
\caption{Extend: run time}
\label{extend_time}
\end{figure}


Next, Figure~\ref{extend_time} shows the runtime of inserting one more
field to a record of a given length.
To force the worst case for |ArrayRecord|, we disable the insertion optimization by immediately looking up the field just inserted.
The insert-lookup process is run one million times.
Only |ArrayRecord| is graphed because the other alternatives are too fast in this case.
The graph exposes the linear time behavior of |ArrayRecord|, its Achilles' heel.
However, we do not expect real life applications to fall in this case.
In general, multiple adjacent insertions preceding a lookup would be the common case.

\begin{figure}[b]
\begin{center}
\begin{tikzpicture}[x=0.03375cm,y=0.045cm]

  \def\xmin{0}
  \def\xmax{150}
  \def\ymin{0}
  \def\ymax{90}

  % grid
  \draw[style=help lines, xstep=10, ystep=7.5] (\xmin,\ymin) grid
  (\xmax,\ymax);

  % axes
  \draw[->] (\xmin,\ymin) -- (\xmax,\ymin) node[right] {field count};
  \draw[->] (\xmin,\ymin) -- (\xmin,\ymax) node[above] {time (s)};

  % xticks and yticks
  \foreach \x in {20,40,...,\xmax}
  \node at (\x, \ymin) [below] {\x};
  \foreach \y in {15,30,...,\ymax}
  \node at (\xmin,\y) [left] {\y};

  \draw[red] plot coordinates {
    (0,   0.04)
    (25,  0.59)
    (50,  2.89)
    (75,  4.31)
    (100, 7.46)
    (125, 10.5)
    (150, 18.6)
  };
  \node[right,red] at (150, 19) {HListRecord};

  \draw[green] plot coordinates {
    (0,   0.75)
    (25,  7.45)
    (50,  15.3)
    (75,  30.5)
    (100, 49.6)
    (125, 67.7)
    (150, 86.7)
  };
  \node[right,green] at (150, 87) {ArrayRecord};

  \draw[blue] plot coordinates {
    (0,   0.049)
    (25,  0.096)
    (50,  0.11)
    (75,  0.11)
    (100, 0.10)
    (125, 0.10)
    (150, 0.12)
  };
  \node[right,blue] at (150, 7) {SkewRecord};

\end{tikzpicture}
\end{center}
\caption{Update: run time}
\label{update_time}
\end{figure}


For Figure~\ref{update_time} we compared updating the first and deepest element
in each implementation.  As expected, |SkewRecord| is negligible.
|HListRecord| is a linear graph picking up somewhat probably after the CPU cache effects begins
to play a role.  |ArrayRecord| is also linear but much slower.


Figure~\ref{compile_time} shows how compile time for the three implementations grows.
|SkewRecord| is twice as slow as |HList| records, and |ArrayRecord| falls in between.
%if False
In previous versions of this paper that run the benchmarks with GHC version 7.4,
|SkewRecord| was comparatively much slower and we had to advise against it for
debugging and development, which require rapid turn around.
%endif
When insertion is rare, we prefer |ArrayRecord| because of the compile time speed.
Otherwise, |SkewRecord| is the best choice

\begin{figure}[t]
\begin{center}
\begin{tikzpicture}[x=0.015cm,y=0.135cm]

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
  \node[right,red] at (400, 10) {HListRecord};

  \draw[green] plot coordinates {
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
  \node[right,green] at (400,15) {ArrayRecord};

  \draw[blue] plot coordinates {
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
  \node[right,blue] at (400,30) {SkewRecord};

\end{tikzpicture}
\end{center}
\caption{Lookup: compile time}
\label{compile_time}
\end{figure}


\section{Conclusions and Future Work}\label{sec:conclusions}

Using type level programming techniques we developed two
new implementations of extensible records for Haskell:
An array-like implementation, with constant time search and linear time insertion, and an impementation based on balanced trees that takes logarithmic time for searching and removing elements and constant time for
inserting elements. This run time performance is achieved by moving
most of the effort to compile time.

In the actual implementations we follow \cite{Leijen:scopedlabels} in allowing label repetition.
A type-predicate |HLabelSet| can be added to disallow this as in \cite{KLS04}, with a slight cost in clarity but no cost in run time performance.

This approach can be used to improve the performance of systems
that make extensive use of extensible records.
Some examples of such systems are the first-class attribute grammars library AspectAG \cite{FlyFirstClass},
the OOHaskell \cite{OOHaskell} library for object-oriented functional programming,
or libraries for relational databases such as CoddFish \cite{SV06} and HaskellDB \cite{haskelldb}.

Although the paper was focused on showing more efficient implementations of extensible records,
our aim was mainly to show how harnessing type level programming techniques it is possible
to improve the run time performance of some operations by moving certain computations to compile time.
Type level programming is commonly used to increase the expressivity and type safety of programs,
but in this paper we showed it can also be helpful for efficiency matters.
This is the case specially for type level programming in Haskell,
where there exists a phase distinction between compile and run time;
types are computed at compile time while values are computed at run time.

Interesting future work is to find a way to reduce compilation time.
Experiments demonstrate that GHC memoizes class instances,
but some particularity of our instances seem to confuse the mechanism.
\cite{PerfLeaks} suggests constraint reordering and striving for tail calls to improve
performance.
It did not work for us and it made the presentation less clear, so we went with the straightforward version.

To improve performance, the code can be rewritten with type families.
The main reason why we based our development on functional dependencies is the lack of overlapping instances at type families.
In case further investigation on type families solves this problem we would be able to rephrase our implementation
in terms of type families with a trivial translation, achieving a more functional style implementation.

An interesting aspect of the proposed approach to extensible records is that it can be encoded as a Haskell library,
using only nowadays established extensions implemented for example in current versions of GHC.
However, better performance could be achieved if our approach is developed as a built-in implementation in a compiler.
In that case, the |ArrayRecord| solution reduces to the standard tuple-based techniques \cite{Gaster96apolymorphic}.
On the other hand, |SkewRecord| provides a novel encoding with fast lookup and insertion that would preserve its advantages
even as a built-in solution.

%\bibliographystyle{plainnat}

%\begin{flushleft}
%\bibliography{biblio}
%\end{flushleft}

% \appendix

%%% Local Variables: **
%%% mode: latex **
%%% TeX-command-default: "LiterateHaskell" **
%%% TeX-master: t **
%%% TeX-default-extension: "lhs" **
%%% TeX-region: "_region_" **
%%% End: **

\begin{code}
main =
    print lastListSing >>
    print lastListClass >>
    print lastListClassCore >>
    print lastArray >>
    print lastSkewSing >>
    print lastSkewClass >>
    return ()

hListUpdate a = undefined
hListRemove = undefined
\end{code}