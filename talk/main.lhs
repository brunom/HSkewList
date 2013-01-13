\documentclass{beamer}
\usepackage{pgfpages}
\usepackage{tikz}
\usepackage{array}
\usetikzlibrary{arrows,decorations.pathmorphing,backgrounds,positioning,fit,petri}
\setbeamertemplate{navigation symbols}{} %remove navigation symbols

\setbeameroption{show notes on second screen}
%\setbeameroption{show only notes} % to count words
%100 words per minute * 25 minutes talk = 2500 words

%include lhs2TeX.fmt
%include polycode.fmt
%format . = "."

\begin{document}

\title{Fast Extensible Records}
\author{Bruno Mart\'inez \\ \texttt{brunom@@fing.edu.uy}}
\date[PEPM '13]{
  ACM SIGPLAN 2013 Workshop on \\
  Partial Evaluation and Program Manipulation
}

\begin{frame}
  \titlepage
  \note{
    Hi. I'm Bruno Martinez from Uruguay.\\
    In this talk I'll show you how to asymptotically speed up extensible records.\\
    .\\
    Asymptotic speed is important for applications where fields are numerous,\\
    such as attribute grammars, our original purpose,\\
    but also algebras and even module systems.\\
    .\\
    Our sample implementation is in Haskell\\
    but the idea works in other languages.\\
  }
\end{frame}

\begin{frame}
  \frametitle{Record types}
  \begin{code}
    me  ::  {name  ::  String,   surname  ::  String } 
    me  =   {name  =   "Bruno",  surname  =   "Martinez" }

    fullname p = p.name ++ " " ++ p.surname

    marcos = {name = "Marcos", surname = "Viera", phone = 555 }
  \end{code}
  \note{
    Record types enable you to use named fields\\
    without the inconvenience of defining the record in advance.\\
    Record construction is anonymous.\\
    .\\
    Here I'm defining the record 'me', \\
    including given name, family name and age,\\
    and the 'fullname' function that concatenates both parts of the name.\\
    .\\
    At compile time, the type checker disallows calling 'fullname'\\
    if 'name' or 'surname' is missing from the argument.\\
    However, calling 'fullname' on my coauthor 'Marcos' record,\\
    which includes a 'phone' field but not an 'age' field,\\
    does work.\\
    Extra fields are just ignored.\\
  }
\end{frame}

\begin{frame}
  \frametitle{Extension}
  \begin{code}
    me' = {spouse = "Analia" | me}

    marry s r = {spouse = s | r} 
  \end{code}
  \note{
    Extra fields can be added to existing records.\\
    The record is extended.\\
    Here I'm updating my data after getting married.\\
    All record values can be constructed from the empty record via extension.\\
    .\\
    Furthermore, extension is polymorphic.\\
    A field can be added to a record\\
    even if the record type is not fully known.\\
    The marry function is an example of polymorphic extension.\\
    It can add a spouse field to any record.\\
    .\\
    Extension does not destroy the old record.\\
    As all values in functional languages,\\
    records are persistent.\\
    The old version of the record is still available.\\
  }
\end{frame}

\begin{frame}
  \frametitle{Basic operations}
  \begin{code}
    selection    r.l
    extension    {l  =   x  | r}
    update       {l  :=  x  | r}
    rename       {l  <-  m  | r}
    restriction  {r - l}
  \end{code}
  \note{
    For completeness, here are the rest of the fundamental record operations.\\
    .\\
    We've already seen selection and extension.\\
    .\\
    Update takes a record already containing a label\\
    and replaces its value.\\
    The type of the value may change.\\
    .\\
    Rename replaces a field with a field of a new label but the same value.\\
    If the record system keeps the labels only at the type level,\\
    maybe as a phantom type,\\
    rename is free at runtime.\\
    .\\
    Finally, restriction removes the field of a certain label.\\
    .\\
    Today I'll talk only of selection and extension.\\
  }
\end{frame}

\begin{frame}
  \frametitle{List}
  \begin{tikzpicture}
    \node (name)    [rectangle,draw=black!50,fill=black!20,label=name] {Bruno};
    \node (surname) [rectangle,draw=black!50,fill=black!20,label=surname,right=of name] {Martinez};
    \draw [->,very thick] (name) -- (surname);
    \node (age)     [rectangle,draw=black!50,fill=black!20,label=age,right=of surname] {30};
    \draw [->,very thick] (surname) -- (age);
    \alert{\uncover<2>{
      \node (age_lookup) [below=of surname] {me.age};
      \draw [->] (age_lookup) to node {1} (name);
      \draw [->] (age_lookup) to node {2} (surname);
      \draw [->] (age_lookup) to node {3} (age);
    }}
    \alert{\uncover<3>{
      \node (spouse)     [rectangle,draw=black!50,fill=black!20,label=spouse,left=of name] {Analia};
      \draw [->,very thick] (spouse) -- (name);
    }}
  \end{tikzpicture}
  \note{
    The simplest implementation for records is an association list.\\
    An association list has a node for each field.\\
    Slide.\\
    To select a field, the list is traversed.\\
    All field nodes before the wanted one are visited.\\
    Slide.\\
    To extend the record,\\
    the new field is inserted at the beginning,\\
    reusing the old list.\\
    .\\
    So, selection is linear time,\\
    and insertion is constant time.\\
  }
\end{frame}

\begin{frame}
  \frametitle{HList}
  \begin{code}
    data HNil           =   HNil
    data HCons e l      =   HCons e l

    newtype Field l v   =   Field { value :: v }
    (.=.)               ::  l -> v -> Field l v
    _  .=.  v           =   Field v
  \end{code}
  \note{
    HList is an implementation of heterogeneous lists\\
    on top of common Haskell extensions.\\
    .\\
    The foundations of HList are data types HNil and HCons.\\
    The type of the empty list is different from the type of a non empty list,\\
    allowing us to statically distinguish them.\\
    The elements themselves can also have different types.\\
    .\\
    To build records, we'll fill HLists with Fields.\\
    The label of a field appears only in the left side of the type definition.\\
    It's a phantom type.\\
    The equals constructor takes a value-level label but keeps only it's type.
  }
\end{frame}

\begin{frame}
  \frametitle{HList example}
  \begin{code}
    data Name     = Name
    data Surname  = Surname
    data Age      = Age
    me =
      (Name     .=.  "Bruno"           )  `HCons` 
      (Surname  .=.  "Martinez"        )  `HCons` 
      (Age      .=.  30                )  `HCons`
      HNil
    me ::    HCons (Field Name String)
          (  HCons (Field Surname String)
          (  HCons (Field Age Int)
             HNil))
  \end{code}
  \note{
    Here's how our record looks like in HList.\\
    We start by defining types and constructors for the labels.\\
    In the latest GHC, promoted literals allows us to omit the label declarations.\\
    HNil is our empty record, and we use HCons to extend it 3 times,\\
    with fields built with the equals constructor.\\
    The type is similar to the ideal record type of previous slides.\\
    Note that the type encodes the length of the record and the label of fields.\\
  }
\end{frame}

\begin{frame}
  \frametitle{Array}
  \begin{tikzpicture}
    \tikzstyle{field}=[rectangle,node distance=0 cm,outer sep = 0pt]
    \node (name)    [field,draw=black!50,fill=black!20,label=name] {Bruno};
    \node (surname) [field,draw=black!50,fill=black!20,label=surname,right=of name] {Martinez};
    \node (age)     [field,draw=black!50,fill=black!20,label=age,right=of surname] {30};
    \only<1>{
      \node (age_lookup) [below=of surname] {me.age};
      \draw [->] (age_lookup) to node {1} (age);
    }
    \alert{\uncover<2>{
        \node (name2)    [field,draw=black!50,fill=black!20,label=name,node distance=2 cm,below=of name] {Bruno};
        \node (surname2) [field,draw=black!50,fill=black!20,label=surname,right=of name2] {Martinez};
        \node (age2)     [field,draw=black!50,fill=black!20,label=age,right=of surname2] {30};
        \node (spouse2)     [field,draw=black!50,fill=black!20,label=spouse,right=of age2] {Analia};
    }}
  \end{tikzpicture}
  \note{
    Native tuples and built-in records in other languages are usually arrays instead of lists.\\
    Holding the fields contiguously in memory\\
    achieves the fastest selection, in constant time.\\
    Slide.\\
    But to extend the record,\\
    we have to copy the old fields and the new one to a new array.\\
    The new array does not reuse any part of the old.\\
    Imperative languages get away with growable arrays and hash tables\\
    because the old version of the object need not be preserved.
    .\\
    While the compiler can coalesce several extensions\\
    and only create the array when the record escapes to other code,\\
    specially helpful when a record is first created,\\
    extension in the general case is linear time.\\
  }
\end{frame}

\begin{frame}
  \frametitle{HArray}
  \begin{code}
    data ArrayRecord r =
      ArrayRecord r (Array Int Any)
  \end{code}
  \note{
    If you can stomach hacks,\\
    constant time selection records can be encoded in today's Haskell.\\
    .\\
    The trick is pairing an HList with an Array of Anys.\\
    Any signals that unsafeCoerce is used to hide the actual type of values.\\
    To find a field, search in the HList for the index and the type of the value.\\
    Then subscript with the index in the array,\\
    and cast back to the correct type.\\
    .\\
    Calculating the index is a compile time operation.\\
    Thanks to compiler inlining, the index is just a constant.\\
    Casting to and from Any has no runtime cost.\\
    So selection is constant time.\\
  }
\end{frame}

\begin{frame}
  \frametitle{Search tree}
  \begin{tabular}{c||c||c}
    structure & selection & extension\\
    \hline
    list & O(n) & O(1)\\
    array & O(1) & O(n)\\
    search tree & O(log(n)) & O(log(n))\\
  \end{tabular}
  \note{
    So far we have a structure with fast extension, list, and a structure with fast selection, array.\\
    .\\
    Naturally, the dictionary implementation in the middle of this trade off is the search tree.\\
    Both selection and extension become log time.\\
    .\\
    But search trees require an ordering on the labels.\\
    This makes it difficult to retrofit search trees into HList,\\
    which uses phantom types as labels.\\
    Types aren't naturally ordered.\\
  }
\end{frame}

\begin{frame}
  \frametitle{Dumb trees}
  \begin{figure}[tp]
    \begin{center}
      \includegraphics[scale=0.75]{../search-skew.pdf}
    \end{center}
  \end{figure}
  \note{
    But don't abandon trees yet.\\
    Get rid of the ordering!\\
    .\\
    Because labels are static,\\
    when compiling a selection,\\
    the type checker already verified the presence of needed labels.\\
    Not only that, but the compiler already knows where the field is.\\
    .\\
    Selection is a two step process.\\
    The lookup visits all nodes in the tree, but only at compile time.\\
    The selection itself digs directly to the field.\\
    You can think of this as a form of partial evaluation or staged compilation.\\
  }
\end{frame}

\newcommand{\twice}[1]{#1 #1}

\begin{frame}
  \frametitle{Skew lists}
  \begin{tikzpicture}
    [
      every node/.style={node distance=2 cm, inner sep=0, outer sep=0},
      level 1/.style={sibling distance=8mm},
      level 2/.style={sibling distance=4mm},
      level 3/.style={sibling distance=2mm},
      level 4/.style={sibling distance=1mm},
      level 5/.style={sibling distance=0.5mm},
      level 6/.style={sibling distance=0.25mm},
      level 7/.style={sibling distance=0.125mm}
    ]
    \node (a) {}
    \twice{child{}};
    \node (b) [right=of a] {}
    \twice{child{}};
    \node (c) [right=of b] {}
    \twice{child{\twice{child{\twice{child{}}}}}};
    \node (d) [right=of c] {}
    \twice{child{\twice{child{\twice{child{\twice{child{}}}}}}}};
    \draw (a) -- (b) -- (c) -- (d);
  \end{tikzpicture}
  \note{
    Common balanced tree implementation such as red black trees support insertion in logarithmic time.\\
    But the extra flexibility of not needing to keep fields ordered allows us to use skew lists instead,\\
    a simple scheme that supports insertion in constant time.\\
    .\\
    A skew list is formed by a list of increasingly larger perfect binary trees.\\
    All nodes store items, not only leafs.\\
    Only the first two trees are allowed to be of the same size.\\
    In that case, a new node becomes the parent of the two old trees.\\
    Otherwise, the new node is just inserted at the beginning of the list.\\
    .\\
    The path to a node traverses the list (of logarithmic length)\\
    and down one tree (also logarithmic height),\\
    so selection is logarithmic.\\
  }
\end{frame}

\begin{frame}
  \frametitle{SkewList example}
  \only<1>{
    \begin{tikzpicture}
      \node (1) [circle,draw=red] {1};
    \end{tikzpicture}
  }
  \only<2>{
    \begin{tikzpicture} [level 1/.style={sibling distance=8mm}]
      \node (2) [circle,draw=red] {2};
      \node (1) [circle,right=of 2,draw=black] {1};
      \draw (2) -- (1);
    \end{tikzpicture}
  }
  \only<3>{
    \begin{tikzpicture} [level 1/.style={sibling distance=8mm}]
      \node (3) [circle,draw=red] {3}
      child {node[circle,draw=black] {2}}
      child {node[circle,draw=black] {1}}
      ;
    \end{tikzpicture}
  }
  \only<4>{
    \begin{tikzpicture} [level 1/.style={sibling distance=8mm}]
      \node (4) [circle,draw=red] {4};
      \node (3) [circle,right=of 4,draw=black] {3}
      child {node[circle,draw=black] {2}}
      child {node[circle,draw=black] {1}}
      ;
      \draw (4) -- (3);
    \end{tikzpicture}
  }
  \only<5>{
    \begin{tikzpicture} [level 1/.style={sibling distance=8mm}]
      \node (5) [circle,draw=red] {5};
      \node (4) [circle,right=of 5,draw=black] {4};
      \node (3) [circle,right=of 4,draw=black] {3}
      child {node[circle,draw=black] {2}}
      child {node[circle,draw=black] {1}}
      ;
      \draw (5) -- (4) -- (3);
    \end{tikzpicture}
  }
  \only<6>{
    \begin{tikzpicture} [level 1/.style={sibling distance=8mm}]
      \node (6) [circle,draw=red] {6}
      child {node (5) [circle,draw=black] {5}}
      child {node (4) [circle,draw=black] {4}}
      ;
      \node (3) [circle,right=of 6,draw=black] {3}
      child {node[circle,draw=black] {2}}
      child {node[circle,draw=black] {1}}
      ;
      \draw (6) -- (3);
    \end{tikzpicture}
  }
  \only<7>{
    \begin{tikzpicture} [level 2/.style={sibling distance=8mm}]
      \node (7) [circle,draw=red] {7}
      child {node (6) [circle,draw=black] {6}
        child {node (5) [circle,draw=black] {5}}
        child {node (4) [circle,draw=black] {4}}
      }
      child {node (3) [circle,right=of 6,draw=black] {3}
        child {node[circle,draw=black] {2}}
        child {node[circle,draw=black] {1}}
      };
    \end{tikzpicture}
  }
  \note{
    Let's see how small skew lists are built.\\
    Unlike search trees, all skew lists of a given size have the same shape.\\
    The first item is the only node of the only tree.\\
    Slide.\\
    The second item forms a new tree and comes first in the list.\\
    Slide.\\
    But to add item number 3 we remove the previous two fields and form a new tree.\\
    The fourth,\\
    Slide.\\
    and fifth items are just added at the beginning.\\
    Slide.\\
    And the sixth item builds a new tree just as the third did.\\
    Slide.\\
    Finally, item 7 becomes the root of the first tree of height two.\\
  }
\end{frame}

\begin{frame}
  \frametitle{HSkew}
  \begin{code}
    data  HNode  e  t  t'  =  HNode  e  t  t'
    data  HLeaf  e         =  HLeaf  e 

    me =
      HLeaf     (Name     .=.  "Bruno"           )   `HCons`
      HNode     (Surname  .=.  "Martinez"        )
        (HLeaf  (Age      .=.  30                ))
        (HLeaf  (Phone    .=.  555               ))  `HCons`
      HNil
  \end{code}
  \note{
    To use skew list at the type level we define HNode, for the case of nodes with children, and HLeaf.\\
    For the list spine of the skew list we just reuse HCond and HNil.\\
    A record with 4 fields is a list with a singleton tree followed by a 3 item tree.\\
    In the paper we define a function to extend skew list records of any size,\\
    but I won't bore you with it here.\\
  }
\end{frame}

\begin{frame}
  \frametitle{Conclusion}
  \begin{tabular}{c||c||c}
    structure & selection & extension\\
    \hline
    list & O(n) & O(1)\\
    array & O(1) & O(n)\\
    search tree & O(log(n)) & O(log(n))\\
    skew list & O(log(n)) & O(1)\\
  \end{tabular}
  \note{
    In the table updated with skew list,\\
    note how skew list asymptoticallydominates HList.\\
    It's better in all scenarios.\\
    Selection is faster even for small reco
    If you replace HList by skew list, no program will get slower.\\
    Try skew list records if you are using HList
    On the other hand, replacing HList by array may improve or not your run time.\\
  }
\end{frame}

\end{document}

