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
    In this talk I'll show you how to speed up extensible records.\\
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
    The simplest implementation for records is association list.\\
    A linked list has a node for each field.\\
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
    data HNil = HNil
    data HCons e l = HCons e l

    newtype Field l v   =   Field { value :: v }
    (.=.)               ::  l -> v -> Field l v
    _  .=.  v           =   Field v
  \end{code}
  \note{
    HList is an implementation of association lists for records\\
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
    In the latest GHC, promoted literals allows us to omit the label declarations.\\
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
    Simple tuples and most record implementations use arrays instead of lists.\\
    Holding the fields contiguous in memory\\
    achieves the fastest selection, in constant time.\\
    Slide.\\
    But to extend the record,\\
    we have to copy the old fields and the new one to a new array.\\
    The new array does not reuse any part of the old.\\
    .\\
    While the compiler can coalesce several extensions\\
    and only create the array when the record is handled to other code,\\
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
    and cast back to the value.\\
    .\\
    The trick is that coming up with the index is a compile time operation.\\
    Thanks to compiler inlining, the index is just a constant,\\
    so selection is constant time.\\
    Casting to and from Any has no runtime cost.\\
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
    Naturally, the data structure that sits in the middle of this trade off is the search tree.\\
    Both selection and extension become log time.\\
    Compared to a list, extension is much better and selection only a little worse.\\
    While the immutable nature of values force us to copy arrays,\\
    it also enables us to reuse most nodes of the tree of the old record in the new one.\\
    Search trees require an ordering on the labels.\\
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
    But don't abandon trees entirely.\\
    For records, the keys, the labels, are known statically and\\
    erased from the runtime representation.\\
    When the compiler runs,\\
    the type checker already verified the presence of needed labels.\\
    Not only that, but the compiler already knows where the field is.\\
    These are the two traversals in the picture,\\
    for an unordered, dumb, tree.\\
    The lookup visits all nodes in the tree, but at compile time.\\
    The selection itself only visits the nodes in the tree path to the field.\\
    You can think of this as a form of partial evaluation.\\
  }
\end{frame}

\begin{frame}
  \frametitle{Skew lists}
    \only<1>{
      \begin{tikzpicture}
        \node (0) [circle,draw=blue] {0};
      \end{tikzpicture}
    }
    \only<2>{
      \begin{tikzpicture}
        \node (1) [circle,draw=blue] {1};
        \node (0) [circle,right=of 1,draw=black] {0};
      \end{tikzpicture}
    }
    \only<3>{
      \begin{tikzpicture}
        \node (2) [circle,draw=blue] {2}
          child {node[circle,draw=black] {1}}
          child {node[circle,draw=black] {0}}
        ;
      \end{tikzpicture}
    }
    \only<4>{
      \begin{tikzpicture}
        \node (3) [circle,draw=blue] {3};
        \node (2) [circle,right=of 3,draw=black] {2}
          child {node[circle,draw=black] {1}}
          child {node[circle,draw=black] {0}}
        ;
      \end{tikzpicture}
    }
    \only<5>{
      \begin{tikzpicture}
        \node (4) [circle,draw=blue] {4};
        \node (3) [circle,right=of 4,draw=black] {3};
        \node (2) [circle,right=of 3,draw=black] {2}
          child {node[circle,draw=black] {1}}
          child {node[circle,draw=black] {0}}
        ;
      \end{tikzpicture}
    }
    \only<6>{
      \begin{tikzpicture} [level 1/.style={sibling distance=8mm}]
        \node (5) [circle,draw=blue] {5}
          child {node (4) [circle,draw=black] {4}}
          child {node (3) [circle,draw=black] {3}}
        ;
        \node (2) [circle,right=of 5,draw=black] {2}
          child {node[circle,draw=black] {1}}
          child {node[circle,draw=black] {0}}
        ;
      \end{tikzpicture}
    }
    \only<7>{
      \begin{tikzpicture} [level 2/.style={sibling distance=8mm}]
        \node (6) [circle,draw=blue] {6}
          child {node (5) [circle,draw=black] {5}
            child {node (4) [circle,draw=black] {4}}
            child {node (3) [circle,draw=black] {3}}
            }
          child {node (2) [circle,right=of 5,draw=black] {2}
            child {node[circle,draw=black] {1}}
            child {node[circle,draw=black] {0}}
            };
      \end{tikzpicture}
    }
  \note{
    Free from the requirement of keeping the nodes ordered,\\
    we can insert nodes in constant time,\\
    while still keeping trees shallow.\\
    This structure is called Skew List.\\
    It's built from a list of perfect trees,\\
    ordered by height.\\
    The list contains at most a pair of trees of the same height.\\
    Whenever the list starts with two trees of the same height,\\
    a new node becomes the parent of the old trees.\\
    Otherwise, the new node is just inserted at the beginning.\\
  }
\end{frame}

\begin{frame}
  \frametitle{Comparison}
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
    note how skew list dominates HList.\\
    Constant time extension makes Skew List a drop in replacement for HList.\\
    
  }
\end{frame}

\end{document}

