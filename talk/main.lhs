\documentclass{beamer}
\usepackage{pgfpages}
\usepackage{tikz}
\usepackage{array}
\usetikzlibrary{arrows,decorations.pathmorphing,backgrounds,positioning,fit,petri}
\setbeamertemplate{navigation symbols}{} %remove navigation symbols

%\setbeameroption{show notes on second screen}
%\setbeameroption{show only notes} % to count words
%100 words per minute * 25 minutes talk = 2500 words

%include lhs2TeX.fmt
%include polycode.fmt
%format . = "."

\begin{document}

\title{Fast Extensible Records}
\author{
  Bruno Mart\'inez \\
  Instituto de Computacion \\
  Universidad de la Republica \\
  Montevideo - Uruguay \\
  \texttt{brunom@@fing.edu.uy}\\
  (joint work with Marcos Viera and Alberto Pardo)  
}
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
    Fast records are needed when fields are many.\\
    Our original application was attribute grammars.\\
    but it's also useful for algebras and module systems.\\
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
    .\\
    Here I'm defining the record 'me', \\
    including given name and family name,\\
    and the 'fullname' function that concatenates both.\\
    .\\
    The type checker disallows calling 'fullname'\\
    if 'name' or 'surname' is missing from the record argument.\\
    However, calling 'fullname' on my coauthor 'Marcos' record,\\
    which ads a 'phone' field does work.\\
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
    The operation is called extension.\\
    Here I'm updating my data after getting married.\\
    .\\
    Furthermore, extension is polymorphic.\\
    A field can be added to a record\\
    even if the record type is not fully known.\\
    The marry function is an example of polymorphic extension.\\
    It can add a spouse field to any record.\\
  }
\end{frame}

\begin{frame}
  \frametitle{Basic operations}
  \begin{code}
    selection    italy.capital
    extension    {airport      =   leonardo  | rome}
    update       {population   :=  2777979   | rome}
    rename       {wife         <-  spouse    | me}
    restriction  {me - spouse}
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
  \frametitle{Array}
  \begin{tikzpicture}
    \tikzstyle{field}=[rectangle,node distance=0 cm,outer sep = 0pt]
    \node (name)    [field,draw=black!50,fill=black!20,label=name] {Bruno};
    \node (surname) [field,draw=black!50,fill=black!20,label=surname,right=of name] {Martinez};
    \node (age)     [field,draw=black!50,fill=black!20,label=age,right=of surname] {30};
    \alert{\uncover<2>{
      \node (age_lookup) [below=of surname] {me.age};
      \draw [->] (age_lookup) to node {1} (age);
    }}
    \alert{\uncover<3>{
        \node (name2)    [field,draw=black!50,fill=black!20,label=name,node distance=2 cm,below=of name] {Bruno};
        \node (surname2) [field,draw=black!50,fill=black!20,label=surname,right=of name2] {Martinez};
        \node (age2)     [field,draw=black!50,fill=black!20,label=age,right=of surname2] {30};
        \node (spouse2)     [field,draw=black!50,fill=black!20,label=spouse,right=of age2] {Analia};
    }}
  \end{tikzpicture}
  \note{
    The most compact representation for records is an array of fields.\\
    This is the implementation that native tuples use.\\
    Slide.\\
    The compiler keeps track of the index of each field,\\
    so selection is just array indexing and constant time.\\
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
    The other simple implementation for records is an association list.\\
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

    class HListGet r l v | r l -> v where
      hListGet :: r -> l -> v

    instance
      (  HEq l l' True
      ,  HListGet' b v' r' l v) =>
      HListGet (HCons (Field l' v') r') l v where
      hListGet (HCons (Field v) _) l = v

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
    Common balanced tree implementations such as red black trees\\
    support insertion in logarithmic time.\\
    But the extra flexibility of not needing to keep fields ordered\\
    enables us to use skew lists instead,\\
    a simple scheme that supports insertion in constant time.\\
    .\\
    A skew list is formed by a list of increasingly larger perfect binary trees.\\
    Items are stored in the internal nodes of the trees.\\
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

    class HSkewExtend f r r' | f r -> r'
      where hSkewExtend :: f -> r -> r'
  \end{code}
  \note{
    To use skew list at the type level we define HNode, for the case of nodes with children, and HLeaf.\\
    For the list spine of the skew list we just reuse HCons and HNil.\\
    A record with 4 fields is a list with a singleton tree followed by a 3 item tree.\\
    In the paper we define a function to extend skew list records of any size,\\
    but I won't bore you with it here.\\
  }
\end{frame}

\begin{frame}
  \frametitle{Lookup graph}
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
    \node[right,red] at (200, 25) {List};
    
    
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
    \node[right,green] at (200, 4) {Array};
    
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
    \node[right,blue] at (200, 2) {Skew};
  \end{tikzpicture}
  \note{
    We run an experiment to confirm that Skew List really leads to fast records.\\
    The graph shows how lookup time grows as the number of fields increases.\\
    We see that simple lists are linear time\\
    and that both array and skew list based record are way faster.\\
    If you pay attention, the skew list graph sometimes dips a little.\\
    This behavior is cause by consolidation of the trees at certain field counts.\\
  }
\end{frame}

\begin{frame}
  \frametitle{The catch}
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
    \node[right,red] at (400, 10) {List};
    
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
    \node[right,green] at (400,15) {Array};
    
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
    \node[right,blue] at (400,30) {Skew};
  \end{tikzpicture}
  \note{
    Preparing the test programs of the previous slide,\\
    we run into the greatest practical limitation of library records:\\
    compile times.\\
    The compile time is superlineal for all approaches.\\
    GHC 7.6 is faster than 7.4.\\
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
    This is the updated time table.\\
    Note how skew list dominates HList.\\
    The only downside is that it needs a little more memory.\\
    Try skew list records if you are using HList.\\
  }
\end{frame}

\end{document}

