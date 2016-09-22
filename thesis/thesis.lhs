%let a4size     = False
%let thesissize = True

%if a4size
\documentclass
  [ a4paper, paper=a4, fontsize=10pt,
  , headings=normal, numbers=noenddot, headinclude, footinclude
  , listof=totoc, bibliography=totoc, index=totoc
  ]{scrbook}
\usepackage
  [ a4paper, ignorehead, ignorefoot, ignoremp, asymmetric   %% , showframe
  , left = 4.00cm, right  = 4.00cm
  , top  = 5.55cm, bottom = 5.55cm
  , headsep = 0.5cm, footskip = 1.0cm
  , marginparsep = 0.3cm, marginparwidth = 2cm
  ]{geometry}
%elif thesissize
\documentclass
  [ a4paper, paper=a4, fontsize=10pt,
  , headings=normal, numbers=noenddot, headinclude, footinclude
  , listof=totoc, bibliography=totoc, index=totoc, twoside, openright
  ]{scrbook}
\usepackage
  [ a4paper, ignorehead, ignorefoot, ignoremp, asymmetric   %% , showframe
  , twoside, pdftex
  , papersize={17cm,24cm}
  , left = 2.50cm, right  = 1.50cm
  , top  = 2.70cm, bottom = 2.70cm
  , headsep = 0.5cm, footskip = 1.0cm
  , marginparsep = 0.3cm, marginparwidth = 1.2cm
  ]{geometry}
%else
\documentclass
  [ a4paper, paper=a4, fontsize=10pt,
  , headings=normal, numbers=noenddot, headinclude, footinclude
  , listof=totoc, bibliography=totoc, index=totoc
  ]{scrbook}
\usepackage
  [ ignorehead, ignorefoot, ignoremp, asymmetric   %% , showframe
  , papersize={15.4cm,21.0cm}
  , left = 1.20cm, right  = 1.20cm
  , top  = 1.2cm, bottom = 1.2cm
  , headsep = 0.5cm, footskip = 1.0cm
  , marginparsep = 0.3cm, marginparwidth = 0.5cm
  ]{geometry}
%endif


\usepackage{listings}
\usepackage{color}

%include lhs2TeX.fmt
%include polycode.fmt

\makeatletter
\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\makeatother


%% OPTIONS TO CONTROL DRAFT AND FULL THESIS
%let singleChapter = False


%include configuration.lhs

\title{Fast Extensible Records In Haskell}
\author{Bruno Martinez Aguerre}

\begin{document}



%if singleChapter

%%% copy and paste a single chapter in this region

%{
%include frontpage.lhs
%}



%else

%%% actual chapters of the thesis


%{
%%include frontpage.lhs
%}

%{
%%include preface.lhs
%}

%%\tableofcontents

%{
%include records.lhs
%}

%{
%include paper.lhs
%}

%{
%%include conclusion/chap-conclusion.lhs
%}

%\appendix

%{
%%include oberon/app-oberon.lhs
%}

\bibliographystyle{apalike}
\bibliography{thesis}

%endif


\cleardoublepage

\end{document}
