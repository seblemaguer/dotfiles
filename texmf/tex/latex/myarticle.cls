\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{myarticle}[2014/08/16 HY Letter Class]
\LoadClass[a4paper,11pt]{article}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The Legrand Orange Book
% Structural Definitions File
% Version 2.1 (26/09/2018)
%
% Original author:
% Mathias Legrand (legrand.mathias@gmail.com) with modifications by:
% Vel (vel@latextemplates.com)
%
% This file was downloaded from:
% http://www.LaTeXTemplates.com
%
% License:
% CC BY-NC-SA 3.0 (http://creativecommons.org/licenses/by-nc-sa/3.0/)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ----------------------------------------------------------------------------------------
%	VARIOUS REQUIRED PACKAGES AND CONFIGURATIONS
% ----------------------------------------------------------------------------------------

\usepackage{graphicx} % Required for including pictures
\usepackage{svg}

\usepackage[svgnames,x11names,table]{xcolor}
\definecolor{gmitblue}{RGB}{93,138,168}
\definecolor{LightSkyBlue}{RGB}{135, 206, 250}
\definecolor{dkgreen}{RGB}{0, 100, 0}


\usepackage{lipsum} % Inserts dummy text

\usepackage{tikz} % Required for drawing custom shapes
\usetikzlibrary{shapes,shadows,calc}

\usepackage[english]{babel} % English language/hyphenation

\usepackage{enumitem} % Customize lists
\setlist{nolistsep} % Reduce spacing between bullet points and numbered lists
% Redefine the description environment
\setlist[description]{
  font=\color{gmitblue}\bfseries, % Automatically set font style and color for labels
  labelwidth=2cm,
  labelsep=0.5cm, % Space between label and description text
  leftmargin=3cm, % Indentation of the entire description list
  itemsep=0.7em, % Space between items
}


\usepackage{booktabs} % Required for nicer horizontal rules in tables

% ----------------------------------------------------------------------------------------
%	MARGINS
% ----------------------------------------------------------------------------------------

\usepackage{geometry} % Required for adjusting page dimensions and margins

\geometry{
    paper=a4paper, % Paper size, change to letterpaper for US letter size
    top=3cm, % Top margin
    bottom=3cm, % Bottom margin
    left=3cm, % Left margin
    right=3cm, % Right margin
    headheight=14pt, % Header height
    footskip=1.4cm, % Space from the bottom margin to the baseline of the footer
    headsep=10pt, % Space from the top margin to the baseline of the header
    % showframe, % Uncomment to show how the type block is set on the page
}


% ----------------------------------------------------------------------------------------
%	FONTS
% ----------------------------------------------------------------------------------------

\usepackage{avant} % Use the Avantgarde font for headings
\usepackage{mathptmx} % Use the Adobe Times Roman as the default text font together with math symbols from the Sym­bol, Chancery and Com­puter Modern fonts

\usepackage{microtype} % Slightly tweak font spacing for aesthetics
\usepackage[utf8]{inputenc} % Required for including letters with accents
\usepackage[T1]{fontenc} % Use 8-bit encoding that has 256 glyphs


% ----------------------------------------------------------------------------------------
%	HEADERS AND FOOTERS
% ----------------------------------------------------------------------------------------
\usepackage{lastpage}
\usepackage{fancyhdr}
\usetikzlibrary{calc}
\renewcommand{\headrulewidth}{0pt}

\fancypagestyle{plain}{
    \fancyhf{}
    \fancyfoot[C]{
        \begin{tikzpicture}[overlay, remember picture]%
            \fill[gmitblue] (current page.south west) rectangle ($(current page.south east)+(0,.5in)$);
            \node[anchor=south, text=white, font=\large\bf, minimum size=.5in] at (current page.south) {Page \thepage\ of \pageref*{LastPage}};
        \end{tikzpicture}
    }
}
\fancypagestyle{fancy}{
    \fancyhf{}
    \fancyfoot[C]{
        \begin{tikzpicture}[overlay, remember picture]%
            \fill[gmitblue] (current page.south west) rectangle ($(current page.south east)+(0,.5in)$);
            \node[anchor=south, text=white, font=\large\bf, minimum size=.5in] at (current page.south) {Page \thepage\ of \pageref*{LastPage}};
        \end{tikzpicture}
    }
}
\pagestyle{fancy}
\setlength{\headheight}{12pt}

% ----------------------------------------------------------------------------------------
%	THEOREM STYLES
% ----------------------------------------------------------------------------------------

\usepackage{amsmath,amsfonts,amssymb,amsthm} % For math equations, theorems, symbols, etc

\newcommand{\intoo}[2]{\mathopen{]}#1\,;#2\mathclose{[}}
\newcommand{\ud}{\mathop{\mathrm{{}d}}\mathopen{}}
\newcommand{\intff}[2]{\mathopen{[}#1\,;#2\mathclose{]}}
\renewcommand{\qedsymbol}{$\blacksquare$}
\newtheorem{notation}{Notation}[section]

% Boxed/framed environments
\makeatletter
\newtheoremstyle{gmitbluenumbox}% Theorem style name
{0pt}% Space above
{0pt}% Space below
{\normalfont}% Body font
{}% Indent amount
{\small\bf\sffamily\color{gmitblue}}% Theorem head font
{\;}% Punctuation after theorem head
{0.25em}% Space after theorem head
{\small\sffamily\color{gmitblue}\thmname{#1}\nobreakspace\thmnumber{\@ifnotempty{#1}{}\@upn{#2}}% Theorem text (e.g. Theorem 2.1)
    \thmnote{\nobreakspace\the\thm@notefont\sffamily\bfseries\color{black}---\nobreakspace#3.}} % Optional theorem note

\newtheoremstyle{blacknumex}% Theorem style name
{5pt}% Space above
{5pt}% Space below
{\normalfont}% Body font
{} % Indent amount
{\small\bf\sffamily}% Theorem head font
{\;}% Punctuation after theorem head
{0.25em}% Space after theorem head
{\small\sffamily{\tiny\ensuremath{\blacksquare}}\nobreakspace\thmname{#1}\nobreakspace\thmnumber{\@ifnotempty{#1}{}\@upn{#2}}% Theorem text (e.g. Theorem 2.1)
    \thmnote{\nobreakspace\the\thm@notefont\sffamily\bfseries---\nobreakspace#3.}}% Optional theorem note

\newtheoremstyle{blacknumbox} % Theorem style name
{0pt}% Space above
{0pt}% Space below
{\normalfont}% Body font
{}% Indent amount
{\small\bf\sffamily}% Theorem head font
{\;}% Punctuation after theorem head
{0.25em}% Space after theorem head
{\small\sffamily\thmname{#1}\nobreakspace\thmnumber{\@ifnotempty{#1}{}\@upn{#2}}% Theorem text (e.g. Theorem 2.1)
    \thmnote{\nobreakspace\the\thm@notefont\sffamily\bfseries---\nobreakspace#3.}}% Optional theorem note

% Non-boxed/non-framed environments
\newtheoremstyle{gmitbluenum}% Theorem style name
{5pt}% Space above
{5pt}% Space below
{\normalfont}% Body font
{}% Indent amount
{\small\bf\sffamily\color{gmitblue}}% Theorem head font
{\;}% Punctuation after theorem head
{0.25em}% Space after theorem head
{\small\sffamily\color{gmitblue}\thmname{#1}\nobreakspace\thmnumber{\@ifnotempty{#1}{}\@upn{#2}}% Theorem text (e.g. Theorem 2.1)
    \thmnote{\nobreakspace\the\thm@notefont\sffamily\bfseries\color{black}---\nobreakspace#3.}} % Optional theorem note
\makeatother

% Defines the theorem text style for each type of theorem to one of the three styles above
\newcounter{dummy}
\numberwithin{dummy}{section}
\theoremstyle{gmitbluenumbox}
\newtheorem{theoremeT}[dummy]{Theorem}
\newtheorem{problem}{Problem}[section]
\newtheorem{exerciseT}{Exercise}[section]

\theoremstyle{blacknumex}
\newtheorem{exampleT}{Example}[section]

\theoremstyle{blacknumbox}
\newtheorem{vocabulary}{Vocabulary}[section]
\newtheorem{definitionT}{Definition}[section]
\newtheorem{corollaryT}[dummy]{Corollary}

\theoremstyle{gmitbluenum}
\newtheorem{proposition}[dummy]{Proposition}

% ----------------------------------------------------------------------------------------
%	DEFINITION OF COLORED BOXES
% ----------------------------------------------------------------------------------------

\usepackage[framemethod=default]{mdframed} % Required for creating the theorem, definition, exercise and corollary boxes

% Theorem box
\newmdenv[skipabove=7pt,
skipbelow=7pt,
backgroundcolor=black!5,
linecolor=gmitblue,
innerleftmargin=5pt,
innerrightmargin=5pt,
innertopmargin=5pt,
leftmargin=0cm,
rightmargin=0cm,
innerbottommargin=5pt]{tBox}

% Exercise box
\newmdenv[skipabove=7pt,
skipbelow=7pt,
rightline=false,
leftline=true,
topline=false,
bottomline=false,
backgroundcolor=gmitblue!10,
linecolor=gmitblue,
innerleftmargin=5pt,
innerrightmargin=5pt,
innertopmargin=5pt,
innerbottommargin=5pt,
leftmargin=0cm,
rightmargin=0cm,
linewidth=4pt]{eBox}

% Definition box
\newmdenv[skipabove=7pt,
skipbelow=7pt,
rightline=false,
leftline=true,
topline=false,
bottomline=false,
linecolor=gmitblue,
innerleftmargin=5pt,
innerrightmargin=5pt,
innertopmargin=0pt,
leftmargin=0cm,
rightmargin=0cm,
linewidth=4pt,
innerbottommargin=0pt]{dBox}

% Corollary box
\newmdenv[skipabove=7pt,
skipbelow=7pt,
rightline=false,
leftline=true,
topline=false,
bottomline=false,
linecolor=gray,
backgroundcolor=black!5,
innerleftmargin=5pt,
innerrightmargin=5pt,
innertopmargin=5pt,
leftmargin=0cm,
rightmargin=0cm,
linewidth=4pt,
innerbottommargin=5pt]{cBox}

% Creates an environment for each type of theorem and assigns it a theorem text style from the "Theorem Styles" section above and a colored box from above
\newenvironment{theorem}{\begin{tBox}\begin{theoremeT}}{\end{theoremeT}\end{tBox}}
\newenvironment{exercise}{\begin{eBox}\begin{exerciseT}}{\hfill{\color{gmitblue}\tiny\ensuremath{\blacksquare}}\end{exerciseT}\end{eBox}}
\newenvironment{definition}{\begin{dBox}\begin{definitionT}}{\end{definitionT}\end{dBox}}
\newenvironment{example}{\begin{exampleT}}{\hfill{\tiny\ensuremath{\blacksquare}}\end{exampleT}}
\newenvironment{corollary}{\begin{cBox}\begin{corollaryT}}{\end{corollaryT}\end{cBox}}

\surroundwithmdframed[
  topline=false,
  bottomline=false,
  backgroundcolor=gmitblue!10,
  rightline=false,
  linecolor=gmitblue,
  linewidth=3pt,
]{quote}

% ----------------------------------------------------------------------------------------
%	REMARK ENVIRONMENT
% ----------------------------------------------------------------------------------------

\newenvironment{remark}{\par\vspace{10pt}\small % Vertical white space above the remark and smaller font size
    \begin{list}{}{
            \leftmargin=35pt % Indentation on the left
            \rightmargin=25pt}\item\ignorespaces % Indentation on the right
        \makebox[-2.5pt]{\begin{tikzpicture}[overlay]
                \node[draw=gmitblue!60,line width=1pt,circle,fill=gmitblue!25,font=\sffamily\bfseries,inner sep=2pt,outer sep=0pt] at (-15pt,0pt){\textcolor{gmitblue}{R}};\end{tikzpicture}} % Orange R in a circle
        \advance\baselineskip -1pt}{\end{list}\vskip5pt} % Tighter line spacing and white space after remark


% ----------------------------------------------------------------------------------------
%	ARTICLE TITLE
% ----------------------------------------------------------------------------------------
\makeatletter
\def\@maketitle{%
    \newpage
    \begin{tikzpicture}[remember picture,overlay]
        \node[yshift=-3cm] at (current page.north)
        {
            \begin{tikzpicture}[remember picture, overlay]
                % Title
                \node[%
                   % anchor=west, xshift=1cm, yshift=-1cm,
                   rectangle, rounded corners=10pt, inner sep=10pt,
                   line width=1mm, draw=gmitblue, fill=gmitblue!10
                ]
                {
                    \begin{minipage}[t]{\linewidth}
                        \begin{center}
                            {\LARGE\bfseries\@title\par}
                            \vskip 1em
                            {\large\@author}
                        \end{center}
                    \end{minipage}
                };
            \end{tikzpicture}
        };
    \end{tikzpicture}
    \vspace{5mm}
}
\makeatother


% ----------------------------------------------------------------------------------------
% SECTION HEADINGS
% ----------------------------------------------------------------------------------------
\usepackage[explicit]{titlesec} % Customize section titles

\makeatletter
\renewcommand{\@seccntformat}[1]{\llap{\textcolor{gmitblue}{\csname the#1\endcsname}\hspace{1em}}}
\renewcommand{\section}{\@startsection{section}{1}{\z@}
    {-4ex \@plus -1ex \@minus -.4ex}
    {1ex \@plus.2ex }
    {\normalfont\large\sffamily\bfseries}}
\renewcommand{\subsection}{\@startsection {subsection}{2}{\z@}
    {-3ex \@plus -0.1ex \@minus -.4ex}
    {0.5ex \@plus.2ex }
    {\normalfont\sffamily\bfseries}}
\renewcommand{\subsubsection}{\@startsection {subsubsection}{3}{\z@}
    {-2ex \@plus -0.1ex \@minus -.2ex}
    {.2ex \@plus.2ex }
    {\normalfont\small\sffamily\bfseries}}
\renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}
    {-2ex \@plus-.2ex \@minus .2ex}
    {.1ex}
    {\normalfont\small\sffamily\bfseries}}


% ----------------------------------------------------------------------------------------
%	Listings
% ----------------------------------------------------------------------------------------

\usepackage{listings}
\lstset{
    % Colors
    backgroundcolor=\color{gray!10},   % choose the background color. You must add \usepackage{color}
    commentstyle=\color{dkgreen},      % comment style
    stringstyle=\color{dkgreen},       % string style
    keywordstyle=\color{gmitblue},     % keyword style
    basewidth={0.5em,0.4em},
    basicstyle=\ttfamily\small,        % the size of the fonts that are used for the code
    breakatwhitespace=false,           % sets if automatic breaks should only happen at whitespace
    breaklines=true,                   % sets automatic line breaking
    captionpos=b,                      % sets the caption-position to bottom
    columns=fullflexible,
    linewidth=\textwidth,
    frame=tb,                          % adds a framearound the code
    numbers=left,
    showspaces=false,                  % show spaces adding particular underscores
    showstringspaces=false,            % underline spaces within strings
    showtabs=false,                    % show tabs within strings adding particular underscores
    tabsize=2,                         % sets default tabsize to 2 spaces
    aboveskip=20pt,                    % sets the space above the listing
    belowskip=20pt                     % sets the space below the listing
}

\lstdefinelanguage{dockerfile}{
    keywords={%
        FROM, RUN, COPY, ADD, ENTRYPOINT, CMD,  ENV, ARG, WORKDIR,%
        EXPOSE, LABEL, USER, VOLUME, STOPSIGNAL, ONBUILD, MAINTAINER
    },
    keywordstyle=\color{blue}\bfseries,
    identifierstyle=\color{black},
    sensitive=false,
    comment=[l]{\#},
    morestring=[b]',
    morestring=[b]"
}

\lstdefinelanguage{bibtex}
{
    keywords={%
        @article,@book,@collectedbook,@conference,@electronic,@ieeetranbstctl,%
        @inbook,@incollectedbook,@incollection,@injournal,@inproceedings,%
        @manual,@mastersthesis,@misc,@patent,@periodical,@phdthesis,@preamble,%
        @proceedings,@standard,@string,@techreport,@unpublished%
    },
    morekeywords={abstract, address, annote, author, booktitle, chapter, crossref,
        edition, editor, eprint, howpublished, institution, journal, key, month,
        note, number, organization, pages, publisher, school, series, title,
        type, url, volume, year,%
    },
    comment=[l][\itshape]{@comment},
    sensitive=false,
}


% ----------------------------------------------------------------------------------------
%	BIBLIOGRAPHY AND INDEX
% ----------------------------------------------------------------------------------------
\usepackage{calc} % For simpler calculation - used for spacing the index letter headings correctly
\usepackage{makeidx} % Required to make an index
\makeindex % Tells LaTeX to create the files required for indexing


% ----------------------------------------------------------------------------------------
%	Some diverse content
% ----------------------------------------------------------------------------------------
\usepackage{parskip}
% fonts and encoding
\usepackage{lmodern}
\usepackage{ifxetex}
\ifxetex
\usepackage{mathspec}
\setprimaryfont{Times}
\setallmonofonts{Courier}
\else
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{mathptmx}
\usepackage{sansmathfonts}
\usepackage{helvet}
\renewcommand\familydefault{phv}
\fi
\usepackage[normalem]{ulem}
\usepackage{amsmath}
\usepackage{textcomp}
\usepackage{amssymb}

% = i18n
\usepackage[english]{babel}
\usepackage[autostyle]{csquotes}

% = SI units
\usepackage[detect-all]{siunitx}


% = table tweakery
\usepackage{tabularx}
\usepackage{tabulary}
\usepackage{booktabs}
\usepackage{color}
\usepackage{colortbl}
\usepackage{multirow}
\usepackage{multicol}
\usepackage{hhline}
\usepackage{longtable}
\usepackage{blkarray}

% = float tweakery
\usepackage{subcaption}
\captionsetup{subrefformat=parens}
\captionsetup[sub]{skip=3pt}
\usepackage{adjustbox}

% % = list tweakery
% \usepackage{enumitem}
% \setlist{leftmargin=*, nosep}

% = listings
\usepackage[cache=false]{minted}
\setminted{breaklines, breakbefore={/D_}, breakbeforesymbolpre={}, fontsize=\tiny}
% hide lexer errors, adapted from https://github.com/gpoore/minted/issues/69#issuecomment-81612590
\AtBeginEnvironment{multicols}{%
    \renewcommand{\fcolorbox}[4][]{#4}}
\usepackage{tcolorbox}
\usepackage{etoolbox}
\BeforeBeginEnvironment{minted}{\begin{tcolorbox}\fontsize{6}{6}\selectfont}
    \AfterEndEnvironment{minted}{\end{tcolorbox}}

% = citations
\usepackage[
backend=biber,
style=authoryear-icomp,
maxcitenames=2,
maxbibnames=2,
uniquelist=false,
isbn=false,
url=false,
doi=false,
natbib=true
]{biblatex}

\makeatletter

\newrobustcmd*{\parentexttrack}[1]{%
  \begingroup
  \blx@blxinit
  \blx@setsfcodes
  \blx@bibopenparen#1\blx@bibcloseparen
  \endgroup}

\AtEveryCite{%
  \let\parentext=\parentexttrack%
  \let\bibopenparen=\bibopenbracket%
  \let\bibcloseparen=\bibclosebracket}

\renewbibmacro*{cite}{%
  \iffieldundef{shorthand}
  {\ifthenelse{\ifnameundef{labelname}\OR\iffieldundef{labelyear}}
    {\usebibmacro{cite:label}%
      \setunit{\addspace}}
    {\printnames{labelname}%
      \setunit{\nameyeardelim}}%
    %   \usebibmacro{cite:labelyear+extrayear}}% DELETED
    \printtext[parens]{\usebibmacro{cite:labeldate+extradate}}}% ADDED
  {\usebibmacro{cite:shorthand}}}

\makeatother
\DeclareCiteCommand{\cite}
{\usebibmacro{prenote}}
{\usebibmacro{citeindex}%
  \printtext[bibhyperref]{\usebibmacro{cite}}}
{\multicitedelim}
{\usebibmacro{postnote}}

\DeclareCiteCommand*{\cite}
{\usebibmacro{prenote}}
{\usebibmacro{citeindex}%
  \printtext[bibhyperref]{\usebibmacro{citeyear}}}
{\multicitedelim}
{\usebibmacro{postnote}}

\DeclareCiteCommand{\parencite}[\mkbibparens]
{\usebibmacro{prenote}}
{\usebibmacro{citeindex}%
  \printtext[bibhyperref]{\usebibmacro{cite}}}
{\multicitedelim}
{\usebibmacro{postnote}}

\DeclareCiteCommand*{\parencite}[\mkbibparens]
{\usebibmacro{prenote}}
{\usebibmacro{citeindex}%
  \printtext[bibhyperref]{\usebibmacro{citeyear}}}
{\multicitedelim}
{\usebibmacro{postnote}}

\DeclareCiteCommand{\footcite}[\mkbibfootnote]
{\usebibmacro{prenote}}
{\usebibmacro{citeindex}%
  \printtext[bibhyperref]{ \usebibmacro{cite}}}
{\multicitedelim}
{\usebibmacro{postnote}}

\DeclareCiteCommand{\footcitetext}[\mkbibfootnotetext]
{\usebibmacro{prenote}}
{\usebibmacro{citeindex}%
  \printtext[bibhyperref]{\usebibmacro{cite}}}
{\multicitedelim}
{\usebibmacro{postnote}}

\DeclareCiteCommand{\textcite}
{\boolfalse{cbx:parens}}
{\usebibmacro{citeindex}%
  \printtext[bibhyperref]{\usebibmacro{textcite}}}
{\ifbool{cbx:parens}
  {\bibcloseparen\global\boolfalse{cbx:parens}}
  {}%
  \multicitedelim}
{\usebibmacro{textcite:postnote}}
\renewcommand*{\bibfont}{\footnotesize}
\renewcommand{\cite}[1]{\citep{#1}}


% ----------------------------------------------------------------------------------------
%	LINKS
% ----------------------------------------------------------------------------------------
\usepackage{hyperref}
\hypersetup{
    colorlinks = true,
    linkcolor=gmitblue,
    filecolor=gmitblue,
    urlcolor=gmitblue,
    citecolor = green!60!black
}
\usepackage[capitalise, nameinlink, noabbrev, nosort]{cleveref}
\urlstyle{same}
\makeatletter
\let\@mycite\@cite
\def\@cite#1#2{{\hypersetup{linkcolor=green!60!black}[{#1\if@tempswa , #2\fi}]}}
\makeatother


% = draft stuff
\usepackage[obeyFinal]{todonotes}
\setlength{\marginparwidth}{1.5cm}
% \colorlet{seb}{red!20}
% \usepackage{lipsum}

% = acronym management
\usepackage{expl3}
\ExplSyntaxOn
\tex_let:D \c_minus_one \scan_stop:
\int_const:Nn \c_minus_one {-1}
\ExplSyntaxOff
