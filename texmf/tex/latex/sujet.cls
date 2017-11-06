%%% sujet.cls --- sujet

%% Author: Sébastien Le Maguer
%% Version: $Id: sujet.cls,v 0.0 2011/08/10 18:13:33 lemaguer Exp $

%%% Commentary:

%% 

%% Put this file into your TEXINPUTS.  A typical document looks like:
%%   \documentclass[OPTIONS]{sujet}
%%   \begin{document}
%%   
%%   \end{document}

%% OPTIONS defaults to 

%%% Code:

\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\def\@rcs@ $#1Date: #2 #3$$#4Revision: #5$ {
   \ProvidesPackage{sujet}[#2 v#5sujet]}
\@rcs@ $Date: 2011/08/10 18:13:33 $$Revision: 0.0 $

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ExecuteOptions{}
\ProcessOptions
\LoadClass[a4paper]{article}


%% =======================================================================================
%% Chargement des extensions
%% =======================================================================================
\RequirePackage{graphicx}
\RequirePackage{fullpage}
\RequirePackage[utf8]{inputenc}
\RequirePackage[francais]{babel}
\RequirePackage[francais]{layout} 
\RequirePackage{graphicx}
\RequirePackage{tikz}
\RequirePackage{listings}
\RequirePackage{answers}
\RequirePackage{color}

\RequirePackage{keyval}
\RequirePackage{ifthen}
\RequirePackage{exercise}
\RequirePackage{tikz}
\RequirePackage{amsmath}
\RequirePackage{amssymb}
\RequirePackage{amsthm}
\RequirePackage{hyperref}
\RequirePackage{fancyhdr}
\RequirePackage{lastpage}


%% =======================================================================================
%% Configuration des extensions
%% =======================================================================================
\definecolor{dkgreen}{rgb}{0,0.6,0}
\definecolor{gray}{rgb}{0.5,0.5,0.5}
\definecolor{mauve}{rgb}{0.58,0,0.82}

\lstset{ %
basicstyle=\footnotesize,          % the size of the fonts that are used for the code
backgroundcolor=\color{white},     % choose the background color. You must add \usepackage{color}
showspaces=false,                  % show spaces adding particular underscores
showstringspaces=false,            % underline spaces within strings
showtabs=false,                    % show tabs within strings adding particular underscores
frame=single,                      % adds a frame around the code
tabsize=2,                         % sets default tabsize to 2 spaces
captionpos=b,                      % sets the caption-position to bottom
breaklines=true,                   % sets automatic line breaking
breakatwhitespace=false,           % sets if automatic breaks should only happen at whitespace
keywordstyle=\color{blue},         % keyword style
commentstyle=\color{dkgreen},      % comment style
stringstyle=\color{mauve},         % string literal style
escapeinside={\%*}{*)},            % if you want to add a comment within your code
morekeywords={*,...}               % if you want to add more keywords to the set
}
\usetikzlibrary{shapes}

%% =======================================================================================
%% Options
%% =======================================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Pour gérer les deux types d'options :                                  %
% 1. les options normales (\DeclareOption{...}{...})                    %
% 2. les options type key-val option=value (\define@key{...}{...}{...}) %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
\@ifundefined{my@unusedlist}{%
\let\my@unusedlist\@empty}{}
\@ifundefined{my@extractkey}{%
\def\my@extractkey#1=#2\my@extractkey{#1}}{}
\newcommand{\ProcessUnusedOptions}[1]{%
\let\my@tempa\@empty
\let\my@tempb\@empty
\@for\CurrentOption:=\my@unusedlist\do{%
\expandafter\expandafter\expandafter\def
\expandafter\expandafter\expandafter\my@key
\expandafter\expandafter\expandafter{%
\expandafter\my@extractkey\CurrentOption=\my@extractkey}%
\@ifundefined{KV@#1@\my@key}{%
\edef\my@tempa{\my@tempa,\CurrentOption,}}{%
\edef\my@tempb{\my@tempb,\CurrentOption,}}}%
\@for\CurrentOption:=\my@tempa\do{%
\ifx\CurrentOption\@empty\else
\PackageWarning{mypack}{`\CurrentOption' not defined.}\fi}%
\edef\my@tempb{%
\noexpand\setkeys{#1}{\my@tempb}}%
\my@tempb
\AtEndOfPackage{\let\@unprocessedoptions\relax}}

\DeclareOption*{%
\expandafter\expandafter\expandafter\def
\expandafter\expandafter\expandafter\my@unusedlist
\expandafter\expandafter\expandafter{%
\expandafter\my@unusedlist\expandafter,%
\CurrentOption}}
%%%%%%%%%%%%%%% 
% Fin du bloc %
%%%%%%%%%%%%%%% 


%% Options passées sous forme key=val
\define@key{sujet}{sujet}{\def\sujet{#1}}
\define@key{sujet}{public}{\def\public{#1}}
\define@key{sujet}{module}{\def\module{#1}}
\define@key{sujet}{nature}{\def\nature{#1}}
\define@key{sujet}{duree}{\def\duree{#1}}
\ProcessOptions
\ProcessUnusedOptions{sujet}


%% =======================================================================================
%% Configuration des extensions
%% =======================================================================================

\newcounter{moncompteur}
\theoremstyle{definition}
\newtheorem{ex}{%
\hyperlink{ex:\theex}{Question}\hypertarget{sol:\theex}{}}
\Newassociation{Solution}{Soln}{corr}
\renewenvironment{Soln}[1]
{\par\bigskip\noindent{\bfseries
\hypertarget{ex:#1}{}\hyperlink{sol:#1}{Solution de la question #1}}\quad}
{\par\bigskip}
\definecolor{dkgreen}{rgb}{0,0.6,0}
\definecolor{gray}{rgb}{0.75,0.75,0.75}
\definecolor{mauve}{rgb}{0.58,0,0.82}

\hypersetup{
pdfnewwindow=true,      % links in new window
colorlinks=true,       % false: boxed links; true: colored links
linkcolor=blue,          % color of internal links
citecolor=green,        % color of links to bibliography
filecolor=magenta,      % color of file links
urlcolor=cyan           % color of external links
}

%% =======================================================================================
%% Configuration des extensions
%% =======================================================================================

\newcommand\importantbox[1]
{
\tikzstyle{mybox} = [draw=black, fill=orange!40, very thick,
rectangle, rounded corners, inner sep=10pt, inner ysep=10pt]
\begin{tikzpicture}
    \node [mybox] (box){%
    \begin{minipage}{0.9\textwidth}
        \textbf{#1}
    \end{minipage}
    };
\end{tikzpicture}%
}

\newenvironment{objectifs}
{% This is the begin code
\underline{\bf\small Objectifs} \begin{it}
    }
    {% This is the end code
\end{it} 
}

\newenvironment{directives}
{% This is the begin code
\underline{\bf\small Directives} \begin{it}
    }
    {% This is the end code
    ~\par
\end{it} 
}

\newenvironment{rendu}
{% This is the begin code
\underline{\bf\small Rendu} \begin{it}
    }
    {% This is the end code
    ~\par
\end{it} 
}

\newenvironment{indication}
{% This is the begin code
\underline{\bf\small Indication :} \begin{emph}
    }
    {% This is the end code
    ~\par
\end{emph} 
}

\newenvironment{remarque}
{% This is the begin code
\underline{\bf\small Remarque :} 
    }
    {}
    
\newenvironment{conclusion}
{% This is the begin code
\underline{\bf\small Conclusion :}
}
{% This is the end code
~\par
}


\newenvironment{hint}[0]{\textbf{\underline{Conseil:}}}{}
% \renewcommand{\ExerciseHeader}{\bigskip%
% \textbf{\large\ExerciseHeaderDifficulty\ExerciseName\ %
% \ExerciseHeaderNB\ExerciseHeaderTitle\ExerciseHeaderOrigin}%
% \\%
% }


%% =======================================================================================
%% Titre
%% =======================================================================================

\makeatletter
\def\maketitle{%
    \null
    % \vfill
    \hbox{
        \parbox{0.4\textwidth}{
            \includegraphics[width=0.3\textwidth]{logo-enssat-bi.png} \par
            \hfill
            \par
            \includegraphics[width=0.3\textwidth]{logoUR1_UEB.png} 
        }
        \hfill
        \parbox{0.6\textwidth}{
            {\LARGE \nature~: \@title~(\duree)\par}
            {\Large \public~: \module \par}	
            Version du~: \@date
        }
    }
    \hrule
    \bigskip
}
\makeatletter


%% =======================================================================================
%% Style des pages
%% =======================================================================================

\pagestyle{fancy}
\fancyhf{}
\renewcommand{\headrulewidth}{0pt}
\fancyfoot{}
\fancyfoot[C]{{\thepage}/\pageref{LastPage}}

% 


\endinput

%%% Local Variables:
%%% End:
