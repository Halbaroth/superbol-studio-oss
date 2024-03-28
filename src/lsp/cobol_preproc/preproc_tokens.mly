%{
(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2022 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)
%}

%token EOL
%token <string> TEXT_WORD             (* Words before text manipulation stage *)
%token <string * Text.quotation> ALPHANUM
%token <string * Text.quotation> ALPHANUM_PREFIX
%token <string> NATLIT
%token <string> BOOLIT
%token <string> HEXLIT
%token <string> NULLIT
%token <Text.pseudotext> PSEUDO_TEXT
%token <Text.text> EXEC_BLOCK                                       (* unused *)

%token LPAR   "("
%token PERIOD "."
%token RPAR   ")"

%token ALSO                      [@keyword]
%token BY                        [@keyword]
%token COPY                      [@keyword]
%token IN                        [@keyword]
%token LAST                      [@keyword]
%token LEADING                   [@keyword]
%token OF                        [@keyword]
%token OFF                       [@keyword]
%token PRINTING                  [@keyword]
%token REPLACE                   [@keyword]
%token REPLACING                 [@keyword]
%token SUPPRESS                  [@keyword]                         (* +COB85 *)
%token TRAILING                  [@keyword]

%%
