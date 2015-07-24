{{{name}}}
===============

:func:`{{{name}}}`

{{{title}}}

Usage
""""""""""""""""""
::

 {{#usage}}
 {{{usage}}}
 {{/usage}}

{{#has_args}}
Arguments

{{#arguments}}
{{name}}
    {{{description}}}
{{/arguments}}
{{/has_args}}

{{#sections}}
{{{title}}}
""""""""""""""""""

{{#contents}}
{{{.}}}
{{/contents}}

{{/sections}}

Examples
""""""""""""""""""
::

 {{#examples}}
 {{{ examples }}}
 {{/examples}}

Aliases:
{{#aliases}}
{{.}}
{{/aliases}}

Keywords:
{{#keywords}}
{{.}}
{{/keywords}}

{{#seealso}}
See also:
{{{ seealso }}}
{{/seealso}}

Author:
{{#author}}
{{{ author }}}
{{/author}}
