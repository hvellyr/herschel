/**
 * Name: herschel
 * Description: Herschel programming language.
 * Author: Gregor Klinke <gck@eyestep.org>
 *
 * Configuration file for (gnu)enscript
 */

state herschel_string extends Highlight
{
  /\\\\./ {
    language_print ($0);
  }
  /\"/ {
    language_print ($0);
    return;
  }
}


state herschel_inlinedoc extends Highlight
{
  /\\\\./ {
    language_print ($0);
  }
  /~/ {
    language_print ($0);
    return;
  }
}


state herschel extends HighlightEntry
{
  /* Comments. */
  /--/ {
    comment_face (true);
    language_print ($0);
    call (eat_one_line);
    comment_face (false);
  }

  /* String constants. */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (herschel_string);
    string_face (false);
  }

  /* inline documentation */
  /\~/ {
    comment_face (true);
    language_print ($0);
    call (herschel_inlinedoc);
    comment_face (false);
  }


  /* Character constants. */
  /\\\\[!\$%&\*\/=\?_^a-zA-Z0-9.+\-]+/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }

  /* Keyword constants. */
  /([ \t])(#[!\$%&\*\/=\?_^a-zA-Z0-9.+\-]*)/ {
    language_print ($1);
    reference_face (true);
    language_print ($2);
    reference_face (false);
  }

  /(def[ \t](generic|slot)[ \t])([!\$%&\*\/=\?_^a-zA-Z0-9.+\-|]+)/ {
    /* Keywords */
    keyword_face (true);
    language_print ($1);
    keyword_face (false);

    /* Function name. */
    function_name_face (true);
    language_print ($3);
    function_name_face (false);
  }


  /(def|let)([ \t])([!\$%&\*\/=\?_^a-zA-Z0-9.+\-|]+)([ \t]?)(\()/ {
    /* Keywords */
    keyword_face (true);
    language_print ($1);
    keyword_face (false);

    language_print ($2);

    /* Function name. */
    function_name_face (true);
    language_print ($3);
    function_name_face (false);

    /* garbage */
    language_print ($4);
    language_print ($5);
  }


  /(module[ \t])([!\$%&\*\/=\?_^a-zA-Z0-9.+\-|]+)/ {
    /* Keywords */
    keyword_face (true);
    language_print ($1);
    keyword_face (false);

    /* Function name. */
    function_name_face (true);
    language_print ($2);
    function_name_face (false);
  }

  /(def[ \t](alias|type|class)[ \t])([!\$%&\*\/=\?_^a-zA-Z0-9.+\-|]+)/ {
    /* Keywords */
    keyword_face (true);
    language_print ($1);
    keyword_face (false);

    /* Function name. */
    type_face (true);
    language_print ($3);
    type_face (false);
  }


  /* generic types */
  /'[!\$%&\*\/=\?_^a-zA-Z0-9.+\-|]+/ {
    type_face (true);
    language_print ($0);
    type_face (false);
  }


  /(def[ \t](var|const)[ \t])([!\$%&\*\/=\?_^a-zA-Z0-9.+\-|]+)/ {
    /* Keywords */
    keyword_face (true);
    language_print ($1);
    keyword_face (false);

    /* Function name. */
    variable_name_face (true);
    language_print ($3);
    variable_name_face (false);
  }

  /(let[ \t])([!\$%&\*\/=\?_^a-zA-Z0-9.+\-|]+)([ \t]?=)/ {
    /* Keywords */
    keyword_face (true);
    language_print ($1);
    keyword_face (false);

    /* Function name. */
    variable_name_face (true);
    language_print ($2);
    variable_name_face (false);

    language_print ($3);
  }

  /* Keywords.
     (build-re '(abort abs abstract accept access aliased
     all and array at begin body case constant declare
     delay delta digits do else elsif end entry
     exception exit for function generic goto if in
     is limited loop mod new not null of or others
     out package pragma private procedure protected raise
     range record rem renames requeue return reverse
     select separate subtype tagged task terminate then type
     until use when while with xor))
   */
  /\b(AND|Function|OR|XOR|a(lias|nd|s)|by|c(har|lass|on(fig|st))|def\
|e(lse|num|of|x(it|port|tend))|f(alse|inal|luid|or|unction)|generic\
|i(f|gnore|mport|n(|clude|it|ner)|sa)|let\
|m(a(cro|tch)|easure|od(|ule))|n(il|ot)|o(n|r|uter)|p(rivate|ublic)\
|reify|s(elect|ignal|lot|ync)|t(hen|rue|ype)|unit|wh(e(n|re)|ile))\b/ {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }
}

/*
Local variables:
mode: c
End:
*/
