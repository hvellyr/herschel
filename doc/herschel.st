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

  /(def[ \t](generic)[ \t])([!\$%&\*\/=\?_^a-zA-Z0-9.+\-|]+)/ {
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


  /((module|library)[ \t])([!\$%&\*\/=\?_^a-zA-Z0-9.+\-|]+)/ {
    /* Keywords */
    keyword_face (true);
    language_print ($1);
    keyword_face (false);

    /* Function name. */
    function_name_face (true);
    language_print ($2);
    function_name_face (false);
  }

  /(def[ \t](alias|type|record)[ \t])([!\$%&\*\/=\?_^a-zA-Z0-9.+\-|]+)/ {
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


  /(def[ \t](const|config)[ \t])([!\$%&\*\/=\?_^a-zA-Z0-9.+\-|]+)/ {
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

  /* Keywords. */
  /\b(AND|Function|OR|XOR|a(lias|nd|s)|by|c(har|on(fig|st))|def\
|e(lse|num|of|xport)|f(alse|inal|or|unction)|generic\
|i(f|gnore|mport|n(|clude|ntern)|sa)|let\
|m(a(cro|tch)|od(|ule))|n(il|ot)|or|p(rivate|ublic)\
|select|t(rue|ype)|wh(e(n|re)|ile)|with)\b/ {
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
