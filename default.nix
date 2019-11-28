{ pkgs ? import <nixpkgs> {} }:

with pkgs.lib;
with builtins;

let

  # Split a string of elisp into individual tokens and add useful
  # metadata.
  tokenizeElisp = elisp:
    let
      # The matchers are all take one string as argument and, on
      # successful match, return a list where the first item is the
      # matched token and the last is the rest of the string (the
      # non-matching part), otherwise they return null.
      matchWhitespace = string: match "([[:space:]]+)(.*)" string;
      matchComment = string: match "(;[^\n]*[\n])(.*)" string;
      matchOpenParen = string: match "([(])(.*)" string;
      matchCloseParen = string: match "([)])(.*)" string;
      matchOpenBracket = string: match "([[])(.*)" string;
      matchCloseBracket = string: match "([]])(.*)" string;
      matchQuote = string: match ''(')(.*)'' string;
      matchBackquote = string: match ''(`)(.*)'' string;
      matchExpand = string: match ''(,)([^@].*)'' string;
      matchSlice = string: match ''(,@)(.*)'' string;
      matchFunction = string: match ''(#')(.*)'' string;
      matchString = string:
        let
          matchedString = match ''("([^"\\]|\\.)*")(.*)'' string;
          len = length matchedString;
        in
          if matchedString != null then [(head matchedString) (elemAt matchedString (len - 1))] else null;

      # These are the only characters that can not be unescaped in a
      # symbol name. We match the inverse of these to get the actual
      # symbol characters and use them to differentiate between
      # symbols and tokens that could potentially look like symbols,
      # such as numbers.
      notInSymbol = '']["'`,#;\\()[:space:][:cntrl:]'';

      # For subexpressions containing subexpressions, what is matched
      # by the outer subexpression is returned first, then what is
      # matched by the inner ones. This means we have to take extra
      # care when the "rest"-subexpression itself contains
      # subexpressions: we're only interested in the outer match.
      matchCharacter = string:
        let
          character = match ''([?]([^\\]|\\.))(([${notInSymbol}]|$).*)'' string;
          len = length character;
        in
          if character != null then [(head character) (elemAt character (len - 2))] else null;
      matchNonBase10Integer = string:
        let
          integer = match ''([+-]?#?([BOX]|[[:digit:]]{1,2}r)[[:digit:]a-fA-F]+)(([${notInSymbol}]|$).*)'' string;
          len = length integer;
        in
          if integer != null then [(head integer) (elemAt integer (len - 2))] else null;
      matchInteger = string:
        let
          integer = match ''([+-]?[[:digit:]]+[.]?)(([${notInSymbol}]|$).*)'' string;
          len = length integer;
        in
          if integer != null then [(head integer) (elemAt integer (len - 2))] else null;
      matchFloat = string:
        let
          float = match ''([+-]?([[:digit:]]*[.][[:digit:]]+|([[:digit:]]*[.])?[[:digit:]]+e([[:digit:]]+|[+](INF|NaN))))(([${notInSymbol}]|$).*)'' string;
          len = length float;
        in
          if float != null then [(head float) (elemAt float (len - 2))] else null;
      matchDot = string:
        let
          dot = match ''([.])(([${notInSymbol}]|$).*)'' string;
          len = length dot;
        in
          if dot != null then [(head dot) (elemAt dot (len - 2))] else null;

      # Symbols can contain pretty much any characters - the general
      # rule is that if nothing else matches, it's a symbol, so we
      # should be pretty generous here and match for symbols last. See
      # https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Type.html
      matchSymbol = string:
        let
          symbolChar = ''([^${notInSymbol}]|\\.)'';
          symbol = match ''(${symbolChar}+)(([${notInSymbol}]|$).*)'' string;
          len = length symbol;
        in
          if symbol != null then [(head symbol) (elemAt symbol (len - 2))] else null;

      # Check for matching tokens one by one; the first matching token
      # type is added to the list and the function then calls itself
      # recursively to check for the next token, until there's nothing
      # left of the string or nothing matches. The order of the
      # matches is significant - matchSymbol will, for example, also
      # match numbers and characters, so we check for symbols last. We
      # also keep track of what line the match was on by counting the
      # number of newlines in the match, adding that many to the line
      # argument when recursing.
      recurse = { acc, line, rest }:
        let
          whitespace = matchWhitespace rest;
          comment = matchComment rest;
          openParen = matchOpenParen rest;
          closeParen = matchCloseParen rest;
          openBracket = matchOpenBracket rest;
          closeBracket = matchCloseBracket rest;
          character = matchCharacter rest;
          nonBase10Integer = matchNonBase10Integer rest;
          integer = matchInteger rest;
          float = matchFloat rest;
          quote = matchQuote rest;
          backquote = matchBackquote rest;
          expand = matchExpand rest;
          slice = matchSlice rest;
          function = matchFunction rest;
          string = matchString rest;
          dot = matchDot rest;
          symbol = matchSymbol rest;
        in
          if whitespace != null then
            recurse { acc = (acc ++ [
                        { type = "whitespace"; value = (head whitespace); inherit line; }
                      ]);
                      rest = (elemAt whitespace 1);
                      line = line;
                    }
          else if comment != null then
            recurse { acc = (acc ++ [
                        { type = "comment"; value = (head comment); inherit line; }
                      ]);
                      rest = (elemAt comment 1);
                      line = (line + 1);
                    }
          else if openParen != null then
            recurse {
              acc = acc ++ [{ type = "openParen"; value = (head openParen); inherit line; }];
              rest = (elemAt openParen 1);
              line = line;
            }
          else if closeParen != null then
            recurse { acc = (acc ++ [
                        { type = "closeParen"; value = (head closeParen); inherit line; }
                      ]);
                      rest = (elemAt closeParen 1);
                      line = line;
                    }
          else if quote != null then
            recurse { acc = (acc ++ [
                        { type = "quote"; value = (head quote); inherit line; }
                      ]);
                      rest = (elemAt quote 1);
                      line = line;
                    }
          else if string != null then
            recurse { acc = (acc ++ [
                        { type = "string"; value = (head string); inherit line; }
                      ]);
                      rest = (elemAt string 1);
                      line = line;
                    }
          else if function != null then
            recurse { acc = (acc ++ [
                        { type = "function"; value = (head function); inherit line; }
                      ]);
                      rest = (elemAt function 1);
                      line = line;
                    }
          else if integer != null then
            recurse { acc = (acc ++ [
                        { type = "integer"; value = (head integer); inherit line; }
                      ]);
                      rest = (elemAt integer 1);
                      line = line;
                    }
          else if float != null then
            recurse { acc = (acc ++ [
                        { type = "float"; value = (head float); inherit line; }
                      ]);
                      rest = (elemAt float 1);
                      line = line;
                    }
          else if openBracket != null then
            recurse { acc = (acc ++ [
                        { type = "openBracket"; value = (head openBracket); inherit line; }
                      ]);
                      rest = (elemAt openBracket 1);
                      line = line;
                    }
          else if closeBracket != null then
            recurse { acc = (acc ++ [
                        { type = "closeBracket"; value = (head closeBracket); inherit line; }
                      ]);
                      rest = (elemAt closeBracket 1);
                      line = line;
                    }
          else if dot != null then
            recurse { acc = (acc ++ [
                        { type = "dot"; value = (head dot); inherit line; }
                      ]);
                      rest = (elemAt dot 1);
                      line = line;
                    }
          else if character != null then
            recurse { acc = (acc ++ [
                        { type = "character"; value = (head character); inherit line; }
                      ]);
                      rest = (elemAt character 1);
                      line = line;
                    }
          else if nonBase10Integer != null then
            recurse { acc = (acc ++ [
                        { type = "nonBase10Integer"; value = (head nonBase10Integer); inherit line; }
                      ]);
                      rest = (elemAt nonBase10Integer 1);
                      line = line;
                    }
          else if backquote != null then
            recurse { acc = (acc ++ [
                        { type = "backquote"; value = (head backquote); inherit line; }
                      ]);
                      rest = (elemAt backquote 1);
                      line = line;
                    }
          else if expand != null then
            recurse { acc = (acc ++ [
                        { type = "expand"; value = (head expand); inherit line; }
                      ]);
                      rest = (elemAt expand 1);
                      line = line;
                    }
          else if slice != null then
            recurse { acc = (acc ++ [
                        { type = "slice"; value = (head slice); inherit line; }
                      ]);
                      rest = (elemAt slice 1);
                      line = line;
                    }
          else if symbol != null then
            recurse { acc = (acc ++ [
                        { type = "symbol"; value = (head symbol); inherit line; }
                      ]);
                      rest = (elemAt symbol 1);
                      line = line;
                    }
          else if rest == "" then
            acc
          else
            throw "Unrecognized syntax on line ${toString line}: ${rest}";
    in recurse { acc = []; rest = (readFile elisp); line = 1; };

  # Produce an AST from a string of elisp.
  parseElisp = elisp:
    let
      # Remove all whitespace tokens from a flat list, e.g. the output
      # from tokenizeElisp.
      removeWhitespace = tokens:
        filter (token: (token.type != "whitespace") && (token.type != "comment")) tokens;

      # Convert all literal value tokens in a flat list to their
      # corresponding nix representation. Character tokens are only
      # converted to single character strings, not their corresponding
      # character codes.
      parseValues = tokens:
        map (token:
          if token.type == "character" then
            token // {
              value = substring 1 (stringLength token.value - 1) token.value;
            }
          else if token.type == "string" then
            token // {
              value = substring 1 (stringLength token.value - 2) token.value;
            }
          # else if token.type == "integer" then
          #   token // {
          #     value = fromJSON (head (match "[+]?(.*)" token.value));
          #   }
          else
            token
        ) tokens;

      # Convert pairs of opening and closing tokens to their
      # respective collection types, i.e. lists and vectors.
      parseCollections = tokens:
        let
          recurse = { acc, tokens, type, depth, line }:
            if tokens == [] then
              if type == "list" && depth != 0 then
                throw "Unmatched opening parenthesis on line ${toString line}"
              else if type == "vector" && depth != 0 then
                throw "Unmatched opening square bracket on line ${toString line}"
              else
                acc
            else
              let
                token = head tokens;
                rest = tail tokens;
              in
                if token.type == "openParen" then
                  let
                    list' = recurse {
                      acc = [];
                      tokens = rest;
                      type = "list";
                      depth = (depth + 1);
                      line = token.line;
                    };
                    list = tail list';
                  in
                    recurse {
                      acc = acc ++ [{ type = "list"; value = list; inherit depth; }];
                      tokens = head list';
                      inherit type depth line;
                    }
                else if token.type == "closeParen" then
                  if depth == 0 || type != "list" then
                    throw "Unmatched closing parenthesis on line ${toString token.line}"
                  else
                    [ rest ] ++ acc
                else if token.type == "openBracket" then
                  let
                    vector' = recurse {
                      acc = [];
                      tokens = rest;
                      type = "vector";
                      depth = (depth + 1);
                      line = token.line;
                    };
                    vector = tail vector';
                  in
                    recurse {
                      acc = acc ++ [{ type = "vector"; value = vector; inherit depth; }];
                      tokens = head vector';
                      inherit type depth line;
                    }
                else if token.type == "closeBracket" then
                  if depth == 0 || type != "vector" then
                    throw "Unmatched closing bracket on line ${toString token.line}"
                  else
                    [ rest ] ++ acc
                else if token.type == "dot" then
                  if type == "list" then
                    if (head rest).type == "openParen" then
                      let
                        list' = recurse {
                          acc = [];
                          tokens = tail rest;
                          depth = depth + 1;
                          line = (head rest).line;
                          inherit type;
                        };
                        list = tail list';
                      in
                        recurse {
                          acc = acc ++ list;
                          tokens = head list';
                          inherit type depth line;
                        }
                    else
                      recurse { tokens = rest; inherit acc type depth line; }
                  else
                    throw ''"Dotted notation"-dot outside list on line ${toString token.line}''
                else
                  recurse { acc = acc ++ [ token ]; tokens = rest; inherit type depth line; };
        in
          recurse { acc = []; inherit tokens; type = null; depth = 0; line = 1; };

      parseQuotes = tokens:
        if tokens == [] then [] else
          let
            token = head tokens;
            rest = tail tokens;
          in
            if token.type == "quote" || token.type == "expand"
               || token.type == "slice" || token.type == "backquote"
               || token.type == "function" then
                 if rest == [] then
                   throw "No value to quote on line ${toString token.line}"
                 else
                   let
                     quotedValue = head rest;
                   in
                     [
                       (token // {
                         value = if isList quotedValue.value then
                                   quotedValue // { value = parseQuotes quotedValue.value; }
                                 else
                                   quotedValue;
                       })
                     ] ++ parseQuotes (tail rest)
            else if isList token.value then
              [
                (token // { value = parseQuotes token.value; })
              ] ++ parseQuotes rest
            else
              [ token ] ++ parseQuotes rest;
    in
      # Parsing is done in three stages: first we remove whitespace and
      # parse literals, then we parse lists and vectors. Lastly, we parse
      # quotes.
      parseQuotes (parseCollections (parseValues (removeWhitespace (tokenizeElisp elisp))));

  fromElisp = elisp:
    let
      ast = parseElisp elisp;
      readObject = object:
        if isList object.value then
          map readObject object.value
        else if object.type == "quote" then
          ["quote" (readObject object.value)]
        else if object.type == "backquote" then
          ["`" (readObject object.value)]
        else if object.type == "expand" then
          ["," (readObject object.value)]
        else if object.type == "slice" then
          [",@" (readObject object.value)]
        else if object.type == "function" then
          ["#'" (readObject object.value)]
        else
          object.value;
    in
      map readObject ast;
in
{
  inherit tokenizeElisp parseElisp fromElisp;
}
