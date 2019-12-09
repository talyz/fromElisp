{ pkgs ? import <nixpkgs> {} }:

with pkgs.lib;
with builtins;

let

  # Split a string of elisp into individual tokens and add useful
  # metadata.
  tokenizeElisp = elisp:
    let
      # These are the only characters that can not be unescaped in a
      # symbol name. We match the inverse of these to get the actual
      # symbol characters and use them to differentiate between
      # symbols and tokens that could potentially look like symbols,
      # such as numbers. Due to the leading bracket, this has to be
      # placed _first_ inside a bracket expression.
      notInSymbol = '']["'`,#;\\()[:space:][:cntrl:]'';

      # The matchers all take one string as argument and, on
      # successful match, returns the match as a string, otherwise
      # they return null.
      matchComment = string:
        let matchedComment = match "(;[^\n]*[\n]).*" string;
        in
          if matchedComment != null then head matchedComment else null;

      matchString = string:
        let matchedString = match ''("([^"\\]|\\.)*").*'' string;
        in
          if matchedString != null then head matchedString else null;

      matchCharacter = string:
        let character = match ''([?]((\\[sSHMAC]-)|\\\^)*(([^][\\()]|\\[][\\()])|\\[^^SHMACNuUx0-7]|\\[uU][[:digit:]a-fA-F]+|\\x[[:digit:]a-fA-F]*|\\[0-7]{1,3}|\\N\{[^}]+}))([${notInSymbol}?]|$).*'' string;
        in
          if character != null then head character else null;

      matchNonBase10Integer = string:
        let integer = match ''(#([BOX]|[[:digit:]]{1,2}r)[[:digit:]a-fA-F]+)([${notInSymbol}]|$).*'' string;
        in
          if integer != null then head integer else null;

      matchInteger = string:
        let integer = match ''([+-]?[[:digit:]]+[.]?)([${notInSymbol}]|$).*'' string;
        in
          if integer != null then head integer else null;

      matchBoolVector = string:
        let matchedBoolVector = match ''(#&[[:digit:]]+"([^"\\]|\\.)*").*'' string;
        in
          if matchedBoolVector != null then head matchedBoolVector else null;

      matchFloat = string:
        let float = match ''([+-]?([[:digit:]]*[.][[:digit:]]+|([[:digit:]]*[.])?[[:digit:]]+e([[:digit:]]+|[+](INF|NaN))))([${notInSymbol}]|$).*'' string;
        in
          if float != null then head float else null;

      matchDot = string:
        let dot = match ''([.])([${notInSymbol}]|$).*'' string;
        in
          if dot != null then head dot else null;

      # Symbols can contain pretty much any characters - the general
      # rule is that if nothing else matches, it's a symbol, so we
      # should be pretty generous here and match for symbols last. See
      # https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Type.html
      matchSymbol = string:
        let
          symbolChar = ''([^${notInSymbol}]|\\.)'';
          symbol = match ''(${symbolChar}+)([${notInSymbol}]|$).*'' string;
        in
          if symbol != null then head symbol else null;

      len = stringLength elisp;

      # Fold over all the characters in a string, checking for
      # matching tokens.
      #
      # The implementation is a bit obtuse, for optimization reasons:
      # nix doesn't have tail-call optimization, thus a strict fold,
      # which should essentially force a limited version of tco when
      # iterating a list, is our best alternative.
      #
      # The string read from is split into a list of its constituent
      # characters, which is then folded over. Each character is then
      # used to determine a likely matching regex "matcher" to run on
      # the string, starting at the position of the aforementioned
      # character. When an appropriate matcher has been found and run
      # successfully on the string, its result is added to
      # `state.acc`, a list of all matched tokens. The length of the
      # matched token is determined and passed on to the following
      # iteration through `state.skip`. If `state.skip` is positive,
      # nothing will be done in the current iteration, except
      # decrementing `state.skip` for the next one: this skips the
      # characters we've already matched. At each iteration,
      # `state.pos` is also incremented, to keep track of the current
      # string position.
      #
      # The order of the matches is significant - matchSymbol will,
      # for example, also match numbers and characters, so we check
      # for symbols last.
      readToken = state: char:
        let
          rest = substring state.pos (len - state.pos) elisp;
          comment = matchComment rest;
          character = matchCharacter rest;
          nonBase10Integer = matchNonBase10Integer rest;
          integer = matchInteger rest;
          float = matchFloat rest;
          function = matchFunction rest;
          boolVector = matchBoolVector rest;
          string = matchString rest;
          dot = matchDot rest;
          symbol = matchSymbol rest;
        in
          if state.skip > 0 then
            state // {
              pos = state.pos + 1;
              skip = state.skip - 1;
              line = if char == "\n" then state.line + 1 else state.line;
            }
          else if char == " " || char == "\n" || char == "\t" || char == "\r" then
            state // {
              pos = state.pos + 1;
              line = if char == "\n" then state.line + 1 else state.line;
            }
          else if char == ";" then
            if comment != null then
              state // {
                acc = state.acc ++ [{ type = "comment"; value = comment; inherit (state) line; }];
                pos = state.pos + 1;
                skip = (stringLength comment) - 1;
              }
            else throw "Unrecognized token on line ${toString state.line}: ${rest}"
          else if char == "(" then
            state // {
              acc = state.acc ++ [{ type = "openParen"; value = "("; inherit (state) line; }];
              pos = state.pos + 1;
            }
          else if char == ")" then
            state // {
              acc = state.acc ++ [{ type = "closeParen"; value = ")"; inherit (state) line; }];
              pos = state.pos + 1;
            }
          else if char == "[" then
            state // {
              acc = state.acc ++ [{ type = "openBracket"; value = "["; inherit (state) line; }];
              pos = state.pos + 1;
            }
          else if char == "]" then
            state // {
              acc = state.acc ++ [{ type = "closeBracket"; value = "]"; inherit (state) line; }];
              pos = state.pos + 1;
            }
          else if char == "'" then
            state // {
              acc = state.acc ++ [{ type = "quote"; value = "'"; inherit (state) line; }];
              pos = state.pos + 1;
            }
          else if char == ''"'' then
            if string != null then
              state // {
                acc = state.acc ++ [{ type = "string"; value = string; inherit (state) line; }];
                pos = state.pos + 1;
                skip = (stringLength string) - 1;
              }
            else throw "Unrecognized token on line ${toString state.line}: ${rest}"
          else if char == "#" then
            let nextChar = substring 1 1 rest;
            in
              if nextChar == "'" then
                state // {
                  acc = state.acc ++ [{ type = "function"; value = "#'"; inherit (state) line; }];
                  pos = state.pos + 1;
                  skip = 1;
                }
              else if nextChar == "&" then
                if boolVector != null then
                  state // {
                    acc = state.acc ++ [{ type = "boolVector"; value = boolVector; inherit (state) line; }];
                    pos = state.pos + 1;
                    skip = (stringLength boolVector) - 1;
                  }
                else throw "Unrecognized token on line ${toString state.line}: ${rest}"
              else if nextChar == "s" then
                if substring 2 1 rest == "(" then
                  state // {
                    acc = state.acc ++ [{ type = "record"; value = "#s"; inherit (state) line; }];
                    pos = state.pos + 1;
                    skip = 1;
                  }
                else throw "List must follow #s in record on line ${toString state.line}: ${rest}"
              else if nextChar == "[" then
                state // {
                  acc = state.acc ++ [{ type = "byteCode"; value = "#"; inherit (state) line; }];
                  pos = state.pos + 1;
                }
              else if nonBase10Integer != null then
                state // {
                  acc = state.acc ++ [{ type = "nonBase10Integer"; value = nonBase10Integer; inherit (state) line; }];
                  pos = state.pos + 1;
                  skip = (stringLength nonBase10Integer) - 1;
                }
              else throw "Unrecognized token on line ${toString state.line}: ${rest}"
          else if char == "+" || char == "-" || char == "."
                  || char == "0" || char == "1" || char == "2" || char == "3"
                  || char == "4" || char == "5" || char == "6" || char == "7"
                  || char == "8" || char == "9" then
            if integer != null then
              state // {
                acc = state.acc ++ [{ type = "integer"; value = integer; inherit (state) line; }];
                pos = state.pos + 1;
                skip = (stringLength integer) - 1;
              }
            else if float != null then
              state // {
                acc = state.acc ++ [{ type = "float"; value = float; inherit (state) line; }];
                pos = state.pos + 1;
                skip = (stringLength float) - 1;
              }
            else if dot != null then
              state // {
                acc = state.acc ++ [{ type = "dot"; value = dot; inherit (state) line; }];
                pos = state.pos + 1;
                skip = (stringLength dot) - 1;
              }
            else if symbol != null then
              state // {
                acc = state.acc ++ [{ type = "symbol"; value = symbol; inherit (state) line; }];
                pos = state.pos + 1;
                skip = (stringLength symbol) - 1;
              }
            else throw "Unrecognized token on line ${toString state.line}: ${rest}"
          else if char == "?" then
            if character != null then
              state // {
                acc = state.acc ++ [{ type = "character"; value = character; inherit (state) line; }];
                pos = state.pos + 1;
                skip = (stringLength character) - 1;
              }
            else throw "Unrecognized token on line ${toString state.line}: ${rest}"
          else if char == "`" then
            state // {
              acc = state.acc ++ [{ type = "backquote"; value = "`"; inherit (state) line; }];
              pos = state.pos + 1;
            }
          else if char == "," then
            if substring 1 1 rest == "@" then
              state // {
                acc = state.acc ++ [{ type = "slice"; value = ",@"; inherit (state) line; }];
                skip = 1;
                pos = state.pos + 1;
              }
            else
              state // {
                acc = state.acc ++ [{ type = "expand"; value = ","; inherit (state) line; }];
                pos = state.pos + 1;
              }
          else if symbol != null then
            state // {
              acc = state.acc ++ [{ type = "symbol"; value = symbol; inherit (state) line; }];
              pos = state.pos + 1;
              skip = (stringLength symbol) - 1;
            }
          else
            throw "Unrecognized token on line ${toString state.line}: ${rest}";
    in (builtins.foldl' readToken { acc = []; pos = 0; skip = 0; line = 1; } (stringToCharacters elisp)).acc;

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
               || token.type == "function" || token.type == "record"
               || token.type == "byteCode" then
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
        else if object.type == "byteCode" then
          ["#"] ++ (readObject object.value)
        else if object.type == "record" then
          ["#s"] ++ (readObject object.value)
        else
          object.value;
    in
      map readObject ast;
in
{
  inherit tokenizeElisp parseElisp fromElisp;
}
