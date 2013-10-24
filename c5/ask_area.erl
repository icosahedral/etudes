%% @author Neal Shrader <neal@digitalocean.com>
%% @doc Gets input from user to calculate area of various shapes
-module(ask_area).
-compile([debug_info]).
-export([area/0]).

%% @doc Translates a single character to a shape we can calculate area for.
-spec(char_to_shape(string()) -> atom()).
char_to_shape(Char) when Char == "r"; Char == "R" -> rectangle;
char_to_shape(Char) when Char == "e"; Char == "E" -> ellipse;
char_to_shape(Char) when Char == "t"; Char == "T" -> triangle;
char_to_shape(_) -> unknown.

%% @doc Area caluclation from Etude 4-1
-spec(area(atom(), number(), number()) -> number()).
area(Shape, X, Y) when X > 0, Y > 0 ->
  case Shape of
    rectangle -> X * Y;
    triangle  -> X * Y / 2.0;
    ellipse   -> math:pi() * X * Y
  end.

%% @doc Gets a string from input and changes it to a float if it's a float.
%% Otherwise try converting to an int.
-spec(get_number(string()) -> {number(), any()}).
get_number(Prompt) ->
  RawInput = io:get_line(Prompt ++ ": "),
  Input = string:substr(RawInput, 1, string:len(RawInput)-1),  %% chop off newline
  case string:to_float(Input) of
    {error, no_float} -> string:to_integer(Input);
    {F, Rest} -> {F, Rest}
  end.
  

%% @doc Gives a tuple of two numbers based on user input from two provided prompts
-spec(get_dimensions(string(), string()) -> {number(), number()}).
get_dimensions(Prompt1, Prompt2) ->
  {X, _} = get_number(Prompt1),
  {Y, _} = get_number(Prompt2),
  {X, Y}.

%% @doc Error check our input and pass it to area/3.
-spec(calculate(atom(), number(), number()) -> any()).
calculate(unknown, _, _) -> io:format("Unknown shape~n");
calculate(Shape, X, Y) when is_number(X), is_number(Y), X > 0, Y > 0 -> area(Shape, X, Y);
calculate(_,_,_) -> io:format("Invalid number inputted~n").

%% @doc External interface to module
-spec(area() -> any()).
area() ->
  %% Just give me the first char of whatever the user inputs
  Char = string:substr(io:get_line("R)ectangle, T)riangle, or E)llipse > "), 1, 1),
  Shape = char_to_shape(Char),
  {X, Y} = if Shape == rectangle  -> get_dimensions("width", "height");
              Shape == triangle   -> get_dimensions("base", "height");
              Shape == ellipse    -> get_dimensions("major axis", "minor axis");
                       true       -> {-1, -1}
          end,
  calculate(Shape, X, Y).
