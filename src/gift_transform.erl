-module(gift_transform).
-export([transform/3]).

% Transformations for atoms from the GIFT parser

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utility Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
join_text(List, third) ->
  join_text(lists:map(fun([_,_,L]) -> L end, List));

join_text(List, second) ->
  join_text(lists:map(fun([_,L]) -> L end, List)).

join_text(Text) ->
  T = lists:foldr(fun(L, Acc) -> do_join(L, Acc) end, <<>>, Text),
  trim_binary(T).

do_join([],Acc) ->
  Acc;

do_join(L, Acc) ->
  <<L/binary, Acc/binary>>.

trim_binary(Text) ->
  re:replace(Text, "^[\\s\\t\\r\\n]+|[\\s\\t\\r\\n]+$", "", [{return, binary}, global]).

add_markup([], Q) ->
  maps:put(markup_language, plain, Q);

add_markup(Markup, Q) ->
  maps:put(markup_language, Markup, Q).

add_title([], Q) ->
  maps:put(title, <<>>, Q);

add_title(Title, Q) ->
  maps:put(title, Title, Q).

strip_comments(ItemList) ->
  lists:filter(fun(Item) -> Item /= comment end, ItemList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ROOT NODE TRANSFORM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform(gift, [_, Questions, _], _Index) when is_list(Questions) ->
  Questions;

transform(item, comment, _Index) ->
  [comment, []];

transform(item, Node, _Index) ->
  Node;

transform(item_list, [ItemList, [LastItem, _]], _Index) ->
  strip_comments(
    lists:map(fun([Q, _]) -> Q end, ItemList) ++ [LastItem]
  );

transform(item_list, [ItemList, []], _Index) ->
  strip_comments(
    lists:map(fun([Q, _]) -> Q end, ItemList)
  );

transform(decorated_question, [Title, Markup, Q, _], _Index) ->
  add_markup(Markup, add_title(Title, Q));

transform(question, [_, Q], _Index) ->
  Q;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform(essay_question, [Q | _], _Index) ->
  #{type => essay_question, text => Q};

transform(true_false_question, [Q, _, _, A, _, _], _Index) ->
  #{type => true_false_question, text => Q, answers => [A]};

transform(matching_question, [Q, _, _, Alist, _, _], _Index) ->
  #{type => matching_question, text => Q, answers => Alist};

transform(fill_in_question, [Prefix, _, _, Alist, _, _, Suffix], _Index) ->
  S = join_text(Suffix, second),
  #{type => fill_in_question, text => <<Prefix/binary, " _ ", S/binary>>, answers => Alist};

transform(short_answer_question, [Q, _, _, Alist, _, _], _Index) ->
  #{type => short_answer_question, text => Q, answers => Alist};

transform(multiple_choice_question, [Q, _, _, Alist, _, _], _Index) ->
  #{type => multiple_choice_question, text => Q, answers => Alist};

transform(numeric_question, [Q, _, _, Alist, _, _], _Index) ->
  #{type => numeric_question, text => Q, answers => Alist};

transform(description, Text, _Index) ->
  #{type => description, text => join_text(Text, third)};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform(command, [_, Text], _Index) ->
  #{type => command, command => join_text(Text, second)};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parts of Questions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
transform(question, Node, _Index) ->
  Node;

transform(question_text, Node, _Index) ->
  Text = lists:map(fun([_,L]) -> L end, Node),
  join_text(Text);

transform(true_answer, [_,_,[]], _Index) ->
  {true, 100};

transform(false_answer, [_,_,[]], _Index) ->
  {false, 100};

transform(true_answer, [_,_,Feedback], _Index) ->
  {true, 100, Feedback};

transform(false_answer, [_,_,Feedback], _Index) ->
  {false, 100, Feedback};

transform(right_answer, [_, [], Answer, Feedback], _Index) ->
  transform(right_answer, [nil, 100, Answer, Feedback], _Index);

transform(right_answer, [_, Weight, Answer, []], _Index) ->
  {Answer, Weight};

transform(right_answer, [_, Weight, Answer, Feedback], _Index) ->
  {Answer, Weight, Feedback};

transform(wrong_answer, [_, [], Answer, Feedback], _Index) ->
  transform(wrong_answer, [nil, 0, Answer, Feedback], _Index);

transform(wrong_answer, [_, Weight, Answer, []], _Index) ->
  {Answer, Weight};

transform(wrong_answer, [_, Weight, Answer, Feedback], _Index) ->
  {Answer, Weight, Feedback};

transform(numeric_answer, [_, [], Answer, _, Feedback], _Index) ->
  transform(numeric_answer, [nil, 100, Answer, nil, Feedback], _Index);

transform(numeric_answer, [_, Weight, Answer, _, []], _Index) ->
  {Answer, Weight};

transform(numeric_answer, [_, Weight, Answer, _, Feedback], _Index) ->
  {Answer, Weight, Feedback};

transform(numeric_with_tolerance, [Val, _, Tolerance], _Index) ->
  {Val - Tolerance, Val + Tolerance};

transform(match_answer, [_, V1, _, V2, _], _Index) ->
  {join_text(V1, third), join_text(V2, second)};

transform(range, [Min, _, Max], _Index) ->
  {Min, Max};

transform(title, [_, Title, _, _], _Index) ->
  Title;

transform(markup, [_, Markup, _], _Index) ->
  list_to_atom(binary_to_list(Markup));

transform(feedback, [_, Feedback], _Index) ->
  Feedback;

transform(weight, [_, Weight, _], _Index) ->
  Weight;

transform(escaped_text, [Text, _], _Index) ->
  join_text(Text, second);

transform(escaped_symbol, [_, Text], _Index) ->
  Text;

transform(number, Node, _Index) ->
  case Node of
    [Int, []] -> list_to_integer(binary_to_list(iolist_to_binary(Int)));
    [Int, Frac] -> list_to_float(binary_to_list(iolist_to_binary([Int, Frac])));
    _ -> list_to_float(binary_to_list(iolist_to_binary(Node)))
  end;

transform(int, Node, _Index) ->
  Node;

transform(frac, Node, _Index) ->
  Node;

transform(digit, Node, _Index) ->
  Node;

transform(non_zero_digit, Node, _Index) ->
  Node;

transform(Symbol, Node, Index) when is_atom(Symbol) ->
  'Elixir.IO':puts("Unmatched entity:"),
  'Elixir.IO':inspect(Index),
  'Elixir.IO':inspect(Symbol),
  'Elixir.IO':inspect(Node),
  Node.
