-module(gift_transform).
-export([transform/3]).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ROOT NODE TRANSFORM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform(gift, [_, Questions], _Index) when is_list(Questions) ->
  Questions;

transform(single_question, [CommandList, Q, _], _Index) ->
  CommandList ++ [Q];

transform(question_list, [QList, SingleQ], _Index) ->
  lists:map(fun([CommandList, Q, _]) -> Q end, QList) ++ SingleQ;

transform(decorated_question, [Comment, Title, Markup, Q, _], _Index) ->
  % Comment is dropped.
  maps:put(markup_language, Markup, maps:put(title, Title, Q));

transform(question, [_, Q], _Index) ->
  Q;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform(essay_question, [Q | _], _Index) ->
  #{'__struct__' => 'Elixir.Gift.EssayQuestion', text => Q};

transform(true_false_question, [Q, _, _, A, _, _], _Index) ->
  #{'__struct__' => 'Elixir.Gift.TrueFalseQuestion', text => Q, answers => [A]};

transform(matching_question, [Q, _, _, Alist, _, _], _Index) ->
    #{'__struct__' => 'Elixir.Gift.MatchingQuestion', text => Q, answers => Alist};

transform(fill_in_question, [Prefix, _, _, Alist, _, _, Suffix], _Index) ->
  S = join_text(Suffix, second),
  #{'__struct__' => 'Elixir.Gift.FillInQuestion', text => <<Prefix/binary, " _ ", S/binary>>, answers => Alist};

transform(short_answer_question, [Q, _, _, Alist, _, _], _Index) ->
  #{'__struct__' => 'Elixir.Gift.ShortAnswerQuestion', text => Q, answers => Alist};

transform(multiple_choice_question, [Q, _, _, Alist, _, _], _Index) ->
  #{'__struct__' => 'Elixir.Gift.MultipleChoiceQuestion', text => Q, answers => Alist};

transform(numeric_question, [Q, _, _, Alist, _, _], _Index) ->
  #{'__struct__' => 'Elixir.Gift.NumericQuestion', text => Q, answers => Alist};

transform(description, Node, _Index) ->
  #{'__struct__' => 'Elixir.Gift.Description', text => Node};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform(command, [_, Text, _], _Index) ->
  #{'__struct__' => 'Elixir.Gift.Command', command => join_text(Text, second)};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parts of Questions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

transform(numeric_with_tolerance, [Val, _, Tolerance], Index) ->
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

transform(Symbol, Node, Index) when is_atom(Symbol) ->
  Node.
