-module(gift).
-export([parse/1,file/1]).
-define(p_anything,true).
-define(p_charclass,true).
-define(p_choose,true).
-define(p_not,true).
-define(p_one_or_more,true).
-define(p_optional,true).
-define(p_scan,true).
-define(p_seq,true).
-define(p_string,true).
-define(p_zero_or_more,true).



-spec file(file:name()) -> any().
file(Filename) -> case file:read_file(Filename) of {ok,Bin} -> parse(Bin); Err -> Err end.

-spec parse(binary() | list()) -> any().
parse(List) when is_list(List) -> parse(unicode:characters_to_binary(List));
parse(Input) when is_binary(Input) ->
  _ = setup_memo(),
  Result = case 'gift'(Input,{{line,1},{column,1}}) of
             {AST, <<>>, _Index} -> AST;
             Any -> Any
           end,
  release_memo(), Result.

-spec 'gift'(input(), index()) -> parse_result().
'gift'(Input, Index) ->
  p(Input, Index, 'gift', fun(I,D) -> (p_seq([p_zero_or_more(fun 'space'/2), p_choose([fun 'question_list'/2, fun 'single_question'/2])]))(I,D) end, fun(Node, Idx) ->transform('gift', Node, Idx) end).

-spec 'question_list'(input(), index()) -> parse_result().
'question_list'(Input, Index) ->
  p(Input, Index, 'question_list', fun(I,D) -> (p_seq([p_one_or_more(p_seq([p_zero_or_more(fun 'command'/2), fun 'decorated_question'/2, fun 'blank_line'/2])), fun 'single_question'/2]))(I,D) end, fun(Node, Idx) ->transform('question_list', Node, Idx) end).

-spec 'single_question'(input(), index()) -> parse_result().
'single_question'(Input, Index) ->
  p(Input, Index, 'single_question', fun(I,D) -> (p_seq([p_zero_or_more(fun 'command'/2), fun 'decorated_question'/2, p_optional(p_choose([fun 'blank_line'/2, fun 'space'/2]))]))(I,D) end, fun(Node, Idx) ->transform('single_question', Node, Idx) end).

-spec 'command'(input(), index()) -> parse_result().
'command'(Input, Index) ->
  p(Input, Index, 'command', fun(I,D) -> (p_seq([p_string(<<"$">>), p_zero_or_more(p_seq([p_not(fun 'line_break'/2), p_anything()])), fun 'line_break'/2]))(I,D) end, fun(Node, Idx) ->transform('command', Node, Idx) end).

-spec 'decorated_question'(input(), index()) -> parse_result().
'decorated_question'(Input, Index) ->
  p(Input, Index, 'decorated_question', fun(I,D) -> (p_seq([p_optional(fun 'comment'/2), p_optional(fun 'title'/2), p_optional(fun 'markup'/2), fun 'question'/2, p_optional(fun 'nbsp'/2)]))(I,D) end, fun(Node, Idx) ->transform('decorated_question', Node, Idx) end).

-spec 'question'(input(), index()) -> parse_result().
'question'(Input, Index) ->
  p(Input, Index, 'question', fun(I,D) -> (p_choose([fun 'essay_question'/2, fun 'true_false_question'/2, fun 'matching_question'/2, fun 'fill_in_question'/2, fun 'short_answer_question'/2, fun 'multiple_choice_question'/2, fun 'numeric_question'/2, fun 'description'/2]))(I,D) end, fun(Node, Idx) ->transform('question', Node, Idx) end).

-spec 'essay_question'(input(), index()) -> parse_result().
'essay_question'(Input, Index) ->
  p(Input, Index, 'essay_question', fun(I,D) -> (p_seq([fun 'question_text'/2, p_string(<<"{">>), p_optional(fun 'nbsp'/2), p_string(<<"}">>)]))(I,D) end, fun(Node, Idx) ->transform('essay_question', Node, Idx) end).

-spec 'true_false_question'(input(), index()) -> parse_result().
'true_false_question'(Input, Index) ->
  p(Input, Index, 'true_false_question', fun(I,D) -> (p_seq([fun 'question_text'/2, p_string(<<"{">>), p_optional(fun 'space'/2), p_choose([fun 'true_answer'/2, fun 'false_answer'/2]), p_optional(fun 'space'/2), p_string(<<"}">>)]))(I,D) end, fun(Node, Idx) ->transform('true_false_question', Node, Idx) end).

-spec 'matching_question'(input(), index()) -> parse_result().
'matching_question'(Input, Index) ->
  p(Input, Index, 'matching_question', fun(I,D) -> (p_seq([fun 'question_text'/2, p_string(<<"{">>), p_optional(fun 'space'/2), p_one_or_more(fun 'match_answer'/2), p_optional(fun 'space'/2), p_string(<<"}">>)]))(I,D) end, fun(Node, Idx) ->transform('matching_question', Node, Idx) end).

-spec 'fill_in_question'(input(), index()) -> parse_result().
'fill_in_question'(Input, Index) ->
  p(Input, Index, 'fill_in_question', fun(I,D) -> (p_seq([fun 'question_text'/2, p_string(<<"{">>), p_optional(fun 'space'/2), p_one_or_more(p_choose([fun 'wrong_answer'/2, fun 'right_answer'/2])), p_optional(fun 'space'/2), p_string(<<"}">>), p_one_or_more(p_seq([p_not(p_charclass(<<"[\r\n]">>)), p_choose([fun 'escaped_symbol'/2, p_anything()])]))]))(I,D) end, fun(Node, Idx) ->transform('fill_in_question', Node, Idx) end).

-spec 'short_answer_question'(input(), index()) -> parse_result().
'short_answer_question'(Input, Index) ->
  p(Input, Index, 'short_answer_question', fun(I,D) -> (p_seq([fun 'question_text'/2, p_string(<<"{">>), p_optional(fun 'space'/2), p_one_or_more(fun 'right_answer'/2), p_optional(fun 'space'/2), p_string(<<"}">>)]))(I,D) end, fun(Node, Idx) ->transform('short_answer_question', Node, Idx) end).

-spec 'multiple_choice_question'(input(), index()) -> parse_result().
'multiple_choice_question'(Input, Index) ->
  p(Input, Index, 'multiple_choice_question', fun(I,D) -> (p_seq([fun 'question_text'/2, p_string(<<"{">>), p_optional(fun 'space'/2), p_one_or_more(p_choose([fun 'right_answer'/2, fun 'wrong_answer'/2])), p_optional(fun 'space'/2), p_string(<<"}">>)]))(I,D) end, fun(Node, Idx) ->transform('multiple_choice_question', Node, Idx) end).

-spec 'numeric_question'(input(), index()) -> parse_result().
'numeric_question'(Input, Index) ->
  p(Input, Index, 'numeric_question', fun(I,D) -> (p_seq([fun 'question_text'/2, p_string(<<"{#">>), p_optional(fun 'space'/2), p_one_or_more(fun 'numeric_answer'/2), p_optional(fun 'space'/2), p_string(<<"}">>)]))(I,D) end, fun(Node, Idx) ->transform('numeric_question', Node, Idx) end).

-spec 'description'(input(), index()) -> parse_result().
'description'(Input, Index) ->
  p(Input, Index, 'description', fun(I,D) -> (fun 'question_text'/2)(I,D) end, fun(Node, Idx) ->transform('description', Node, Idx) end).

-spec 'question_text'(input(), index()) -> parse_result().
'question_text'(Input, Index) ->
  p(Input, Index, 'question_text', fun(I,D) -> (p_one_or_more(p_seq([p_not(p_string(<<"{">>)), fun 'escaped_text'/2])))(I,D) end, fun(Node, Idx) ->transform('question_text', Node, Idx) end).

-spec 'true_answer'(input(), index()) -> parse_result().
'true_answer'(Input, Index) ->
  p(Input, Index, 'true_answer', fun(I,D) -> (p_seq([p_choose([p_string(<<"TRUE">>), p_string(<<"T">>)]), p_optional(fun 'space'/2), p_optional(fun 'feedback'/2)]))(I,D) end, fun(Node, Idx) ->transform('true_answer', Node, Idx) end).

-spec 'false_answer'(input(), index()) -> parse_result().
'false_answer'(Input, Index) ->
  p(Input, Index, 'false_answer', fun(I,D) -> (p_seq([p_choose([p_string(<<"FALSE">>), p_string(<<"F">>)]), p_optional(fun 'space'/2), p_optional(fun 'feedback'/2)]))(I,D) end, fun(Node, Idx) ->transform('false_answer', Node, Idx) end).

-spec 'wrong_answer'(input(), index()) -> parse_result().
'wrong_answer'(Input, Index) ->
  p(Input, Index, 'wrong_answer', fun(I,D) -> (p_seq([p_string(<<"~">>), p_optional(fun 'weight'/2), fun 'escaped_text'/2, p_optional(fun 'feedback'/2)]))(I,D) end, fun(Node, Idx) ->transform('wrong_answer', Node, Idx) end).

-spec 'right_answer'(input(), index()) -> parse_result().
'right_answer'(Input, Index) ->
  p(Input, Index, 'right_answer', fun(I,D) -> (p_seq([p_string(<<"=">>), p_optional(fun 'weight'/2), fun 'escaped_text'/2, p_optional(fun 'feedback'/2)]))(I,D) end, fun(Node, Idx) ->transform('right_answer', Node, Idx) end).

-spec 'match_answer'(input(), index()) -> parse_result().
'match_answer'(Input, Index) ->
  p(Input, Index, 'match_answer', fun(I,D) -> (p_seq([p_string(<<"=">>), p_zero_or_more(p_seq([p_not(p_string(<<"->">>)), p_not(p_charclass(<<"[=}#]">>)), p_choose([fun 'escaped_symbol'/2, p_anything()])])), p_string(<<"->">>), p_zero_or_more(p_seq([p_not(p_charclass(<<"[=}#]">>)), p_choose([fun 'escaped_symbol'/2, p_anything()])])), p_optional(fun 'space'/2)]))(I,D) end, fun(Node, Idx) ->transform('match_answer', Node, Idx) end).

-spec 'numeric_answer'(input(), index()) -> parse_result().
'numeric_answer'(Input, Index) ->
  p(Input, Index, 'numeric_answer', fun(I,D) -> (p_seq([p_optional(p_string(<<"=">>)), p_optional(fun 'weight'/2), p_choose([fun 'numeric_with_tolerance'/2, fun 'range'/2, fun 'number'/2]), p_optional(fun 'space'/2), p_optional(fun 'feedback'/2)]))(I,D) end, fun(Node, Idx) ->transform('numeric_answer', Node, Idx) end).

-spec 'comment'(input(), index()) -> parse_result().
'comment'(Input, Index) ->
  p(Input, Index, 'comment', fun(I,D) -> (p_seq([p_string(<<"\/\/">>), p_zero_or_more(p_seq([p_not(fun 'line_break'/2), p_anything()])), fun 'line_break'/2, p_optional(fun 'comment'/2)]))(I,D) end, fun(Node, Idx) ->transform('comment', Node, Idx) end).

-spec 'title'(input(), index()) -> parse_result().
'title'(Input, Index) ->
  p(Input, Index, 'title', fun(I,D) -> (p_seq([p_string(<<"::">>), fun 'escaped_text'/2, p_string(<<"::">>), p_optional(p_one_or_more(fun 'line_break'/2))]))(I,D) end, fun(Node, Idx) ->transform('title', Node, Idx) end).

-spec 'markup'(input(), index()) -> parse_result().
'markup'(Input, Index) ->
  p(Input, Index, 'markup', fun(I,D) -> (p_seq([p_string(<<"[">>), p_choose([p_string(<<"plain">>), p_string(<<"textile">>), p_string(<<"html">>), p_string(<<"LaTeX">>), p_string(<<"markdown">>)]), p_string(<<"]">>)]))(I,D) end, fun(Node, Idx) ->transform('markup', Node, Idx) end).

-spec 'range'(input(), index()) -> parse_result().
'range'(Input, Index) ->
  p(Input, Index, 'range', fun(I,D) -> (p_seq([fun 'number'/2, p_string(<<"..">>), fun 'number'/2]))(I,D) end, fun(Node, Idx) ->transform('range', Node, Idx) end).

-spec 'numeric_with_tolerance'(input(), index()) -> parse_result().
'numeric_with_tolerance'(Input, Index) ->
  p(Input, Index, 'numeric_with_tolerance', fun(I,D) -> (p_seq([fun 'number'/2, p_string(<<":">>), fun 'number'/2]))(I,D) end, fun(Node, Idx) ->transform('numeric_with_tolerance', Node, Idx) end).

-spec 'feedback'(input(), index()) -> parse_result().
'feedback'(Input, Index) ->
  p(Input, Index, 'feedback', fun(I,D) -> (p_seq([p_string(<<"#">>), fun 'escaped_text'/2]))(I,D) end, fun(Node, Idx) ->transform('feedback', Node, Idx) end).

-spec 'weight'(input(), index()) -> parse_result().
'weight'(Input, Index) ->
  p(Input, Index, 'weight', fun(I,D) -> (p_seq([p_string(<<"%">>), fun 'number'/2, p_string(<<"%">>)]))(I,D) end, fun(Node, Idx) ->transform('weight', Node, Idx) end).

-spec 'number'(input(), index()) -> parse_result().
'number'(Input, Index) ->
  p(Input, Index, 'number', fun(I,D) -> (p_seq([fun 'int'/2, p_optional(fun 'frac'/2)]))(I,D) end, fun(Node, Idx) ->transform('number', Node, Idx) end).

-spec 'int'(input(), index()) -> parse_result().
'int'(Input, Index) ->
  p(Input, Index, 'int', fun(I,D) -> (p_choose([p_seq([p_optional(p_string(<<"-">>)), p_seq([fun 'non_zero_digit'/2, p_one_or_more(fun 'digit'/2)])]), p_seq([p_optional(p_string(<<"-">>)), fun 'digit'/2])]))(I,D) end, fun(Node, Idx) ->transform('int', Node, Idx) end).

-spec 'frac'(input(), index()) -> parse_result().
'frac'(Input, Index) ->
  p(Input, Index, 'frac', fun(I,D) -> (p_seq([p_string(<<".">>), p_one_or_more(fun 'digit'/2)]))(I,D) end, fun(Node, Idx) ->transform('frac', Node, Idx) end).

-spec 'non_zero_digit'(input(), index()) -> parse_result().
'non_zero_digit'(Input, Index) ->
  p(Input, Index, 'non_zero_digit', fun(I,D) -> (p_charclass(<<"[1-9]">>))(I,D) end, fun(Node, Idx) ->transform('non_zero_digit', Node, Idx) end).

-spec 'digit'(input(), index()) -> parse_result().
'digit'(Input, Index) ->
  p(Input, Index, 'digit', fun(I,D) -> (p_charclass(<<"[0-9]">>))(I,D) end, fun(Node, Idx) ->transform('digit', Node, Idx) end).

-spec 'escaped_text'(input(), index()) -> parse_result().
'escaped_text'(Input, Index) ->
  p(Input, Index, 'escaped_text', fun(I,D) -> (p_seq([p_one_or_more(p_seq([p_not(p_charclass(<<"[=~{}}#:]">>)), p_choose([fun 'escaped_symbol'/2, p_anything()])])), p_optional(fun 'space'/2)]))(I,D) end, fun(Node, Idx) ->transform('escaped_text', Node, Idx) end).

-spec 'escaped_symbol'(input(), index()) -> parse_result().
'escaped_symbol'(Input, Index) ->
  p(Input, Index, 'escaped_symbol', fun(I,D) -> (p_seq([p_string(<<"\\">>), p_charclass(<<"[={}~#:]">>)]))(I,D) end, fun(Node, Idx) ->transform('escaped_symbol', Node, Idx) end).

-spec 'blank_line'(input(), index()) -> parse_result().
'blank_line'(Input, Index) ->
  p(Input, Index, 'blank_line', fun(I,D) -> (p_seq([fun 'line_break'/2, p_optional(fun 'nbsp'/2), fun 'line_break'/2, p_optional(fun 'space'/2)]))(I,D) end, fun(_Node, _Idx) ->blank_line end).

-spec 'nbsp'(input(), index()) -> parse_result().
'nbsp'(Input, Index) ->
  p(Input, Index, 'nbsp', fun(I,D) -> (p_one_or_more(p_charclass(<<"[\s\t\s]">>)))(I,D) end, fun(_Node, _Idx) ->nbsp end).

-spec 'line_break'(input(), index()) -> parse_result().
'line_break'(Input, Index) ->
  p(Input, Index, 'line_break', fun(I,D) -> (p_choose([p_string(<<"\r\n">>), p_string(<<"\n">>)]))(I,D) end, fun(_Node, _Idx) ->line_break end).

-spec 'space'(input(), index()) -> parse_result().
'space'(Input, Index) ->
  p(Input, Index, 'space', fun(I,D) -> (p_one_or_more(p_choose([fun 'nbsp'/2, fun 'line_break'/2])))(I,D) end, fun(_Node, _Idx) ->space end).


transform(Symbol,Node,Index) -> gift_transform:transform(Symbol, Node, Index).
-file("peg_includes.hrl", 1).
-type index() :: {{line, pos_integer()}, {column, pos_integer()}}.
-type input() :: binary().
-type parse_failure() :: {fail, term()}.
-type parse_success() :: {term(), input(), index()}.
-type parse_result() :: parse_failure() | parse_success().
-type parse_fun() :: fun((input(), index()) -> parse_result()).
-type xform_fun() :: fun((input(), index()) -> term()).

-spec p(input(), index(), atom(), parse_fun(), xform_fun()) -> parse_result().
p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
  case get_memo(StartIndex, Name) of      % See if the current reduction is memoized
    {ok, Memo} -> %Memo;                     % If it is, return the stored result
      Memo;
    _ ->                                        % If not, attempt to parse
      Result = case ParseFun(Inp, StartIndex) of
        {fail,_} = Failure ->                       % If it fails, memoize the failure
          Failure;
        {Match, InpRem, NewIndex} ->               % If it passes, transform and memoize the result.
          Transformed = TransformFun(Match, StartIndex),
          {Transformed, InpRem, NewIndex}
      end,
      memoize(StartIndex, Name, Result),
      Result
  end.

-spec setup_memo() -> ets:tid().
setup_memo() ->
  put({parse_memo_table, ?MODULE}, ets:new(?MODULE, [set])).

-spec release_memo() -> true.
release_memo() ->
  ets:delete(memo_table_name()).

-spec memoize(index(), atom(), parse_result()) -> true.
memoize(Index, Name, Result) ->
  Memo = case ets:lookup(memo_table_name(), Index) of
              [] -> [];
              [{Index, Plist}] -> Plist
         end,
  ets:insert(memo_table_name(), {Index, [{Name, Result}|Memo]}).

-spec get_memo(index(), atom()) -> {ok, term()} | {error, not_found}.
get_memo(Index, Name) ->
  case ets:lookup(memo_table_name(), Index) of
    [] -> {error, not_found};
    [{Index, Plist}] ->
      case proplists:lookup(Name, Plist) of
        {Name, Result}  -> {ok, Result};
        _  -> {error, not_found}
      end
    end.

-spec memo_table_name() -> ets:tid().
memo_table_name() ->
    get({parse_memo_table, ?MODULE}).

-ifdef(p_eof).
-spec p_eof() -> parse_fun().
p_eof() ->
  fun(<<>>, Index) -> {eof, [], Index};
     (_, Index) -> {fail, {expected, eof, Index}} end.
-endif.

-ifdef(p_optional).
-spec p_optional(parse_fun()) -> parse_fun().
p_optional(P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end.
-endif.

-ifdef(p_not).
-spec p_not(parse_fun()) -> parse_fun().
p_not(P) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {fail,_} ->
          {[], Input, Index};
        {Result, _, _} -> {fail, {expected, {no_match, Result},Index}}
      end
  end.
-endif.

-ifdef(p_assert).
-spec p_assert(parse_fun()) -> parse_fun().
p_assert(P) ->
  fun(Input,Index) ->
      case P(Input,Index) of
        {fail,_} = Failure-> Failure;
        _ -> {[], Input, Index}
      end
  end.
-endif.

-ifdef(p_seq).
-spec p_seq([parse_fun()]) -> parse_fun().
p_seq(P) ->
  fun(Input, Index) ->
      p_all(P, Input, Index, [])
  end.

-spec p_all([parse_fun()], input(), index(), [term()]) -> parse_result().
p_all([], Inp, Index, Accum ) -> {lists:reverse( Accum ), Inp, Index};
p_all([P|Parsers], Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail, _} = Failure -> Failure;
    {Result, InpRem, NewIndex} -> p_all(Parsers, InpRem, NewIndex, [Result|Accum])
  end.
-endif.

-ifdef(p_choose).
-spec p_choose([parse_fun()]) -> parse_fun().
p_choose(Parsers) ->
  fun(Input, Index) ->
      p_attempt(Parsers, Input, Index, none)
  end.

-spec p_attempt([parse_fun()], input(), index(), none | parse_failure()) -> parse_result().
p_attempt([], _Input, _Index, Failure) -> Failure;
p_attempt([P|Parsers], Input, Index, FirstFailure)->
  case P(Input, Index) of
    {fail, _} = Failure ->
      case FirstFailure of
        none -> p_attempt(Parsers, Input, Index, Failure);
        _ -> p_attempt(Parsers, Input, Index, FirstFailure)
      end;
    Result -> Result
  end.
-endif.

-ifdef(p_zero_or_more).
-spec p_zero_or_more(parse_fun()) -> parse_fun().
p_zero_or_more(P) ->
  fun(Input, Index) ->
      p_scan(P, Input, Index, [])
  end.
-endif.

-ifdef(p_one_or_more).
-spec p_one_or_more(parse_fun()) -> parse_fun().
p_one_or_more(P) ->
  fun(Input, Index)->
      Result = p_scan(P, Input, Index, []),
      case Result of
        {[_|_], _, _} ->
          Result;
        _ ->
          {fail, {expected, Failure, _}} = P(Input,Index),
          {fail, {expected, {at_least_one, Failure}, Index}}
      end
  end.
-endif.

-ifdef(p_label).
-spec p_label(atom(), parse_fun()) -> parse_fun().
p_label(Tag, P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} = Failure ->
           Failure;
        {Result, InpRem, NewIndex} ->
          {{Tag, Result}, InpRem, NewIndex}
      end
  end.
-endif.

-ifdef(p_scan).
-spec p_scan(parse_fun(), input(), index(), [term()]) -> {[term()], input(), index()}.
p_scan(_, <<>>, Index, Accum) -> {lists:reverse(Accum), <<>>, Index};
p_scan(P, Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail,_} -> {lists:reverse(Accum), Inp, Index};
    {Result, InpRem, NewIndex} -> p_scan(P, InpRem, NewIndex, [Result | Accum])
  end.
-endif.

-ifdef(p_string).
-spec p_string(binary()) -> parse_fun().
p_string(S) ->
    Length = erlang:byte_size(S),
    fun(Input, Index) ->
      try
          <<S:Length/binary, Rest/binary>> = Input,
          {S, Rest, p_advance_index(S, Index)}
      catch
          error:{badmatch,_} -> {fail, {expected, {string, S}, Index}}
      end
    end.
-endif.

-ifdef(p_anything).
-spec p_anything() -> parse_fun().
p_anything() ->
  fun(<<>>, Index) -> {fail, {expected, any_character, Index}};
     (Input, Index) when is_binary(Input) ->
          <<C/utf8, Rest/binary>> = Input,
          {<<C/utf8>>, Rest, p_advance_index(<<C/utf8>>, Index)}
  end.
-endif.

-ifdef(p_charclass).
-spec p_charclass(string() | binary()) -> parse_fun().
p_charclass(Class) ->
    {ok, RE} = re:compile(Class, [unicode, dotall]),
    fun(Inp, Index) ->
            case re:run(Inp, RE, [anchored]) of
                {match, [{0, Length}|_]} ->
                    {Head, Tail} = erlang:split_binary(Inp, Length),
                    {Head, Tail, p_advance_index(Head, Index)};
                _ -> {fail, {expected, {character_class, binary_to_list(Class)}, Index}}
            end
    end.
-endif.

-ifdef(p_regexp).
-spec p_regexp(binary()) -> parse_fun().
p_regexp(Regexp) ->
    {ok, RE} = re:compile(Regexp, [unicode, dotall, anchored]),
    fun(Inp, Index) ->
        case re:run(Inp, RE) of
            {match, [{0, Length}|_]} ->
                {Head, Tail} = erlang:split_binary(Inp, Length),
                {Head, Tail, p_advance_index(Head, Index)};
            _ -> {fail, {expected, {regexp, binary_to_list(Regexp)}, Index}}
        end
    end.
-endif.

-ifdef(line).
-spec line(index() | term()) -> pos_integer() | undefined.
line({{line,L},_}) -> L;
line(_) -> undefined.
-endif.

-ifdef(column).
-spec column(index() | term()) -> pos_integer() | undefined.
column({_,{column,C}}) -> C;
column(_) -> undefined.
-endif.

-spec p_advance_index(input() | unicode:charlist() | pos_integer(), index()) -> index().
p_advance_index(MatchedInput, Index) when is_list(MatchedInput) orelse is_binary(MatchedInput)-> % strings
  lists:foldl(fun p_advance_index/2, Index, unicode:characters_to_list(MatchedInput));
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.
