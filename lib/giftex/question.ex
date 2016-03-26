defmodule Giftex.Question do
  def check_answer(%{type: :matching_question, answers: answers}, given_answers) do
    do_check_matching(answers, given_answers)
  end

  def check_answer(%{type: :numeric_question, answers: answers}, answer) do
    do_check_numeric(answers, answer)
  end

  def check_answer(%{type: :short_answer_question, answers: answers}, answer) do
    downcase_answers = Enum.map answers,
              fn
                {a,score} -> {String.downcase(a), score}
                {a, score, feedback} -> {String.downcase(a), score, feedback}
              end
    check_answer(%{answers: downcase_answers}, String.downcase(answer))
  end

  def check_answer(%{type: :essay}, _answer) do
    0
  end

  def check_answer(%{answers: answers}, given_answers) when is_list(given_answers) do
    do_check_multiple_answers(answers, given_answers, 0, [])
  end

  def check_answer(%{answers: answers}, answer) do
    case List.keyfind(answers, answer, 0, {nil, 0}) do
      {_, score} -> score
      {_, score, feedback} -> {score, feedback}
    end
  end

  defp do_check_multiple_answers(_answers, [], total_score, []) do
    total_score
  end

  defp do_check_multiple_answers(_answers, [], total_score, total_feedback) do
    {total_score, total_feedback}
  end

  defp do_check_multiple_answers(answers, [answer | rest], total_score, total_feedback) do
    case List.keyfind(answers, answer, 0, {nil, 0}) do
      {_, score} ->  do_check_multiple_answers(answers, rest, total_score + score, total_feedback)
      {_, score, feedback} -> do_check_multiple_answers(answers, rest, total_score + score, total_feedback ++ [feedback])
    end
  end

  defp do_check_matching([], []) do
    100
  end

  defp do_check_matching(_answer, []) do
    0
  end

  defp do_check_matching(answers, [answer | rest]) do
    if :lists.member(answer, answers) do
      do_check_matching(:lists.delete(answer, answers), rest)
    else
      0
    end
  end

  defp do_check_numeric(answers, answer) do
    {_, score} = Enum.find(answers, {nil,0},
                  fn {{min, max},_} -> answer >= min && answer <= max
                     {val, _}       -> answer == val
                  end)
    score
  end
end
