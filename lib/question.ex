defmodule Giftex.Question do
  def check_answer(%{type: :matching_question, answers: answers}, given_answers) do
    do_check_matching(answers, given_answers)
  end

  def check_answer(%{answers: answers}, answer) do
    {_, score} = List.keyfind(answers, answer, 0, {nil, 0})
    score
  end

  defp do_check_matching([], []) do
    100
  end

  defp do_check_matching(answer, []) do
    0
  end

  defp do_check_matching(answers, [answer | rest]) do
    if :lists.member(answer, answers) do
      do_check_matching(:lists.delete(answer, answers), rest)
    else
      0
    end
  end
end
