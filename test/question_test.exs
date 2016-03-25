defmodule Giftex.QuestionTest do
  use ExUnit.Case

  alias Giftex.Question

  test "Answering a true false question correctly" do
    q = %{type: :true_false_question, answers: [{:true, 100}]}
    assert Question.check_answer(q, :true) == 100
  end

  test "Answering a true false question incorrectly" do
    q = %{type: :true_false_question, answers: [{:true, 100}]}
    assert Question.check_answer(q, :false) == 0
  end

  test "Answering a multiple choice question correctly" do
    q = %{type: :multiple_choice_question, answers: [{"A", 100}, {"B", 0}]}
    assert Question.check_answer(q, "A") == 100
  end

  test "Answering a multiple choice question incorrectly" do
    q = %{type: :multiple_choice_question, answers: [{"A", 100}, {"B", 0}]}
    assert Question.check_answer(q, "B") == 0
  end

  test "Answering a multiple choice question with a nonexistent answer" do
    q = %{type: :multiple_choice_question, answers: [{"A", 100}, {"B", 0}]}
    assert Question.check_answer(q, "C") == 0
  end

  test "Answering a fill in question correctly" do
    q = %{type: :fill_in_question, answers: [{"A", 100}]}
    assert Question.check_answer(q, "A") == 100
  end

  test "Answering a fill in question incorrectly" do
    q = %{type: :fill_in_question, answers: [{"A", 100}]}
    assert Question.check_answer(q, "X") == 0
  end

  test "Answering a matching question correctly" do
    q = %{type: :matching_question, answers: [{"foo", "bar"}, {"bar", "baz"}]}
    assert Question.check_answer(q, [{"foo", "bar"}, {"bar", "baz"}]) == 100
  end

  test "Answering a matching question incorrectly" do
    q = %{type: :matching_question, answers: [{"foo", "bar"}, {"bar", "baz"}]}
    assert Question.check_answer(q, [{"foo", "baz"}, {"bar", "bar"}]) == 0
  end

  test "Answering a matching question with too few pairs" do
    q = %{type: :matching_question, answers: [{"foo", "bar"}, {"bar", "baz"}]}
    assert Question.check_answer(q, [{"foo", "bar"}]) == 0
  end

  test "Answering a matching question with too many pairs" do
    q = %{type: :matching_question, answers: [{"foo", "bar"}, {"bar", "baz"}]}
    assert Question.check_answer(q, [{"foo", "baz"}, {"bar", "bar"}, {"frob", "buzz"}]) == 0
  end

  test "Answering a numeric question correctly" do
    q = %{type: :numeric_question, answers: [{{1,10},100}]}
    assert Question.check_answer(q, 7) == 100
  end

  test "Answering a numeric question incorrectly" do
    q = %{type: :numeric_question, answers: [{{1,10},100}]}
    assert Question.check_answer(q, 11) == 0
    assert Question.check_answer(q, 0) == 0
    assert Question.check_answer(q, -2.34) == 0
  end

  test "Answering a numeric question with multiple choices correctly" do
    q = %{type: :numeric_question, answers: [{{3.1415,3.1416},100}, {{3,4}, 50}]}
    assert Question.check_answer(q, 3.14159) == 100
    assert Question.check_answer(q, 3.2) == 50
  end
end
